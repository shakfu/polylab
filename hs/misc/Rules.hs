module Main where

import Prelude hiding (lookup)
import Data.Char
import Data.Maybe
import Control.Monad
import Control.Monad.Error
import Data.List hiding (lookup, insert)
import Data.Map as Map hiding (map, null, union, delete, mapMaybe, (\\))
import Text.Printf
import Text.Regex
import System.IO
 
data Answer = Yes | No | DontKnow
type Relation = Map Class [Class]
data Class = SomeClass Int | Class String
           deriving (Eq, Ord, Show)
 
data KB = KB {isDB :: Relation, hasDB :: Relation,
              isntDB :: Relation, lastID :: Int}
 
data Rule   = All | None | SomeAre | SomeArent
            deriving (Show)
data Action = Question Rule String String | Stmt Rule String String | Describe String
            deriving (Show)
 
(?) a  b c = if a then b else c
 
fail_msg    = "I didn't understand that phrase."
unknown :: String -> String
unknown str = printf "I don't know anything about %s." str
 
-- Find all the nodes that can be reached from 
-- the nodes in xs, using the given map.  
nodes_from :: [Class] -> Relation -> [Class]
nodes_from xs map = snd $ traverse [] xs [] where
  traverse visited [] rest = (visited, rest)
  traverse visited (x:xs) rest = case x `elem` visited of
    True  -> traverse visited xs rest
    False -> let (v2, branch) = traverse (x:visited) 
                                (fromMaybe [] $ lookup x map) rest2
                 (v3, rest2)  = traverse v2 xs rest
             in (v3, x:branch)
 
supersets x kb = nodes_from [Class x] (isDB kb)
 
overlapping_sets x kb = 
  let subsets = nodes_from [Class x] (hasDB kb)
  in nodes_from subsets (isDB kb)
 
complement_set x kb = 
  let compl = nub $ concatMap (\x -> fromMaybe [] $ lookup x (isntDB kb)) $
              supersets x kb
  in nodes_from compl (hasDB kb)
 
complement_subsets x kb =
  let compl = nub $ concatMap (\x -> fromMaybe [] $ lookup x (isntDB kb)) $
              nodes_from [Class x] (hasDB kb)
  in nodes_from compl (hasDB kb)
 
all_are x y kb    = (Class y) `elem` supersets x kb
no_are x y kb     = (Class y) `elem` complement_set x kb
some_are x y kb   = (Class y) `elem` overlapping_sets x kb
some_arent x y kb = no_are x y kb || 
                    (Class y) `elem` complement_subsets x kb
 
kbmember n kb = let 
  cls = Class (map toLower n) 
  in member cls (isDB kb) ||
     member cls (hasDB kb) ||
     member cls (isntDB kb)
 
parse_rule :: String -> KB -> Either String Action  
parse_rule inp kb = let
  str = map toLower $ unwords $ words inp
 
  check_word n = if kbmember n kb then 
                   Right n else Left (unknown n)
 
  check2 cons n m = do
    check_word n; check_word m
    Right (cons n m)
 
  -- try each combination of pairs agains the words
  -- in the knowledgebase
  try_pairs cons (n:m@(h:t)) = 
    check2 cons n (unwords m) `mplus`
    try_pairs cons ((n++" "++h):t)
  try_pairs cons _ = Left (unknown "such a thing")
 
  stmt rule rx =
    do [n, m] <- matchRegex (mkRegex rx) str
       Just $ Right $ Stmt rule n m
 
  question2 rule rx =
    do [n, m] <- matchRegex (mkRegex rx) str
       Just $ check2 (Question rule) n m
 
  question1 rule rx =
    do [s] <- matchRegex (mkRegex rx) str
       Just $ try_pairs (Question rule) (words s)
 
  describe = do [n] <- matchRegex (mkRegex "describe ([^.]+)\\.?") str
                Just $ if kbmember n kb 
                       then Right $ Describe n
                       else Left $ unknown n
 
  parsed = msum [
    stmt All "all (.+) are ([^.]+)\\.?",
    stmt None "no (.+) are ([^.]+)\\.?",
    stmt SomeArent "some (.+) are not ([^.]+)\\.?",
    stmt SomeAre "some (.+) are ([^.]+)\\.?",
    question2 SomeArent "are any (.+) not (.+)\\?",
    question1 All "are all (.+)\\?",
    question1 None "are no (.+)\\?",
    question1 SomeAre "are any (.+)\\?",
    describe]
 
  in fromMaybe (Left fail_msg) parsed
 
check_statement kb rule n m = case rule of
  All -> 
    all_are    n m kb ? Yes $
    some_arent n m kb ? No  $
    DontKnow
  None -> 
    no_are     n m kb ? Yes $
    some_are   n m kb ? No  $
    DontKnow
  SomeAre -> 
    some_are n m kb ? Yes $
    no_are   n m kb ? No  $
    DontKnow
  SomeArent -> 
    some_arent n m kb ? Yes $
    all_are n m kb    ? No  $
    DontKnow
 
show_confirm rule s s2 =
  case rule of
    All       -> printf "Yes, all %s are %s.\n" s s2
    None      -> printf "Yes, no %s are %s.\n" s s2
    SomeAre   -> printf "Yes, some %s are %s.\n" s s2
    SomeArent -> printf "Yes, some %s are not %s.\n" s s2
 
show_negate rule s s2 =
  case rule of
    All       -> printf "No, not all %s are %s.\n" s s2
    None      -> printf "No, some %s are %s.\n" s s2
    SomeAre   -> printf "No, no %s are %s.\n" s s2
    SomeArent -> printf "No, all %s are %s.\n" s s2
 
insert_rel :: Class -> Class -> Relation -> Relation
insert_rel n m db = insert n (m: (fromMaybe [] $ lookup n db)) db
 
insert_is_rel n m (KB isDB hasDB ndb l) =
  KB (insert_rel n m isDB) 
     (insert_rel m n hasDB)
     ndb l
 
insert_isnt_rel n m kb =  
  kb {isntDB = insert_rel m n $ 
               insert_rel n m (isntDB kb)}
 
new_id kb = (SomeClass $ lastID kb,
             kb {lastID = 1 + lastID kb})
 
add_rule (Stmt rule s t) kb = let
  n = Class s; m = Class t 
  in case rule of
    All       -> insert_is_rel n m kb
    None      -> kb {isntDB = insert_rel n m (isntDB kb)}
    SomeAre   -> let (it, kb') = new_id kb
                 in insert_is_rel it n $
                    insert_is_rel it m kb'
    SomeArent -> let (it, kb') = new_id kb
                 in insert_is_rel it n $
                    kb' {isntDB = insert_rel it m (isntDB kb)}
 
process_statement :: Action -> KB -> IO KB
process_statement (Question rule n m) kb = 
  do case check_statement kb rule n m of
       Yes -> show_confirm rule n m
       No  -> show_negate rule n m
       DontKnow -> putStrLn "I don't know."
     return kb
 
process_statement stmt@(Stmt rule n m) kb = 
  case check_statement kb rule n m of
    Yes -> do putStrLn "I know."; return kb
    No  -> do putStrLn "Sorry, that contradicts what I already know.";
              return kb
    DontKnow -> do putStrLn "OK."; return (add_rule stmt kb)
 
process_statement (Describe b) kb =
  let get_classes l = [x | (Class x) <- l]
      all  = delete b (get_classes $ supersets b kb)
      some = (get_classes $ overlapping_sets b kb) \\ (b:all)
      none = get_classes $ complement_set b kb
      somenot = (get_classes $ complement_subsets b kb) \\ (b:none)
  in do
    mapM_ (printf "All %s are %s.\n" b) all
    mapM_ (printf "Some %s are %s.\n" b) some
    mapM_ (printf "No %s are %s.\n" b) none
    mapM_ (printf "Some %s are not %s.\n" b) somenot
    return kb
 
main = respond (KB Map.empty Map.empty Map.empty 0) where
  respond :: KB -> IO ()
  respond kb = do
    putStr "> "; hFlush stdout
    l <- getLine
    case parse_rule l kb of
      Left err    -> do 
        putStrLn err; respond kb
      Right action -> do
        kb' <- process_statement action kb
        respond kb'
