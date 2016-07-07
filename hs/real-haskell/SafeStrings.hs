module SafeStrings

where

class Language l where
    litfrag  :: String -> l   -- String is a literal language fragment
    littext  :: String -> l   -- String is literal text
    natrep   :: l -> String   -- Gets the native-language representation
    language :: l -> String   -- Gets the name of the language

data Language l => SafeString l
    = SSEmpty
    | SSFragment l
    | SSCat (SafeString l) (SafeString l)

empty      :: Language l => SafeString l
empty       = SSEmpty

frag, text :: Language l => String -> SafeString l
frag f      = SSFragment (litfrag f)
text s      = SSFragment (littext s)


-- join two SafeStrings of the same language
(+++) :: Language l => SafeString l -> SafeString l -> SafeString l
(+++)  = SSCat

-- join a list of same-language SafeStrings
cat   :: Language l => [SafeString l] -> SafeString l
cat    = foldr (+++) empty

render ss = renders ss ""

renders SSEmpty        = id
renders (SSFragment a) = (natrep a ++)
renders (SSCat l r)    = renders l . renders r

instance Language l => Show (SafeString l) where
    showsPrec _ ss =
        (lang ss ++) . (":\"" ++) . renders ss . ('"':)

lang ss =
    let SSFragment e = ss in language (undefined `asTypeOf` e)






