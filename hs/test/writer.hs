module MWriter where

import Control.Monad.Writer

-- this is the format of our log entries
data Entry = Log {timestamp::ClockTime, msg::String} deriving Eq
type Rule = String
type Packet = String

instance Show Entry where
  show (Log t s) = (show t) ++ " | " ++ s

-- this is the combined monad type
type LogWriter a = WriterT [Entry] IO a

-- add a message to the log
logMsg :: String -> LogWriter ()
logMsg s = do t <- liftIO getClockTime
              tell [Log t s]

-- this handles one packet
filterOne :: [Rule] -> Packet -> LogWriter (Maybe Packet)
filterOne rules packet = do rule <- return (match rules packet)
                            case rule of
                              Nothing  -> do logMsg ("DROPPING UNMATCHED PACKET: " ++ (show packet))
                                             return Nothing
                              (Just r) -> do when (logIt r) (logMsg ("MATCH: " ++ (show r) ++ " <=> " ++ (show packet)))
                                             case r of
                                               (Rule Accept _ _) -> return (Just packet)
                                               (Rule Reject _ _) -> return Nothing

-- this filters a list of packets, producing a filtered packet list
-- and a log of the activity
filterAll :: [Rule] -> [Packet] -> LogWriter [Packet]
filterAll rules packets = do logMsg "STARTING PACKET FILTER"
                             out <- mapM (filterOne rules) packets
                             logMsg "STOPPING PACKET FILTER"
                             return (catMaybes out)

-- read the rule data from the file named in the first argument, and the packet data from
-- the file named in the second argument, and then print the accepted packets followed by
-- a log generated during the computation.
main :: IO ()
main = do args       <- getArgs
          ruleData   <- readFile (args!!0)
          packetData <- readFile (args!!1)
          let rules   = (read ruleData)::[Rule]
              packets = (read packetData)::[Packet]
          (out,log) <- runWriterT (filterAll rules packets)
          putStrLn "ACCEPTED PACKETS"
          putStr (unlines (map show out))
          putStrLn "\n\nFIREWALL LOG"
          putStr (unlines (map show log))
