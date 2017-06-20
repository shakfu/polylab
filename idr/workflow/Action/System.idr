module Action.System

import System.Info

%access public export

getInfo : String
getInfo = "OS: " ++ os ++ " Backend: " ++ backend
