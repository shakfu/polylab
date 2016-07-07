module Activity where

import Process

data Activity = Activity {  
    title    :: String,
    kind     :: Maybe String,
    process  :: Maybe Process
}
