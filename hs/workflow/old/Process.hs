module Process where

import Activity
import ProcessInstance
import Transition

data Process = Process {  
    title :: String,
    start_activity :: Maybe Activity,
    end_activity :: Maybe Activity,
    process_instances :: [Maybe ProcessInstance],
    transitions :: [Maybe Transition],
    activities :: [Maybe Activity]
}