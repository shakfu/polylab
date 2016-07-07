module ProcessInstance where

import Process
import WorkItem

data ProcessInstance = ProcessInstance {
    title :: String,
    process :: Maybe Process,
    user :: Maybe String,
    old_status :: Maybe String,
    status :: Maybe String,
    content :: Maybe String,
    workitems :: [Maybe WorkItem]
}
