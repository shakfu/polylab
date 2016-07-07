module WorkItem where

import ProcessInstance
import Activity


data WorkItem = WorkItem {
    workitem_title :: String,
    workitem_user :: Maybe String,
    process_instance :: Maybe ProcessInstance,
    activity :: Maybe Activity,
    workitem_from :: Maybe WorkItem
}