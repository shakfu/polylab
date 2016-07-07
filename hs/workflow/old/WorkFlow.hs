module WorkflowLib where

class Engine e where
    register :: e -> Bool

-- main = putStrLn "Hello There"


data Activity = Activity
    { activityTitle    :: String
    , activityKind     :: Maybe String
    , activityProcess  :: Maybe Process
    }


data Process = Process {  
    processTitle :: String,
    startActivity :: Maybe Activity,
    endActivity :: Maybe Activity,
    processInstances :: Maybe [ProcessInstance],
    transitions :: Maybe [Transition],
    activities :: Maybe [Activity]
    }


data ProcessInstance = ProcessInstance {
    processInstanceTitle :: String,
    processInstanceProcess :: Maybe Process,
    user :: Maybe String,
    oldStatus :: Maybe String,
    status :: Maybe String,
    content :: Maybe String,
    workitems :: Maybe [WorkItem]
}


data WorkItem = WorkItem {
    workitemTitle :: String,
    workitemUser :: Maybe String,
    processInstance :: Maybe ProcessInstance,
    activity :: Maybe Activity,
    workitemFrom :: Maybe WorkItem
}

data Transition = Transition {
    transitionTitle :: String,
    fromActivity :: Maybe Activity,
    condition :: Maybe Bool,
    toActivity :: Maybe Activity
}

a1 = Activity {activityTitle="a1", 
               activityKind=Nothing, 
               activityProcess=Nothing}

p1 = Process {processTitle="p1", 
              startActivity=Just a1,
              endActivity=Nothing, 
              processInstances=Nothing, 
              transitions=Nothing,
              activities=Nothing}


