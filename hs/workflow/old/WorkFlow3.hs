module WorkflowLib where

class Engine e where
    register :: e -> Bool

class Node a where
    place :: a -> Bool

-- main = putStrLn "Hello There"

type Name              = String
type ActivityType      = String
type ActivityCondition = (Activity -> Bool)

data Activity = Activity
    { activityName     :: Name
    , activityType     :: ActivityType
    , activityProcess  :: Maybe Process
    } deriving (Show)


data Process = Process 
    { processName          :: Name
    , processStartActivity :: Maybe Activity
    , processEndActivity   :: Maybe Activity
    , processInstances     :: [ProcessInstance]
    , processTransitions   :: [Transition]
    , processActivities    :: [Activity]
    } deriving (Show)


data ProcessInstance = ProcessInstance 
    { processInstanceName    :: Name
    , processInstanceProcess :: Maybe Process
    , user                   :: Maybe String
    , oldStatus              :: Maybe String
    , status                 :: Maybe String
    , content                :: Maybe String
    , workitems              :: [WorkItem]
    } deriving (Show)


data WorkItem = WorkItem
    { workitemName            :: Name
    , workitemUser            :: Maybe String
    , workitemProcessInstance :: Maybe ProcessInstance
    , workitemActivity        :: Maybe Activity
    , workitemFrom            :: Maybe WorkItem
    } deriving (Show)

data Transition = Transition 
    { transitionName         :: String
    , transitionFromActivity :: Maybe Activity
    , transitionCondition    :: Maybe ActivityCondition
    , transitionToActivity   :: Maybe Activity
    } deriving (Show)





a1 = Activity { activityName="a1" 
              , activityType="management"
              , activityProcess=Nothing
              }

p1 = Process { processName="p1"
             , processStartActivity=Just a1
             , processEndActivity=Nothing
             , processInstances=[] 
             , processTransitions=[]
             , processActivities=[]
             }


