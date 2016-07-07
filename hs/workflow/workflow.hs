module WorkflowLib where

import Text.Show.Functions
import Data.List 

class Engine e where
    register :: e -> Bool

class Node a where
    place :: a -> Bool


type Name              = String
type ActivityRelation  = (Activity, Activity)
type ActivityCondition = (ActivityRelation -> Bool)
type Role              = String
type Date              = (Integer, Int, Int)
--type Group             = String
--type Permission        = (User -> Bool)
data State             = Initiated | Active | Terminated | Suspended | Running | Complete  
                       deriving (Show)
data User = User
    { userName  :: Name
    , userRoles :: [Role]
    } deriving (Show)


data Task = Task 
    { taskName       :: Name
    , taskPriority   :: Double
    , taskValue      :: Double
    , taskDue        :: Date
    , taskImportance :: Double
    , taskUrgency    :: Double
    } deriving (Show)


data Activity = Activity
    { activityName   :: Name
    , activityRole   :: Role
--    , activityStatus :: State
    --, activityTask :: Task
    } deriving (Show, Eq)

data Process = Process 
    { processName        :: Name
    , processTransitions :: [Transition]
    } deriving (Show)


data ProcessInstance = ProcessInstance 
    { processInstanceName      :: Name
    , processInstanceProcess   :: Process
    , processInstanceStartedBy :: User
    , processInstanceStatus    :: State
    , processInstanceWorkitems :: [WorkItem]
    } deriving (Show)


data WorkItem = WorkItem
    { workitemName            :: Name
    , workitemUser            :: Maybe String
    , workitemProcessInstance :: Maybe ProcessInstance
    , workitemActivity        :: Maybe Activity
    , workitemFrom            :: Maybe WorkItem
    } deriving (Show)


data Transition = Transition 
    { transitionName         :: Name
    , transitionRelation     :: ActivityRelation
    , transitionCondition    :: Maybe ActivityCondition
    } deriving (Show)


u1 = User 
    { userName  = "sa"
    , userRoles = ["requestor", "employee"]
    }

s1 = Activity 
    { activityName  = "start" 
    , activityRole  = "requestor"
    --, activityState = Initiated
    }

a1 = Activity 
    { activityName = "submit complaint"
    , activityRole = "employee"
    }

a2 = Activity 
    { activityName = "review complaint"
    , activityRole = "manager"
    }

a3 = Activity
    { activityName = "action complaint"
    , activityRole = "manager"
    }

a4 = Activity 
    { activityName = "reject complaint"
    , activityRole = "manager"
    }


e1 = Activity
    { activityName = "end" 
    , activityRole = "requestor"
    }

t1 = Transition 
    { transitionName       = "initiate"
    , transitionRelation   = (s1, a1)
    , transitionCondition  = Just always
    }

t2 = Transition 
    { transitionName       = "process"
    , transitionRelation   = (a1, a2)
    , transitionCondition  = Just always
    }

t3 = Transition 
    { transitionName       = "action"
    , transitionRelation   = (a2, a3)
    , transitionCondition  = Nothing
    }

t4 = Transition 
    { transitionName       = "reject"
    , transitionRelation   = (a2, a4)
    , transitionCondition  = Nothing
    }

t5 = Transition 
    { transitionName       = "finalize1"
    , transitionRelation   = (a3, e1)
    , transitionCondition  = Nothing
    }

t6 = Transition 
    { transitionName       = "finalize2"
    , transitionRelation   = (a4, e1)
    , transitionCondition  = Nothing
    }


p1 = Process 
    { processName="p1"
    , processTransitions   = [t1, t2, t3, t4, t5, t6]
    }

always :: ActivityRelation -> Bool
always (from, to) = True

start :: Process -> (Transition -> IO a) -> IO ()
start p f = mapM_ f (processTransitions p)

next :: Transition -> IO ()
next t = case transitionCondition t of 
    Nothing -> putStrLn "."
    Just cond -> if cond (transitionRelation t) then do
        putStrLn (activityName from ++ " -> " ++ activityName to)
        else putStrLn "-"
    where
        from = fst $ pair
        to   = snd $ pair
        pair = transitionRelation t


processActivities :: Process -> [Activity]
processActivities p = nub (left ++ right)
    where left = map (fst . transitionRelation) transitions
          right = map (snd . transitionRelation) transitions
          transitions = processTransitions p

disp p = strip txt arrow
    where 
        txt = concat $ map ((++ arrow) . activityName) (processActivities p)
        arrow = " --> "
        strip s w = slice 0 (length s - length w - 1) s 
        slice from to xs = take (to - from + 1) (drop from xs)

main = start p1 next
