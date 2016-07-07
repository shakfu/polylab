module Transition where

import Activity

data Transition = Transition {
    transition_title :: String,
    from_activity :: Maybe Activity,
    condition :: Maybe Bool,
    to_activity :: Maybe Activity
}