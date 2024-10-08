module Store where

type Store = { member :: String
             , friend :: String
             }

initialStore :: Store
initialStore = { member: "", friend: "" }
