module Room where

import Item
import Direction
import Data.List

type RoomName = String


type Exit = (Direction, RoomName)


data Room
  = Room { rname :: RoomName
         , desc :: String
         , exits :: [Exit]
         , objects :: [ItemName] }
    deriving (Show, Eq)


{-'addItem' takes an ItemName and a Room as input, and 
  returns a new Room with that ItemName added to its objects record field.-}
addItem :: ItemName -> Room -> Room
addItem iname room = room { objects = iname : objects room }


{-'removeItem' takes an ItemName and a Room as input, 
  and returns a new Room with that ItemName removed from 
  its objects record field.-}
removeItem :: ItemName -> Room -> Room
removeItem iname room = 
  room { objects = filter (\x -> x/=iname) (objects room) }


--'hasObjects' is a predicate that checks whether the room has objects
hasObjects :: Room -> Bool
hasObjects room = objects room /= []

