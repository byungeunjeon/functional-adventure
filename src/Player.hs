module Player where

import Item
import Room

import Data.List


data Player
  = Player { inventory :: [ItemName]
            , maxWeight :: Integer
            , location :: RoomName }
    deriving (Show, Eq)


--'addItem' adds an ItemName to a player's inventory
addItem :: ItemName -> Player -> Player
addItem i p = p { inventory = i : inventory p }


{-'removeItem' it takes an ItemName and a player as input 
  and maps them to a new player with that ItemName 
  removed from their inventory -}
removeItem :: ItemName -> Player -> Player
removeItem i p = p { inventory = filter (\x -> x/=i) (inventory p) }


--'newLocation' changes the location of the player
newLocation :: RoomName -> Player -> Player
newLocation l p = p { location = l }


{-'isCarryingAnything' maps a player to True 
  if that player's inventory has something in it, 
  and False otherwise. -}
isCarryingAnything :: Player -> Bool
isCarryingAnything p 
  | inventory p == []
  = False
  | otherwise
  = True