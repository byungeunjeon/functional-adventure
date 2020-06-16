module GameState where

import Data.List
import Control.Exception
import qualified Data.Map as M
import Data.Either

import Item
import Room
import Player
import Direction


type GameMap = M.Map RoomName Room


data GameState
  = GameState { message :: Maybe String
              , gmap :: GameMap
              , universe :: Universe
              , player :: Player }
    deriving (Show)


--'mkMap' takes a list of Rooms and construct a GameMap
mkMap :: [Room] -> GameMap
mkMap lst = M.fromList (zip (map (\x -> rname x) lst) lst)


---- These getter functions are not the most elegant codes ------------
---- But they do the job ----------------------------------------------

data KeyError = KeyError
  deriving Show

instance Exception KeyError

--'getObject' is a getter for object within universe
getObjectUniv :: ItemName -> Universe -> Item
getObjectUniv iname u
  = case M.lookup iname u of
      Just obj -> obj
      Nothing -> throw KeyError


--'getObject' is a getter for object within gamestate
getObject :: ItemName -> GameState -> Item
getObject iname st = getObjectUniv iname (universe st)


--'getRoomMap' is a getter for room within gamemap
getRoomMap :: RoomName -> GameMap -> Room
getRoomMap rname mp
  = case M.lookup rname mp of
      Just room -> room
      Nothing -> throw KeyError


--'getRoom' is a getter for room within gamestate
getRoom :: RoomName -> GameState -> Room
getRoom name st = getRoomMap name (gmap st)
---------------------------------------------------------------------

{- 'setRoomMap' takes a room name, a room, and a game map as inputs. 
   Then it looks up the room in the input game map based on the input 
   room name. Then it replaces that room in the old game map with 
   the input room. -}
setRoomMap :: RoomName -> Room -> GameMap -> GameMap
setRoomMap rname newr gm = M.insert rname newr gm


{-'setMessage' takes a String and a GameState as an input, 
  then returns the same GameState 
  with its message field replaced with the input string.-}
setMessage :: String -> GameState -> GameState
setMessage msg gs = 
  if msg == ""
  then GameState (Nothing) (gmap gs) (universe gs) (player gs)
  else GameState (Just msg) (gmap gs) (universe gs) (player gs)


{-'currentInventory' takes a GameState as an input and 
  returns the inventory of that GameState's player.-}
currentInventory :: GameState -> [ItemName]
currentInventory gs = inventory $ player gs


{-'currentRoom' takes a GameState as an input, and 
  returns whatever room the Player is located at in that game state.-}
currentRoom :: GameState -> Room
currentRoom gs = let gmp = gmap gs
                     loc = location $ player gs
                 in getRoomMap loc gmp


{-'nearbyObjects' takes a game state as input and 
returns the list of item names in the room where the player is, 
in that game state. -}
nearbyObjects :: GameState -> [ItemName]
nearbyObjects gs = objects $ currentRoom gs


{-'takeItem' lets the player, pick up an item from 
  a room player is in and add it to player's inventory.-}
takeItem :: ItemName -> GameState -> GameState
takeItem iname gs = 
  let cond1 = isLeft $ alreadyHaveTakeCheck iname gs
      cond2 = isLeft $ inRoomTakeCheck iname gs
      cond3 = isLeft $ weightCheck iname gs
  in if cond1
     then gs { message = Just $ fromLeft " " $ alreadyHaveTakeCheck iname gs }
     else if cond2
     then gs { message = Just $ fromLeft " " $ inRoomTakeCheck iname gs }
     else if cond3
     then gs { message = Just $ fromLeft " " $ weightCheck iname gs }
     else 
       let msg = Just $ "You take the " ++ iname
           gmp = setRoomMap (rname $ currentRoom gs) 
                            (Room.removeItem iname $ currentRoom gs) 
                            (gmap gs)
           uni = (universe gs)
           ply = Player.addItem iname (player gs)
       in GameState msg gmp uni ply


{-'dropItem' lets the player, drop an item at 
  a room player is in and add it to player's inventory.-}
dropItem :: ItemName -> GameState -> GameState
dropItem iname gs = 
  let cond1 = isLeft $ anywhereDropCheck iname gs
      cond2 = isLeft $ inRoomDropCheck iname gs
  in if cond1
     then gs { message = Just $ fromLeft " " $ anywhereDropCheck iname gs }
     else if cond2
     then gs { message = Just $ fromLeft " " $ inRoomDropCheck iname gs }
     else 
       let msg = Just $ "You drop the " ++ iname ++ "."
           gmp = setRoomMap (rname $ currentRoom gs) 
                            (Room.addItem iname $ currentRoom gs) 
                            (gmap gs)
           uni = (universe gs)
           ply = Player.removeItem iname (player gs)
       in GameState msg gmp uni ply


{- 'inventoryWeight' takes a GameState as input, then return 
the total weight of the inventory that GameState's Player is carrying. -}
inventoryWeight :: GameState -> Integer
inventoryWeight gs = 
  let invlst = inventory $ player gs
      sumWeights = foldl (+) 0
  in sumWeights (map (\iname -> weight $ getObject iname gs) invlst)


-- Error Handling with Either: type alias for Error
type Error a = Either String a


{-'alreadyHaveTakeCheck' takes an ItemName and GameState as input, and 
  if the player is carrying the relevant item, it returns a Left value 
  containing the error message saying that you are already carrying 
  the item (where the item is named in the error message). 
  If the player is not carrying the item in the input game state, 
  it returns a Right value containing the input GameState.-}
alreadyHaveTakeCheck :: ItemName -> GameState -> Error GameState
alreadyHaveTakeCheck iname gs = 
  if foldl (||) False (map (\inv -> inv == iname) (currentInventory gs))
  then Left ("You are already carrying the " ++ iname ++ ".")
  else Right gs


{-'inRoomTakeCheck' takes an ItemName and GameState as inputs, and 
  if the room where the Player is contains the item, then it returns 
  a Right value containing the input state. Otherwise, it returns 
  a Left value containing an error message saying there is no such item 
  in the room (an error message mentioning the item by name).-}
inRoomTakeCheck :: ItemName -> GameState -> Error GameState
inRoomTakeCheck iname gs = 
  if foldl (||) False (map (\inv -> inv == iname) (nearbyObjects gs))
  then Right gs
  else Left ("There is no " ++ iname ++ " in this room.")


{-'weightCheck' checks to see if the Player is able to carry a new item, 
  based on the weight of what they're currently carrying.-}
weightCheck :: ItemName -> GameState -> Error GameState
weightCheck iname gs = 
  let newWeight = (inventoryWeight gs) + (weight $ getObject iname gs)
  in if newWeight > (maxWeight $ player gs)
     then Left "That's too much weight for you to carry."
     else Right gs


{-'anywhereDropCheck' checks on whether an item is either in the 
  player's inventory or in the room where the player is. -}
anywhereDropCheck :: ItemName -> GameState -> Error GameState
anywhereDropCheck iname gs = 
  let isInRoom = foldl (||) 
                       False 
                       (map (\inv -> inv == iname) (nearbyObjects gs))
      isInInventory = foldl (||) 
                            False 
                            (map (\inv -> inv == iname) 
                            (currentInventory gs))
  in if isInRoom || isInInventory
     then Right gs
     else Left ("What do you mean, drop the \"" ++ iname ++ "\"?")


{-checks to see whether the item the user asked to drop is present in 
  the room where the player is.-}
inRoomDropCheck :: ItemName -> GameState -> Error GameState
inRoomDropCheck iname gs = 
  if foldl (||) 
           False 
           (map (\inv -> inv == iname) 
           (nearbyObjects gs))
  then Left ("You aren't carrying the " ++ iname ++ ".")
  else Right gs


{-'roomHasObjects' takes a GameState as input and tells you whether 
  there are any objects in the room where the Player in that GameState is.-}
roomHasObjects :: GameState -> Bool
roomHasObjects gs = hasObjects $ getRoom (location . player $ gs) gs


--[helper] 'helperDestinationName' is a helper function for 'destinationName'
helperDestinationName :: Eq t => t -> [(t, p)] -> p
helperDestinationName d (x : xs) = if d == fst x then snd x 
                                   else helperDestinationName d xs


{-'destinationName' takes a Direction and a Room as inputs, 
  and if there is an exit in that direction in the input room, 
  it outputs the name of the room that you would end up in, 
  if you walked out of the input room in the input direction 
  (wrapped in a Just). Otherwise, it outputs Nothing.-}
destinationName :: Direction -> Room -> Maybe RoomName
destinationName dirc room = 
  if foldl (||) 
           False 
           (map (\(d, r) -> d == dirc) (exits room))
  then Just $ helperDestinationName dirc (exits room) 
  else Nothing


{-'move' takes a Direction and GameState as inputs, and if it is possible for 
  the player to move in that direction, returns a new game state with the 
  player in the new location, and a message field containing 
  Just "You go ...", where ... is the direction the player moves in to 
  get to the new location. If it is not possible for the player to move 
  in that direction, it returns the input game state, except with the 
  message field changed to Just "There is no exit in that direction.".-}
move :: Direction -> GameState -> GameState
move dirc gs = 
  let room = getRoom (location $ player gs) gs
  in case destinationName dirc room of
       Nothing -> gs { message = Just "There is no exit in that direction." }
       Just destin -> gs { message = Just $ "You go " ++ show dirc ++ ".", 
                           player = (player gs) { location = destin } }


{-'haveWonGame' returns True if the Player is located in the backyard and 
  is carrying the jug, and otherwise returns False. -}
haveWonGame :: GameState -> Bool
haveWonGame gs = 
  case location (player gs) of
    "backyard" -> foldl (||) 
                        False 
                        (map (\inv -> inv == "matchstick") 
                             (inventory $ player gs))
    otherwise -> False


{-'haveLostGame' returns True if the Player is carrying the ball, 
  and otherwise returns False. -}
haveLostGame :: GameState -> Bool
haveLostGame gs = foldl (||) 
                        False 
                        (map (\inv -> inv == "ball") (inventory $ player gs))