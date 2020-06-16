module Initialize where

import qualified Data.Map as M

import Room
import Item
import Direction
import Player
import GameState


{- list of objects that will be used for an initial simple skeleton of 
the game that you can play and win for testing purposes. -}
pot :: Item
pot = Item "pot" 5

matchstick :: Item
matchstick = Item "matchstick" 2

sandbag :: Item
sandbag = Item "sandbag" 4

stove :: Item
stove = Item "stove" 20

couch :: Item
couch = Item "couch" 28

tarragon :: Item
tarragon = Item "tarragon" 8

beans :: Item
beans = Item "beans" 3

grill :: Item
grill = Item "grill" 22

bed :: Item
bed = Item "bed" 36

ball :: Item
ball = Item "ball" 8

bathtub :: Item
bathtub = Item "bathtub" 25


{- list of rooms that fit together to form the game map. -}
kitchen :: Room
kitchen = Room "kitchen" 
               "You are in a small kitchen" 
               [(N,"living room"), (E,"pantry"), (S,"backyard")] 
               ["pot", "stove"]

pantry :: Room
pantry = Room "pantry" 
              "You are in a pantry" 
              [(W,"kitchen"),(S,"garage"),(N,"shower room")] 
              ["tarragon", "beans"]

backyard :: Room
backyard = Room "backyard" 
                "You are in a backyard" 
                [(N,"kitchen"),(E,"garage")] 
                ["grill"]

livingRoom :: Room
livingRoom = Room "living room" 
                  "You are in a living room" 
                  [(N,"bedroom"),(S,"kitchen")] 
                  ["couch", "ball", "sandbag"]

bedroom :: Room
bedroom = Room "bedroom" 
               "You are in a bedroom" 
               [(S,"living room"),(E,"shower room")] 
               ["bed"]

showerRoom :: Room
showerRoom = Room "shower room" 
               "You are in a shower room" 
               [(W,"bedroom"), (S,"pantry")] 
               ["bathtub", "matchstick"]

garage :: Room
garage = Room "garage" 
               "You are in a garage" 
               [(N,"pantry"), (W,"backyard")] 
               ["ball"]


--'uni' is the Universe consisting of all the items.
univ :: Universe
univ = mkUniverse [stove, pot, couch, sandbag, matchstick, grill, 
                   bed, tarragon, beans, ball, bathtub]


--'itemNames' is a list of the names of all the items in our universe.
itemNames :: [ItemName]
itemNames = M.keys univ


--'gameMap' has type GameMap and consists of all the rooms.
gameMap :: GameMap
gameMap = mkMap [kitchen, livingRoom, pantry, bedroom, backyard, 
                 showerRoom, garage]


--'roomNames' is the RoomNames of our gameMap
roomNames :: [RoomName]
roomNames = M.keys gameMap


--'heaviestWeight' returns the weight of the heaviest item in our universe
heaviestWeight :: Integer
heaviestWeight = maximum . map weight $ M.elems univ


--'you' is the player of the game. It's a helper for initialState
you :: Player
you = Player
      []
      heaviestWeight
      "kitchen"


--'initialState' is the state when the game begins
initialState :: GameState
initialState
  = GameState
    Nothing
    gameMap
    univ
    you