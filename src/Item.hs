module Item where

import qualified Data.Map as M

type ItemName = String


data Item
  = Item { iname :: ItemName
         , weight :: Integer }
    deriving (Show, Eq)


type Universe = M.Map String Item


--'mkUniverse' takes a list of items and construct a Universe
mkUniverse :: [Item] -> Universe
mkUniverse lst = M.fromList (zip (map (\x -> iname x) lst) lst)