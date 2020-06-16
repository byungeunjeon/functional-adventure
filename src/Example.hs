module Example where

import System.Random
import Data.List
import qualified Data.Map as M

import Item
import Direction
import Room
import Player
import Initialize
import GameState


-- Create the type class: Example. See 3.2.1 Exercise
class Example a where
  example :: IO a


--'choose' outputs a random element from list.
choose :: [a] -> IO a
choose lst = do
  i <- randomRIO (0, length lst-1)
  return $ lst !! i


{-'exampleList'  takes a side effect-ful value (i.e. a value of type IO a) 
  and a side-effectful integer, and it outputs an IO list of 
  randomly-generated items, whose length is that of the side effect-ful 
  integer. -}
exampleList :: IO a -> IO Int -> IO [a]
exampleList num len = do
  l <- len
  lst <- sequence (replicate l num)
  return lst


-- Defining Example Item instance.
instance Example Item where
  example = do
    x <- choose itemNames
    i <- randomRIO (0, 100)
    return $ Item x i


-- Defining Example Direction instance.
instance Example Direction where
  example = do
    d <- choose [N, S, E, W]
    return $ d


-- Defining exitExample of type IO Exit.
exitExample :: IO Exit
exitExample = do
  d <- choose [N, S, E, W]
  r <- choose roomNames
  return $ (d, r)


-- Defining Example Room instance.
instance Example Room where
  example = do 
    rn <- choose roomNames
    et <- exampleList (exitExample) (randomRIO (2, 5))
    ob <- exampleList (choose itemNames) (randomRIO (2, 5))
    let ds = "You are in a randomly-generated room, which is the " ++ rn
    return $ Room rn ds et ob


{-'getHeaviest' is a helper function for Example Player. 
  It returns the heaviest weight among all input list of items.-}
getHeaviest :: [Item] -> Integer
getHeaviest [] = 0
getHeaviest (x : xs) = if weight x > getHeaviest xs 
                       then weight x 
                       else getHeaviest xs


{-'getLightest' is a helper function for Example Player (3.1.6 Exercise). 
  It returns the lightest weight among all input list of items.
  It does not work on empty list, but input will never have empty list-}
getLightest :: [Item] -> Integer
getLightest [x] = weight x
getLightest (x : xs) = if weight x < getLightest xs 
                       then weight x 
                       else getLightest xs


-- Defining Example Player instance.
instance Example Player where
  example = do 
    iv <- exampleList (choose itemNames) (randomRIO (0, 10))
    lc <- choose roomNames
    let h = getHeaviest (M.elems univ)
        l = getLightest (M.elems univ)
    mw <- (randomRIO (h-(h-l), h+(h-l)))
    return $ Player (nub iv) mw lc


-- Defining Example GameState instance.
instance Example GameState where
  example = do
    msg <- choose [Just "One possible message.", 
                   Just "Yet another possible message", Nothing]
    rms <- exampleList (example :: IO Room) (randomRIO (2, 3))
    its <- exampleList (example :: IO Item) (randomRIO (0, 10))
    ply <- example :: IO Player
    return $ GameState msg (mkMap rms) (mkUniverse its) ply