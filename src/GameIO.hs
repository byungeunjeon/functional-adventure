module GameIO where

import Control.Monad.State
import System.Exit
import System.IO

import GameState
import Player
import Room
import Command
import Item
import Initialize


{-'GameIO' is a type alias that passes IO into StateT, 
  using GameState as a type for simulated state -}
type GameIO a = StateT GameState IO a


--'eval' is the filp evalStateT, a helper function used for main function
eval :: GameIO a -> IO a
eval = (flip evalStateT) initialState


{-'effectChange' takes a transition between game states as an input, 
  and outputs a value of type GameIO that performs that transition 
  within the monadic GameIO value -}
effectChange :: (GameState -> GameState) -> GameIO ()
effectChange f = do
  gm <- get
  put $ f gm
  pure ()


{-'prompt' prints the string "-> " (without a newline character) 
  to the console, which functions for the user as a visible prompt -}
prompt :: GameIO ()
prompt = liftIO (putStr "-> " >> hFlush stdout)


{-'printMessage' checks the current state of the game, and if there is 
  Just value in the message field of the current game state, 
  it prints the message to the screen, then sets the game state message 
  field to Nothing. If there is a Nothing value in the message field 
  of the current game state, it does nothing.-}
printMessage :: GameIO ()
printMessage = do
  gm <- get
  case message gm of
    Nothing -> pure ()
    Just s -> liftIO (putStrLn s) >> (effectChange $ setMessage "")


{-'printDescription' prints a description of the room where the player 
  is in the current game state -}
printDescription :: GameIO ()
printDescription = do
  gm <- get
  liftIO (putStrLn (desc $ GameState.currentRoom gm))


--[helper] 'recursivePrintObj' is a helper function for printObjects
recursivePrintObj :: [ItemName] -> GameState -> IO ()
recursivePrintObj [] gm = pure ()
recursivePrintObj (x : xs) gm = (putStrLn (iname (GameState.getObject x gm))) 
                                >> (recursivePrintObj xs gm)


{-'printObjects' prints "You see the following objects:" followed by 
  a list of all the items in the room where the player is assuming there 
  are any. If there are no objects in the room where the player is, 
  it does nothing -}
printObjects :: GameIO ()
printObjects = do
  gm <- get
  if (nearbyObjects gm) == []
  then pure ()
  else liftIO (putStrLn "You see the following objects:" 
               >> recursivePrintObj (nearbyObjects gm) gm)


--[helper] 'recursivePrintExit' is a helper function for printExits
recursivePrintExit :: Show a => [(a, b)] -> IO ()
recursivePrintExit [] = pure ()
recursivePrintExit ((x1, x2) : xs) = putStrLn (show x1) 
                                     >> recursivePrintExit xs


{-'printExits' prints "There are exits in the following directions:" 
  followed by a list of all the directions there are exits in, 
  in the room where the player currently is. If there are no exits in 
  the room where the player currently is, it does nothing:-}
printExits :: GameIO ()
printExits = do
  gm <- get
  if exits (currentRoom gm) == []
  then pure ()
  else liftIO (putStrLn "There are exits in the following directions:" 
               >> recursivePrintExit (exits $ currentRoom gm))


{-'printInventory' checks the player's current inventory, and if it's empty, 
  prints "You aren't carrying anything." If the player's current inventory 
  is nonempty, then it prints "You are carrying the following items:" 
  followed by a list of all the ItemName-s in the player's inventory -}
printInventory :: GameIO ()
printInventory = do
  gm <- get
  if currentInventory gm == []
  then liftIO (putStrLn "You aren't carrying anything.")
  else liftIO (putStrLn "You are carrying the following objects:" 
               >> recursivePrintObj (currentInventory gm) gm)


{-'actionOverList' takes a function describing an action on items 
  (i.e. like taking or dropping them) and a list of item names as input, 
  and performs the action on each item in the list, in order. 
  Each time it executes an action, it runs printMessage-}
actionOverList :: (ItemName -> GameState -> GameState)
               -> [ItemName]
               -> GameIO ()
actionOverList func [] = pure ()
actionOverList func (x : xs) = effectChange (func x)
                               >> printMessage 
                               >> actionOverList func xs


{-'finishGame' performs the action of printing a success message to the 
  screen, then quits the program. This is what will be run when the user 
  wins the game. -}
finishGame :: GameIO ()
finishGame = 
  liftIO (putStrLn "You brought the matchstick into the backyard."
          >> putStrLn "You started a fire."
          >> putStrLn "A helicopter noticed your fire!!"
          >> putStrLn "You are rescued. Congrats! You win!"
          >> exitSuccess)


{-'finishLostGame' performs the action of printing a success message 
  to the screen, then quits the program. This is what will be run 
  when the user wins the game. -}
finishLostGame :: GameIO ()
finishLostGame = 
  liftIO (putStrLn "This is not a ball. it's a grenade!" 
          >> putStrLn "You accidently pulled out the pin... BOOM!!"
          >> putStrLn "Sorry, you are dead. Try playing again." 
          >> exitSuccess)


{-'exit' is for when the user decides to quit the game, 
  rather than when they win. -}
exit :: IO ()
exit = putStrLn "Goodbye!" >> exitSuccess


{-'checkGameOver' checks whether the current game state is 
  the winning state or the losing state. 
  If yes, it runs finishGame or finishLostGame. 
  Otherwise, it does nothing -}
checkGameOver :: GameIO ()
checkGameOver = do
  gm <- get
  if haveWonGame gm 
  then finishGame 
  else if haveLostGame gm
  then finishLostGame 
  else pure ()


--'syntaxError' prints the message 'I don't understand that.'
syntaxError :: GameIO ()
syntaxError = liftIO (putStrLn "I don't understand that.")


{-'performCommand' takes any Command as an input, and executes the 
  action corresponding to the command -}
performCommand :: Command -> GameIO ()
performCommand c = case c of
  Look -> printDescription >> printObjects >> printExits
  Move d -> (effectChange $ move d) >> printMessage
  Inventory -> printInventory
  Drop lst -> actionOverList dropItem lst
  Take lst -> actionOverList takeItem lst
  Exit -> liftIO exit


--'performConjunction' performs every command in a Conjunction, in order
performConjunction :: Conjunction -> GameIO ()
performConjunction [] = pure ()
performConjunction (x : xs) = (performCommand x) >> performConjunction xs


{-'parseConjunction' parses an input string, and if the parse succeeds, 
  runs performConjunction on the result. If the parse fails, 
  it runs syntaxError-}
parseConjunction :: String -> GameIO ()
parseConjunction inputStr = 
  case parse inputStr of
    Nothing -> syntaxError
    Just commands -> performConjunction commands


{-'repl' performs one round of printing the prompt, getting input from 
  the user, parsing it, performing the command the input denotes if 
  the parse succeeds and printing a syntax error message otherwise, 
  then running checkGameOver-}
repl :: GameIO ()
repl = prompt >> liftIO getLine 
       >>= parseConjunction >> checkGameOver