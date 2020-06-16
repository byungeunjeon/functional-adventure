module Main where

import Control.Monad
import Control.Exception
import System.IO

import GameIO


--'controlD' takes IOError (CTRL-D) and returns exit
controlD :: IOError -> IO ()
controlD eofErrorType = exit


--'main' is the main function run when the program is executed
main :: IO ()
main = do
  putStrLn " "
  putStrLn "        Z           "
  putStrLn "  ZZZ  ZZZ ZZ       "
  putStrLn "    ZZZZZZZZ        "
  putStrLn "   ZZ ZZ ZZ         "
  putStrLn "  ZZ  Z   ZZ        "
  putStrLn "     ZZ             "
  putStrLn "    ZZ     ______   "
  putStrLn "   ZZZ    /______|  "
  putStrLn " ZZZZZZZ    | O |   "
  putStrLn "ZZZZZZZZZZZZZZZZZZZZ"
  putStrLn " "
  putStrLn "You were sailing, but you met a storm and your boat crashed."
  putStrLn "When you woke up, you found yourself lost in an island."
  putStrLn "You see a house. No one seems to be living in here now."
  putStrLn "Can you find something useful to get rescued? Let's go in!"
  handle controlD (eval (forever repl))