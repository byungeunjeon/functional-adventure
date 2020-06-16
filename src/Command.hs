module Command where

import Data.Char
import Data.List
import Text.ParserCombinators.ReadP
import Control.Applicative hiding (optional)

import Item
import Direction

type Parser = ReadP

runParser :: ReadP a -> String -> [(a, String)]
runParser = readP_to_S

data Command
  = Inventory
  | Look
  | Drop [ItemName]
  | Take [ItemName]
  | Move Direction
  | Exit
  deriving (Eq, Show)

type Conjunction = [Command]


{-'inventoryP' only accepts the string 'inventory' and rejects 
  everything else. It ignores whitespace on either side of the 
  word 'inventory', if there is any.-}
inventoryP :: ReadP Command
inventoryP = pure (\x -> Inventory)
             <* skipSpaces
             <*> string "inventory"
             <* skipSpaces


{-'takeP' parses the word 'take' plus a noun phrase into a Command. 
  There needs to be at least one space between 'take' and the 
  first word of the noun phrase, but the parser should accept any 
  additional amount of whitespace in between 'take' and the 
  noun phrase, and it should also accept any amount of whitespace 
  before 'take' and after the noun phrase.-}
takeP :: ReadP Command
takeP = pure (\x -> Take x)
        <* skipSpaces
        <* string "take "
        <*> nounPhrase


{-'exitP' is a parser for the command to quit the game. 
  This parser will accept either the single word 'quit' 
  or the single word 'exit', and it will allow any amount of 
  whitespace on either side of either word-}
exitP :: ReadP Command
exitP = pure (\x -> Exit)
        <* skipSpaces
        <*> (string "exit" <|> string "quit")
        <* skipSpaces


{-'dropP' parses the word 'drop' plus a noun phrase into a Command. 
  There needs to be at least one space between 'drop' and the 
  first word of the noun phrase, but the parser should accept any 
  additional amount of whitespace in between 'drop' and the noun phrase, 
  and it should also accept any amount of whitespace before 'drop' 
  and after the noun phrase.-}
dropP :: ReadP Command
dropP = pure (\x -> Drop x)
        <* skipSpaces
        <* string "drop "
        <*> nounPhrase


{-'lookP' only accepts the string 'look' and rejects everything else. 
  It ignores whitespace on either side of the word 'look', 
  if there is any.-}
lookP :: ReadP Command
lookP = pure (\x -> Look)
        <* skipSpaces
        <*> string "look"
        <* skipSpaces


{-'directionP' expects a single lowercase word denoting a direction, 
  making the obvious map from the word 'north', 'south', 'east', 
  or 'west' to the relevant Direction. It allows for whitespace 
  on either side, and does not require the string to end right after -}
directionP :: ReadP Direction
directionP = pure (\x -> case x of 
                   "north" -> N
                   "south" -> S
                   "east" -> E
                   "west" -> W)
             <* skipSpaces
             <*> (string "north" 
                  <|> string "south" 
                  <|> string "east" 
                  <|> string "west")
             <* skipSpaces


{-'moveP' parses a move command. It expects one of the four words, 
  'north', 'south', 'east', or 'west' (with possible but not obligatory 
  whitespace on either side). It consumes the relevant word off the input, 
  and makes that word into a command telling the game to move in 
  the relevant direction-}
moveP :: ReadP Command
moveP = pure (\d -> Move d) 
        <*> directionP


{-'commandP' accepts any single command that is syntactically well-formed, 
  according to the grammar of the game's language, and returns the Command 
  corresponding to the string in the language. -}
commandP :: ReadP Command
commandP = choice [inventoryP, lookP, dropP, takeP, moveP, exitP]


{-'conjunctionP' parses a list of commands, separated by the word 'and', 
  into a Conjunction, which we defined above to be a type alias for 
  a list of Command-s. There can be an arbitrary amount of whitespace 
  on either side of any of the words in a conjunction.-}
conjunctionP :: ReadP Conjunction
conjunctionP = sepBy1 commandP (string "and") <* eof


{-'parse' is a helper function that takes a string in and returns the 
  conjunction wrapped in a Just, if the input string is well-formed 
  according to the game language's syntax, and otherwise returns Nothing
  This is used in 'parseConjunction' function in GameIO module -}
parse :: String -> Maybe Conjunction
parse s = case nub $ runParser conjunctionP s of
               [(x,"")] -> Just x
               _ -> Nothing


{-[helper] 'helperNounP' is a helper function for nounPhrase-}
helperNounP :: ReadP ItemName
helperNounP = pure (\x -> x)
              <* skipSpaces
              <*> munch1 isAlpha
              <* skipSpaces


{-'nounPhrase' will parse a comma-separated list of nouns rather than 
  accepting a single noun. There can be an arbitrary amount of whitespace 
  between each noun and each comma.-}
nounPhrase :: ReadP [ItemName]
nounPhrase = sepBy1 helperNounP (string ",")