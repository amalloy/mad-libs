module Main where

import Prelude hiding (sequence)
import Control.Applicative (liftA2)
import System.Random
import Control.Monad.Random hiding (sequence)
import Text.Parsec hiding (choice)
import Text.Parsec.Char

data Flow a = Force a | Choose [Flow a] | Sequence [Flow a] deriving Show
type Parser a = Parsec String () a

construct :: RandomGen g => Flow a -> Rand g [a]
construct (Force x) = pure [x]
construct (Sequence xs) = concat <$> traverse construct xs
construct (Choose xs) = construct =<< uniform xs

flow, sequence, choice, force :: Parser (Flow String)

flow = spaces *> (sequence <|> choice <|> force) <* spaces

sequence = char '(' *> (Sequence <$> sepBy1 flow (char ',')) <* char ')'
choice = char '[' *> (Choose <$> sepBy1 flow (char '|')) <* char ']'
force = Force <$> escapedTerminators "()[]|,"

escapedTerminators :: [Char] -> Parser String
escapedTerminators specials = go
  where go = lookAhead (oneOf specials) *> pure [] <|> liftA2 (:) (escape <|> anyChar) go
        escape = char '\\' *> anyChar

main :: IO ()
main = do
  stdin <- getContents
  case parse flow "stdin" stdin of
    Left error -> print error
    Right tree -> putStrLn . unwords =<< evalRandIO (construct tree)
