module Main where

import System.Random
import Control.Monad.Random

data Flow a = Force a | Choose [Flow a] | Sequence [Flow a]

xkcd :: Flow String
xkcd = Sequence [Force "Did you know that",
                 Choose [Force "Easter",
                         Sequence [Force "leap",
                                   Choose [Force "day",
                                           Force "year"]]],
                 Force "because of"]

construct :: RandomGen g => Flow a -> Rand g [a]
construct (Force x) = pure [x]
construct (Sequence xs) = concat <$> traverse construct xs
construct (Choose xs) = construct =<< uniform xs


main :: IO ()
main = print . unwords =<< evalRandIO (construct xkcd)
