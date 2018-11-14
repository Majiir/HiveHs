module Lib
    ( someFunc
    ) where

import Control.Monad.Random (runRandT)
import Control.Monad.State.Strict (runStateT)
import System.Random (mkStdGen)
import Hive.Players
import Hive.Game

someFunc :: IO ()
someFunc = do
    ((r, _), _) <- runStateT (runRandT (playGame thinkingPlayer terminalPlayer) (mkStdGen 1)) 1
    let (_, _, g) = last r
    putStrLn ("Game ended in " ++ (show (length r)) ++ " moves.")
    putStrLn ("Player: " ++ show (nextTurn g))
    putStrLn ("Victorious: " ++ show (victorious g))
    putStrLn ("Defeated: " ++ show (defeated g))
