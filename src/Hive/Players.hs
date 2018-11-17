{-# LANGUAGE FlexibleContexts #-}

module Hive.Players where

import Control.Monad.IO.Class
import Control.Monad.Random
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import Data.List (sortOn, maximumBy, foldl', intercalate)
import Data.Ord (comparing)
import Hive.Game
import Hive.Coordinate
import System.IO (hFlush, stdout)
import Control.Parallel.Strategies

type Agent m = GameState -> [(Move, GameState)] -> m (Move, GameState)



playGame :: (MonadIO m) => Agent m -> Agent m -> m [(Player, Move, GameState)]
playGame white black =
    do
        let
            play g =
                if (nextTurn g) == White
                    then white g (moves g)
                    else black g (moves g)
        let
            step g =
                if (victorious g) || (defeated g)
                    then do
                        liftIO $ putStrLn "Game over!"
                        return Nothing
                    else
                        do
                            if null (moves g)
                                then do
                                    liftIO $ putStrLn ((show (nextTurn g)) ++ ": No moves. Passing.")
                                    liftIO $ hFlush stdout
                                    step (advanceTurn g)
                                else do
                                    (m, g') <- play g
                                    liftIO $ putStrLn ((show (nextTurn g)) ++ ": " ++ (showMove g g' m))
                                    liftIO $ hFlush stdout
                                    let p = nextTurn g
                                    return $ Just ((p, m, g'), g')
        unfoldrM step newGame

unfoldrM :: (Monad m) => (a -> m (Maybe (b, a))) -> a -> m [b]
unfoldrM f = go
    where go z = do
            x <- f z
            case x of
                Nothing         -> return []
                Just (x, z')    -> do
                        xs <- go z'
                        return (x : xs)


showMove :: GameState -> GameState -> Move -> String
showMove g g' (Move a b) = "Move " ++ (show . snd . pieceAt g $ a) ++ " " ++ show a ++ " (" ++ describeNeighbors g a ++ ") -> " ++ show b ++ " (" ++ describeNeighbors g' b ++ ")"
showMove g g' (Place p x) = "Place " ++ show p ++ " -> " ++ show x ++ " (" ++ describeNeighbors g' x ++ ")"

showPiece :: (Player, Piece) -> String
showPiece (o, p) = show o ++ " " ++ show p

pieceAt :: GameState -> Coordinate -> (Player, Piece)
pieceAt g c = head . M.findWithDefault [] c . board $ g -- TODO: Remove partial function

describeNeighbors :: GameState -> Coordinate -> String
describeNeighbors g c = intercalate ", " . map (\d -> describeDirection d ++ " " ++ showPiece (pieceAt g (neighbor c d))) . filter (isOccupied g . neighbor c) $ directions

describeDirection :: Direction -> String
describeDirection d = M.findWithDefault "?" d . M.fromList . zip directions $ ["-o", "^o", "o^", "o-", "ov", "vo"]

terminalPlayer :: (MonadIO m) => Agent m
terminalPlayer g moves = do
    liftIO $ putStrLn "Moves available:"
    let numbered = zip moves [0..]
    liftIO $ forM_ numbered (\((m, g'), c) -> putStrLn ("  " ++ show c ++ ": " ++ showMove g g' m))
    liftIO $ putStrLn ""
    liftIO $ putStr "Pick a move: "
    i <- liftIO $ getLine -- TODO: Validate
    liftIO $ putStrLn ""
    liftIO $ putStrLn . show . snd $ (moves !! (read i))
    return $ moves !! (read i)

dumbPlayer :: Agent IO
dumbPlayer g moves = do
    putStr "DUMB: "
    let m = head moves -- Always pick the first move
    print (fst m)
    return m

randomPlayer :: (MonadRandom m) => Agent m
randomPlayer g moves = do
    i <- getRandomR (0, (length moves) - 1)
    return (moves !! i)

badPlayer :: (MonadState Int m) => Agent m
badPlayer g moves = do
    i <- get
    put (i + 1)
    return (moves !! (i `mod` (length moves)))  

oppressivePlayer :: (Monad m) => Agent m
oppressivePlayer g m = do
    let score (_, g) = (length (moves g)) - (aroundQueen g)
    let s = (sortOn score m)
    let m = (head s)
    return m

selfishPlayer :: (Monad m) => Agent m
selfishPlayer g m = do
    let score g = (length (moves g)) - (aroundQueen g)
    let s = (sortOn (\(_, g) -> negate (score (advanceTurn g))) m)
    let m = (head s)
    return m

balancedPlayer :: (Monad m) => Agent m
balancedPlayer g m = do
    let score g = (length (moves g)) - (aroundQueen g)
    let s = (sortOn (\(_, g) -> (2 * score g) + negate (score (advanceTurn g))) m)
    let m = (head s)
    return m

thinkingPlayer :: (Monad m) => Agent m
thinkingPlayer g m = do
    let o = opponent . nextTurn . snd . head $ m
    let score g = length (moves g) - 4 * (min 1 (aroundQueen g))
    let score' g = (score g) - (score (advanceTurn g))
    return $ maximumOn (searchAhead (fromIntegral . score') o 3 . snd) m

opponent :: Player -> Player
opponent White = Black
opponent Black = White

searchAhead :: (Fractional n, Ord n) => (GameState -> n) -> Player -> Int -> GameState -> n
searchAhead f o 0 g = -- Score the given gamestate.
    let
        x = f g
    in
        if o == nextTurn g
            then x
            else negate x
searchAhead f o c g =
    let mov = moves g in
    if null mov
        then searchAhead f o (c - 1) (advanceTurn g)
        else
            let 
                m = fmap snd mov
                m' = take 2 . sortOn (if o == nextTurn g then (negate . f) else f) $ m
                s = searchAhead f o (c - 1)
            in
                if o == nextTurn g
                    then maximum (map s m')
                    else minimum (map s m')

maximumOn :: (Ord a, Foldable t) => (b -> a) -> t b -> b
maximumOn = maximumBy . comparing
-- find the _average_ value of opponent moves
-- but only pick the _best_ move we have available at each step.

average :: (Fractional n) => [n] -> n
average xs = let (count, total) = foldl' (\(c, t) x -> (c + 1, t + x)) (0, 0) xs in total / count

freePieces :: GameState -> Int
freePieces = length . moveableTiles

pinnedPieces :: GameState -> Int
pinnedPieces g = pieceCount g - freePieces g

pieceCount :: GameState -> Int
pieceCount g = length . filter (\(o, _) -> o == (nextTurn g)) . concat . M.elems . board $ g

pick :: Int -> [a] -> Maybe a
pick c xs = if null xs then Nothing else Just (xs !! (c `mod` length xs))

    