{-# LANGUAGE TupleSections #-}

module Hive.Game where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (nub, delete, uncons, foldl')
import Data.Maybe (catMaybes)
import Hive.Coordinate

data Player = White | Black deriving (Eq, Show, Ord)

data GameState = GameState {
    board :: Board,
    unplaced :: M.Map Player [Piece],
    nextTurn :: Player,
    openingTurns :: Int -- How many turns are left where the queen doesn't need to be placed
} deriving (Show) -- Can't derive Eq because gamestates can be "equivalent" with different in-mem representations


type Grid a = M.Map (Int, Int) a
type Board = Grid Placement

data Move = Place Piece Coordinate | Move Coordinate Coordinate deriving (Show)

type Placement = [(Player, Piece)] -- Bleh, this type sucks too. I want this to be NonEmpty!
-- Or I want some kind of ordered-multi-map thing.


data Piece =
    Queen
    | Ant
    | Beetle
    | Grasshopper
    | Spider
    deriving (Eq, Show)

startingPieces :: [Piece]
startingPieces = [Queen, Ant, Ant, Ant, Grasshopper, Grasshopper, Grasshopper, Spider, Spider, Beetle, Beetle]

newGame :: GameState
newGame = GameState {
    board = M.empty,
    unplaced = M.fromList [(White, startingPieces), (Black, startingPieces)],
    nextTurn = White,
    openingTurns = 3
}

-- TODO: Tournament option which prohibits placing Queen as the first move
placementMoves :: GameState -> [(Move, GameState)]
placementMoves g =
    let
        p = nextTurn g
        b = board g
        t = openingTurns g
        c = M.keysSet b
        c' = M.keysSet (M.filter (\xs -> fst (head xs) /= p) b) -- TODO: Remove partial
        u = nub . M.findWithDefault [] p . unplaced $ g
        u' =
            if t > 0 || queenPlaced g
                then u
                else Prelude.filter (== Queen) u
        open = 
            if M.null b
                then S.singleton (0,0)
                else (Hive.Game.concatMap (S.fromList . neighbors) c) S.\\ c
        o' =
            if t /= 3 -- TODO: Make this less brittle. Not sure what to do though.
                then open S.\\ (c' `S.union` Hive.Game.concatMap (S.fromList . neighbors) c')
                else open
    in
        [(Place x y, placePiece p x y g) | x <- u', y <- (S.toList o') ]

mapBoard :: (Board -> Board) -> GameState -> GameState
mapBoard f g = g { board = f (board g) }

mapUnplaced :: (M.Map Player [Piece] -> M.Map Player [Piece]) -> GameState -> GameState
mapUnplaced f g = g { unplaced = f (unplaced g) }

isEmpty :: GameState -> Coordinate -> Bool
isEmpty g c = let b = board g in M.notMember c b

isOccupied :: GameState -> Coordinate -> Bool
isOccupied g = not . isEmpty g

advanceTurn :: GameState -> GameState
advanceTurn g =
    case nextTurn g of
        White -> g {
            nextTurn = Black
        }
        Black -> g {
            nextTurn = White,
            openingTurns = max 0 ((openingTurns g) - 1)
        }

placePiece :: Player -> Piece -> Coordinate -> GameState -> GameState
placePiece o p c =
    putPiece o p c
    . mapUnplaced (M.adjust (delete p) o)

putPiece :: Player -> Piece -> Coordinate -> GameState -> GameState
putPiece o p c =
    advanceTurn
    . mapBoard (M.insertWith (++) c [(o, p)])

removePiece :: Coordinate -> GameState -> GameState
removePiece c g = mapBoard (M.update remove c) g
    where
        remove [] = Nothing
        remove (_:[]) = Nothing
        remove (_:xs) = Just xs

type CrawlMap = M.Map Coordinate (S.Set Coordinate)

movementMoves :: GameState -> [(Move, GameState)]
movementMoves g =
    let
        -- TODO: Don't lookup from the map; have this info passed in.
        moveable = moveableTiles g
        -- TODO: We avoided partial functions here, but it's ugly. FIX.
        pieces = catMaybes . fmap (\x -> do { pl <- M.lookup x (board g); (p, _) <- uncons pl; return (x, p) }) $ moveable
        b = board g
        -- TODO: Adjust crawlMap on all movements and placements, and thread it throughout the whole game tree.
        crawlMap = M.fromSet (crawlOptions g) (Hive.Game.concatMap (\x -> S.insert x . S.fromList . neighbors $ x) (M.keysSet b))
    in
        concat (fmap (\(c, (o,p)) -> pieceMoves g crawlMap o p c) pieces)

pieceMoves :: GameState -> CrawlMap -> Player -> Piece -> Coordinate -> [(Move, GameState)]
pieceMoves g' crawlMap o p c =
    let
        g = removePiece c g'
        -- TODO: Improve crawlMap adjustment
        crawlMap' = adjustCrawlMap g c crawlMap
        --
        m = case p of
            Queen -> queenMovement crawlMap'
            Ant -> antMovement crawlMap'
            Beetle -> beetleMovement
            Grasshopper -> grasshopperMovement
            Spider -> spiderMovement crawlMap'
    in
        fmap (\x -> (Move c x, putPiece o p x g)) (m g c)

-- TODO: Only insert cells that have any crawlable targets.
adjustCrawlMap :: GameState -> Coordinate -> CrawlMap -> CrawlMap
adjustCrawlMap g c m = foldl' ins m (c : neighbors c)
        where ins x y =
                let co = crawlOptions g y
                in if S.null co
                    then M.delete y x
                    else M.insert y co x

queenPlaced :: GameState -> Bool
queenPlaced g = notElem Queen $ M.findWithDefault [] (nextTurn g) (unplaced g)

aroundQueen :: GameState -> Int
aroundQueen g =
    let
        b = board g
        o = nextTurn g
        queens = M.keys . M.filter (any (\(o', p) -> o == o' && p == Queen)) $ b
    in
        length . filter (isOccupied g) . Prelude.concatMap neighbors $ queens

defeated :: GameState -> Bool
defeated g = aroundQueen g >= 6

victorious :: GameState -> Bool
victorious = defeated . advanceTurn

moves :: GameState -> [(Move, GameState)]
moves g = 
    if defeated g || victorious g
        then []
        else
            placementMoves g ++
            if queenPlaced g
                then movementMoves g
                else []

moveableTiles :: GameState -> [Coordinate]
moveableTiles g =
    let
        b = board g
        p = nextTurn g
        playerTiles = M.keys . M.filter (\ps -> p == fst (head ps)) $ b -- TODO: Remove partial function
    in
        Prelude.filter (oneHiveRule . flip removePiece g) playerTiles

oneHiveRule :: GameState -> Bool
oneHiveRule g =
    let
        tiles = M.keysSet (board g)
    in
        case S.lookupMin tiles of
            Nothing -> True
            Just x -> tiles == S.insert x (explore (S.intersection tiles . S.fromList . neighbors) x)

explore :: (Ord a) => (a -> S.Set a) -> a -> S.Set a
explore expand x = go (S.singleton x) S.empty
    where
        go open visited =
            let
                e = Hive.Game.concatMap expand open
                o = e S.\\ visited
                v = visited `S.union` e
            in
                if S.null o
                    then v
                    else go o v

beetleMovement :: GameState -> Coordinate -> [Coordinate]
beetleMovement g c = 
    let
        b = board g
        height x = length (M.findWithDefault [] x b)
        c' = height c
        heightRule d =
            let
                n = neighbor c d
                (a, b) = adjacentNeighbors c d
                n' = height n
                a' = height a
                b' = height b
            in
                (c' >= a') || (c' >= b') || (n' >= a') || (n' >= b')
        n = neighbors c
        tiles = filter (isOccupied g) (c : n)
        empty = filter (isEmpty g) . Prelude.concatMap neighbors $ tiles
        domain = S.intersection (S.fromList n) $ S.fromList (tiles ++ empty) -- TODO: Yuck
    in
        filter (flip S.member domain)
        . map (neighbor c)
        . filter heightRule
        $ directions


-- Rethinking crawl:
-- We can crawl to any *empty* neighbor that has one adjacent empty neighbor and one adjacent occupied neighbor.
-- Where we can crawl is therefore only impacted by neighboring cells.
-- So if we change a cell, we need only recompute the crawls for that cell and its neighbors.*
    -- *Note that when adding or removing a piece, we _also_ change the valid set, which constrains crawls.

-- A few ways to crawl:
--  Make a list and traverse it both forward and back
--  Check the forward and back cases as we go through a list once

crawlOptions :: GameState -> Coordinate -> S.Set Coordinate 
crawlOptions g c =
    let
        n = neighbors c
        e = cycle $ map (isEmpty g) n
        a = zip n $ zip3 (drop 1 e) e (drop 5 e)
        canCrawl (l, x, r) = x && (l /= r)
        v = filter (canCrawl . snd) a
    in
        S.fromList (map fst v)

queenMovement :: CrawlMap -> GameState -> Coordinate -> [Coordinate]
queenMovement crawlMap g c = (crawlMovement g crawlMap c) !! 1

spiderMovement :: CrawlMap -> GameState -> Coordinate -> [Coordinate]
spiderMovement crawlMap g c = (crawlMovement g crawlMap c) !! 3

antMovement :: CrawlMap -> GameState -> Coordinate -> [Coordinate]
antMovement crawlMap g c = nub . concat . tail . takeWhile (not . null) . crawlMovement g crawlMap $ c

crawlMovement :: GameState -> CrawlMap -> Coordinate -> [[Coordinate]]
crawlMovement g crawlMap c =
    let
        step (x, v) = fmap (, S.insert x v) . S.toList . (S.\\ v) . M.findWithDefault S.empty x $ crawlMap
        start = [(c, S.empty)]
        steps = iterate (concat . fmap step) start
    in
        map (nub . fmap fst) $ steps -- TODO: Figure out if nub is needed here

grasshopperMovement :: GameState -> Coordinate -> [Coordinate]
grasshopperMovement g c =
    fmap (head . dropWhile (isOccupied g) . walk c) -- TODO: Partial function `head` - so maybe `walk` should give a Stream?
    . filter (isOccupied g . neighbor c)
    $ directions

allPairs :: [a] -> [(a, a)]
allPairs x = zip x (tail (cycle x)) 

concatMap :: (Ord a, Ord b) => (a -> S.Set b) -> S.Set a -> S.Set b
concatMap f = S.unions . map f . S.toList 



-- Changes that occur:

-- Move
    -- Changes the next player
        -- For best reusability, any player-specific mappings should be cached and updated for both players, since the player constantly flips.
    -- May decrement the initial-turns counter
        -- Only impacts the first few moves, which are small anyway.
    -- Placement
        -- Removes an available piece
            -- Only impacts future Placement
        -- Puts down a piece (on the flat board only)
    -- Movement
        -- Picks up a piece
        -- Puts down _that same_ piece
    -- Pick up a piece
        -- Impacts victory/defeat (indirectly)
        -- Changes occupied set
            -- Only need to consider the one cell
        -- Changes domain set
            -- Need to consider neighbors and neighbors-of-neighbors
        -- Changes articulation points
            -- Need to consider: ??? Maybe recompute entirely?
        -- Changes crawl mappings
            -- Picking up can add or remove crawl options, since crawling requires adjacent occupied cells.
            -- Mappings for the now-empty cell (if not beetle) need to be generated.
            -- Mappings from neighbors need to be cleared and re-generated.
        -- Changes 'valid' set
            -- Need to formalize what this means vs. 'domain' set
            -- Need to consider neighbors and neighbors-of-neighbors
    -- Put down a piece
        -- Impacts victory/defeat


articulationPoints :: GameState -> [Coordinate]
articulationPoints g =
    let
        k = M.keysSet (board g)
        r = S.maxView k
        search node visited = -- need to sequence visiting a node and recursing before visiting another node
            let
                v = S.insert node visited
                n = (S.intersection k) . S.fromList . neighbors $ node
                -- TODO: Fold over `n` to DFS-traverse.
                -- `search` should produce an annotated graph (tree?) which we can then use to find articulation points.
            in [] -- TODO
    in
        case r of
            Nothing -> []
            Just (root, _) -> search root S.empty
                
