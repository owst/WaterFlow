module Main where

import Control.Monad.Trans.State (State, modify, gets, execState)
import Control.Monad ((>=>), when)
import Data.List (partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty, (==>))
import Test.HUnit ((@=?))

data EdgeReachability = EdgeReachability
    { reachTop :: Bool
    , reachLeft :: Bool
    , reachBottom :: Bool
    , reachRight :: Bool
    } deriving Show

canReachAllEdges :: EdgeReachability -> Bool
canReachAllEdges (EdgeReachability x y z w) = and [x, y, z, w]

instance Monoid EdgeReachability where
    mempty = EdgeReachability False False False False
    mappend (EdgeReachability x1 x2 x3 x4) (EdgeReachability y1 y2 y3 y4) =
        EdgeReachability (x1 || y1) (x2 || y2) (x3 || y3) (x4 || y4)

type Height = Int
type Coord = (Int, Int)

-- For each of output, we use a "curried" map: y -> x -> v, rather than one from (x, y) -> v, since
-- we can more easily loop over the entries in row-order.
type CurriedCoordMap v = Map Int (Map Int v)
type CoordHeightMap = CurriedCoordMap Height
type MemoMap = CurriedCoordMap EdgeReachability

insertMemoMap :: Coord -> EdgeReachability -> MemoMap -> MemoMap
insertMemoMap (x, y) r = M.insertWith M.union y (M.singleton x r)

lookupMemoMap :: Coord -> MemoMap -> Maybe EdgeReachability
lookupMemoMap (x, y) = M.lookup y >=> M.lookup x

checkEdgeReachabilityForEachCoord :: CoordHeightMap -> MemoMap
checkEdgeReachabilityForEachCoord coordHeightMap
    | M.null coordHeightMap = M.empty
    | otherwise = execState (mapM (checkEdgeReachabilityForOneCoord S.empty) allCoords) M.empty
  where
    allCoords = [(x, y) | x <- [0..size], y <- [0..size]]
    heightFun (x, y) = (coordHeightMap M.! y) M.! x
    size = fst . M.findMax $ coordHeightMap

    checkEdgeReachabilityForOneCoord :: Set Coord -> Coord -> State MemoMap EdgeReachability
    checkEdgeReachabilityForOneCoord currentPathCoords coord@(x, y) = do
        memoisedResult <- gets (lookupMemoMap coord)
        case memoisedResult of
            Just result -> return result
            Nothing -> do
                let recurse = checkEdgeReachabilityForOneCoord currentPathCoords'
                reachInOneStep <- mconcat <$> mapM recurse noLoopNextSteps
                let thisCoordReach = thisReach `mappend` reachInOneStep
                -- Only update the cache if we visited all oneSteps (i.e. we didn't skip any that
                -- would form loops) and thus have a complete result.
                when (null loopNextSteps) $
                    modify $ insertMemoMap coord thisCoordReach
                return thisCoordReach
      where
        currentPathCoords' = S.insert coord currentPathCoords
        thisReach = EdgeReachability (y == 0) (x == 0) (y == size) (x == size)

        coordHeight = heightFun coord
        isValidStep nextCoord = not $ outOfBounds nextCoord || heightFun nextCoord > coordHeight
        outOfBounds (cx, cy) = cx < 0 || cy < 0 || cx > size || cy > size

        validOneSteps = filter isValidStep [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
        (loopNextSteps, noLoopNextSteps) = partition (`S.member` currentPathCoords') validOneSteps


parseCoordHeightMap :: [String] -> CoordHeightMap
parseCoordHeightMap = toMap . map toColumnMap . assertSquare . map words
  where
    assertSquare a = if isSquare a then a else error "Input must be square!"
    isSquare [] = True
    isSquare (c : cs) = let cLen = length c in all ((== cLen) . length) cs

    toColumnMap = toMap . map read
    toMap = M.fromList . zip [0..]

unparseResult :: MemoMap -> String
unparseResult = unlines . map (processRow . snd) . M.toAscList
  where
    processRow = unwords . map (toDigit . canReachAllEdges . snd) . M.toAscList

    toDigit True = "1"
    toDigit False = "0"

main :: IO ()
main = defaultMain $ testGroup "WaterFlow"
    [ testCase "empty" $ testIsEqual (@=?) [] []
    , testProperty "1 cell" $ \i -> testIsEqual (==)
        [show (i :: Int)]
        ["1"]
    , testProperty "all same height" $ \i -> testIsEqual (==)
        (replicate 2 (unwords $ replicate 2 (show (i :: Int))))
        [ "1 1"
        , "1 1"
        ]
    , testProperty "diagonal" $ \i j -> (i > 0) ==> (j < i) ==> testIsEqual (==)
        [ unwords [show j, show (i :: Int)]
        , unwords [show i, show (j :: Int)]
        ]
        [ "0 1"
        , "1 0"
        ]
    , testCase "weird one" $ testIsEqual (@=?)
        [ "0 1 2"
        , "0 3 1"
        , "0 1 1"
        ]
        [ "0 0 1"
        , "0 1 1"
        , "0 1 1"
        ]
    , testProperty "cross" $ \i j -> (i > 0) ==> (j < i) ==> testIsEqual (==)
        (let row = unwords [show (j :: Int), show i, show j]
         in [row, unwords $ replicate 3 (show (i :: Int)), row])
        [ "0 1 0"
        , "1 1 1"
        , "0 1 0"
        ]
    , testCase "spiral" $ testIsEqual (@=?)
        [ "1 2 3"
        , "8 9 4"
        , "7 6 5"
        ]
        [ "0 0 0"
        , "1 1 0"
        , "1 1 1"
        ]
    , testProperty "outsideWall" $ \i j -> (i > 0) ==> (j < i) ==> testIsEqual (==)
        [ unwords $ replicate 3 (show (i :: Int))
        , unwords [show i, show (j :: Int), show i]
        , unwords $ replicate 3 (show i)
        ]
        [ "1 1 1"
        , "1 0 1"
        , "1 1 1"
        ]
    ]
  where
    testIsEqual op input expected = unlines expected `op` getActual input
    getActual = unparseResult . checkEdgeReachabilityForEachCoord . parseCoordHeightMap
