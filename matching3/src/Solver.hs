-- |

module Solver where

import           Control.Monad.State
import           Data.Matrix
import           Data.Maybe
import qualified Debug.Trace         as D
import           Prelude             hiding ((!!))

data Obslete = Wood
             | Steel
             | Ice
             | Others
             deriving Show

data Pokemon = Pokemon
             { name   :: String
             , attack :: Int
             , skill  :: Skill
             } deriving Eq

instance Show Pokemon where
  show p = show (name p)

instance Ord Pokemon where
  a `compare` b = attack a `compare` attack b

data Skill = Skill deriving (Eq, Show)

data Node = P Pokemon | O Obslete
          deriving Show

data Zipper a = Zipper [Maybe a] a [Maybe a]
                       deriving (Eq, Show)

push :: Eq a => a -> State [(a, Int)] ()
push a = do
  s <- get
  case s of
    (x : xs) -> if (fst x) == a then put $ (a, snd x + 1) : xs
                                else put $ (a, 1) : x : xs
    [] -> put [(a, 1)]

pushMaybe Nothing = return ()
pushMaybe (Just x) = push x

pop :: State [(a, Int)] a
pop = do
  s <- get
  case s of
    [] -> error "empty list"
    (x : xs) -> if snd x - 1 == 0 then do put xs
                                          return (fst x)
                                  else do put $ (fst x, snd x - 1) : xs
                                          return (fst x)

pokeA, pokeB, pokeC, pokeD :: Pokemon
pokeA = Pokemon { name = "A"
                , attack = 60
                , skill = Skill
                }

pokeB = Pokemon { name = "B"
                , attack = 73
                , skill  = Skill
                }

pokeC = Pokemon { name = "C"
                , attack = 80
                , skill = Skill
                }

pokeD = Pokemon { name = "D"
                , attack = 50
                , skill = Skill
                }

toPoke :: Int -> Pokemon
toPoke 1 = pokeA
toPoke 2 = pokeB
toPoke 3 = pokeC
toPoke 4 = pokeD
toPoke _ = undefined

testMatrix :: Matrix Int
testMatrix = fromLists [ [1,1,2,1,3,4]
                       , [1,2,3,2,1,1]
                       , [2,4,2,3,4,3]
                       , [2,3,2,1,2,2]
                       , [3,4,1,3,3,2]
                       , [3,3,1,3,4,3] ]

m = fmap toPoke testMatrix

n = fromLists
    [ [1,1,1,2,3,4]
    , [1,2,3,4,2,3]
    , [1,2,3,4,2,3]
    , [2,3,4,1,2,3]
    , [3,4,1,2,3,4]
    , [4,1,2,3,4,1]]

(!!) :: Matrix a -> (Int, Int) -> Maybe a
(!!) m (row, col) = safeGet row col m

moveLeft :: (Int, Int) -> (Int, Int)
moveLeft (row, col) = (row, col - 1)

moveRight :: (Int, Int) -> (Int, Int)
moveRight (row, col) = (row, col + 1)

moveUp :: (Int, Int) -> (Int, Int)
moveUp (row, col) = (row - 1, col)

moveDown :: (Int, Int) -> (Int, Int)
moveDown (row, col) = (row + 1, col)

swap :: Matrix a -> (Int, Int) -> (Int, Int) -> Matrix a
swap m i j = let t = m ! i
                 m' = setElem (m ! j) i m
             in  setElem t j m'

mkZipper :: Matrix a -> (Int, Int) -> Zipper a
mkZipper m index = Zipper ( m !! (moveLeft . moveLeft $ index)
                          : m !! moveLeft index
                          : m !! moveRight index
                          : m !! (moveRight . moveRight $ index)
                          : []
                          )
                          (fromJust (m !! index))
                          ( m !! (moveUp . moveUp $ index)
                          : m !! moveUp index
                          : m !! moveDown index
                          : m !! (moveDown . moveDown $ index)
                          : []
                          )

everyElement :: Matrix (Int, Int)
everyElement = fromList 6 6 [(x,y) | x <- [1..6], y <- [1..6]]

scoreZipper :: Ord a => Zipper a -> [(a, Int)]
scoreZipper (Zipper a b c) = let lr = filter possible $ execState (traverse pushMaybe a) []
                                 ud = filter possible $ execState (traverse pushMaybe c) []
                             in lr ++ ud
                                where possible = (\(x,y) -> y >= 2)

pushMatrix :: (Eq a) => Matrix a -> [(a, Int)]
pushMatrix m = concat $ fmap (\l -> execState (traverse push l) []) (fmap reverse . toLists $ m)

pushMatrixCol :: (Eq a) => Matrix a -> [(a, Int)]
pushMatrixCol = pushMatrix . transpose

popMatrix :: [(a, Int)] -> Matrix a
popMatrix l = fromList 6 6 $ evalState (traverse (\_ -> pop) (toList $ zero 6 6)) l

clean :: (Eq a, Num a, Show a) => Matrix a -> Matrix a
clean m = let h = pushMatrix m
              v = pushMatrixCol m
              l = popMatrix (replace h 0 id)
              r = transpose (popMatrix (replace v 0 (\_ -> 1)))
          in  l `dot` r

replace t i f = fmap (\(x,y) -> if y >= 3 then (i, y) else (f x,y)) t

dot :: (Num a) => Matrix a -> Matrix a -> Matrix a
dot m n = fromList 6 6 $ zipWith (*) (toList m) (toList n)

sortZero :: (Num a, Ord a) => [(a, Int)] -> [(a, Int)]
sortZero l = filter ((== 0) . fst) l ++ filter ((/= 0) . fst) l

sink :: (Num a, Ord a) => Matrix a -> Matrix a
sink m = transpose . popMatrix . concat . fmap sortZero $
         fmap (\l -> execState (traverse push l) []) (fmap reverse . toLists $ transpose m)

