{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module New where

import           Control.Applicative
import           Control.Comonad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Functor.Identity
import           Data.Matrix

class Adj (m :: * -> * -> *) r where
  adj :: Position -> m r [[r]]
  chain :: Position -> m r [[r]]

class HasDamage r where
  damageOf :: r -> Int

type Position = (Int,Int)

data Env r a = Env { runEnv :: Reader (Matrix r) a }

instance HasDamage Int where
  damageOf = id

instance Adj Env r where
  adj (i,j) = Env $ do mat <- ask
                       let l = [[mat !? (i-2,j), mat !? (i-1,j)]
                               ,[mat !? (i+1,j), mat !? (i+2,j)]
                               ,[mat !? (i,j-2), mat !? (i,j-1)]
                               ,[mat !? (i,j+1), mat !? (i,j+2)]]
                        in return $ fmap may l
                           where may (x:xs) = case x of
                                                Just x -> x : may xs
                                                Nothing -> may xs
                                 may [] = []

  chain (i,j) = Env $ do mat <- ask
                         (l:r:u:d: []) <- runEnv $ adj (i,j)
                         let (Just focus) = mat !? (i,j)
                          in return $ [l ++ [focus] ++ r, u ++ [focus] ++ d]

sample :: Matrix Int
sample = fromLists
         [[1,2,2,3,4,4]
         ,[2,2,2,1,2,4]
         ,[3,2,1,3,4,3]
         ,[1,1,3,4,2,1]
         ,[1,1,3,2,3,4]
         ,[4,3,2,1,2,3]]

(!?) :: Matrix r -> Position -> Maybe r
mat !? (i,j) = safeGet i j mat

run :: Env r a -> Matrix r -> a
run (Env e) m = runReader e m

everyElement :: [Position]
everyElement = [(x,y) | x <- [1..6], y <- [1..6]]

exchange :: Position -> Position -> Env r (Matrix r)
exchange x y = Env $ do m <- ask
                        let content = m ! x
                            m' = setElem (m ! y) x m
                        return $ setElem content y m'

allPossible :: [Env r (Matrix r)]
allPossible = liftA2 exchange everyElement everyElement

fold :: (HasDamage r, Eq r) => [[r]] -> Int
fold ~(x:xs:[]) = loop x 1 (head x) + loop xs 1 (head xs)
                  where loop (y:ys:yss) n o | n >= 3 = n * damageOf o
                                            | otherwise = if y == ys
                                                             then loop (ys:yss) (n+1) ys
                                                             else loop (ys:yss) 1 ys
                        loop _ n o | n >= 3 = n * damageOf o
                                   | otherwise = 0
