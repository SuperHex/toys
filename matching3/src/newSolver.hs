{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module New where

import           Control.Comonad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Functor.Identity
import           Data.Matrix

class Adj (m :: * -> * -> *) r where
  adj :: Position -> m r [[r]]
  chain :: Position -> m r [[r]]

type Position = (Int,Int)

data Env r a = Env { runEnv :: Reader (Matrix r) a }

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

(!?) :: Matrix r -> Position -> Maybe r
mat !? (i,j) = safeGet i j mat

run :: Env r a -> Matrix r -> a
run (Env e) m = runReader e m
