#!/usr/bin/env execthirdlinedocker.sh
-- compile it with ghcjs and  execute it with runghc
-- runghc ${1}  ${2} ${3}

-- multithreaded quicksort modifying minimally the standard definition

{-# LANGUAGE MonadComprehensions #-}
import           Transient.Base
import           Data.Monoid
import           Control.Monad.IO.Class
import           Control.Concurrent(myThreadId)

main= keep' $ qsort [9,3,4,6,9,12,3,7,0,2,1,45 :: Int] >>= liftIO . print

qsort :: (Eq a,Ord a,Show a) => [a] -> TransIO [a]

qsort []	= return []
qsort (x:xs) =  newThread(qsort small) <>  mid <> newThread(qsort large)
  where
    small = [y | y<-xs, y<x]
    mid   = return $ [y | y<-xs, y==x] ++ [x]
    large = [y | y<-xs, y>x]

newThread mx= do
    abduce 
    list <- mx
    th <- liftIO myThreadId
    return  list

