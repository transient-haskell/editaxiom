#!/usr/bin/env execthirdlinedocker.sh
-- compile it with ghcjs and  execute it with runghc
-- mkdir -p ./static && ghcjs ${1} -o static/out && echo ${1} && runghc ${1}  ${2} ${3}

import Prelude hiding (div, id, span)
import Transient.Base
import GHCJS.HPlay.View
import Transient.Move
import Transient.Indeterminism
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Data.Monoid

main= keep . initNode . onBrowser $ do 
    local . render $  wlink () (h1 "hello fibona cci numbers")

    r <-  atRemote $ do
                r <- local . threads 1 . choose $ take 10 fibs
                localIO $ print r
                localIO $ threadDelay 1000000
                return r

    local . render . rawHtml $ (p r)
    where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]  -- fibonacci numb. definition

