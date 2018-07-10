#!/usr/bin/env execthirdlinedocker.sh

-- runghc ${1}  ${2} ${3}

import Control.Monad.IO.Class (liftIO)
import Transient.Base
import Transient.Indeterminism

main = keep $ do
    option "run" "run"
    name <- input (const True) "What is your name? " :: TransIO String
    n <- choose' [1..10 ::Int] 
    liftIO $ do
        print name
        print n
