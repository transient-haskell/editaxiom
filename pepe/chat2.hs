#!/usr/bin/env execthirdlinedocker.sh
-- compile it with ghcjs and  execute it with runghc
-- runghc ${1}  ${2} ${3}

import Prelude hiding (div, id, span)
import Transient.Base
import Transient.Move
import Transient.Move.Utils
import Control.Monad.IO.Class
import Control.Applicative

main= keep $ initNode $ inputNodes <|> do
   local $ option "msg" "send a message"
   nodes <- local getNodes
   if length nodes < 2 
      then do
       localIO $ print "no nodes connected"
       empty
      else return ()
   txt <- local $ input (const True) "message> "
   runAt (nodes !! 1) $ localIO $ putStrLn txt