#!/usr/bin/env execthirdlinedocker.sh

-- runghc ${1}  ${2} ${3}

import Control.Applicative
import Data.Monoid

main = do
  print $ Just "hello" <|> Just "world" 
  print $ empty <|> Just "world"
  print $ Just "hello" <> Just " world"
  print $ Just empty   <> Just " world"
  
  print $  (,)  <$> Just "hello" <*> Just "world"
  