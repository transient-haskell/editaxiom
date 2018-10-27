#!/usr/bin/env execthirdlinedocker.sh

-- mkdir -p ./static && ghcjs ${1} -o static/out && echo ${1} && runghc ${1}  ${2} ${3}
{-# LANGUAGE   CPP, NoMonomorphismRestriction, DeriveDataTypeable  #-}

module Main where

import Prelude hiding (div,id)
import Transient.Internals

import GHCJS.HPlay.Cell
import GHCJS.HPlay.View hiding (map, input,option,parent)
--import Transient.TLS
import Transient.Move
import Transient.EVars
import Transient.Indeterminism

import Control.Applicative
import qualified Data.Vector as V
import qualified Data.Map as M
import Transient.MapReduce
import Control.Monad.IO.Class
import Control.Monad
import Data.String
import qualified Data.Text as T

#ifdef ghcjs_HOST_OS
import qualified Data.JSString as JS hiding (span,empty,strip,words)
#endif

import Data.Typeable
import Data.Monoid

import qualified Data.ByteString.Lazy.Char8 as BS
import Transient.Logged


import Transient.Internals
import Control.Concurrent.MVar
import System.IO.Unsafe
import Control.Concurrent
import Control.Monad.State
import Control.Concurrent.STM

data Options= MapReduce | Chat | MonitorNodes | AllThree deriving (Typeable, Read, Show)


main = do
--  initTLS
  keep $  initNode  $ menuApp  <|> thelink <|> inputNodes <|> consolemp


consolemp= onServer $  do
   local $ option "count"  "count words with distributed map-reduce"
   content <- local $   input (const True)  "give me a phrase between quotes: "
   r<- reduce  (+) . mapKeyB (\w -> (w, 1 :: Int))  $ distribute $ V.fromList $ words content
   localIO $ print  (r :: M.Map String Int)





thelink=   do
     local . render $ rawHtml $ do
         br;br
         a ! href (fs "https://github.com/agocorona/transient-universe/blob/master/examples/distributedApps.hs") $ "source code"
     empty

menuApp= do
     local . render . rawHtml $ do
        h1 "Transient Demo"
        br; br
     op <- local .  render $
               wlink MapReduce (b "map-reduce") <++ fs "   " <|>
               wlink Chat (b "chat")  <++ fs "   " <|>
               wlink MonitorNodes (b "monitor nodes") <++ fs "   "  <|>
               wlink AllThree  (b "all widgets")

     case op of
               AllThree -> allw
               MapReduce -> mapReduce                        -- !> " option mapReduce"
               Chat -> chat
               MonitorNodes -> monitorNodes


allw=   mapReduce <|> chat  <|>  monitorNodes


placeholder= atr "placeholder"

-- A Web node launch a map-reduce computation in all the server nodes, getting data from a
-- textbox and render the results returned

mapReduce= onBrowser $ do

    content <- local . render $
            h1 "Map-Reduce widget" ++>
            p "Return the frequencies of words from a text using all the server nodes connected" ++>
            textArea   (fs "") ! placeholder (fs "enter the content")
                               ! atr "rows"  (fs "4")
                               ! atr "cols"  (fs "80")
            <++  br
            <**  inputSubmit "send"  `fire` OnClick
            <++  br
--    return () !> ("content",content)

    guard (content /= "")
    msg <-   local genNewId

    r <- atRemote $ do
           lliftIO $ print content
           r<- reduce  (+) . mapKeyB (\w -> (w, 1 :: Int))  $ distribute $ V.fromList $ words content
           lliftIO $ putStr "result:" >> print r
           return   (r :: M.Map String Int)


    local . render $ rawHtml $ do
             h1 "Results"
             mconcat[i "word " >> b w >> i " appears " >> b n >> i " times" >> br
                    | (w,n) <- M.assocs r]

    empty

fs= fromString
size= atr (fs "size")
-- a chat widget that run in the browser and in a cloud of servers


chat = onBrowser $ do
    let chatbox= fs "chatbox" -- <- local  genNewId
    local . render . rawHtml $ do   -- Perch monads
            h1 "Federated chat server"

            div ! id chatbox
                ! style (fs $"overflow: auto;height: 200px;"
                         ++  "background-color: #FFCC99; max-height: 200px;")
                $ noHtml  -- create the chat box

    sendMessages   <|>  waitMessages chatbox

  where

  sendMessages   = do

      let msg = fs "messages" -- <-   local genNewId
      let entry= boxCell msg ! size  (fs "60")
      (nick,text) <- local . render $  (,) <$> getString (Just "anonymous") ! size (fs "10")
                                           <*> mk entry Nothing  `fire` OnChange
                                           <** inputSubmit "send"
                                           <++ br
      local $ entry .= ""
      guard (not $ null text)

      atRemote $ do
          node <- local getMyNode
          clustered $ local $ putMailbox  (showPrompt nick node ++ text )  >> empty :: Cloud ()
          empty

      where
      fs= fromString

      showPrompt u (Node h p _ _)= u ++ "@" ++ h ++ ":" ++ show p ++ "> "

  waitMessages chatbox = do

      resp <-  atRemote . local $ do
           labelState $ "getMailbox"
           r <-  single getMailbox
           return r
                                                           -- wait in the server for messages

      local . render . at (fs "#" <> chatbox) Append $ rawHtml $ do
                                          p (resp :: String)    -- display the response
#ifdef ghcjs_HOST_OS
                                          liftIO $ scrollBottom $ fs "chatbox"


foreign import javascript unsafe
  "var el= document.getElementById($1);el.scrollTop=  el.scrollHeight"
  scrollBottom  :: JS.JSString -> IO()
#endif

monitorNodes= addNode <|> listNodes

addNode= onBrowser $ do

   (host,port) <- local . render $ br ++> br ++>
                  ((,) <$> getTextBox Nothing ! placeholder (fs "host")
                       <*> getTextBox (Just 80)
                       <** submitButton "connect" `fire` OnClick)

   atRemote $ do
      node <- localIO $ createNode host port
      connect' node

--showThreads= do
--    ths <- atRemote threads

listNodes= onBrowser $ do
    local . render $ rawHtml $ do
         h1 "Nodes connected"
         div ! atr (fs "id") (fs "nodes") $ noHtml

    nodes <- atRemote . local  . single $  sample getNodes 1000000

    local . render . at (fs "#nodes") Insert . rawHtml $
           table $ mconcat[tr $ td h >> td p >> td s | Node h p _ s  <- nodes]
    empty
