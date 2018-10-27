#!/usr/bin/env execthirdlinedocker.sh
-- compile it with ghcjs and  execute it with runghc
-- mkdir -p ./static && ghcjs -itransient/src -itransient-universe/src  -iaxiom/src ${4} ${1} -o static/out && ghc -itransient/src -itransient-universe/src -iaxiom/src -threaded ${4} ${1} &&  ./editaxiom/`basename -s .hs  ${1}` ${4}  ${2} ${3}

-- set -e && port=`echo ${3} | awk -F/ '{print $(3)}'` && docker run -it -p ${port}:${port} -v $(pwd):/work agocorona/transient:new  bash -c "cd /work && mkdir -p ./static && ghcjs -itransient/src -itransient-universe/src  -iaxiom/src ${4} ${1} -o static/out && ghc -itransient/src -itransient-universe/src -iaxiom/src -threaded ${4} ${1} &&  ./editaxiom/`basename -s .hs  ${1}` ${4}  ${2} ${3}"

{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable #-}

import Control.Concurrent(threadDelay)
import Control.Exception(SomeException, catch)
import Control.Monad  
import Control.Monad.State hiding (get)
import Data.IORef
import Data.Monoid
import GHCJS.HPlay.View hiding (input, option)
import GHCJS.HPlay.Cell
import Prelude hiding (div, id, span)
import System.Directory
import System.IO.Unsafe
import System.IO
import System.Process
import Transient.Internals 
import Transient.Move.Internals hiding(pack,JSString)
import Transient.Parse
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List hiding(span)
import Data.Typeable
#ifdef ghcjs_HOST_OS
import GHCJS.Marshal (fromJSValUnchecked)
import GHCJS.Prim (JSVal)
#endif

{- TODO
option : start/env-host/env-port

añadir ejemplos
  carpeta
inicio automatico

user authentication
file persistence

add compiler service as a compile.hs which uses the docker image
  compile :: Source -> Tar(Binary,JScript)
 also with REST
 download compiled file
 multiple source files imports
 compile on file change done
 import of a library not present in the docker image?

 añadir nodos, ver carpetas remotas, ejecutar distribuido.
 chat edicion simultanea, widgets
-}


-- main= keep $ initNode $  onBrowser $ synchr $ do
--   -- local $ setSynchronous True >> return ()
--   line  <- local $  threads 0 $ choose[1..10::Int] 
--   localIO $ print ("1",line)
--   line2 <- atRemote $ synchr $ local $  choose [100..102 :: Int] --localIO $ print line -- >> empty :: Cloud String
--   localIO $ print ("2", line,line2) 
  

main = keep . initNode $  doit
  
newtype Port= Port String deriving (Read,Show,Typeable)

doit= onBrowser $ do
  port <-  local $ Port <$> getPort

  atRemote $ local $ setRState (Nothing ::Maybe ProcessHandle)
  let filenamew= boxCell "filename" 
  
  ide filenamew port <|> consoleControlFrames <|> folderNav filenamew -- <* resizable
  where
  ide filenamew port= do
    local $ (render .  rawHtml $ do
                    div ! id "entername" $ noHtml
                    aceEdit) <** zenEditor
                    
    (file,source) <- editsOfCode  filenamew
    local $ when (null file) $ alert "enter source file name, for examaple: 'yourfile.hs'" >> empty
    result <- compile file source port
    present result port
  
  folderNav filenamew=  do
    (file,source) <-  folder  "."
    localIO $ setEditorContent $ pack source
    local $ filenamew .= file

newtype ZenEditor= ZenEditor Bool
zenEditor= do

    setRState $ ZenEditor True

    render $ (div ! id "zened" ! style unzenStyle $ str "zen")  `pass` OnClick
    ZenEditor mode <- getRState
    setRState $ ZenEditor $ not mode
    render $ do
      case mode of
        True  -> do
            rawHtml $ forElemId "editor" $ this ! style zoomedEditorStyle
            rawHtml $ forElemId "zened" $ clear >> this ! style zenStyle `child` str "unzeen"

        False -> do
            rawHtml $ forElemId "editor" $ this ! style unzoomedEditorStyle
            rawHtml $ forElemId "zened" $ clear >> this ! style unzenStyle `child` str "zen"
    where
    unzoomedEditorStyle=  "width: 83%;height:70%;z-index:0"
    zoomedEditorStyle=    "width: 100%;height:100%;background-color:#ffffff;z-index:10"
    zenStyle=             "position:absolute;top:0%;left:90%;height:20px;cursor:pointer;background-color: #eeaaaa;z-index:10" 
    unzenStyle=           "position:absolute;top:0%;left:75%;height:10px;cursor:pointer;background-color: #eeaaaa" 

resizable= local $ do
  render $ rawHtml $ do
    link ! href "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/themes/base/jquery-ui.css" 
         ! atr "rel" "stylesheet" 
         ! atr "type" "text/css" 
    script ! src "http://ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js" $ noHtml
    script ! src "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/jquery-ui.min.js" $ noHtml

  liftIO $ threadDelay 1000000
  liftIO resizejs
  return ()

clas= atr "class"


newtype ZenConsole = ZenConsole Bool deriving Typeable
consoleControlFrames= local $ do 
  setRState $ ZenConsole True

  render $ rawHtml $ do
        div ! id "frame" ! clas "resize" !style "overflow:auto;position:absolute;top:75%;left:0;right:0;height:20%;background-color: #eeeeee;" $ noHtml
        div ! id "frameinput" !clas "resize" ! style "position:absolute;top:95%;bottom:100%" $ noHtml

  render $ (div ! id "zen" ! style "position:absolute;top:75%;left:95%;height:20px;cursor:pointer;background-color: #eeaaaa" $ str "zen")  `pass` OnClick
  ZenConsole status <- getRState
  setRState $ ZenConsole $ not status
  render $ rawHtml $ case status of
        True ->do
            forElemId "frame" $ this ! style "overflow:auto;position:absolute;top:0%;left:0;right:0;height:95%;z-index:10;background-color: #eeeeee;" 
            forElemId "zen" $ clear >> this ! style "position:absolute;top:0%;left:92%;height:20px;cursor:pointer;z-index:20;background-color: #eeaaaa" `child` str "unzeen"

        False -> do 
            forElemId "frame" $ this ! style "overflow:auto;position:absolute;top:75%;left:0;right:0;height:20%;z-index:0;background-color: #eeeeee;"  
            forElemId "zen" $ clear >> this ! style "position:absolute;top:75%;left:95%;height:20px;cursor:pointer;z-index:0;background-color: #eeaaaa" `child` str "zen"
  
  liftIO $ scrollBottom "frame"


folder fol= onBrowser $ do
  local $ render $ rawHtml $ div ! id "dir" ! clas "resize" ! style "overflow:auto;position:absolute;left:85%;height:70%" $ noHtml

  files <- atRemote . localIO $ getDirectoryContents fol >>= return . sort
  folder' "." $ filter (/= ".")  files
  where
  folder' :: String -> [String] -> Cloud (String,String)
  folder' dir files=  do
    file <- local . render $ 
          at "#dir" Insert $ do
                    rawHtml $ b $ "folder: " <> dir
                    foldr  (<|>) empty [pre f ! style "cursor:pointer" 
                                          `pass` OnClick >> return f | f <- files]
    
    let filedir= if file== ".." 
                   then  take (let is= elemIndices '/' dir 
                               in if null is then length dir else last is) dir 
                   else dir ++ "/"++ file

    r <- atRemote $  localIO $ do

      isFile <- doesFileExist  filedir !>  ("FILEDIR",filedir,file)
      case isFile of 
        True  -> Right <$> do  source <- readFile filedir 
                                     `catch` \(e:: SomeException) -> return "File is not a text"
                               return (filedir, source)

        False -> Left  <$> tail <$> liftIO (getDirectoryContents filedir)
    case r of
      Right fileinfo -> return fileinfo 
      Left files  -> folder' filedir files     

getPort = liftIO $ do
  x <- atomicModifyIORef counter $ \n -> (n + 1, n + 1)
  return . show $ 8000 + x
  where
  counter = unsafePerformIO $ newIORef 0

str s= s :: JSString


editsOfCode :: Cell String -> Cloud (String, String)    
editsOfCode filenamew =  local $ do 
      rlive <- liftIO $ newIORef False
      fileNameWidget  **> saveCompile  <|> changeContent rlive
      content <- copyContent
      name <- get filenamew
      liftIO $ js_setAnnotations  "[]"
      return (name,content)
  where
    fileNameWidget = render $ at "#entername" Insert 
                      $ mk filenamew Nothing 
                      ! placeholder "enter yourfilename.hs please" 
  
    saveCompile=  render (at "#entername" Append $ inputSubmit ("save/compile" ::String) `fire` OnClick >> return())
  
    changeContent rlive=  do
      liveCoding rlive  <|>  setLive rlive
      
      where 
      liveCoding rlive= do
          react onmodify $ return () 
          r <- liftIO $ readIORef rlive 
          if r then return () else empty 

    setLive rlive= do
        r <- render $ at "#entername" Append $ getCheckBoxes 
                    $ setCheckBox False ("autosave" :: String) `fire` OnChange 
                    <++ span ("autosave" ::String)
        liftIO $ writeIORef rlive $ if not $ null r then True else False  
        empty     
 

compile :: String -> String -> Port -> Cloud (Maybe String)
compile file source (Port port) = atServer $ do
    localIO $ maybeKillProgram port

    localIO $ writeFile file source !> file
    
    -- r <- execShell  $  "chmod 777 "++ file ++ " && cd `dirname "++ file ++ "` && eval ./`basename "++ file ++ "` -p start/localhost/"++ port
    r <- execShell file ["-p","start/localhost/"++ port]        
      --file ++ " -p start/localhost/"++ port -- ++" &"
    return r
    

subst _ _ [] = []
subst a b s@(x:xs) = 
                       if isPrefixOf a s
                                then b++subst a b (drop (length a) s)
                                else x:subst a b xs


#ifndef ghcjs_HOST_OS
createProcess1 x= createProcess x
#else
createProcess1 x= return (Just $ error "IIII", Just $ error "OOOOO", Just $ error "ERRR", error "HANDLE")
#endif 

onlyOnServer :: x -> x
onlyOnServer x =
#ifndef ghcjs_HOST_OS
     x
#else
     error "this statement should be executed on server" 
#endif 


type Errors= String
execShell :: String ->[String] -> Cloud (Maybe Errors) -- [(BS.ByteString,Int,Int,BS.ByteString)]
execShell expr params = onServer $ do
      return () !> ("EXECUTING", expr)
      r <- lazy $ liftIO $ createProcess1 $ (proc expr params){std_in=CreatePipe,std_err=CreatePipe,std_out=CreatePipe}
      local $ setRState $ Just $ handle r
      makeinput r <|> watch r <|> watcherror r
      where
      input1 r= inp where (Just inp,_,_,_)= r
      output r= out where (_,Just out,_,_)= r
      err r= err where  (_,_,Just err,_)= r
      handle r= h where  (_,_,_,h)= r
      makeinput r= onServer $ do 
        local $ setSynchronous False
        local abduce

        inp <- atBrowser . local . render $ do  
            let command= boxCell "command" 
            r <- at "#frameinput" Insert $ mk command Nothing 
                                          ! placeholder "command"  
                                          ! atr "size" "100"
                                          `fire` OnChange
            command .= ""
            return r
       
        localIO $ do print inp ; hPutStrLn (input1 r) inp ; hFlush (input1 r) 
        empty

      watch r=  onServer $ syncStream $  do

        mline  <- local $ threads 0 $ (parallel $  (SMore <$> hGetLine' (output r)) `catch` \(e :: SomeException) -> return SDone)
                          --   <|> async (return  $ SMore "-------------------- Executing ---------------------")
        case mline of
           SDone -> empty
           SMore line ->  do
              atRemote $ local $ do
                  render . at "#frame"  Append  $ rawHtml $ pre ! style "line-height:50%;word-wrap:break-word" $line

                  liftIO $ scrollBottom "frame"
                  
              localIO $ putStrLn line
              if ("port to listen?" `isPrefixOf` line) 
                then return $ Just "[]"
                else empty
        where
        hGetLine' h= do
          buff <- newIORef []
          getMore buff
          where

          getMore buff= do
            b <- hWaitForInput h 10
            if not b 
                then do
                   r <-readIORef buff 
                   if null r then getMore buff else return r 
                else do
                      c <- hGetChar h
                      if c== '\n' then readIORef buff else do 
                        modifyIORef buff $ \str -> str ++ [c]
                        getMore buff

      watcherror r= onServer $ do
        local abduce
        localIO $ waitForProcess $ handle r
        errors <- localIO $  hGetContents (err r)
        atBrowser $ local $ do
           render . at "#frame"  Append  $ rawHtml $ pre ! style "color:red;word-wrap:break-word" $  errors
           liftIO $ scrollBottom "frame"
        r <- local $ Just <$> parseResp errors
        localIO $ print r
        return r


present :: Maybe Errors  -> Port -> Cloud()
present result (Port port)= local $ do
  serverNode <- getWebServerNode
  render . rawHtml $ do

    case result of
      Nothing -> liftIO $ js_setAnnotations  "[]"

      Just "[]" -> do

        liftIO $ js_setAnnotations  "[]"
        liftIO $ wopen $ "http://" <> (pack $ nodeHost serverNode) <> ":" <> (pack $ show $ nodePort serverNode) <>"/relay/localhost/"<> pack port <> "/" 


      Just errors ->
        liftIO $ js_setAnnotations $ toJSString errors


maybeKillProgram port =  
  callCommand ("pkill --full "++ port)  `catch` \(e :: SomeException) -> print e
  --  ms <- getRState <|> return Nothing
  --  when (isJust ms) $  liftIO $ do 
  --     let s = fromJust ms
  --     -- code <- liftIO $ getProcessExitCode s

  --     terminateProcess s
  --     e <- waitForProcess s
  --     print e
  --     return ()


     
parseResp :: String-> TransIO String -- [(BS.ByteString,Int,Int,BS.ByteString)]
parseResp []= return ""
parseResp err2  = do
    setState $ ParseContext (return SDone) $ BS.pack err2 !> ("ERRORS",err2)
    r <-  parseErrors
    return $ toJSON r
  <|> 
    (return $ "[{'row': '2','column':'0','text':'" <> mapchars err2 <> "','type':'error'}]")

  where
  parseErrors = try(manyTill multilineError isDonep) <|> (isDone >>= \c -> if c !> ("d",c) then empty else anyChar >> parseErrors !> "parseerrors")
  isDonep= isDone >>= \c -> if c  then return ' ' else empty

  toJSON ers= "[" <> (concat $ map oneerr ers) <> "{}]"
    
  oneerr (file,l,c,msg,typ)="{'row': '" <> (show $ l-1) <> "','column':'" <> (show c) 
                             <> "','text':'" <> (mapchars $ BS.unpack  msg) <> "','type':'" <> typ <>"'},"

  mapchars rs=  subst "\CAN" "\\\'" $ subst "\EM" "\\\'"  $ subst "'" "\\\'" $ subst "\n" "\\n"  rs :: String

--  isRight x = x /= '\n'  && ( x == ':' || x == '/' || x == '-' || x == '>' || x== '<' || x== '.' || x=='\\' || isAlphaNum x || isSpace x || isNumber x)

  multilineError :: TransIO (BS.ByteString,Int,Int,BS.ByteString,String)
  multilineError= do
    file <- tTakeWhile' (/= ':')  !>  "multilineError"
    line <- int
    anyChar
    col  <- int
    anyChar
    typ <- (try $ string "\n" >> return "error") <|> 
           (try $ string " Warning:\n" >> return "warning") <|> 
           return "error"
    lines <- getErrLines
    let linesError = BS.concat $ map (<> "\\n") lines 
    return (file, line, col, linesError,typ)

  getErrLines  ::  TransIO [BS.ByteString]
  getErrLines = manyTill ( tTakeWhile' (/= '\n')) endError 
  endError= tChar '\n' <|> isDonep
 


placeholder = atr "placeholder"

type_ = atr "type"


aceEdit  = do
  let styleElem x = nelem "style" `child` x

  styleElem ! type_ "text/css" ! atr "media" "screen" $
    ("#editor {" :: String) ++
    "position:absolute;" ++ "width: 83%;" ++ "height:70%" ++ "}"

  script ! src "//cdnjs.cloudflare.com/ajax/libs/ace/1.3.2/ace.js" !
    type_ "text/javascript" !
    atr "charset" "utf-8" $
    noHtml

  span ! id "editor" ! atr "style" "position:absolute;width: 83%;height:70%" $ noHtml


  liftIO $ threadDelay 1000000
  script ! type_ "text/javascript" $
    str $
    "var editor = ace.edit('editor');" <>
    -- "editor.setTheme(\"ace/theme/monokai\");" <>
    "editor.session.setMode(\"ace/mode/haskell\");"

#ifdef ghcjs_HOST_OS

foreign import javascript unsafe "window.open($1,'run')" wopen :: JSString -> IO ()

foreign import javascript safe "editor.getValue()" js_copyContent  :: IO JSVal

foreign import javascript safe "editor.setValue($1)" setEditorContent  :: JSString -> IO()

foreign import javascript safe 
   "var div = document.getElementById($1);div.scrollTop = div.scrollHeight;" 
   scrollBottom :: JSString ->IO()

foreign import javascript unsafe "editor.getSession().setAnnotations(eval($1))"
    js_setAnnotations :: JSString -> IO ()

copyContent :: TransIO String
copyContent = liftIO $ js_copyContent >>= fromJSValUnchecked

foreign import javascript safe "editor.getSession().on('change',$1)"
   js_onmodify :: JSVal -> IO ()

foreign import javascript safe "editor.getSession().on('change',alert('no'))"
    js_nomodify :: IO ()

--newtype ModifyEvent = ModifyEvent JSVal deriving Typeable
onmodify ::   (() ->IO()) -> IO ()
onmodify continuation= do
   cb <- makeCallback  (continuation ())
   js_onmodify cb

foreign import javascript unsafe "$('#editor').resizable({ alsoResize: '.resize' });$('.resize').resizable({ alsoResize: '.resize' });"  resizejs :: IO()


#else
resizejs= undefined
wopen :: JSString -> IO ()
wopen= undefined
setEditorContent :: JSString -> IO()
setEditorContent= undefined
js_setAnnotations = undefined

scrollBottom :: JSString -> IO()
scrollBottom= undefined


copyContent :: TransIO String
copyContent = empty

onmodify ::   (() ->IO()) -> IO ()
onmodify= undefined
js_nomodify= undefined
#endif
    
