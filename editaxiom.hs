#!/usr/bin/env execthirdlinedocker.sh

-- mkdir -p ./static && ghcjs -i../transient/src -i../transient-universe/src  -i../axiom/src -DDEBUG ${1} -o static/out && ghc -DDEBUG -threaded -i../transient/src -i../transient-universe/src -i../axiom/src  ${1} && ./`basename $1 .hs`  ${2} ${3} 


{-# LANGUAGE CPP, FlexibleContexts,OverloadedStrings, FlexibleInstances, UndecidableInstances, ScopedTypeVariables, DeriveDataTypeable #-}

import Control.Concurrent(threadDelay)
import Control.Exception(SomeException, catch)
import Control.Monad  
import Control.Monad.State hiding (get)
import Data.IORef
import Data.Monoid
import GHCJS.HPlay.View hiding (input, option, select)
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
import GHCJS.Marshal
-- import           GHCJS.Foreign.Internal
#else 
import Data.TCache hiding (onNothing)
import Data.TCache.DefaultPersistence
import Data.TCache.IndexQuery
import Control.Monad.STM(atomically)
-- import qualified Data.HashTable.IO as HT
#endif



{- TODO
option : start/env-host/env-port

añadir ejemplos
  carpeta
inicio automatico

user authentication x
file persistence  x

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

#ifdef ghcjs_HOST_OS 
#define GHC(exp) return undefined
#else 
#define GHC(exp) (exp)
#endif



main =   do
  GHC(databaseIndices)
  keep $ initNode doit


#ifndef ghcjs_HOST_OS

databaseIndices=  do

  index userName
  index sharedDoc
  index sharedNode
  index sharedOwner
  index sharedUser
  index connectedUser

#endif


newtype Port= Port String deriving (Read,Show,Typeable)


doit= onBrowser $ do
  port <- authenticate 
  verifyDir 

  let filenamew= boxCell "filename" 
  
  ide filenamew port <|> consoleControlFrames  <|>  folderNav filenamew :: Cloud () -- <* resizable 
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
    UserName u <- local getState
    (file,source) <-  folder  u u
    localIO $ setEditorContent $ pack source
    local $ filenamew .= file

type Name= String
type Pass= String
type Email= String

#ifndef ghcjs_HOST_OS

data User= User{ userName:: Name, email :: Email, userPassword :: Pass, userConnected :: Maybe (DBRef Connected)}
            deriving (Read,Show, Typeable)

type File= String 
type ServerNode= String
type WebNode= String
-- newtype Invites= Invites [(Node,File)] deriving (Read,Show,Typeable)

  
data Shared= Shared{ sharedId     :: Int
                   , sharedDoc    :: File
                   , sharedNode   :: Node
                   , sharedOwner  :: String
                   , sharedUser   ::String}
                   deriving (Read,Show,Typeable)
                   
instance Indexable Shared where key Shared{sharedId=id}= "Shared#"++show id

data Connected= Connected{ connectedId :: Int,connectedUser :: DBRef User, connectedHost :: Node
                         , connectedWebNode :: Node} deriving (Read,Show, Typeable)

instance Indexable Connected where key = (++) "Connected#"  . show . connectedId

instance Indexable User where key = userName
instance (Show a, Read a) => Serializable a where
  serialize  = pack . show
  deserialize= read . unpack
#endif

newtype UserName= UserName String deriving(Read,Show, Typeable)

verifyDir =  do
  UserName u <- local getState <|> error "verifyDir: no user set"
  atServer $ do
      exist <- localIO $ doesDirectoryExist u 
      when (not exist) $ do 
          localIO $ createDirectory  u 
          localIO $ callCommand $ "cp -r ./editaxiom/examples/* "++ u
          return()

authenticate :: Cloud String
authenticate = do
  local $ render $ rawHtml $ div ! id "auth" ! style  "position:absolute;width:50%;height: 10%;margin-left: 50%" $ noHtml
  auth ""
  where
  auth :: String -> Cloud  String
  auth n = do
    (n,p,p') <- local $ render $ at "#auth" Insert $ 
                  (,,) <$> inputString (Just n) ! placeholder "username"  ! size "8"
                       <*> inputPassword ! size "8"
                       <*> inputPassword ! size "8" `fire` OnChange
                       <** inputSubmit ("ok" ::String) `fire` OnClick
    mr <- atRemote $ local $ do
#ifndef ghcjs_HOST_OS
            let ruser= getDBRef n
            mu <- liftIO $ atomically $ readDBRef ruser 
            case mu of
                Nothing -> 
                  if p== p' 
                    then do
                      port <-  liftIO $ getPort
                      on <-  updateConnectionInfo port
                      liftIO $ atomically $ newDBRef (User n p  [] $ Just on); 
                      return $ Just (n ,port) 
                    else return Nothing
                Just (user@User{userPassword=pstored}) -> do
                  port <- liftIO getPort
                  if p == pstored then do
                       on <- updateConnectionInfo port
                       liftIO $ atomically $ do
                          writeDBRef ruser $ user{userConnected= Just on}  -- XXX como borrarlo cuando se desconecta????
                          return $ Just (n,port)
                  else return Nothing
#else 
            undefined  :: TransIO (Maybe (String,String))

#endif

    case mr of
      Nothing ->   auth n 
      Just (r,port) -> do
        setState $ UserName n 

        local $ render $ at  "#auth" Insert $ do
            rawHtml $ clear >> span n
            (span  (str " change user") ! style "cursor:pointer")  `pass` OnClick
            return () 
        auth n
       <|> return port

    where
    size= atr "size"
#ifndef ghcjs_HOST_OS

    updateConnectionInfo port= do
      id <- genGlobalId
      host <- getMyNode
      webNode' <- liftIO createWebNode
      webNode'' <- setConnectionIn webNode'
      let webNode'''= webNode''{nodePort= read port}
      addNodes[webNode''']
      liftIO $ atomically $ newDBRef (Connected id (getDBRef n) host webNode''' )

#endif
        
      

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
    unzoomedEditorStyle=  "width: 83%;height:68%;z-index:0"
    zoomedEditorStyle=    "width: 100%;height:100%;background-color:#ffffff;z-index:10"
    zenStyle=             "position:absolute;top:2%;left:95%;height:20px;cursor:pointer;background-color: #eeaaaa;z-index:10" 
    unzenStyle=           "position:absolute;top:2%;left:75%;height:20px;cursor:pointer;background-color: #eeaaaa" 

-- resizable= local $ do
--   render $ rawHtml $ do
--     link ! href "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/themes/base/jquery-ui.css" 
--          ! atr "rel" "stylesheet" 
--          ! atr "type" "text/css" 
--     script ! src "http://ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js" $ noHtml
--     script ! src "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/jquery-ui.min.js" $ noHtml

--   liftIO $ threadDelay 1000000
--   liftIO resizejs
--   return ()

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


folder user fol= onBrowser $ do
  local $ render $ rawHtml $ div ! clas "resize" ! style "overflow:auto;position:absolute;left:85%;height:68%" 
                 $ div ! id "dirs" $ do
                     div ! id "invites" $ b $ str "Invites"
                     div ! id "dir" $ b $ str "Folder"
                     
  (node,files) <- atServer . local $ (,) <$> getMyNode <*> liftIO (getDirectoryContents fol >>= return . sort)
  -- setRenderTag "dir"

  invites <|> (folder' node "." $ map ((fol++"/") ++) files) 
  where
  invites= invitesNew <|> currentInvites user
    where
    invitesNew= do
        (n,f) <- atRemote $ local getMailbox :: Cloud (Node,String)
        elemFolder "#invites" n "" f
         
    currentInvites user= atServer $  do
       is <- localIO $ GHC(atomically $ select  (sharedNode,sharedDoc) $ sharedUser .==. user)
       foldr  (<|>) empty [elemFolder "#invites" n "" f | (n,f) <- is]
     
     
  folder' :: Node  -> String -> [String] -> Cloud (String,String)
  folder' n  dir files= do
      local . render $ rawHtml $ b $ "folder: " <> dir
      foldr  (<|>) empty [elemFolder "#dir" n dir f | f <- files]
    
    

  elemFolder tag node dir file= do
    local $ render $ at tag Append $ pre file ! style "cursor:pointer" `pass` OnClick >> return ()
    let filedir= if file== ".." 
                    then  take (let is= elemIndices '/' dir 
                                in if null is then length dir else last is) dir 
                    else if null dir then file else dir ++ "/"++ file

    r <- atServer $ runAt node $ localIO $ do

              isFile <- doesFileExist  filedir !>  ("FILEDIR",filedir,file)
              case isFile of 
                  True  -> Right <$> do  source <- readFile filedir 
                                           `catch` \(e:: SomeException) -> return "File is not a text"
                                         return (filedir, source)

                  False -> Left  <$> liftIO (getDirectoryContents filedir >>= return . sort)
    case r of
        Right fileinfo -> return fileinfo 
        Left files  -> do
             let id1= filedir -- id1 <- genNewId
             folder' node  filedir files

--   invites1 = do
--     user <- local getState :: Cloud User
--     userNames <- localIO $ do
--                  ur <- atomically $ indexOf userName 
--                  return $ fst $ unzip ur

--     inv <- atServer $  GHC (exploreNet $ local $ getInvites userNames) :: Cloud [(Node,File,Connected)]
--     -- Invites inv  <- local getMailbox <|> (atServer $ localIO $ currentInvites $ userName user)  
      
--     local $ render $ rawHtml $ b $ str "invites:"
--     foldr (<|>) empty [elemFolder n "" f | (n,f,c) <- inv]
--     where
-- #ifndef ghcjs_HOST_OS
--     getInvites :: [String] -> TransIO [(Node,File,Connected)]
--     getInvites users = do
--       modified <- liftIO $ newEmptyMVar

--       single $ liftIO $ addTrigger $ \ref (ma :: Maybe Shared) 
--                                    -> unsafeIOToSTM $ putMVar modified ()

--       waitEvents (takeMVar modified) <|> return ()
--       liftIO $ atomically $ select  (sharedNode,sharedDoc,sharedConnected) 
--                             $ foldr (.||.) emptySet  $ map (sharedUser .==.) users
--       where
--       emptySet :: STM [DBRef Shared]
--       emptySet= return []
-- #endif
    

    
  -- currentInvites user= GHC(
  --   let userRef = getDBRef user
  --   in Invites <$> (atomically $ (do u <- readDBRef userRef ; return $ fmap userInvites u) `onNothing` return []))

getPort = do
  x <- atomicModifyIORef counter $ \n -> (n + 1, n + 1)
  return . show $ 8000 + x
  where
  counter = unsafePerformIO $ newIORef 0

str s= s :: JSString



newtype Live= Live Bool deriving (Typeable,Read,Show)

instance Monoid Bool where 
  mempty = False
  mappend= (||)

editsOfCode :: Cell String -> Cloud (String, String)    
editsOfCode filenamew =  do 
      fileNameWidget  **> saveCompile  <|> changeContent 
      local $ do
         content <-  copyContent
         name <- get filenamew
         liftIO $ js_setAnnotations  "[]"
         return (name,content)
  where
    fileNameWidget = local $ render $ at "#entername" Insert 
                                    $ mk filenamew Nothing 
                                    ! placeholder "enter yourfilename.hs please" 
  
    saveCompile=  local $ render (at "#entername" Append $ inputSubmit ("save/compile" ::String) `fire` OnClick >> return())
  
    changeContent =  do
      UserName currentUser <- local getState

      local $ setRState $ Live False
      typingControl currentUser   <|>  setLive 
      
      where 
      typingControl currentUser = do

        delta <- local $ do
              ModifyEvent delta <- react onmodify $ return () 
              liftIO $ deltaToTuple delta
        interpretDelta delta <|> return ()
        -- if invited then atRemote 

        Live r <- local  getRState 
        if r then return () else empty 
        where
        interpretDelta (iniline,inicol,endline,endcol,action,content)=  do
          -- showDelta delta

          guard (iniline==endline-1) 
         
          l <- localIO getCurrentLine 
          local $ do
              setParseString $ BS.pack $ unpack l
              dropUntilToken  "-- "
              string "invite" 
          filename <- local $ get filenamew
          do
              local $ string "folder"
              user <- local parseString
              cancelIfSelf user
              inviteTo True (BS.unpack user) filename
            <|> do
              user <- local parseString
              cancelIfSelf user
              inviteTo False (BS.unpack user) filename
          where
          cancelIfSelf user= local $ do
              if user ==  (BS.pack  currentUser) then do liftIO $ insertText  " : You can not invite yourself" ; empty
                        else return ()

          inviteTo shareFolder user file= do
              let fil= if shareFolder then take (lastIndexOf '/' file) file else file

              found <- atServer $ GHC( exploreNet $ localIO $ atomically $ select userName $ userName .==. user) :: Cloud [String]
              if  null found then insertInEditor "user not found"
                
               else do
                isOnline <- atServer $ loggedc $ do 
#ifndef ghcjs_HOST_OS

                    nodeFile <- local getMyNode
                    sharedId <- local genGlobalId
                    let newReg=  Shared
                                            { sharedId        =    sharedId
                                            , sharedDoc       =    fil
                                            , sharedNode      =    nodeFile
                                            , sharedOwner     =    currentUser
                                            , sharedUser      =    user}
    
                    exploreNet $   do
                            localIO $ atomically $ newDBRef newReg
                            thisNode <- local  getMyNode
                            wnds <- localIO $ atomically $ select connectedWebNode $ connectedUser .==. (getDBRef user :: DBRef User)
                            localIO $ print wnds
                            return (not $ null wnds) <|> notifyWebNodes nodeFile fil wnds
#else 
                            error "this should not be executed" 
#endif
    
    
                      -- -- mclustered $ do
                      --     us <-localIO $
                      --       (readIORef usersOnline >>= \us -> return $ fmap Right $ lookup user us)
                      --           `onNothing` (Left <$> GHC(atomically $ readDBRef $ getDBRef user) )
                          
                              
                      --     case us of
                      --       Left Nothing -> return Nothing
                      --       Left (Just (_ :: User)) -> do 
                      --                 inviteAction user nodeFile shareFolder fil 
                      --                 return $ Just False
                      --       Right webnodes -> do
                      --         inv <- inviteAction user nodeFile shareFolder fil 
                      --         mapM_ (\wn -> runAt wn $ local $ putMailbox $ Invites inv) webnodes
                      --         return $ Just True

                insertInEditor $ case  isOnline of
                  True  -> "-- invitation sent"
                  False -> "-- invitation sent, but user not online"

            where
            notifyWebNodes node doc wnds=  callNodes'  wnds (<|>) empty $ local $ putMailbox ((node,doc) :: (Node,String)) >> empty
            insertInEditor txt= atBrowser $ localIO $ insertText txt

--             inviteAction user nodeFile shareFolder fil= local $ do
-- #ifndef ghcjs_HOST_OS
--               let userRef = getDBRef $ key User{userName=user}
--               liftIO $ atomically $ do
--                 user@User{userInvites=inv} <- readDBRef userRef `onNothing` error ("User not found: " ++ user)
--                 let inv'= (nodeFile,fil):inv
--                 writeDBRef userRef $ user{userInvites= inv'}
--                 return inv'
-- #else 
--               empty :: TransIO [(Node, String)]
-- #endif
                
            lastIndexOf c str= last $ elemIndices c str

    setLive = local $ do
        r <- render $ at "#entername" Append $ getCheckBoxes 
                    $ setCheckBox False ("autosave" :: String) `fire` OnChange 
                    <++ span ("autosave" ::String)
        setRState $ Live $ if not $ null r then True else False  
        empty     

 

compile :: String -> String -> String -> Cloud (Maybe String)
compile file source  port = atServer $ do
    localIO $ maybeKillProgram port

    localIO $ writeFile file source !> file
    
    r <- execShell  $  "chmod 777 "++ file ++ " && cd `dirname "++ file ++ "` && eval ./`basename "++ file ++ "` -p start/localhost/"++ port
    -- r <- execShell file ["-p","start/localhost/"++ port]        
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




type Errors= String
execShell :: String  -> Cloud (Maybe Errors) -- [(BS.ByteString,Int,Int,BS.ByteString)]
execShell expr  = onServer $ do
      return () !> ("EXECUTING", expr)
      r <- lazy $ liftIO $ createProcess1 $ (shell expr){std_in=CreatePipe,std_err=CreatePipe,std_out=CreatePipe}
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
           render . at "#frame"  Append  $ rawHtml $ pre ! style "color:red;word-wrap:break-word" $ errors
           liftIO $ scrollBottom "frame"
        r <- local $ Just <$> parseResp errors
        localIO $ print r
        return r


present :: Maybe Errors  -> String -> Cloud()
present result  port= local $ do
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
    "position:absolute;" ++ "width: 83%;" ++ "height:68%" ++ "}"

  script ! src "//cdnjs.cloudflare.com/ajax/libs/ace/1.3.2/ace.js" !
    type_ "text/javascript" !
    atr "charset" "utf-8" $
    noHtml

  span ! id "editor" ! atr "style" "position:absolute;width: 83%;height:68%" $ noHtml


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

foreign import javascript safe "editor.session.on('change',$1)"
   js_onmodify :: JSVal -> IO ()

foreign import javascript safe "alert(JSON.stringify($1))" showDelta :: JSVal -> IO()

-- foreign import javascript safe "editor.getSession().on('change',alert('no'))"
--     js_nomodify :: IO ()

newtype ModifyEvent = ModifyEvent JSVal deriving Typeable
onmodify ::   (ModifyEvent ->IO()) -> IO ()
onmodify continuation= do
  cb <- makeCallback1 ModifyEvent continuation
  js_onmodify cb

--foreign import javascript unsafe "$('#editor').resizable({ alsoResize: '.resize' });$('.resize').resizable({ alsoResize: '.resize' });"  resizejs :: IO()

foreign import javascript unsafe "editor.getSession().insert(editor.getCursorPosition(), $1)"
  insertText :: JSString -> IO()

foreign import javascript unsafe "editor.session.getLine(editor.getCursorPosition().row)" 
  getCurrentLine ::  IO JSString

foreign import javascript unsafe "[$1.start.row, $1.start.column,$1.end.row,$1.end.column,$1.action,$1.lines]"
  deltaToArray :: JSVal -> IO JSVal

deltaToTuple :: JSVal  -> IO (Int,Int,Int,Int,String,[String])
deltaToTuple delta=  deltaToArray delta >>= fromJSValUnchecked

#else
newtype ModifyEvent = ModifyEvent () deriving Typeable

resizejs= undefined
wopen :: JSString -> IO ()
wopen= undefined
setEditorContent :: JSString -> IO()
setEditorContent= undefined
js_setAnnotations = undefined

scrollBottom :: JSString -> IO()
scrollBottom= undefined

showDelta= undefined

copyContent :: TransIO String
copyContent = empty

onmodify ::   (ModifyEvent ->IO()) -> IO ()
onmodify= undefined
js_nomodify= undefined
fromJSVal= undefined
deltaToTuple = undefined :: () -> IO   (Int,Int,Int,Int,String,[String])

getCurrentLine :: IO JSString
getCurrentLine= undefined
insertText=undefined
#endif


exploreNet :: (Loggable a,Monoid a) => Cloud a  -> Cloud a
exploreNet action = do
   r <-  action
  --  n <- localIO randomIO
   nodes' <- local getNodes
   r' <- callNodes' nodes'  (<>) mempty $ do
    -- ns<- readIORef requestHistory
    --if n `elem` ns then return Nothing else do
      -- t  <- getCPUTime 
      -- HT.insert requestHistory n time
      nodes <- local getNodes
      callNodes' (nodes \\ nodes')  (<>) mempty $ exploreNet action 
   return $ r <> r'

-- deletekeys interval= do 
--    time <- waitEvents $ do
--               threadWait $ interval * 1000000
--               t  <- getCPUTime 
--               return t
--    liftIO $ do
--      vs <- fromList requestHistory
--      mapM (handle time) vs
--    where
--    handle time (k,t)= when (time - t > 2000000) $ HT.delete k
