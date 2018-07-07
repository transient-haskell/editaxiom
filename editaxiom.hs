#!/usr/bin/env execthirdlinedocker.sh

-- cd `dirname $1` && mkdir -p ./static && ghcjs  -i../transient/src -i../transient-universe/src  -i../axiom/src -DDEBUG `basename $1` -o static/out && ghc -DDEBUG -threaded  -i../develold/TCache -i../transient/src -i../transient-universe/src -i../axiom/src  `basename $1` && ./`basename $1 .hs`  ${2} ${3}

-- cd `dirname $1`&& runghc  -threaded  -i../transient/src -i../transient-universe/src -i../axiom/src  `basename $1` 


{-# LANGUAGE CPP, FlexibleContexts,OverloadedStrings, FlexibleInstances, UndecidableInstances, ScopedTypeVariables, DeriveDataTypeable #-}
import qualified Data.Map as M
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
import Data.List hiding(span, replicate)
import Data.Typeable

#ifdef ghcjs_HOST_OS
import GHCJS.Marshal (fromJSValUnchecked)
import GHCJS.Prim (JSVal)
import GHCJS.Marshal
import Control.Monad.STM(atomically)

#else
import Data.TCache hiding (onNothing)
import Data.TCache.DefaultPersistence
import Data.TCache.IndexQuery
import Data.TCache.IndexText
-- import System.Signal

#endif


import Data.Maybe
import qualified Data.Text.Lazy as T
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
#define GHC(exp) error "GHC: this code should not run in the Browser"
#else
#define GHC(exp) exp
#endif



-- main2= keep $ initNode $ onBrowser $ do
--   node <- local getMyNode
--   atRemote $ local $ return $ str "hi"
--   r <- atRemote $  do
--     local $ GHC(updateConnectionInfo node "pepe") >> return ()
    
--     nodes <- local getNodes
--     runAt (nodes !! 0) $ do
--         (runAt (nodes !! 1) $ localIO (alert "hello") >> empty >> return ()) <|> local (alert "world")
--         return $ str "world2"
--   localIO $ print r

-- main3= keep $ runCloud' $ do
--   runTestNodes [2000..2002]
--   local $ option  (str "f") "fire"
--   nodes <- local getNodes
--   (runAt (nodes !! 0) $ local  empty) <|> showNode
--   where
--   showNode= do
--     n <- local getMyNode
--     localIO $ print n


main = do
#ifndef ghcjs_HOST_OS
  GHC(databaseIndices)
#endif
  keep $  initNode doit  -- GHC(<|> signals) 


#ifndef ghcjs_HOST_OS

-- signals= do
--     sig <- react installHandler  (return())
--     liftIO $ print "Signal: " ++ sig
--     cleanConnectionData


databaseIndices=  do
  index connectedId
  index userName
  index sharedDoc
  index sharedNode
  index sharedOwner
  index sharedInvited
  index connectedUser
  indexList connectedEditingShared (map (T.pack . keyObjDBRef ))
  clearSyncCacheProc 5 defaultCheck 1000000
  cleanConnectionData

#endif


newtype Port= Port String deriving (Read,Show,Typeable)

doit= onBrowser $ do
  -- replicateData :: Cloud Shared       -- any register change is replicated in all nodes for that register
  -- replicateData :: Cloud Connected
  authenticate
  verifyDir

  let filenamew= boxCell "filename"
  port <- localIO getPort
  ide filenamew port <|> consoleControlFrames <|> folderNav filenamew :: Cloud () -- <* resizable
  where
  ide filenamew port= loggedc $ do
    local $ (render .  rawHtml $ do
                    div ! id "entername" $ noHtml
                    aceEdit) <** zenEditor
    editSave filenamew <|> typingControl filenamew
    where
    editSave filenamew= do
      (file,source) <- editsOfCode  filenamew
      local $ when (null file) $ alert "enter source file name, for examaple: 'yourfile.hs'" >> empty
      result <- compile file source port
      present result port

  folderNav filenamew=  do
    UserName u <- local getState
    (file,source) <- folder  u u
    localIO $ setEditorContent $ pack source
    local $ filenamew .= file

newtype Name= Name String deriving (Read,Show,Typeable,Eq,Ord)
newtype Pass= Pass String deriving (Read,Show,Typeable,Eq,Ord)
newtype Email= Email String deriving (Read,Show,Typeable,Eq,Ord)

#ifndef ghcjs_HOST_OS

data User= User{ userName:: Name, email :: Email, userPassword :: Pass, userConnected :: Maybe (DBRef Connected)}
            deriving (Read,Show, Typeable)

newtype File= File String  deriving (Read,Show,Typeable,Eq,Ord)
newtype ServerNode= ServerNode String deriving (Read,Show,Typeable)
newtype WebNode= WebNode String deriving (Read,Show,Typeable)

newtype Owner= Owner String  deriving (Read,Show,Typeable,Eq,Ord)
newtype Invited= Invited String deriving (Read,Show,Typeable,Eq,Ord)

data Shared= Shared{ sharedId       :: Int
                   , sharedDoc      :: File
                   , sharedNode     :: Node
                   , sharedOwner    :: Owner
                   , sharedInvited  :: Invited}
                   deriving (Read,Show,Typeable)

-- XXX necesario identificar los ficheros que esta editando un usuario

instance Indexable Shared where key Shared{sharedId=id}= keyShared id
keyShared id= "Shared#"++show id

data Connected= Connected{ connectedId :: Int,connectedUser :: DBRef User, connectedHost :: Node
                         , connectedWebNode :: Node, connectedEditingShared :: [DBRef Shared]} deriving (Read,Show, Typeable)

instance Indexable Connected where key = (++) "Connected#"  . show . connectedId

instance Indexable User where key u= let Name uname= userName u in keyUserName uname
keyUserName uname=  "user#"++uname
instance (Show a, Read a) => Serializable a where
  serialize  = BS.pack . show
  deserialize= read . BS.unpack

cleanConnectionData= atomically $ do
    conn <- indexOf connectedId
    us <- indexOf userName 
    mapM (mapM delDBRef . snd) conn 
    mapM (\(_,[ru]) -> readDBRef ru >>= \(Just u) -> writeDBRef ru u{userConnected=Nothing})  us

#endif

newtype UserName= UserName String deriving(Read,Show, Typeable)

verifyDir =  do
  UserName u <- local getState <|> error "verifyDir: no user set"
  atServer $ do
      exist <- localIO $ doesDirectoryExist u
      when (not exist) $ do
          localIO $ createDirectory  u
          localIO $ callCommand $ "cp -r ./examples/* "++ u
          return()

authenticate :: Cloud ()
authenticate = do
  local $ render $ rawHtml $ div ! id "auth" ! style  "position:absolute;width:50%;height: 10%;margin-left: 50%" $ noHtml
  auth ""
  where
  auth :: String -> Cloud  ()
  auth n = do
    (n,p,p') <- local $ render $ at "#auth" Insert $
                  (,,) <$> inputString (Just n) ! placeholder "username"  ! size "8"
                       <*> inputPassword ! size "8"
                       <*> inputPassword ! size "8" `fire` OnChange
                       <** inputSubmit ("ok" ::String) `fire` OnClick
    webnode <- local getMyNode
    mr <- atRemote $ local $ do
#ifndef ghcjs_HOST_OS
            let ruser= getDBRef n
            mu <- liftIO $ atomically $ readDBRef ruser
            case mu of
                Nothing ->
                  if p== p'
                    then do
                      (on,wnode) <-  updateConnectionInfo webnode n 
                      ex <- getSData <|> error "No disconnection bacpoint"
                      onExceptionPoint ex $ \(e :: SomeException)  -> removeConnectionInfo n on wnode
                
                      liftIO $ atomically $ newDBRef (User (Name n) (Email n) (Pass p)  $ Just on)
                      return $ Just n
                    else return Nothing
                Just (user@User{userPassword=pstored}) -> do
                  --port <- liftIO getPort
                  if Pass p == pstored then do
                       (on,wnode) <- updateConnectionInfo webnode n 
                       ex <- getSData <|> error "No disconnection bacpoint"
                       onExceptionPoint ex $ \(e :: SomeException)  -> removeConnectionInfo n on wnode
                       liftIO $ atomically $ do
                          writeDBRef ruser $ user{userConnected= Just on}  
                          return $ Just n
                  else return Nothing
#else
            error "should not run in browser"   :: TransIO (Maybe String)

#endif

    case mr of
      Nothing ->   auth n
      Just r -> do
        setState $ UserName n

        local $ render $ at  "#auth" Insert $ do
            rawHtml $ clear >> span n
            (span  (str " change user") ! style "cursor:pointer")  `pass` OnClick
            return ()
        auth n
       <|> return ()

    where
    size= atr "size"

#ifndef ghcjs_HOST_OS

updateConnectionInfo webNode n = do
      
      id <- genGlobalId
      host <- getMyNode
      webNode' <- setConnectionIn webNode
      addNodes[webNode']
      con <-liftIO $ atomically $ newDBRef (Connected id (getDBRef n) host webNode' [])
      return (con,webNode')

removeConnectionInfo :: String -> DBRef Connected -> Node -> TransIO ()
removeConnectionInfo n con node= do
      return () !> "REMOVECONNECITION INFOOOOOOOOOOOOOOOOOO"
      liftIO $ atomically $  do
           delDBRef con
           withSTMResources [User{userName=Name n}]  $ \[muser] -> 
                case muser of
                   Just user -> resources{toAdd=[user{userConnected=Nothing}]}
                   Nothing -> error $ "removeConnectionInfo: user not found " ++ n
      delNodes [node]
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

folder :: String -> String -> Cloud (String,String)
folder user fol= onBrowser $ do
  return ()!> "VA A PINTAR INVITES Y FOLDERS DE NUEVO"
  local $ render $ rawHtml $ div ! clas "resize" ! style "overflow:auto;position:absolute;left:85%;height:68%"
                 $ div ! id "dirs" $ do
                     div ! id "invites" $ b $ str "Invites"
                     div ! id "dir" $ b $ str "Folders"

  (node,files) <- atServer . local $ (,) <$> getMyNode <*> liftIO (getDirectoryContents fol >>= return . sort)
  -- setRenderTag "dir"

  invites <|>  folders node files
  where
  folders node files= do 
    local $ setRenderTag "dir" 
    folder' node "." $ map ((fol++"/") ++) files

  invites= do
    invitesNew <|> currentInvites user
    where
    invitesNew= do
        (n,o,f) <- local getMailbox :: Cloud (Node,String,String)
        return () !> ("NEW ELEM SHARED", n,o,f)
        elemShared n o f

    currentInvites user=  do
       return () !> "INVITEEEEEEEEEEES"
      
       is <- atRemote $ localIO  $ do
                      return ()  !> "INVITEEEEEEEEEEES2222"

                      ns <- GHC(atomically $ select  (sharedNode,sharedOwner,sharedDoc) $ sharedInvited .==. Invited user)
                      GHC(return () !> ("NSSSSSSSSSSSSSSSSSSS",ns))
                      GHC(return  $ map (\(n,Owner o,File doc) ->  (n, o, doc)) ns) :: IO [(Node,String,String)]
       foldr  (<|>) empty [elemShared n o f | (n, o, f) <- is]

  folder' :: Node  -> String -> [String] -> Cloud (String,String)
  folder' n  dir files= do
      foldr  (<|>) empty [elemFolder  n dir f | f <- files]

  elemShared node owner file= do
    return () !> "ELEMSHARED"
    return () !> (node, owner,file)
    local $ setRenderTag "invites"

    local $ render $ pre file ! style "cursor:pointer" `pass` OnClick >> return ()
    local $ alert "elemShared"

    (f,s) <- atServer $ runAt node $ loggedc $

      do
        isFile <- localIO $ doesFileExist file
        case isFile of
          True  ->  do
              wn <- localIO $ GHC(atomically $ do)
                  GHC(rs <- select  sharedInvited $  sharedDoc .==. File file .&&. sharedOwner .==. Owner owner)
                  GHC(let invs= map (\(Invited r) -> getDBRef r) rs :: [DBRef User])
                  GHC(return () !> ("INVSSSS", invs))

                  -- get the key of the register with the sharing info
                  GHC([shareid] <- select sharedId $ sharedDoc .==. File file .&&. sharedOwner .==. Owner owner)
                  

                  -- search if the owner or some user invited to that document is editing it in a web node
                  GHC(select (connectedHost,connectedWebNode) $ connectedEditingShared `containsElem` keyShared shareid  .&&.)
                            GHC((connectedUser .==. (getDBRef owner :: DBRef User)   .||.)
                            GHC((foldr (.||.)  (return [] :: STM [DBRef Connected]) $ map (connectedUser .==.)  invs)))

              return () !> ("WN",wn)
              if null wn
                then localIO $ do
                    source <- readFile file  `catch` \(e:: SomeException) -> return "File is not a text"
                    return (file, source)
                else do
                    let (host,webnode)= head wn
                    source <- runAt host $ runAt webnode $ local  copyContent

                    return (file,source)

          False -> do
              files <- localIO $ getDirectoryContents file >>= return . sort
              folder' node  file files

    UserName currentUser <- local $ getState <|> error "user not set"
    Cloud . single $ runCloud' $ sendGetDeltas True f currentUser

    return (f,s)

  sendGetDeltas False _ _ = return ()
  sendGetDeltas True f currentUser = do
      let hash= (f <> "@" <> currentUser)
      getDeltas hash <|> sendDeltas  hash <|> return ()

    where
    getDeltas hash= do
      Delta delta <- suscribe hash
      localIO $ applyDeltas delta
      empty


    sendDeltas  hash= do 
      del <- local $ getMailbox' ("out" :: JSString)
      publish hash (del :: Delta)
      empty











  elemFolder  node dir file= do
    local $ render $ pre file ! style "cursor:pointer" `pass` OnClick >> return ()

    let filedir= if file== ".."
                    then  take (let is= elemIndices '/' dir
                                in if null is then length dir else last is) dir
                    else if null dir then file else dir ++ "/"++ file

    UserName username <- local getSData <|> error "no user set"

    r <- atServer $ runAt node $ localIO $ do
#ifndef ghcjs_HOST_OS
              isFile <- doesFileExist  filedir !>  ("FILEDIR",filedir,file)
              case isFile of
                  True  -> Right <$> do  
                        source <- do
                              s <- readFile filedir
                              atomically $ do
                                  rs <- recordsWith $ 
                                                 connectedUser    .==. (getDBRef $ keyUserName username :: DBRef User) 
                                          .&&.   connectedHost    .==. node
                                        --  .&&.   connectedWebnode .==. wnd
                                  case rs of 
                                    [] -> return()
                                    [r] -> do
                                      [id] <- select sharedId $  sharedDoc .==. File file .&&. sharedNode .==. node
                                      withSTMResources [r] $ \[Just r] -> 
                                        resources{toAdd=[r{connectedEditingShared= 
                                            getDBRef (keyShared id) : connectedEditingShared r}]}
                                    _ -> error "more than one session in the same machine. need to specify the web node in the query"
                              return s
                          `catch` \(e:: SomeException) -> return "File is not a text"
                        
                        return (filedir, source)

                  False -> Left  <$> liftIO (getDirectoryContents filedir >>= return . sort)
#else
                    empty 
#endif


    case r of
        Right fileinfo -> do
          Cloud . single $ runCloud' $ sendGetDeltas False undefined undefined
          local $ return fileinfo

        Left files     -> folder' node  filedir files


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
      local $ setRState $ Live False
      setLive
      where
      setLive = saveIfLive <|> liveControl
        where
        saveIfLive= do
          Live r <- local  getRState
          if r then return() else empty
  
        liveControl= local $ do
          r <- render $ at "#entername" Append $ getCheckBoxes
                      $ setCheckBox False ("autosave" :: String) `fire` OnChange
                      <++ span ("autosave" ::String)
          setRState $ Live $ if not $ null r then True else False
          empty
  
newtype Delta= Delta JSString deriving (Read,Show, Typeable)
typingControl  filenamew= do
        UserName currentUser <- local getState
        (delta :: JSString, hdelta) <- local $ do
              ModifyEvent jsdelta <- react onmodify $ return ()
              hdelta <- liftIO $ deltaToTuple jsdelta
              delta <- liftIO $ stringify jsdelta

              return (delta,hdelta)
        local $ putMailbox' ("out" ::JSString) $ Delta delta
        --sendDeltas delta hash <|> 
        interpretDelta hdelta  filenamew currentUser
        empty
        where


        interpretDelta (iniline,inicol,endline,endcol,action,content) filenamew currentUser=  do
          -- showDelta delta
          filename <- local $ get filenamew

          guard (iniline==endline-1)

          l <- localIO getCurrentLine
          local $ do
              setParseString $ BS.pack $ unpack l
              dropUntilToken  "-- "
              string "invite"
          do
              local $ string "folder"
              user <- local parseString
              cancelIfSelf user
              filename <- local $ get filenamew
              inviteTo True (BS.unpack user) filename
            <|> do
              user <- local parseString
              cancelIfSelf user
              filename <- local $ get filenamew
              inviteTo False (BS.unpack user) filename
          where
          cancelIfSelf user= local $ do
              if user ==  (BS.pack  currentUser) then do liftIO $ insertText  " : You can not invite yourself" ; empty
                        else return ()
          inviteTo:: Bool -> String -> String -> Cloud ()
          inviteTo shareFolder user file= do
              let fil= if shareFolder then take (lastIndexOf '/' file) file else file
              -- let found= True
              -- found <- atServer $ loggedc $ do     
              --             ns <- GHC( exploreNet $ localIO $ atomically $ select userName $ userName .==. Name user)
              --             return $ GHC(map (\(Name n) -> n) ns) :: Cloud [String]
              -- if  null found then insertInEditor $ str "-- " <> pack user <> str ": user not found"

              --  else do
              isOnline <- atRemote $  do
                    return () !> "ATREMOTEEEEEEEEEEE"
                    nodeFile <- local getMyNode
                    sharedId <- local genGlobalId
                    let newReg=  GHC((Shared
                                            { sharedId        =    sharedId
                                            , sharedDoc       =    File fil
                                            , sharedNode      =    nodeFile
                                            , sharedOwner     =    Owner currentUser
                                            , sharedInvited   =    Invited user}))

                    (us,wnds) <-  exploreNet $ do  
                            (us,wnds) <- local $ do
                                 liftIO $ atomically $ do
                                   GHC(newDBRef newReg)
                                   users <- GHC(select userName $ userName .==. Name user)
                                   webconnections <- GHC(select connectedWebNode $ connectedUser .==. (getDBRef user :: DBRef User))
                                   return(users :: [Name], webconnections :: [Node])
                            return () !> ("BROWSER NODEBROWSER NODESSSSSSSSSSSSSSSSSSSSSSSSS",us,wnds)
                            notifyWebNodes nodeFile fil wnds currentUser
                            return () !> "AFTER NOTIFY"
                            return (not $ null us :: Bool , not $ null wnds :: Bool)

                    -- if invitation sucessful add this to the list of invitation of this user
                    when us $ localIO $ atomically $ do
                      GHC(Just ureg <- readDBRef $ getDBRef $ keyUserName currentUser)
                      GHC(let coninfo= fromJust $ userConnected ureg)

                      GHC(Just conreg <- readDBRef  coninfo)
                      GHC(return () !> (conreg,newReg,coninfo,getDBRef (key newReg):: DBRef Shared))
                      -- add document to the list of shared documents  owned by this user that are being edited by him
                      GHC(writeDBRef coninfo $ conreg{connectedEditingShared=getDBRef (key newReg):connectedEditingShared conreg})
                      return()

                    return (us,wnds)

              insertInEditor $ case  isOnline of
                  (True,True)  -> "-- invitation sent"
                  (True,False) -> "-- invitation sent, but user not online"
                  (False,False)-> "-- "<> pack user <> str ": user not found"

            where
            notifyWebNodes node doc wnds owner= do
              return() !> ("NOTIFYWEBNODES",wnds,doc)
              callNodes'  wnds (<|>) empty $ local $ do
                return () !> "notify WEB NODES PUTMAILBOX" 
                putMailbox ((node,owner,doc) :: (Node,String,String))
                empty
             <|> return ()

            insertInEditor txt= atBrowser $ localIO $ insertText txt



            lastIndexOf c str= last $ elemIndices c str




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

foreign import javascript safe "JSON.stringify($1)"  stringify :: JSVal -> IO JSString

foreign import javascript unsafe "document.getElementById($1).childNodes.length" numChildNodes :: JSString -> IO Int

foreign import javascript unsafe "editor.getSession().applyDeltas([JSON.parse($1)])" applyDeltas :: JSString -> IO ()


#else
stringify= undefined
fromJSValUnchecked= undefined
numChildNodes= undefined

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
insertText= undefined
applyDeltas :: JSString -> IO ()
applyDeltas= undefined







replicateData :: (Loggable a,IResource a) => Cloud a
replicateData  = do
  (key, ma) <- local $ react callCont  (return ())  
  exploreNet $  localIO $ atomically $ do
    let ref= getDBRef key
    case ma of 
     Nothing -> delDBRef ref
     Just ma -> writeDBRef ref ma
  return $ fromJust ma
  where
  callCont cont= addTrigger $ \ref ma ->  safeIOToSTM $ cont (keyObjDBRef ref,ma)

#endif



exploreNet :: (Loggable a,Monoid a) => Cloud a  -> Cloud a
exploreNet action = loggedc $ do
   return () !> "EXPLORENET"
   r <-  action
  --  n <- localIO randomIO
   nodes' <- local getEqualNodes
   -- call the rest of the nodes connected
   r' <- callNodes' (tail nodes')  (<>) mempty $ do
    -- ns<- readIORef requestHistory
    --if n `elem` ns then return Nothing else do
      -- t  <- getCPUTime 
      -- HT.insert requestHistory n time
      nodes <- local getEqualNodes
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



suscribed= unsafePerformIO $ newIORef (M.empty :: M.Map String [Either Node Node])

suscribe :: Loggable a => String -> Cloud a
suscribe hash= do
  node <- local getMyNode
  local (getMailbox' hash) <|> (atServer $ susc (Right node))
    where
    susc node = do 
          localIO $ atomicModifyIORef suscribed $ \ss -> (insert hash node ss,())
          nodes <- local getEqualNodes
          callNodes' (tail nodes)  (<|>) empty  $ susc $ Left (head nodes)
          empty
      
          
    insert h node susc=
       let ns = fromMaybe [] $ M.lookup h susc
       in M.insert h (node:ns) susc

      
unsuscribe hash= do
  node <- local getMyNode
  ( atServer $ unsusc (Right node)) <|> return ()
    where 
    unsusc :: Either Node Node -> Cloud ()
    unsusc node = do 
          localIO $ atomicModifyIORef suscribed $ \ss -> (delete hash node ss,())
          nodes <- local getEqualNodes
          callNodes' (tail nodes)  (<|>) empty $ unsusc $ Left (head nodes)
          empty
          
          
    delete h node susc=
       let ns = fromMaybe [] $ M.lookup h susc
       in M.insert h (ns \\[node]) susc
  
publish hash dat= atServer $ do
    nodes <- localIO $ readIORef suscribed >>= return . fromMaybe [] . M.lookup hash
    foldr (<|>) empty  $ map pub nodes 
    return()

    where 
    pub  (Left node)= runAt node $ publish hash dat >> empty >> return ()
    pub  (Right node)= runAt node $ local $ putMailbox' hash dat >> empty