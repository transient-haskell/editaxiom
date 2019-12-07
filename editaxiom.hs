#!/usr/bin/env execthirdlinedocker.sh

-- cd `dirname $1` && mkdir -p ./static && ghcjs --make  -DDEBUG  -i../transient/src -i../transient-universe/src  -i../axiom/src  `basename $1` -o static/out && ghc -DDEBUG -threaded  -i../develold/TCache -i../transient/src -i../transient-universe/src -i../axiom/src   `basename $1` && ./`basename $1 .hs`  ${2} ${3} 

-- cd `dirname $1` && mkdir -p ./static && ghcjs -DDEBUG `basename $1`  -o static/out && ghc -DDEBUG -threaded  `basename $1` && ./`basename $1 .hs`  ${2} ${3}


{-# LANGUAGE CPP, OverloadedStrings, FlexibleInstances, UndecidableInstances, 
             ScopedTypeVariables, DeriveDataTypeable, DeriveAnyClass, DeriveFunctor #-}
import qualified Data.Map as M
import Control.Concurrent(threadDelay)
import Control.Exception(SomeException, catch)
import Control.Monad
import Control.Monad.IO.Class 
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
import Transient.Move.PubSub
import Transient.Indeterminism(groupByTime)
import Transient.Parse
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List hiding(span, replicate)
import Data.Typeable
import Transient.Move.Services

#ifdef ghcjs_HOST_OS

import GHCJS.Marshal (fromJSValUnchecked)
import GHCJS.Prim (JSVal)
import GHCJS.Marshal
import Control.Monad.STM(atomically)
import qualified Data.JSString as JS
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

add compiler service as a compile.hs which uses the docker image
  compile :: Source -> Tar(Binary,JScript)
 also with REST
 download compiled file
 multiple source files imports
 compile on file change done
 import of a library not present in the docker image?

-}




newtype Name= Name String deriving (Read,Show,Typeable,Eq,Ord)
newtype Pass= Pass String deriving (Read,Show,Typeable,Eq,Ord)
newtype Email= Email String deriving (Read,Show,Typeable,Eq,Ord)
newtype Owner= Owner String  deriving (Read,Show,Typeable,Eq,Ord)


data User= User{ userName:: Name, email :: Email, userPassword :: Pass
               , userConnected :: Maybe (DBRef Connected)}
            deriving (Read,Show, Typeable)

newtype File= File String  deriving (Read,Show,Typeable,Eq,Ord)
newtype ServerNode= ServerNode String deriving (Read,Show,Typeable)
newtype WebNode= WebNode String deriving (Read,Show,Typeable)


newtype Invited= Invited String deriving (Read,Show,Typeable,Eq,Ord)

data Shared= Shared{ sharedId :: Int
                   , sharedFile     :: DBRef FileReg
                   , sharedNode     :: Node
                   , sharedInvited  :: Invited} 
                   deriving (Read,Show,Typeable)

data FileReg= FileReg { fileId       :: Int
                      , fileName     :: File
                      , fileNode     :: Node
                      , fileOwner    :: Owner}
                deriving (Read,Show,Typeable)

data Connected= Connected{ connectedId :: Int,connectedUser :: DBRef User, connectedHost :: Node
                         , connectedWebNode :: Node,
                         -- no vale DBRef, porque puede señalar a otro nodo
                        
                             
                         connectedEditing :: [DBRef FileReg]} deriving (Read,Show, Typeable)




main = do

#ifndef ghcjs_HOST_OS
  syncWrite  $ Asynchronous  1 defaultCheck 1000000
  databaseIndices  
#endif

  keep $ do
     initNode $ inputNodes <|>  doit  -- GHC(<|> signals) 


#ifndef ghcjs_HOST_OS

-- signals= do
--     sig <- react installHandler  (return())
--     liftIO $ print "Signal: " ++ sig
--     cleanConnectionData


databaseIndices=  do
  index connectedId
  index userName
  index sharedFile
  index sharedNode
  index sharedInvited
  index connectedUser
  index connectedHost
  index connectedWebNode
  index fileName
  index fileOwner
  indexList connectedEditing (map (T.pack . keyObjDBRef ))
  clearSyncCacheProc 5 defaultCheck 1000000
  cleanConnectionData
  return()

#endif


newtype Port= Port String deriving (Read,Show,Typeable)

doit= onBrowser $ do
  -- replicateData :: Cloud Shared       -- any register change is replicated in all nodes for that register
  -- replicateData :: Cloud Connected
  port <- atServer  $ local $ do

            liftIO $ do
                port <- getPort
                print ("New user at PORT",port)
                return port
  local $ setRState (("",Nothing) :: (String,Maybe Owner))
  authenticate 

  verifyDir

  let filenamew= boxCell "filename"

  editor filenamew port <|> folderNav filenamew  <|> consoleControlFrames    :: Cloud () -- <* resizable
  where
  editor filenamew port= loggedc $ do
    local $ (render .  rawHtml $ do
                    div ! id "entername" $ noHtml
                    aceEdit) <** zenEditor
    editSave filenamew   <|> typingControl filenamew
    where
    editSave filenamew= do
      (file,source) <- editsOfCode  filenamew
      local $ when (null file) $ alert "enter source file name, for example: 'yourfile.hs'" >> empty

      result <- runTheShow file source port 
      
      present result port

  folderNav filenamew=  do
    return () !> "FOLDERNAV"
    UserName u <- local getState

    (node,file,source,mowner) <- folderAndInvites  u u

    local $ filenamew .= file
    
    suscribeModifications file mowner
    notifyEditing node file mowner
    local $ setRState (file, mowner)

    localIO $ setEditorContent $ pack source
    return()
    

#ifndef ghcjs_HOST_OS

instance Indexable FileReg where key f=  keyFile (fileId f) (fileNode f)
keyFile id node= "file#" ++ show id ++"@" ++ nodeHost node ++ ":" ++ show (nodePort node)

instance Indexable Shared where key sh= keyShared (sharedId sh) (sharedNode sh) 
keyShared id node= "Shared#"++show id++"@"++ nodeHost node ++ ":" ++ show (nodePort node)


instance Indexable Connected where key c= keyConnected $ connectedId c
keyConnected conId= "Connected#"  ++ show conId

instance Indexable User where key User{userName= Name uname}= keyUserName uname
keyUserName uname=   "user#"++uname

-- read-show serialization for registers
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
  auth u = do
    (user,p,p') <- local $ render $ at "#auth" Insert $
                  (,,) <$> inputString (Just u) ! placeholder "username"  ! size "8"
                       <*> (inputPassword ! placeholder "pass" ! size "8") --`fire` OnClick)
                       <*> (inputPassword ! placeholder "pass again to register" ! size "8"  <|> return "")
                       <**  inputSubmit ("ok" ::String)  `fire` OnClick 

    webnode <- local getMyNode

    mr <- atRemote $ local $ do

            let ruser= getDBRef $ keyUserName user
            mu <- liftIO $ atomically $ readDBRef ruser
            case mu of
                Nothing ->
                  if p== p'
                    then do
                      (on,wnode) <-  updateConnectionInfo webnode user 


                      setExceptionOnConnection user wnode on
                
                      liftIO $ atomically $ newDBRef (User (Name user) (Email user) (Pass p)  $ Just on)
                      return $ Just  user
                    else return Nothing
                Just (userReg@User{userPassword=pstored}) -> do
                  if Pass p == pstored then do
                       (on,wnode) <- updateConnectionInfo webnode user

                       setExceptionOnConnection user wnode on
                       liftIO $ atomically $ do
                          return () !> ("UPDATE USER", on)
                          writeDBRef ruser $ userReg{userConnected= Just on} 

                          return $ Just user
                  else return Nothing

    case mr of
      Nothing ->   auth user
      Just r -> do

        setState $ UserName user

        local $ render $ at "#auth" Insert $ do
            rawHtml $ clear >> span user
            (span  (str " change user") ! style "cursor:pointer")  `pass` OnClick
            return ()
        auth user
       <|> return ()

    where
    size= atr "size"
    setExceptionOnConnection username wnode on = do
       ex <- getSData <|> error "No disconnection backpoint"
       onExceptionPoint ex $ \(e :: SomeException)  -> do
            names ::  [String]  <- liftIO $ atomically $ do
                    (editingDocs :: [[DBRef FileReg]]) <- select connectedEditing $ connectedWebNode .==. wnode 
                    docRegs <- mapM (\r -> readDBRef r `onNothing` error "doc register not found") $concat editingDocs 
                    return $ map (\dreg -> let File name= fileName dreg in name )  docRegs
            mapM (\nam -> removeSuscNode nam username wnode) names

            removeConnectionInfo username  on wnode
            

#ifndef ghcjs_HOST_OS

updateConnectionInfo webNode n = do
      
      id <- genGlobalId
      host <- getMyNode
      webNode' <- setConnectionIn webNode
      addNodes[webNode']
      con <-liftIO $ atomically $ newDBRef (Connected id (getDBRef $ keyUserName n) host webNode' [])
      return (con,webNode')

removeConnectionInfo :: String -> DBRef Connected -> Node -> TransIO ()
removeConnectionInfo n con node= do
      return () !> "REMOVECONNECITION INFOOOOOOOOOOOOOOOOOO"
      liftIO $ atomically $  do
           delDBRef con
           return () !> ("CLEAN",n)
           withSTMResources [User{userName=Name n}]  $ \[muser] -> 
                case muser of
                   Just user -> resources{toAdd=[user{userConnected=Nothing}]}
                   Nothing -> error $ "removeConnectionInfo: user not found " ++ n
      delNodes [node]
#else 
updateConnectionInfo= shouldnot
removeConnectionInfo= shouldnot
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

folderAndInvites :: String -> String -> Cloud (Node, String,String, Maybe Owner)
folderAndInvites user fol= onBrowser $ do
  return ()!> "VA A PINTAR INVITES Y FOLDERS DE NUEVO"
  local $ render $ rawHtml $ div ! clas "resize" 
                                 ! style "overflow:auto;position:absolute;left:85%;height:68%"
                 $ div ! id "dirs" $ do
                   div ! id "invites" $ b $ str "Invites"
                   div ! id "dir" $ b $ str "Folders"
  
  (node,files) <- atServer . local $ (,) <$> getMyNode <*> liftIO (getDirectoryContents fol >>= return . sort)
  -- setRenderTag "dir"

  invites <|>  folders node files
  where
  folders node files= do 
    -- local $ setRenderTag "dir" 
    return() !> "FOLDERS"
    folder'  node "." $ map ((fol++"/") ++) files

  invites= do
    invitesNew <|> currentInvites user 
    where
    invitesNew= do
        (n,o,f) <- local getMailbox :: Cloud (Node,String,String)
        return () !> ("NEW ELEM SHARED", n,o,f)
        elemShared n o f

    currentInvites user=  do
       local $ return "INVITEEEEES"  !> "INVITEEEEEEEEEEES" :: Cloud String
      
       is <- atRemote $ localIO  $ do


                      ns <- atomically $ do
                              refs <-  select sharedFile $   sharedInvited .==. Invited user
                              mapM (\r -> readDBRef r `onNothing` error "user register error") refs


                      return  $ map (\(n,Owner o,File doc) ->  (n, o, doc)) $ map (\n -> (fileNode n, fileOwner n, fileName n)) ns :: IO [(Node,String,String)]


       foldr  (<|>) empty [elemShared n o f | (n, o, f) <- is]

folder' ::   Node  -> String -> [String] -> Cloud (Node,String,String, Maybe Owner)
folder' n  dir files= do
      foldr  (<|>) empty [elemFolder  n dir f | f <- files]

elemShared :: Node -> String -> String -> Cloud (Node, String,String, Maybe Owner)
elemShared node owner file= do

    return () !> (node, owner,file)
    UserName username <- local getSData <|> error "no user set"

    local $ render $ at "#invites" Append $ pre file ! style "cursor:pointer" `pass` OnClick >> return ()


    (node,f,s,shared) <- atServer $  loggedc $

      do
        isFile <- localIO $ doesFileExist file
        case isFile of
          True  ->  getFile node file owner



          False -> do
              files <- localIO $ getDirectoryContents file >>= return . sort
              folder' node  file files

    return (node,f,s,shared)



getFile :: Node -> String -> String -> Cloud (Node,String,String,Maybe Owner)
getFile node file owner = runAt node $  do

        return () !> ("GETFILE",node, file , owner)


        (idfile :: Maybe Int,invs  :: [DBRef User]) <- 
           localIO $ atomically $ do
                 -- this file has been shared?
                 -- 
                 midfile <- select  fileId $  fileName .==. File file .&&. fileOwner .==. Owner owner

                 case midfile of
                   [] ->  return (Nothing,[])
                   [idfile] -> do 
                         let refFile= getDBRef $ keyFile idfile node :: DBRef FileReg
                         return () !> ("refFile",refFile)
                         
                         rs <- select sharedInvited $ sharedFile .==. refFile

                         let invs= map (\(Invited r) -> getDBRef $ keyUserName r) rs :: [DBRef User]
                         return () !> ("INVSSSS", invs)
                         return (Just idfile,invs)

        wn <- if isNothing idfile then return [] else do
              
              exploreNet $ localIO $ do
                  return () !> ("EXPLORENET", invs, idfile, node)
                  atomically $ do
                    -- search if the owner or some user invited to that 
                    -- document is editing it right now in a web node
    
                    return () !> (getDBRef $ keyUserName owner :: DBRef User)
                    
                    select (connectedHost,connectedWebNode) $
                            connectedEditing `containsElem` (keyFile (fromJust idfile) node) .&&.
                                (connectedUser .==. (getDBRef $ keyUserName owner :: DBRef User) .||. 
                                  (foldr (.||.)  (return [] :: STM [DBRef Connected]) $ map (connectedUser .==.)  invs)
                                )
                                    



        return () !> ("WN",wn)
        if null wn
            then local $ do
                source <- liftIO $ readFile file  `catch` \(e:: SomeException) -> return "File is not a text"
                node <- getMyNode
                return (node,file, source, Just $ Owner owner) 

            else do
                let (host ,webnode)= head wn
                source <- runAt host $ runAt webnode $ local  copyContent
                return (node,file,source,Just $ Owner owner)

-- #endif

elemFolder :: Node -> String -> String -> Cloud (Node,String,String, Maybe Owner)
elemFolder  node dir file= do
    return () !> "ELEMFOLDER"

    local $ render $ at "#dir" Append $ pre file ! style "cursor:pointer" `pass` OnClick >> return ()
    
    let filedir= if file== ".."
                    then  take (let is= elemIndices '/' dir
                                in if null is then length dir else last is) dir
                    else if null dir then file else dir ++ "/"++ file

    UserName username <- local getSData <|> error "no user set"
    return () !> "BEFORE ATSERVER"
    r <- atServer $  do 

              isFile <- localIO $ doesFileExist  filedir !>  ("FILEDIR",filedir,file)
              case isFile of
                  True  -> Right <$> do  

                              return () !> (filedir, file, username)
                              getFile node filedir username


                             


                  False -> Left  <$> localIO (getDirectoryContents filedir >>= return . sort)



    case r of
        Right fileinfo -> return fileinfo

        Left files -> folder' node  filedir files

-- if the file is shared by this or other user, notify that the file is being edited by this user

removeSuscNode file user node = liftIO $ do
    return () !> "REMOVESUSCNODE"
    let hash= (file <> "@" <> user)
    atomicModifyIORef suscribed $ \ss -> (delete hash [Right node] ss,())
    where
    delete h nodes susc=
       let ns = fromMaybe [] $ M.lookup h susc
       in M.insert h (ns \\ nodes) susc

sendGetDeltas _ _ Nothing = return ()
sendGetDeltas False file (Just (Owner user)) = do
    let hash= (file <> "@" <> user)
    local $ cleanMailbox' ("out" :: JSString) $ Delta undefined 
    unsuscribe hash $ Delta undefined
    
sendGetDeltas True f (Just (Owner user)) = do
    let hash= (f <> "@" <> user)
    getDeltas hash <|> sendDeltas hash <|> return ()
    where
    getDeltas hash= do
          Delta delta <- suscribe hash
    
          localIO $ applyDeltas delta
          empty


    sendDeltas hash= do 

          del <- local $ getMailbox' ("out" :: JSString)
          publish hash (del :: Delta)
          empty

{-# NOINLINE portcounter #-}      
portcounter = unsafePerformIO $ newIORef 0

getPort = do
  x <- atomicModifyIORef portcounter $ \n -> (n + 1, n + 1)
  return . show $ 8000 + x
  


str s= s :: JSString



newtype Live= Live Bool  deriving (Typeable,Read,Show)

--instance Semigroup Bool where (<>)= (||)
--instance Monoid Bool where mempty = False



editsOfCode :: Cell String -> Cloud (String, String)
editsOfCode filenamew =  do
      fileNameBox  **> saveCompileButton  <|> changeContent 
      local $ do
         content <-  copyContent
         name <- get filenamew
         liftIO $ js_setAnnotations  "[]"
         return (name,content)
    where
    fileNameBox = local $ render $ at "#entername" Insert
                                    $ mk filenamew Nothing
                                    ! placeholder "enter yourfilename.hs please"

    saveCompileButton=  local $ render (at "#entername" Append $ inputSubmit ("save/compile" ::String) `fire` OnClick >> return())

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
        
        deltas <- local $ groupByTime 1000000 $ runCloud' $ do
            (delta :: JSString, hdelta) <- reactOnModify
 
            (interpretDelta hdelta filenamew currentUser >> empty) <|> return ("," <> delta) 

        guard ( deltas /= "")
        
        return () !> "GUARD"
        
        local $ putMailbox' ("out" :: JSString) $ Delta $ jstail deltas        
        
        where
        reactOnModify=  local $ do
                  ModifyEvent jsdelta <- react onmodify $ return ()
                  hdelta              <- liftIO $ deltaToTuple jsdelta
                  delta               <- liftIO $ stringify jsdelta
                  return () !> delta
                  return (delta,hdelta)

        interpretDelta (iniline,inicol,endline,endcol,action,content) filenamew currentUser=  do
          -- showDelta delta
          filename <- local $ get filenamew

          guard (iniline==endline-1)

          l <- localIO getCurrentLine
          isComment l >> decorate currentUser >> isInvite currentUser 
          
        isComment l = local $ do
              setParseString $ BS.pack $ unpack l
              tDropUntilToken  "-- "

        decorate us= localIO $ insertText  $ "(" <> pack us  <> ")"
        isInvite currentUser= do
          local $ string "invite"
          do
              return () !> "FOLDER"
              local $ string "folder"
              user <- local parseString
              cancelIfSelf user
              filename <- local $ get filenamew
              inviteTo True (BS.unpack user) filename
            <|> do
              return () !> "USER"
              user <- local parseString
              
              return () !> "AFTER FOLDER"
              cancelIfSelf user

              filename <- local $ get filenamew
              inviteTo False (BS.unpack user) filename
              return () !> "AFTER INVITE TO"
          where
          
          cancelIfSelf user= local $ do
              if user ==  (BS.pack  currentUser) then do liftIO $ insertText  " : You can not invite yourself" ; empty
                        else return ()
                        
          inviteTo:: Bool -> String -> String -> Cloud ()
          inviteTo shareFolder user file= do
              let fil= if shareFolder then take (lastIndexOf '/' file) file else file



 

              inviteeStatus <- atRemote $  do
                    
                    nodeFile <- local getMyNode

                 -- if already invited, do nothing
                 -- no tiene sentido recorrer toda la red para obtener el identificativo de un fichero
                 -- mejor que al acceder el nodo reciba el registo shared completo y usar CloudRefs
                 --inv <- select  sharedId $ sharedFile .==. (getDBRef $ keyFile fileid nodeFile :: DBRef File) 
                 --                .&&. sharedInvited `containsElem` Invited user
                                 
                 -- if not $ null inv then return (False,True) else do

                    (Any us,Any wnds) <- exploreNet $ do  
                         -- if isBrowserInstance then return (Any False, Any False) else do
                            (us,wnds) <- local $ do

                                liftIO $ atomically $ do

                                   users <- select userName $ userName .==. Name user  -- may be more than one user with same name
                                   webconnections <- select connectedWebNode 
                                                      $ connectedUser .==. (getDBRef $ keyUserName user :: DBRef User)
                                   return(users :: [Name], webconnections :: [Node])
                            return () !> ("BROWSER NODEBROWSER NODESSSSSSSSSSSSSSSSSSSSSSSSS",us,wnds)

                            notifyWebNodes nodeFile fil wnds currentUser

                            return () !> "AFTER NOTIFY"
                            return (Any $ not $ null us  , Any $ not $ null wnds ) 


                    -- if invitation sucessful add this to the list of invitation of this user
                    when us $ local $  do

                      sharedId <- genGlobalId
                      liftIO $ atomically $ do
 
                          [fileid] <- select fileId $  fileName .==. File file

                          newDBRef $ Shared
                                            { sharedId        =    sharedId
                                            , sharedFile      =    getDBRef $ keyFile fileid nodeFile
                                            , sharedNode      =    nodeFile
                                            , sharedInvited   =    Invited user}
                                                            
                          Just ureg <- readDBRef $ getDBRef $ keyUserName currentUser
                          return () !> ureg
                          let coninfo= fromJust $ userConnected ureg
                          return ()
                          -- Just conreg <- readDBRef  coninfo
                          -- return () !> (conreg,newReg,coninfo,getDBRef (key newReg):: DBRef Shared) 
                          
                          -- add document to the list of edited documents  owned by this user that are being edited by him
                          -- writeDBRef coninfo $ conreg{connectedEditing= fileRef: connectedEditing conreg}
                      return()
                    return() !> "AFTER EXPLORENET 1"
                    return (us,wnds)

              insertInEditor $ case  inviteeStatus of
                  (True,True)  -> "-- invitation sent"
                  (True,False) -> "-- invitation sent, but user not online"
                  (False,False)-> "-- " <> pack user <> str ": user not found"
                  (False,True) -> "-- " <> pack user <> str ": Already invited"
              return () !> "INSERTINEDITOR"
              when (not shareFolder) $ suscribeModifications file $ Just $ Owner currentUser
                  
              
            where

                  
            notifyWebNodes node doc wnds owner= do
              return() !> ("NOTIFYWEBNODES",wnds,doc)
              callNodes'  wnds (<|>) empty $ local $ do
                return () !> "notify WEB NODES PUTMAILBOX" 
                putMailbox ((node,owner,doc) :: (Node,String,String))
                empty
             <|> return ()

            insertInEditor :: JSString -> Cloud ()
            insertInEditor txt= atBrowser $ localIO $ insertText txt
            



            lastIndexOf c str= last $ elemIndices c str

getCurrentFile :: TransIO (String, Maybe Owner)
getCurrentFile = getRState <|> error "getCurrentFile state not defined"

suscribeModifications file currentUser= loggedc $ do
                  (prevFile,prevmowner) <- local getCurrentFile
    
                  sendGetDeltas False prevFile prevmowner
                  sendGetDeltas True file  currentUser
                  
notifyEditing :: Node -> String -> Maybe Owner -> Cloud ()           
notifyEditing node file currentUser = loggedc $ do
          mywebnode <- local getMyNode
          return () !> "NOTIFYEDITING"
          (prevFile :: String, prevmowner :: Maybe Owner) <- local getCurrentFile  
          atServer $ local$ do
                -- mynode <- getMyNode
                fileid <- genGlobalId

                liftIO $ atomically $ do 
                  
                      -- delete the previous file from the connectedEditing list
                      [connId ::Int] <- select connectedId $ connectedWebNode .==. mywebnode
                      let conRef = getDBRef $ keyConnected connId
                      reg <- readDBRef conRef `onNothing` error "user connection register not found" 
                      return () !> ("conRef",conRef :: DBRef Connected)
                      when (isJust prevmowner) $ do
                          return () !> "prevowner"
                          fids <- select fileId $ fileName  .==. File prevFile
                          when (not $ null fids) $ 
                            writeDBRef conRef $ reg{connectedEditing= connectedEditing reg \\[getDBRef $ keyFile (head fids) node  ]} 
                  
                  
                  
                      -- add Shared register, and add sharedId from the connectedEditing field
                      
                      fids <- select fileId $ fileName .==. File file

                      if  not $ null fids  -- if the file is  shared
                        then 
                           writeDBRef conRef reg{connectedEditing= union (connectedEditing reg) [getDBRef $ keyFile  (head fids) node ]} 
                        else do


                           fileReg <- newDBRef FileReg{ fileId= fileid
                                                    , fileName= File file 
                                                    , fileOwner= fromJust currentUser
                                                    , fileNode= node
                                            }
                           return () !> ("fileReg", fileReg)
                           writeDBRef conRef reg{connectedEditing= union (connectedEditing reg) [fileReg]} 
                           
                        




runTheShow :: String -> String -> String -> Cloud (Maybe String)
runTheShow file source  port = atServer $ do
    localIO $ maybeKillProgram port

    localIO $ writeFile file source !> file

    r <- displayExecution  $  "chmod 777 "++ file ++ " && cd `dirname "++ file ++ "` && eval ./`basename "++ file ++ "` -p start/localhost/"++ port
    
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
displayExecution :: String  -> Cloud (Maybe Errors) -- [(BS.ByteString,Int,Int,BS.ByteString)]
displayExecution expr  = onServer $ do

      r <- lazy $ liftIO $ createProcess1 $ (shell expr){std_in=CreatePipe,std_err=CreatePipe,std_out=CreatePipe}
      local $ setRState $ Just $ handle r 
      return () !> ("EXECUTEDDDDDD", expr)      
      makeinput r <|> watch r <|> watcherror r
      
      where
      
      input1 r= inp where (Just inp,_,_,_)= r
      output r= out where (_,Just out,_,_)= r
      err r= err where  (_,_,Just err,_)= r
      handle r= h where  (_,_,_,h)= r
      
      makeinput r=  onServer $ do
        local $ setSynchronous False
        local abduce
        localIO $ print "displayEXECUTIONNNNNNNNNNNNNNNNNNNNNNNNNNNNN"
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

      watch r= onServer $ syncStream $  do
                mline  <- local $ threads 0 $ (parallel $  (SMore <$> hGetLine' (output r)) `catch` \(e :: SomeException) -> return SDone)
                                  --   <|> async (return  $ SMore "-------------------- Executing ---------------------")
                case mline of
                   SDone -> empty
                   SMore line ->  do
                      atRemote $ local $ do
                          render . at "#frame"  Append  $ rawHtml $ pre ! style "line-height:50%;word-wrap:break-word" $line
        
                          liftIO $ scrollBottom "frame"

                      localIO $ print ("LINEEEEEEEEEEEEEEEE=", line)
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
  liftIO $ do

    case result of
      Nothing -> js_setAnnotations  "[]"

      Just "[]" -> do

        js_setAnnotations  "[]"

        wopen $ "http://" <> (pack $ nodeHost serverNode) <> ":" <> (pack $ show $ nodePort serverNode) <>"/relay/localhost/"<> pack port <> "/"


      Just errors ->
        js_setAnnotations $ toJSString errors


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
    setParseString $ BS.pack err2 -- setState $ ParseContext (return SDone) $ BS.pack err2 !> ("ERRORS",err2)
    r <-  parseErrors
    return $ toJSON r
  <|>
    (return $ "[{'row': '2','column':'0','text':'" <> mapchars err2 <> "','type':'error'}]")

  where
  parseErrors = try(manyTill multilineError isDonep')  <|> 
                   (isDone >>= \c -> if c !> ("d",c) then empty else anyChar >> parseErrors !> "parseerrors")
  
  isDonep'= isDone >>= \c -> if c  then return ' ' else empty

  toJSON ers= "[" <> (concat $ map oneerr ers) <> "{}]"

  oneerr (file,l,c,msg,typ)="{'row': '" <> (show $ l-1) <> "','column':'" <> (show c)
                             <> "','text':'" <> (mapchars $ BS.unpack  msg) <> "','type':'" <> typ <>"'},"

  mapchars rs=  subst "\CAN" "\\\'" $ subst "\EM" "\\\'"  $ subst "'" "\\\'" $ subst "\n" "\\n"  rs :: String

--  isRight x = x /= '\n'  && ( x == ':' || x == '/' || x == '-' || x == '>' || x== '<' || x== '.' || x=='\\' || isAlphaNum x || isSpace x || isNumber x)

  multilineError :: TransIO (BS.ByteString,Int,Int,BS.ByteString,String)
  multilineError= do
    file <- tTakeWhile' (/= ':')  !>  "multilineError"
    line <- int

    col  <- int

    typ <- (try $ string "\n" >> return "error") <|>
           (try $ string " Warning:\n" >> return "warning") <|>
           return "error"
    lines <- getErrLines
    let linesError = BS.concat $ map (<> "\\n") lines
    return (file, line, col, linesError,typ)

  getErrLines  ::  TransIO [BS.ByteString]
  getErrLines = manyTill5 ( tTakeWhile' (/= '\n')) endError
     where
     manyTill5 proc end= do
       nlines <- liftIO $ newIORef 0
       manyTill (do
          m <- liftIO $ atomicModifyIORef nlines $ \n -> (n+1, n+1)
          let prefix= if (m == 5) then "<a href='alert(\"hello\")'>" else mempty
          return prefix <> proc)
         end
       
  endError= tChar '\n' <|> isDonep'
  


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

copyContent :: MonadIO m => m String
copyContent = liftIO $ js_copyContent >>= fromJSValUnchecked

-- foreign import javascript safe "editor.session.on('change',$1)"
foreign import javascript safe "editor.session.on('change',function(ev){if (editor.curOp && editor.curOp.command.name)$1(ev);})"
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

foreign import javascript unsafe "editor.getSession().getDocument().applyDeltas(JSON.parse('['+$1+']'))" applyDeltas :: JSString -> IO ()

jstail= JS.tail
#else
jstail= tail
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
insertText :: JSString -> IO()
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

{-

exploreNet :: (Loggable a,Monoid a) => Cloud a -> Cloud a
exploreNet action = atServer $ exploreNetExclude []   
    where
    exploreNetExclude nodesexclude = loggedc $ do
       local $ return () !> "EXPLORENETTTTTTTTTTTT"
       action <> otherNodes
       where
       otherNodes= do
             node <- local getMyNode
             nodes <- local getNodes' 
             return () !> ("NODES to explore",nodes)
             let nodesToExplore= tail nodes \\ (node:nodesexclude)
             callNodes' nodesToExplore (<>) mempty $
                          exploreNetExclude (union (node:nodesexclude) nodes) 
                          
       getNodes'= getEqualNodes -- if isBrowserInstance then  return <$>getMyNode  -- getEqualNodes
                     -- else (++) <$>  getEqualNodes <*> getWebNodes


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

suscribe :: (Loggable a) => String -> Cloud a
suscribe hash= do
  node <- local getMyNode
  local (getMailbox' hash) <|> atServer (do
       localIO $ atomicModifyIORef suscribed $ \ss -> (insert hash [Right node] ss,())
       susc node)
  where
  susc node=do
      exploreNet $ local $ do
             nodes <-  getEqualNodes
             let nodes'= if node == head nodes then tail nodes else nodes
             return () !> ("insert", nodes')

             liftIO $ atomicModifyIORef suscribed $ \ss -> (insert hash (map Left nodes') ss,())

             empty
             return()
      empty

  insert h node susc=
       let ns = fromMaybe [] $ M.lookup h susc
       in M.insert h (union node ns) susc
       

    

unsuscribe hash withness= do
  node <- local getMyNode
  local $ cleanMailbox' hash withness 
  atServer $ do
       localIO $ atomicModifyIORef suscribed $ \ss -> (delete hash [Right node] ss,())

       nodes <- localIO $ atomicModifyIORef suscribed $ \ss -> let ns= fromMaybe [] $ M.lookup hash ss in (ss,ns)
       when (null $ filter isRight nodes) $  do
           servernode <- local getMyNode
           exploreNet $ localIO $ atomicModifyIORef suscribed $ \ss -> let ns= delete hash [Left servernode] ss in (ns,())

  where
  isRight ::  Either Node Node -> Bool
  isRight (Right _)= True
  isRight _ = False

    
      
  
  delete h nodes susc=
       let ns = fromMaybe [] $ M.lookup h susc
       in M.insert h (ns \\ nodes) susc
          


publish :: Loggable a => String -> a -> Cloud ()
publish hash dat= do
   n <- local getMyNode
   publishExclude [Right n] hash dat

publishExclude :: Loggable a => [Either Node Node] -> String -> a -> Cloud ()
publishExclude excnodes hash dat= atServer $ do
    
    nodes <- localIO $ readIORef suscribed >>= return . fromMaybe [] . M.lookup hash
    let unodes= union nodes excnodes
    return() !> ("NODES PUB",nodes \\ excnodes)
    foldr (<|>) empty $ map (pub unodes) (nodes \\ excnodes)
    empty


    where 
    pub  unodes (Left node) = runAt node $ publishExclude unodes hash dat >> empty >> return ()
    pub  _      (Right node)= runAt node $ local $ do 
                     putMailbox' hash dat  
                     return () !> "PUTMAILBOX"
                     empty
-}

#ifdef ghcjs_HOST_OS
optionn x y = render $ wbutton  x y
#else
optionn x y= option x y
#endif


main3= keep $ initNode $ onBrowser $ local $ do
    r <- render $ 
                  (,,) <$> inputString Nothing 
                       <*> inputPassword
                       <*> (inputPassword <|> return "" )

                       <** inputSubmit ("ok" ::String)  `fire` OnClick
    alert r
    where

    size= atr "size"
   
main1= keep $ initNode $ onBrowser $ do
     addThisNodeToRemote
     atServer $ do
         addThisNodeToRemote

         local  $ option "fire" "fire" :: Cloud String
         local $ do
            abduce
            labelState "mailbox"
         (local $ getMailbox :: Cloud ()) <|> (do local $ putMailbox (); empty)
         nodes <- local getNodes 
         runAt (nodes !! 1) $ do
             nodes <- local $ getNodes 
             runAt (nodes !! 1) $ localIO $ print "SERVEEEEEEEEEER"
     local $ alert "COMPLETED"
     
main2 = keep $ initNode $  inputNodes <|>  (onBrowser $ do

    --addWebNode

    --local $ optionn ("f" :: String) "fire" 
    --r <- exploreNet $ local  $ return <$> getMyNode :: Cloud [Node]
    --localIO $ print r 
    --empty 

    wnode <- local getMyNode
    atRemote $  local $ updateConnectionInfo wnode "" >> return ()


    r <- suscribe "hello" <|> do
              local  $ optionn ("f" :: String) "fire"
              publish ("hello" ::String) ("world" :: String)
              empty

    local $ render $ rawHtml $ p (r :: String) )

   
   
-- | add the web node to the server so that the server can recognize it and call back the web node asynchronously.
addWebNode= doit <|> return()
 where
 doit= onBrowser $ do
   wnode <- local getMyNode
   atRemote $ local $ do
       updateConnectionInfo wnode ""
       ex <- getSData <|>  error "NO BACKPOINT"
       onExceptionPoint ex $ \(e :: SomeException)  -> do
                                liftIO $ print ("REMOVING NODE", wnode)
                                delNodes [wnode]
       empty
       return()  


#ifdef ghcjs_HOST_OS   -- void definitions for database access in the browser
shouldnot= error "this should not run on browser"
data DBRef x=  DBRef JSString deriving (Read, Show, Typeable,Eq) 
keyShared= shouldnot
keyUserName x= shouldnot 
keyConnected= shouldnot
keyFile= shouldnot
readDBRef= shouldnot
newDBRef= shouldnot
writeDBRef= shouldnot
key= shouldnot
data STM x= STM x deriving (Functor,Applicative,Monad) 
containsElem x y = shouldnot
getDBRef x= shouldnot
select x = shouldnot
x .==. y = shouldnot
x .&&. y = shouldnot
x .||. y = shouldnot
#endif    
