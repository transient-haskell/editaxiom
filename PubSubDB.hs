-- A persistent pubsub implementation
{-# LANGUAGE  TypeSynonymInstances,FlexibleInstances, UndecidableInstances #-}
module PubSubDB where
import Transient.Base
import Transient.Internals ((!>))
import Transient.Move
import Transient.Move.Utils
import Data.TCache
import Data.TCache.DefaultPersistence
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.IORef
import System.IO.Unsafe
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as BSS

{-

para grabar una transacción con conflicto:

  execSuscribe  $ ejecutar >> retornar momentos de ejecución
  si para esa misma key me llega de otro nodo una transaccion con otro valor ...
  bloquear?
    habria que bloquear todos los elementos que intervienen en la transaccion atomica:
    execSuscribe $ atomically 
      ...
      ..
  atomically $ do
       safeIOtoSTM execSuscribe
       
  se necesita usar el abort de STM como trigger para repetir
    meter como ultima orden un cheque de algun aborto en la red para hacer un retry en caso neceasario
    
    execSuscribe $ atomically
       ..
       ..
       ..
       abort <- safeIOToSTM $execSuscribe algun abort?
         no es posible sin acabar la trasacción
         necesario STM a nivel cloud
    
  meter en cada registro el valor de fecha de peticion de modificacion
     pero no tiene la operacion, por tanto no puede deshacerse.
  
   get the vars to preserve
   onException $ CloudException... set vars
   execSuscribe key $ atomically $ do
      comp
      val <- readDNRef ref
      if val/= val' then throwt CloudException

  
rollbackOn ref comp= do
   time <- setTimeRef ref
   val' <- readDBRef ref `onException'` $ CloudException... writeDBRef ref newval;return newval 
   execSuscribe key $ atomically $ do
      comp
      val   <- readDBRef ref
      unsafeIOToSTM $ do
        time' <- getTimeRef ref
        if val/= val' && time > time' then throwt CloudException newval
-}

      

  
suscribeAndGetValue ref key node = atServer $ do       
      vals <- exploreNet $  local $ do  -- suscription should be local, to the nodes directly connected
                
                 nodes <-  getEqualNodes
                 let nodes'= if node == head nodes then tail nodes else nodes
                 return () !> ("insert", nodes')
                 liftIO $ atomicModifyDBRef suscribed $ \ss -> (insert key [node] ss,())
                 let ref= getDBRef key
                 liftIO $ atomically $ readDBRef ref >>= \v -> return [v]

      let mv= filter isJust vals
      let res = if null mv then  Nothing else  head mv

      localIO $ atomically $ if (isJust res) then  writeDBRef ref $ fromJust res else delDBRef ref
      localIO $ atomicModifyDBRef suscribed $ \ss -> (insert key [Right node] ss,())
      return res
      
      where
      insert key node susc=
           let ns = fromMaybe [] $ M.lookup key susc
           in M.insert key (union node ns) susc



getCloudRef :: (Indexable a,Loggable a) => String -> Cloud (DBRef a)     
getCloudRef key= do
     let ref = getDBRef key
     susc <- localIO $ issuscribed key 
     when (not susc) $ do
          node <- local getMyNode
          suscribeAndGetValue ref key node >> return ()
     return ref
     
     where
     issuscribed key= do -- and must have the value
         susc <- atomically $ readDBRef  suscribed `onNothing` return M.empty :: IO Suscribed
         return $ isJust $ M.lookup key susc
 
-- add trigger to unsuscribe on delDBRef and drop from cache

unsuscribeRef key' withness= do
  let key= BSS.unpack key'
  node <- local getMyNode
  local $ killState key'  -- to remove the watch added in suscribe 
  u

publishRef :: (Loggable a, Indexable a) => String -> a -> Cloud ()
publishRef key dat= do
   n <- local getMyNode
   publishExclude [Right n] key dat

publishExclude :: (Loggable a, Indexable a) => [Either Node Node] -> String -> a -> Cloud ()
publishExclude excnodes key dat= publishExecute excnodes key $ local $ do 
                     liftIO $ atomically $ writeDBRef (getDBRef key) dat
                     return () !> "PUTMAILBOX"
                     empty  -- if sync then return else empty
                     return()


