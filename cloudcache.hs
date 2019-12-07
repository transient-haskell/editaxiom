{-# LANGUAGE ScopedTypeVariables #-}
module CloudCache where
import Transient.Base
import Transient.Move
import Data.TCache
import Data.TCache.DefaultPersistence
import Data.Typeable
import Control.Exception hiding (onException)
import Control.Monad.IO.Class
import Control.Applicative


import System.Random
-- CLRefs : references to registers in the cloud

{-
Alternativa usando publish.suscribe:
usar publish en vez de callNodes
 callNodes' sync nodes $ writeDBRef ref x
 publish key $ writeDBRef ref x >> case sync of False -> empty; ....
 necesita suscribe:
 
 readCLRef key...= do
    if not suscribed then suscribeGet key else getValue
    -- nuevas funciones de publish suscribe
    sobra readCLref key no necesario
    
  sobra clRefNodes
  
ahora bien, queries? construir encima de publish suscribe la misma estructura que para DBRefs


getLastValue= buscar con cloudShell en las definiciones
o suscribe que retorne getLastValue
 
 donde se meten los nodos para cada
-}

-- the updates will be replicated synchronously or asynchronously?
type Synchronous= Bool
type Statistics = [(Node, Int)]
type Hash = Int

data ClRefData a= ClRef{ hash       :: Hash
                       , clRefNodes :: [Node]
                       , clRefsync  :: Synchronous 
                       , clRefStats :: Statistics 
                       , clRefDbref :: DBRef a}
                       deriving (Read,Show, Typeable)


instance Indexable (ClRefData a) where
  key clr= show $ hash clr


newtype ClRef= CLN Hash

{- 
alternativa: usar DBRefs directamente, y hacer writeDBRef, readDBRef  sobre otros nodos
   se tiene que usar la IO monad -> servicios

que pasa si almanecamos el CLRefData  como una DBRef local?
   usar un HT.hasmap en memoria: ya lo tiene TCache
      registros persistentes? si porque si no no se puede saber donde está
      usar key o DBRef para apuntar al objeto?
            DBRef al menos puede apuntar directamente al registro si es local
   usar indices?
  no usar DBB Hash? con hash the key of the DBRef. las DBRefs necesitan una clave, que es le hash
   
   
     
     usarlos par saber si hay algun nodo que tenga todos los registros de la query
         select nodes $ DBRef .==. key .&&.  
         no hay mecanismo para saber por adelantado los nodos de cada registro?
             si, cuando se va ejecutando la query, se puede encontrar.
   agrupar por rangos de clave

no se pueden intercambiar CLN's luego no valen para distibuido
--     si valen si se usa hash en lugar de key
       pero el hash recibido no vale nada si no está dada de alta en el map del nodo
       puede preguntar por la referencia, pero donde está? broadcast con exploreNet?
       CLN hash <- local query...
       


clref <- transferClRef clref


se puede llenar el hash de entradas: borrar por estadísticas.                   

           
-}



    


-- create the register in the local node
newClRef ::  a -> TransIO ClRef
newClRef x =  do
   ref <- liftIO $ atomically $ newDBRef x
   node <- getMyNode
   let key= keyResource 
   hash <- liftIO $ randomIO
   let clref= ClRef hash [node] True [] ref
   liftIO $ atomically $ newDBRef clref
   return $ CLN hash 

-- | transfer a ClRef to other node. if the ClRefData is not stored, get it from the origin, using atRemote.
transferClRef :: ClRef -> Cloud ClRef
transferClRef (clref@(CLN hash))= do 
           mref <- localIO $ atomically $ readDBRef (getDBRef $ show hash) 
           case mref of
              Just ref -> return clref
              Nothing  -> do
                   dbrefdata <- atRemote $ localIO $ atomically $ readDBRef (getDBRef $ show hash) 
                   localIO $ atomically $ newDBRef  dbrefdata
                   return clref
   
readClRef :: ClRef -> Cloud a
readClRef (clr@(CLN k))= do
    clref@(ClRef hash nodes _ _ dbref) <- (localIO $ atomically $ readDBRef (getDBRef $ show k))  `onNothing` empty
    -- try to read in any of the nodes. on creation, put the local node if it is, at the beginning of the list
    mr <- tryNodes nodes $ readit dbref
    case mr of
        Right r -> return r
        Left (nodes',r) -> do
            localIO $ atomically $ writeDBRef (getDBRef $show k) clref{clRefNodes= nodes'}
            return r
    where
    tryNodes :: [Node] -> Cloud a -> Cloud a
    tryNodes [] _ = empty
    tryNodes (n:ns) proc= do
        r <- runAt n proc
        case r of 
           Just x -> return x
           Nothing -> tryNodes ns proc
           
    readit node dbref= do
       onException $ \(e :: SomeException) -> do continue; return Nothing       --  communicatin failure, try another node
       runAt node $ localIO $ atomically $ do
               r <- readDBRefByKey k 
               return $ Just $ Right r
                   `onException'`  \(e :: SomeException) -> do
                       r <- readClRef clr                -- probably the register was removed
                                                         -- recursive search in the list of the remote node (1)
                       
                       ClRef nodes _ _ _ <- readDBRef (getDBRef k)  `onNothing` error "ClRef not found"
                       -- must return the actual list of nodes
                       -- to update the local ClRef. xxx
                       return $ Just $ Left (nodes, r)

writeClRef :: ClRef -> a -> Cloud ()
writeClRef ref x = do
    ClRef nodes sync _ dbref <- readDBRef (keyObjDBred dbref)  `onNothing` empty
    callNodes' sync nodes $ writeDBRef ref x
    

{-
todas las ClRefs deben ser añadidos a los maps de todos los nodos? NO
   solo a los que hayan recibido esa ClRef o la hayan creado.
   pero como se distinguen las que lo han recibido de los que los tienen almacenadas?
      hay que hacer updates tambien de los que lo han recibido si cambian los nodos para que sepan los nodos en los que están
      hacer una traza?
      al acceder: redirecciones.  como se codifica  la redirección?
  RESUELTO:      
         ClRef [Node]  Synchronous Statistics (DBRef a))
         cuando se borra una clref de un nodo, en vez de borrarlo, poner la nueva lista en él.
         asi se puede ordenar una búsqueda recursiva (1)
-}

-- set the number of nodes to hold the register
setReplication :: Int -> ClRef -> Cloud ()
setReplication n (clref@(ClRef key))= do
  ClRef hash ns sync _ dbref <- readDBRef (getDBRef key) 
  
  if length ns > n then removeNodes (ns -n) else addNodes (n - ns)
  
  where
  removeNodes n= do
       callNodes'  sync (take n  ns) $ do  --update all remaining nodes
             clref <- readDBRef (getDBRef key) `onNothing`  error "no key found"
             writeDBRef (getDBRef key) clref{clRefNodes= take n $ nodes clref }
             
       callNodes' sync (drop n ns) $ do   --delete the leftovers
             clref <- readDBRef $ getDBRef (getDBRef key) `onNothing` error "error key" -- remove the registers, but keep the map updated
                                                                -- so requesters could be redirected to a node that holds it (1)
             writeDBRef (getDBRef key) clref{clRefNodes= take n $ nodes clref }
             delDBRef dbref
             
             
  addNodes n= do
     nodes <- getNodes
     let candidates = take n $ nodes \\ ns
     callNodes' sync ns $ do
             let kref= getDBRef key
             clref <- readBRef kref `onNothing` error "key error"
             let ncleref= clref{clRefNodes=  nodes clref ++ candidates }
             callNodes' sync ns $ writeDBRef kref nclref   -- update the already existent
             callNodes' sync candidates $ do  -- update the candidates
                  writeDBRef kref neclref
                  writeDBRef dbref dbrefVal
     
    
callNodes' sync nodes proc= 
   if sync then callNodes <> mempty proc  
           else callNodes <|> empty (proc >> empty) <|> return()    
{- 
-- replicate in another node
copyClRef :: [Node] -> ClRef  -> Cloud ()
copyClRef nodes  (ClRef key) = do
     nodes <- getNodes
     let candidates = take n $ nodes \\ ns
     callNodes' ns $ do
             let kref= getDBRef key
             clref <- readBRef kref `onNothing` error "key error"
             let ncleref= clref{clRefNodes=  nodes clref ++ candidates }
             callNodes' sync ns $ writeDBRef kref nclref   -- update the already existent
             callNodes' sync candidates $ do  -- update the candidates
                  writeDBRef kref neclref
                  writeDBRef dbref dbrefVal


removeClRef :: [Node] -> ClRef -> Cloud ()
removeClRef nodes clref= do 
       callNodes'  sync (take n  ns) $ do  --update all remaining nodes
             clref <- readDBRef (getDBRef key) `onNothing`  error "no key found"
             writeDBRef (getDBRef key) clref{clRefNodes= take n $ nodes clref }
             
       callNodes' sync (drop n ns) $ do   --delete the leftovers
             clref <- readDBRef $ getDBRef (getDBRef key) `onNothing` error "error key" -- remove the registers, but keep the map updated
                                                                -- so requesters could be redirected to a node that holds it (1)
             writeDBRef (getDBRef key) clref{clRefNodes= take n $ nodes clref }
             delDBRef dbref

-}