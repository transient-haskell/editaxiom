#!/usr/bin/env execthirdlinedocker.sh

-- mkdir -p ./static && ghcjs -DDEBUG ${1} -o static/out && echo ${1} && runghc -DDEBUG ${1}  ${2} ${3}
{-# LANGUAGE OverloadedStrings,  CPP #-}

module Main where

import GHCJS.HPlay.View
#ifdef ghcjs_HOST_OS
   hiding (map)
#else
   hiding (map, option,runCloud')
#endif

import Prelude hiding (div)
import Transient.Base
import Transient.Move
import Transient.Indeterminism
import Transient.Move.Utils
import Control.Applicative
import Control.Monad
import Data.Typeable
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad.State
import Control.Concurrent.MVar
import System.Random
import System.IO.Unsafe
import Data.String

newtype Operation= Operation String deriving (Read,Show, Typeable)

-- Follows  http://www.math-cs.gordon.edu/courses/cs211/ATMExample/
-- to demostrate how it is possible to program at the user requiremente level
-- the program follows closely the specifications and be clear enough to be understood
-- by the client

main= keep $ initNode $ atm



atm= do
   card <- waitCard
   --validateBank  card
   setData card
   performTransactions <|> cancel
   

performTransactions = do
    --clearScreen
    operation <- withdrawal  <|> deposit <|> transfer <|> balanceInquiry
    printReceipt operation
    empty

withdrawal= do
    local . render $ wlink ()  $ toElem $ jstr "withdrawall"
    local $ jprint "choose bank account"
    account <- local chooseAccount
    local $ jprint "Enter the quantity"
    quantity <- local . render $ getInt Nothing
    if quantity `rem` 20 /= 0
      then do
        local $ jprint "multiples of $20.00 please"
        empty
      else do
        r <- approbalBank account quantity
        case r of
            False ->  do
                local $ do 
                   jprint "operation denied. sorry"
                   jprint "Try another transaction"
                   empty
--                r <- local $ render $ wlink True (b "yes ") <|> wlink False  (b "No")
--                if not r then return ()
--                                 else performTransactions
            True  ->do  
                      giveMoney r
                      return $ Operation $ "withdrawal " ++ show quantity

deposit= local $ do
    render $ wlink () $ b $jstr "Deposit "
    jprint "choose bank account"
    account <- chooseAccount
    r <- approbalBankDeposit account
    case r of
        False -> do jprint "operation denied. sorry"
                    empty
        True  -> do
            let timeout t = collect' 1 t
            r <- timeout 10000000 waitDeposit 
            case r of
                [] -> do jprint "timeout, sorry"; empty
                _  -> return $ Operation "deposit"

transfer= local $ do
    render $ wlink () $ b $ jstr "Transfer "
    jprint "From"
    ac <- chooseAccount
    jprint "amount"
    amount <- render $ inputDouble Nothing
    jprint "To"
    ac' <- chooseAccount
    transferAccBank ac ac' amount
    return $ Operation $ "transfer from "++ show ac ++ " to " ++ show ac'

balanceInquiry= local $ do
    render $ wlink () $ b $ jstr "Balance inquiry "

    jprint "From"
    ac <- chooseAccount
    r <- getBalanceBank ac
    sprint $ "balance= "++ show r
    return $ Operation $ "balanceInquiry: "++ show r

validateBank :: Card -> Cloud ()
validateBank  card = validate'  card 0
   where
   validate'  card times=  do
    pin <- enterPIN
    localIO $ print "ENTERED PIN"
    atServer $ do
        localIO $ print "AT SERVEr"
        r <- verifyPinBank pin card
        if r  then return () 
        else if times == 2 then do
                local $ jprint "three tries. card will be retained"
                empty

        else validate'  card $ times + 1


rtotal= unsafePerformIO $ newEmptyMVar
ractive= unsafePerformIO $ newMVar False

switchOnOff= on <|> off
  where
  on= do
     render $ wbutton () "On"
     jprint "enter total amount of money"
     total <- render $ getInt Nothing
     liftIO $ do
       tryTakeMVar rtotal
       putMVar rtotal total
  off= do
     render $ wbutton () "Off"
     active <- liftIO $ readMVar ractive
     if active then empty else jprint "ATM stopped"

type AccountNumber= Int
newtype Card= Card [AccountNumber]  deriving Typeable

waitCard = do
   local $ render $ wbutton () "enter card" ! atr "class" "button"
 --  atServer . local $ do
 --     option1 "enter"  "simulate enter card"
   return $ Card [1111]

enterPIN= local $ do
      jprint "Enter PIN"

      render $ getInt Nothing `fire` OnChange  <**  inputSubmit  ("enter" :: JSString) -- `fire` OnClick

cancel=  do
  local . render $ wbutton () "Cancel"
  returnCard

returnCard=  do
  clearScreen
  local $ jprint "Card returned"

clearScreen=  local  $ do
   delData $ Alternative undefined
   render . wraw $ forElems "body" $ this  >> clear `child` (div ! atr "id" "body1" $ noHtml)
   setRenderTag "body1"

printReceipt (Operation str)= local . sprint $ "receipt: Operation:"++ str

chooseAccount :: TransIO AccountNumber
chooseAccount= do
    Card accounts <- getSData <|> error "transfer: no card"
    jprint "choose an account"
    render $ foldr (<|>) empty [wlink ac (fromString $ ' ':show ac) | ac <- accounts]

approbalBank ac quantity= atServer $ return True

giveMoney n= local $ sprint $ "Your money : " ++ show n ++ " Thanks"

approbalBankDeposit ac= return True

transferAccBank ac ac' amount= sprint $ "transfer from "++show ac ++ " to "++show ac ++ " done"

getBalanceBank ac= liftIO $ do
    r <- rand
    return $ r * 1000

verifyPinBank pin _= localIO $ do
    putStr "verifyPinBank "
    print pin
    r <- rand
    if r > 0.2 then do liftIO $ print "valid" ; return True 
               else do liftIO $ print "invald"; return False

waitDeposit =  do
     n <- liftIO rand
     if n > 0.5 then return () else empty

rand:: IO Double
rand= randomRIO(0,1)



jprint :: JSString -> TransIO()
jprint = render . wprint

sprint :: String -> TransIO()
sprint = render . wprint

jstr :: JSString -> JSString
jstr= Prelude.id