#!/usr/bin/env execthirdlinedocker.sh

-- mkdir -p ./static && ghcjs -DDEBUG ${1} -o static/out && echo ${1} && runghc -DDEBUG ${1}  ${2} ${3}

{-
Programmed following the requirements of a canonical Java project: 

http://www.math-cs.gordon.edu/courses/cs211/ATMExample/

It demonstates that it is possible to program clearly at the level of the requirements so that the author of the requirements
and even the business client can understand the program. This is possible thanks to funcional programming, specially to the
power of continuations and other features like pure state, early termination and asynchronicity, which are included out of the box
in the Transient libraries  (https://github.com/transient-haskell).

The program includes the server and the client program.

This is in a sharp contrast with OOP programming, which perform a complete deconstructio and transformation of the requirements 
in a set of components and add a lot of accidental complexity.
-}

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


main= keep $ initNode atm

data Option= Withdrawal | Deposit | Transfer | BalanceInquiry | Cancel deriving (Show, Read, Typeable)

atm= onBrowser $ do
   card <- waitCard
   validateBank  card
   setData card
   performTransactions 
   

performTransactions = do
    clearScreen
    option <- mainMenu
    operation <- case option of
                   Withdrawal      -> withdrawal
                   Deposit         -> deposit
                   Transfer        -> transfer
                   BalanceInquiry  -> balanceInquiry
                   Cancel          -> cancel
       
    printReceipt operation
    

mainMenu= do
    local . render $ wbutton Withdrawal     ("Withdrawall ")     <|>
                     wbutton Deposit        ("Deposit ")         <|>
                     wbutton Transfer       ("Transfer ")        <|>
                     wbutton BalanceInquiry ("Balance inquiry ") <|>
                     wbutton Cancel         ("Cancel ")





withdrawal= do

    account <- chooseAccount
    local $ jprint "Enter the quantity"
    quantity <- local . render $ getInt Nothing `fire` OnChange <** inputSubmit (jstr "send")
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

deposit=  do
    local $ jprint "choose bank account"
    account <- chooseAccount
    r <- approbalBankDeposit account
    case r of
        False -> do local $ jprint "operation denied. sorry"
                    empty
        True  -> do
            let timeout t = collect' 1 t
            r <- atServer $ local $ timeout 10000000 waitDeposit 
            case r of
                [] -> do local $ jprint "timeout, sorry"; empty
                _  -> do
                    local $ jprint "deposit done"
                    return $ Operation "deposit"

transfer=  do
    local $ jprint "From"
    ac <- chooseAccount
    local $ jprint "amount"
    amount <- local . render $ inputDouble Nothing `fire` OnChange <** inputSubmit  (jstr "send")
    local $ jprint "To"
    ac' <- chooseAccount
    transferAccBank ac ac' amount
    return $ Operation $ "transfer from "++ show ac ++ " to " ++ show ac'

balanceInquiry=  do

    local $ jprint "From"
    ac <- chooseAccount
    r <- getBalanceBank ac
    local $ sprint $ "balance= "++ show r
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

cancel=    returnCard 

returnCard=  do
  clearScreen
  local $ jprint "Card returned"
  empty



printReceipt (Operation str)= local . sprint $ "receipt: Operation:"++ str

chooseAccount :: Cloud AccountNumber
chooseAccount=  local $ do
    Card accounts <- getSData <|> error "transfer: no card"
    jprint "choose an account"
    render $ foldr (<|>) empty [wlink ac (fromString $ ' ':show ac) | ac <- accounts]

approbalBank ac quantity= atServer $ do
    -- r <- async . simpleHTTP $ getRequest "http://bank" -- simulation of request
    localIO $ print "Approbed by the bank"
    return True

giveMoney n= local $ sprint $ "Your money, Thanks"

approbalBankDeposit ac = atServer $ do
    -- r <- async . simpleHTTP $ getRequest "http://bank" -- simulation of request
    localIO $ print "deposit pprobed by the bank"
    return True

transferAccBank ac ac' amount= do
   atServer $ localIO $ print $ "transfer from "++show ac ++ " to "++show ac ++ " done"
   local $ sprint $ "transfer from "++show ac ++ " to "++show ac ++ " done"

getBalanceBank ac= atServer . localIO $ do
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