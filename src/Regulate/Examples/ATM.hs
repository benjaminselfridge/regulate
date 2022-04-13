{-# LANGUAGE RankNTypes #-}

module Regulate.Examples.ATM where

import Regulate.NFA
import Regulate.NFA.GraphViz

data CustomerEvent = Insert_Card | Identification_Succeeds | Request_Withdrawal
                   | Get_Money | Not_Sufficient_Funds | Identification_Fails
  deriving (Show, Read, Eq, Ord)

instance SymbolLabel CustomerEvent where
  symbolLabel = eventLabel

data ATMEvent = Read_Card | Validate_Id | Id_Successful | Check_Balance
              | Sufficient_Balance | Dispense_Money | Unsufficient_Balance
              | Id_Failed
  deriving (Show, Read, Eq, Ord)

instance SymbolLabel ATMEvent where
  symbolLabel = eventLabel

type Event = Either CustomerEvent ATMEvent

c :: CustomerEvent -> MNFA s Event
c = symbol . Left

a :: ATMEvent -> MNFA s Event
a = symbol . Right

type Scope = forall s . MNFA s Event -> MNFA s Event

customer :: Scope -> MNFA s Event
customer scope = scope $
  c Insert_Card `cat`
  ( ( c Identification_Succeeds `cat`
      c Request_Withdrawal `cat`
      (c Get_Money `union` c Not_Sufficient_Funds)) `union`
    c Identification_Fails )

atm :: Scope -> MNFA s Event
atm scope = scope $
  ( a Read_Card `cat`
    a Validate_Id `cat`
    ( ( a Id_Successful `cat`
        a Check_Balance `cat`
        ( ( a Sufficient_Balance `cat`
            a Dispense_Money ) `union`
          a Unsufficient_Balance ) ) `union`
      a Id_Failed ) )

data_base :: Scope -> MNFA s Event
data_base scope = scope ( a Validate_Id `cat` optional (a Check_Balance))

atm_withdrawal :: Scope -> NFA Event
atm_withdrawal scope =
  buildNFA (customer scope) `join'`
  buildNFA (star (c Insert_Card `cat` a Read_Card)) `join'`
  buildNFA (star (c Request_Withdrawal `cat` a Check_Balance)) `join'`
  buildNFA (star (a Id_Successful `cat` c Identification_Succeeds)) `join'`
  buildNFA (star (a Dispense_Money `cat` c Get_Money)) `join'`
  buildNFA (star (a Unsufficient_Balance `cat` c Not_Sufficient_Funds)) `join'`
  buildNFA (star (a Id_Failed `cat` c Identification_Fails)) `join'`
  buildNFA (atm scope) `join'`
  buildNFA (data_base scope)

type Trace = ([CustomerEvent], [ATMEvent])

traces :: NFA Event -> [Trace]
traces = generateAll cons
  where cons (Left  e) (cs, as) = (e:cs, as)
        cons (Right e) (cs, as) = (cs, e:as)
