module SubLisp.Term where

import Prelude
import Data.Array as A
import Data.List (List)
import Data.String (joinWith)
import SubLisp.Const (Const)

data Term
  = TmConst Const
  | TmQuoted Term
  | TmVar String
  | TmPair Term (List Term)

derive instance eqTerm :: Eq Term

instance showTerm :: Show Term where
  show (TmConst c) = show c
  show (TmQuoted t) = "'" <> show t
  show (TmVar v) = v
  show (TmPair h t) = "(" <> show h <> " " <> (joinWith " " <<< A.fromFoldable <<< map show) t <> ")"
