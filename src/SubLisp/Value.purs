module SubLisp.Value where

import Prelude
import Data.Array as A
import Data.List (List)
import Data.String (joinWith)
import SubLisp.Const (Const)
import SubLisp.Term (Term)

data Value
  = VConst Const
  | VSymbol String
  | VQuoted Term
  | VPair Value Value
  | VFun (List String) (List Term)

derive instance eqValue :: Eq Value

instance showValue :: Show Value where
  show (VConst c) = show c
  show (VSymbol s) = s
  show (VQuoted t) = "'" <> show t
  show (VPair h t) = "(" <> show h <> " . " <> show t <> ")"
  show (VFun args body) = "#<function (# (" <> (joinWith " " <<< A.fromFoldable) args <> ") " <> show body <> ")"
