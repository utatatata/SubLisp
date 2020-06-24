module SubLisp.Const where

import Prelude

data Const
  = CNil
  | CT
  -- Special forms
  | CIf
  | CQuote
  | CFun
  | CDef
  -- Built-in functions
  | CAtom
  | CEq
  | CHead
  | CTail
  | CCons

derive instance eqConst :: Eq Const

instance showConst :: Show Const where
  show (CNil) = "nil"
  show (CT) = "t"
  show (CIf) = "#<syntax (if cond then else)>"
  show (CQuote) = "#<syntax (quote obj)>"
  show (CFun) = "#<syntax (fun (...args) ...body)>"
  show (CDef) = "#<syntax (def bound-variable obj)>"
  show (CAtom) = "#<built-in (atom? obj)>"
  show (CEq) = "#<built-in (eq obj1 obj2)>"
  show (CHead) = "#<built-in (head obj)>"
  show (CTail) = "#<built-in (tail obj)>"
  show (CCons) = "#<built-in (cons obj obj)>"
