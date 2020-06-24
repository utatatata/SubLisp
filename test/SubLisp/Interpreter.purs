module Test.SubLisp.Interpreter where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Either (either)
import SubLisp.Const (Const(..))
import SubLisp.Interpreter (InterpretError(..), interpret, runInterpreter)
import SubLisp.Parser (parse)
import SubLisp.Term (Term(..))
import SubLisp.Value (Value(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.Parser (runParser)

testInterpret :: Spec Unit
testInterpret =
  describe "SubLisp.Interpreter" do
    describe "interpret" do
      describe "valid tests" do
        validTest (TmConst CNil) $ VConst CNil
        validTestWithParser "t" $ VConst CT
        validTestWithParser "(cons t nil)" $ VPair (VConst CT) (VConst CNil)
        validTestWithParser "(head (cons t nil))" $ VConst CT
        validTestWithParser "(tail (cons t nil))" $ VConst CNil
        validTestWithParser "(eq nil (tail (tail (cons t (cons t nil)))))" $ VConst CT
        validTestWithParser "(atom? t)" $ VConst CT
        validTestWithParser "(if t t t)" $ VConst CT
        validTestWithParser "(if t (eq t nil )  nil  )" $ VConst CNil
        validTestWithParser "(if t t (eq t nil))" $ VConst CT
        validTestWithParser "(if (eq t nil) t nil)" $ VConst CNil
        validTestWithParser "(if (eq t nil) (cons t nil) (cons nil nil))" $ VPair (VConst CNil) (VConst CNil)
  where
  validTest term expected =
    it (show term) do
      runInterpreter mempty (interpret term) `shouldEqual` pure expected

  validTestWithParser code expected =
    it code
      $ either
          (throwError <<< SyntaxError)
          (runInterpreter mempty <<< interpret)
          (runParser code parse)
          `shouldEqual`
            pure expected
