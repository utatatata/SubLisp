module Test.SubLisp.Interpreter where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Either (either, isLeft)
import SubLisp.Const (Const(..))
import SubLisp.Interpreter (InterpretError(..), interpret, runInterpreter)
import SubLisp.Parser (parse)
import SubLisp.Value (Value(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.Parser (runParser)

testInterpret :: Spec Unit
testInterpret =
  describe "SubLisp.Interpreter" do
    describe "interpret" do
      describe "valid tests" do
        validTestWithParser "nil" $ VConst CNil
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
        validTestWithParser "((fun (x y z) (if x y z)) t t nil)" $ VConst CT
        validTestWithParser "((fun (x y z) (def a x) (if a y z)) t t nil)" $ VConst CT
        validTestWithParser "((fun () (def _if (fun (x y z) (if x y z))) (_if nil t nil)))" $ VConst CNil
      describe "invalid tests" do
        invalidTestWithParser "((fun (x y z) (def a x) (if b y z)) t t nil)"
  where
  validTestWithParser code expected =
    it code
      $ either
          (throwError <<< SyntaxError)
          (runInterpreter mempty <<< interpret)
          (runParser code parse)
          `shouldEqual`
            pure expected

  invalidTestWithParser code =
    it code
      $ isLeft
          ( either
              (throwError <<< SyntaxError)
              (runInterpreter mempty <<< interpret)
              (runParser code parse)
          )
          `shouldEqual`
            true
