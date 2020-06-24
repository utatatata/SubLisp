module Test.SubLisp.Parser where

import Prelude
import Data.Either (isLeft)
import Data.List (fromFoldable)
import SubLisp.Const (Const(..))
import SubLisp.Parser (parse)
import SubLisp.Term (Term(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.Parser (runParser)

testParse :: Spec Unit
testParse =
  describe "SubLisp.Parser" do
    describe "parse" do
      describe "valid tests" do
        validTest "x" $ TmVar "x"
        validTest "  x" $ TmVar "x"
        validTest "x  " $ TmVar "x"
        validTest "t" $ TmConst CT
        validTest "nil" $ TmConst CNil
        validTest "'x" $ TmQuoted (TmVar "x")
        validTest "'(a b c)" $ TmQuoted (TmPair (TmVar "a") $ fromFoldable [ TmVar "b", TmVar "c" ])
        validTest "  (  a  b  c  )  " $ TmPair (TmVar "a") $ fromFoldable [ TmVar "b", TmVar "c" ]
        validTest "(  a  b  (  c  d  )  )  " $ TmPair (TmVar "a") $ fromFoldable [ TmVar "b", TmPair (TmVar "c") $ fromFoldable [ TmVar "d" ] ]
        validTest "()" $ TmConst CNil
        validTest "  (   )  " $ TmConst CNil
        validTest "t" $ TmConst CT
        validTest "nil" $ TmConst CNil
        validTest "if" $ TmConst CIf
        validTest "quote" $ TmConst CQuote
        validTest "fun" $ TmConst CFun
        validTest "atom?" $ TmConst CAtom
        validTest "eq" $ TmConst CEq
        validTest "head" $ TmConst CHead
        validTest "tail" $ TmConst CTail
        validTest "cons" $ TmConst CCons
        validTest "(  a  b  (  c  d  )  e  )  " $ TmPair (TmVar "a") $ fromFoldable [ TmVar "b", TmPair (TmVar "c") $ fromFoldable [ TmVar "d" ], TmVar "e" ]
        validTest "(  (  )  )" $ TmPair (TmConst CNil) $ fromFoldable []
        validTest "(  (  )  a  )" $ TmPair (TmConst CNil) $ fromFoldable [ TmVar "a" ]
        validTest "(  (  )a  )" $ TmPair (TmConst CNil) $ fromFoldable [ TmVar "a" ]
        validTest "(  (  ) a)" $ TmPair (TmConst CNil) $ fromFoldable [ TmVar "a" ]
        validTest "(  (  )a)" $ TmPair (TmConst CNil) $ fromFoldable [ TmVar "a" ]
        validTest "('a'b'c)" $ TmPair (TmQuoted (TmVar "a")) $ fromFoldable [ TmQuoted (TmVar "b"), TmQuoted (TmVar "c") ]
      describe "invalid tests" do
        invalidTest "'   x  "
  where
  validTest code expected =
    it code do
      runParser code parse `shouldEqual` pure expected

  invalidTest code =
    it code do
      isLeft (runParser code parse) `shouldEqual` true
