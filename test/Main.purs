module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.SubLisp.Interpreter (testInterpret)
import Test.SubLisp.Parser (testParse)

main :: Effect Unit
main =
  launchAff_ $ runSpec [ consoleReporter ]
    $ do
        testParse
        testInterpret
