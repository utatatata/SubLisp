module SubLisp.Parser where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array as A
import Data.List (List(..))
import Data.Maybe (maybe)
import Data.String.CodeUnits (fromCharArray)
import SubLisp.Const (Const(..))
import SubLisp.Term (Term(..))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (between, optionMaybe, sepEndBy)
import Text.Parsing.Parser.String (char, eof, noneOf, satisfy, skipSpaces, string, whiteSpace)

parse :: Parser String Term
parse = fix \term' -> skipSpaces *> term <* skipSpaces <* eof
  where
  trim p = skipSpaces *> p <* skipSpaces

  lparen = trim $ char '('

  rparen = trim $ char ')'

  const =
    TmConst
      <$> ( (CNil <$ string "nil")
            <|> ( CIf
                  <$ string "if"
              )
            <|> ( CQuote
                  <$ string "quote"
              )
            <|> ( CFun
                  <$ string "fun"
              )
            <|> ( CDef
                  <$ string "def"
              )
            <|> ( CAtom
                  <$ string "atom?"
              )
            <|> ( CEq
                  <$ string "eq"
              )
            <|> ( CHead
                  <$ string "head"
              )
            <|> ( CTail
                  <$ string "tail"
              )
            <|> ( CCons
                  <$ string "cons"
              )
            <|> ( CT
                  -- `t` must be behind `tail`
                  
                  <$ string "t"
              )
        )

  quoted term' = char '\'' *> (TmQuoted <$> term')

  var = TmVar <$> fromCharArray <$> (A.some $ noneOf [ ' ', '(', ')', '\'', '"' ])

  pair term' =
    between lparen rparen (term' `sepEndBy` whiteSpace)
      >>= case _ of
          Nil -> pure $ TmConst CNil
          Cons h t -> pure $ TmPair h t

  term = fix \term' -> const <|> (quoted term') <|> var <|> (pair term')
