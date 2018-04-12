{-# LANGUAGE OverloadedStrings #-}

module UnificationParser
       ( Parser
       , parseEquation
       ) where

import           Unification

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Control.Monad              (void)
import qualified Data.Text                  as T
import           Data.Void


type Parser = Parsec Void T.Text


parseEquation :: T.Text -> Either (ParseError Char Void) Equation
parseEquation = parse (equation <* eof) ""

equation :: Parser Equation
equation = do
  lhs <- term
  _   <- eq
  rhs <- term
  return $ lhs :=: rhs

term :: Parser Term
term = function <|> variable

function :: Parser Term
function = lexeme $ do
    head <- satisfy (\c -> 'a' <= c && c <= 'h')
    tail <- literalTail
    let functionName = T.cons head tail
    args <- parens $ sepBy1 term comma
    return $ Function functionName args

variable :: Parser Term
variable = lexeme $ do
    head <- satisfy (\c -> 'i' <= c && c <= 'z')
    tail <- literalTail
    return $ Variable $ T.cons head tail

literalTail :: Parser T.Text
literalTail = T.pack <$> many (try lowerChar <|> try digitChar <|> try (char '\''))

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (char ')')

comma :: Parser ()
comma = void $ symbol ","

eq :: Parser ()
eq = void $ symbol "="
