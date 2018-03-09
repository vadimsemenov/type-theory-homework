{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           LambdaCalculus

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Control.Monad              (void)
import qualified Data.Text                  as T
import           Data.Void


type Parser = Parsec Void T.Text


parseLambda :: T.Text -> Either (ParseError Char Void) Lambda
parseLambda = parse (expression <* eof) ""

parseLambdaWithSubstitution :: T.Text -> Either (ParseError Char Void) LambdaWithSubstitution
parseLambdaWithSubstitution = parse (substitution <* eof) ""


substitution :: Parser LambdaWithSubstitution
substitution = LambdaWithSubstitution <$> expression
                                      <*> (symbol "[" *> lexeme literal <* assignment)
                                      <*> (expression <* symbol "]")

expression :: Parser Lambda
expression = lexeme expression'

expression' :: Parser Lambda
expression' = do
    space
    as <- atom `sepEndBy1` space1
    return $ foldl1 Application as

atom :: Parser Lambda
atom = parens expression <|> abstraction <|> variable

abstraction :: Parser Lambda
abstraction = Abstraction <$> (lambda *> lexeme literal) <*> (dot *> expression')

variable :: Parser Lambda
variable = Variable <$> literal

literal :: Parser Literal
literal = do
    h <- lowerChar
    t <- many (try digitChar <|> try (char '\'') <|> lowerChar)
    return $ T.pack (h : t)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (char ')')

squaredParens :: Parser a -> Parser a
squaredParens = between (symbol "[") (char ']')

lambda :: Parser ()
lambda = void $ symbol "\\"

dot :: Parser ()
dot = void $ symbol "."

assignment :: Parser ()
assignment = void $ symbol ":="
