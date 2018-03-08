{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Control.Monad              (void)
import qualified Data.Text                  as T
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           LambdaCalculus


type Parser = Parsec () T.Text

{-
<Expression>  ::= [<Application>] '\' <Variable> '.' <Expression>
                | <Application>
<Application> ::= <Application> <Atom> | <Atom>
<Atom>        ::= '(' <Expression> ')' | <Variable>
<Variable>    ::= ('a'...'z') {'a'...'z'|'0'...'9'|'''}*
-}

parseLambda :: T.Text -> Either (ParseError Char ()) Lambda
parseLambda = parse (expression >>= \res -> eof >> return res) ""

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

lambda :: Parser ()
lambda = void $ symbol "\\"

dot :: Parser ()
dot = void $ symbol "."
