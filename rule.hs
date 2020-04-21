{-# LANGUAGE FlexibleContexts #-}

module Rule where

import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding ((<|>))

type Selector = String
data Rule = Rule String String deriving Show
data Ruleset = Ruleset Selector [Rule] deriving Show

paddedChar c = char c <* spaces

rule :: Parser Rule
rule = do
  p <- many1 letter <* paddedChar ':'
  v <- many1 (noneOf ";") <* paddedChar ';'

  return $ Rule p v

ruleset :: Parser Ruleset
ruleset = do
    s <- selector `sepBy1` spaces
    r <- paddedChar '{' *> many1 rule <* paddedChar '}'

    return $ Ruleset (unwords s) r

selector :: Parser String
selector = many1 (oneOf ".#" <|> letter <|> digit) <* spaces
