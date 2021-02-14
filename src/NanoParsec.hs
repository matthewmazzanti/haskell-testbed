{-# LANGUAGE BlockArguments, LambdaCase #-}

module NanoParsec where

import Data.Char
import Control.Monad
import Control.Applicative
import Data.Bifunctor

newtype Parser a = Parser { parse :: String -> [(String, a)] }

instance Functor Parser where
  fmap fn (Parser parseFn) = Parser $ fmap (fmap fn) . parseFn

instance Applicative Parser where

runParser :: Parser a -> String -> a
runParser parser cs =
    case parse parser cs of
        [([], res)] -> res
        [(_, _)]    -> error "Did not consume entire stream"
        _           -> error "Parser Error"

item :: Parser Char
item = Parser \case
    []     -> []
    (c:cs) -> [(cs, c)]

