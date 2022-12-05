module SantaLib.Parsing
  ( module Text.Megaparsec,
    module Text.Megaparsec.Char,
    L.decimal,
    Parser,
    space,
    lexeme,
    lexemeLn,
    lexemeSp,
    symbol,
    symbolLn,
    symbolSp,
    filterNums,
  )
where

import Control.Monad (void)
import Data.Char (isNumber)
import Data.Function (on)
import Data.List (groupBy)
import Data.Void (Void)
import Text.Megaparsec hiding (getInput)
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

-- | My default space consumer, accepts only horizontal spaces, and uses // for
-- line comments, /* */ define a block comment
space :: Parser ()
space = L.space hspace1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

-- |  Parse lexeme and (horizontal) white space following it
lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

-- | Parse lexeme and consume following eol
lexemeLn :: Parser a -> Parser a
lexemeLn = L.lexeme (void eol)

-- | Parse lexeme and one or zero following ' '
lexemeSp :: Parser a -> Parser a
lexemeSp = L.lexeme (void . optional . char $ ' ')

-- | Parse symbol and (horizontal) white space following it
symbol :: String -> Parser String
symbol = L.symbol space

-- | Parse symbol and consume following eol
symbolLn :: String -> Parser String
symbolLn = L.symbol (void eol)

-- | Parse symbol and one or zero following ' '
symbolSp :: String -> Parser String
symbolSp = L.symbol (void . optional . char $ ' ')

filterNums :: String -> [Int]
filterNums = map read . filter (all isNumber) . groupBy ((==) `on` isNumber)
