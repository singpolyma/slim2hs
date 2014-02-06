{-# LANGUAGE NoMonomorphismRestriction #-}
module Slim where

import Data.Char
import Control.Applicative
import Text.Parsec hiding (optional, many, (<|>))
import Text.Parsec.Indent

data SlimOutput = NoOutput | EscapedOutput | UnescapedOutput
	deriving (Show)

data Slim = Tag String [Slim] | Code SlimOutput String [Slim] | Text String | Comment Bool String
	deriving (Show)

run sourceName p source = runIndent sourceName (runParserT (many1 p <* eof) () sourceName source)

parser =
	textBlock Text "|" <|>
	textBlock comment "/" <|>
	withBlock ($) (code <* lineSepSpace True) parser <|>
	withBlock ($) (tag <* lineSepSpace True) parser <|>
	(lineSepSpace True *> pure (Comment False "")) -- HACK

comment ('!':txt) = Comment True (dropWhile isSpace txt)
comment txt = Comment False txt

-- does folding
textBlock cons sigil = (string sigil *> inlineSpaces *> pure (cons . concat)) <*/> textBlockLine
textBlockLine = (++) <$> many1 (noneOf "\n") <*> (maybe "" (const $ " ") . snd <$> lineSepSpace True)

lineSepSpace reqNewline = inlineSpaces *> ((,) <$> nl <*> optional inlineSpace) <* inlineSpaces
	where
	nl | reqNewline = fmap Just newline
	   | otherwise = optional newline

inlineSpaces = skipMany (satisfy isInlineSpace) <?> "inline white space"
inlineSpace = satisfy isInlineSpace <?> "inline white space character"
isInlineSpace c = c /= '\n' && isSpace c

tag = (\x y -> Tag $ maybe y (:y) x) <$> optional (oneOf "#.") <*> many1 alphaNum

code = (\sigil txt -> Code (slimOutputSigil sigil) txt) <$>
	(string "-" <|> try (string "==") <|> string "=") <*>
	(inlineSpaces *> many1 (noneOf "\n"))

slimOutputSigil "-" = NoOutput
slimOutputSigil "=" = EscapedOutput
slimOutputSigil "==" = UnescapedOutput
