{-# LANGUAGE NoMonomorphismRestriction #-}
module Slim where

import Data.Char
import Data.Maybe
import Control.Applicative
import Control.Monad
import Text.Parsec hiding (optional, many, (<|>))
import Text.Parsec.Indent
import Control.Monad.Trans.Reader

data SlimOutput = NoOutput | EscapedOutput | UnescapedOutput
	deriving (Show)

data Slim = Tag String [SlimAttr] [Slim] | Code SlimOutput String [Slim] | Text String | Comment Bool String
	deriving (Show)

data SlimAttr = Attr String String
	deriving (Show)

data SlimConfig = SlimConfig {
		shortcuts :: [Char],
		attributeWrap :: [(Char,Char)]
	}

run sourceName p source = runIndent sourceName (runParserT (some p <* eof) () sourceName source)

-- This forces unindents to be sane
parser config = checkIndent >> parser' config

parser' config =
	textBlock Text "|" <|>
	textBlock comment "/" <|>
	withBlock ($) (code <* lineSepSpace True) (parser config) <|>
	withBlock ($) (tag config <* lineSepSpace True) (parser config) <|>
	(lineSepSpace True *> pure (Comment False "")) -- HACK

comment ('!':txt) = Comment True (dropWhile isSpace txt)
comment txt = Comment False txt

-- does folding
textBlock cons sigil = withPos $ (string sigil *> inlineSpaces *> pure (cons . concat)) <*/> textBlockLine
textBlockLine = (++) <$> some (noneOf "\n") <*> (maybe "" (const $ " ") . snd <$> lineSepSpace True)

lineSepSpace reqNewline = inlineSpaces *> ((,) <$> nl <*> optional inlineSpace) <* inlineSpaces
	where
	nl | reqNewline = fmap Just newline
	   | otherwise = optional newline

inlineSpaces = skipMany (satisfy isInlineSpace) <?> "inline white space"
inlineSpace = satisfy isInlineSpace <?> "inline white space character"
isInlineSpace c = c /= '\n' && isSpace c

tag config@(SlimConfig {shortcuts = s}) =
	(\x y c -> Tag (maybe y (:y) x) c) <$> optional (oneOf s) <*> some alphaNum <*> attr config

attr (SlimConfig {attributeWrap = wrap}) = (inlineSpaces *>) $ withPos $ foldr (<|>) (many $ attr' False) $
	map (\(o,c) -> pure id <-/> string [o] <-/> inlineSpaces <*/> attr' True <-/> string [c]) wrap

attr' allowAlone = do
	key <- some alphaNum
	val <- if allowAlone then optional valParser else fmap Just valParser
	inlineSpaces
	return $ Attr key (fromMaybe key val)
	where
	valParser = inlineSpaces *> string "=" *> inlineSpaces *>
		(quotedString "\"" <|> quotedString "'")

quotedString q = string q *> many (noneOf q) <* string q

code = (\sigil txt -> Code (slimOutputSigil sigil) txt) <$>
	(string "-" <|> try (string "==") <|> string "=") <*>
	(inlineSpaces *> some (noneOf "\n"))

slimOutputSigil "-" = NoOutput
slimOutputSigil "=" = EscapedOutput
slimOutputSigil "==" = UnescapedOutput
