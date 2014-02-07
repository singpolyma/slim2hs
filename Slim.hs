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

data Slim = Tag [String] [SlimAttr] [Slim] | SelfCloseTag [String] [SlimAttr] | Code SlimOutput String [Slim] | Text String | Comment Bool String
	deriving (Show)

data SlimAttr = Attr String (Either String String) -- left string, right code, for now
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
	textBlock (\s -> Text (s ++ " ")) "'" <|>
	textBlock comment "/" <|>
	withBlock ($) (code <* lineSepSpace True) (parser config) <|>
	try (selfCloseTag config) <|>
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

selfCloseTag config = do
	(ts, as) <- (,) <$> tagAndShortcuts config <*> attr config
	inlineSpaces
	string "/"
	lineSepSpace True
	return $ SelfCloseTag ts as

tag config = (\ts a c cs -> Tag ts a (maybe cs (:cs) c)) <$>
	tagAndShortcuts config <*> attr config <*> inlineContent

tagAndShortcuts (SlimConfig {shortcuts = s}) = do
	(:) <$> (some alphaNum <|> shortcut) <*> many shortcut
	where
	shortcut = (:) <$> oneOf s <*> some alphaNum

inlineContent = do
	inlineSpaces
	output <- optional (slimOutputSigil <$> (try (string "==") <|> string "="))
	inlineSpaces
	case output of
		Just typ -> (\x -> Just $ Code typ x []) <$> some (noneOf "\n")
		Nothing -> optional (Text <$> some (noneOf "\n"))

attr (SlimConfig {attributeWrap = wrap}) = (inlineSpaces *>) $ withPos $ foldr (<|>) (many $ try $ attr' False) $
	map (\(o,c) -> pure id <-/> string [o] <-/> inlineSpaces <*/> attr' True <-/> string [c]) wrap

attr' allowAlone = do
	key <- some alphaNum
	val <- if allowAlone then optional valParser else fmap Just valParser
	inlineSpaces
	return $ Attr key (fromMaybe (Left key) val)
	where
	valParser = inlineSpaces *> string "=" *> inlineSpaces *> (
			fmap Left (quotedString "\"" "\"" <|> quotedString "'" "'") <|>
			fmap Right (quotedString "(" ")" <|> many1 alphaNum)
		)

quotedString o c = concat <$> (string o *> many (some (noneOf (o++c)) <|> nested) <* string c)
	where
	nested
		| o == c = empty
		| otherwise = (\s -> o ++ s ++ c) <$> quotedString o c

code = (\sigil txt -> Code (slimOutputSigil sigil) txt) <$>
	(string "-" <|> try (string "==") <|> string "=") <*>
	(inlineSpaces *> some (noneOf "\n"))

slimOutputSigil "-" = NoOutput
slimOutputSigil "=" = EscapedOutput
slimOutputSigil "==" = UnescapedOutput
