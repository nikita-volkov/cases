-- |
-- Convert arbitrary text into spinal, snake, camel, and custom case formats.
--
-- The parser is tolerant to mixed delimiters and punctuation, and it can split
-- words out of mixed-case identifiers like @myHTMLProcessor@.
module Cases
  ( -- * Processor
    process,

    -- ** Case Transformers
    CaseTransformer,
    lower,
    upper,
    title,

    -- ** Delimiters
    Delimiter,
    spinal,
    snake,
    whitespace,
    camel,

    -- * Default Processors
    spinalize,
    snakify,
    camelize,
  )
where

import Cases.Prelude hiding (Word)
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T

-- * Part

-- | A parsed info and a text of a part.
data Part
  = Word Case T.Text
  | Digits T.Text

data Case = Title | Upper | Lower

partToText :: Part -> T.Text
partToText = \case
  Word _ t -> t
  Digits t -> t

-- * Parsers

upperParser :: A.Parser Part
upperParser = Word Upper <$> T.pack <$> A.many1 char
  where
    char = do
      c <- A.satisfy isUpper
      ok <- maybe True (not . isLower) <$> A.peekChar
      if ok
        then return c
        else empty

lowerParser :: A.Parser Part
lowerParser = Word Lower <$> (A.takeWhile1 isLower)

titleParser :: A.Parser Part
titleParser = Word Title <$> (T.cons <$> headChar <*> remainder)
  where
    headChar = A.satisfy isUpper
    remainder = A.takeWhile1 isLower

digitsParser :: A.Parser Part
digitsParser = Digits <$> (A.takeWhile1 isDigit)

partParser :: A.Parser Part
partParser = titleParser <|> upperParser <|> lowerParser <|> digitsParser

-- |
-- A parser, which does in-place processing, using the supplied 'Folder'.
partsParser :: (Monoid r) => Folder r -> A.Parser r
partsParser fold = loop mempty
  where
    loop r =
      (partParser >>= loop . fold r)
        <|> (A.anyChar *> loop r)
        <|> (A.endOfInput *> pure r)

-- * Folders

type Folder r = r -> Part -> r

-- |
-- Strategy for joining parsed parts into the output text.
--
-- A delimiter controls where separators are inserted (for example @-@, @_@,
-- whitespace, or camel-style concatenation).
newtype Delimiter = Delimiter (Maybe T.Text -> Part -> Maybe T.Text)

-- |
-- Join parts with @-@.
--
-- Useful with 'lower' to produce spinal case:
--
-- @
-- process lower spinal "myHTMLProcessor" == "my-html-processor"
-- @
spinal :: Delimiter
spinal =
  Delimiter
    ( (. partToText)
        . fmap Just
        . maybe id (\l r -> l <> "-" <> r)
    )

-- |
-- Join parts with @_@.
--
-- Useful with 'lower' to produce snake case:
--
-- @
-- process lower snake "myHTMLProcessor" == "my_html_processor"
-- @
snake :: Delimiter
snake =
  Delimiter
    ( (. partToText)
        . fmap Just
        . maybe id (\l r -> l <> "_" <> r)
    )

-- |
-- Join parts with spaces.
--
-- @
-- process lower whitespace "myHTMLProcessor" == "my html processor"
-- @
whitespace :: Delimiter
whitespace =
  Delimiter
    ( (. partToText)
        . fmap Just
        . maybe id (\l r -> l <> " " <> r)
    )

-- |
-- Concatenate parts and title-case every part after the first one.
--
-- @
-- process id camel "parse DBM XML" == "parseDbmXml"
-- @
camel :: Delimiter
camel =
  Delimiter
    ( fmap Just
        . maybe partToText (\l r -> l <> partToText (title r))
    )

-- * CaseTransformers

-- |
-- Strategy for transforming the case of each parsed word part.
--
-- Typical transformers are 'lower', 'upper', and 'title'.
type CaseTransformer = Part -> Part

-- |
-- Lower-case every word part.
--
-- @
-- process lower spinal "APIResponse2XX" == "api-response-2-xx"
-- @
lower :: CaseTransformer
lower = \case
  Word c t -> Word Lower t'
    where
      t' = case c of
        Title ->
          T.uncons t |> \case
            Nothing -> t
            Just (h, t) -> T.cons (toLower h) t
        Upper -> T.toLower t
        Lower -> t
  p -> p

-- |
-- Upper-case every word part.
--
-- @
-- process upper snake "myHtmlProcessor" == "MY_HTML_PROCESSOR"
-- @
upper :: CaseTransformer
upper = \case
  Word c t -> Word Upper t'
    where
      t' = case c of
        Title ->
          T.uncons t |> \case
            Nothing -> t
            Just (h, t) -> T.cons h (T.toUpper t)
        Upper -> t
        Lower -> T.toUpper t
  p -> p

-- |
-- Title-case every word part.
--
-- @
-- process title whitespace "myHTMLProcessor" == "My Html Processor"
-- @
title :: CaseTransformer
title = \case
  Word c t -> Word Title t'
    where
      t' = case c of
        Title -> t
        Upper ->
          T.uncons t |> \case
            Nothing -> t
            Just (h, t) -> T.cons (toUpper h) (T.toLower t)
        Lower ->
          T.uncons t |> \case
            Nothing -> t
            Just (h, t) -> T.cons (toUpper h) t
  p -> p

-- * API

-- |
-- Extract separate words from an arbitrary text using a smart parser and
-- produce a new text using case transformation and delimiter functions.
--
-- Note: to skip case transformation use the 'id' function.
--
-- Examples:
--
-- @
-- process lower spinal "abcDEF" == "abc-def"
-- process id camel "parse DBM XML" == "parseDbmXml"
-- process title whitespace "abc_def" == "Abc Def"
-- @
process :: CaseTransformer -> Delimiter -> T.Text -> T.Text
process tr (Delimiter fo) =
  fromMaybe ""
    . either (error . ("Parse failure: " <>)) id
    . A.parseOnly (partsParser $ (. tr) . fo)

-- |
-- Transform an arbitrary text into a lower spinal case.
--
-- Same as @('process' 'lower' 'spinal')@.
--
-- @
-- spinalize "myHTMLProcessor" == "my-html-processor"
-- spinalize " abc_-:,/def " == "abc-def"
-- @
spinalize :: T.Text -> T.Text
spinalize = process lower spinal

-- |
-- Transform an arbitrary text into a lower snake case.
--
-- Same as @('process' 'lower' 'snake')@.
--
-- @
-- snakify "Ёжик лижет мёд." == "ёжик_лижет_мёд"
-- @
snakify :: T.Text -> T.Text
snakify = process lower snake

-- |
-- Transform an arbitrary text into a camel case,
-- while preserving the case of the first character.
--
-- Same as @('process' 'id' 'camel')@.
--
-- @
-- camelize "abc-def" == "abcDef"
-- camelize "Abc-def" == "AbcDef"
-- camelize "parse DBM XML" == "parseDbmXml"
-- @
camelize :: T.Text -> T.Text
camelize = process id camel
