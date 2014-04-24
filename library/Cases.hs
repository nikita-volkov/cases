module Cases
(
  -- * Processor
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
  camel,
  -- * Default Processors
  spinalize,
  snakify,
  camelize,
)
where

import Cases.Prelude hiding (Word)
import qualified Data.Attoparsec.Text.Lazy as A
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Char as C


-- * Part
-------------------------

-- | A parsed info and a text of a part.
data Part = 
  Word Case TL.Text |
  Digits TL.Text

data Case = Title | Upper | Lower

partToLazyText :: Part -> TL.Text
partToLazyText = \case
  Word _ t -> t
  Digits t -> t


-- * Parsers
-------------------------

upperParser :: A.Parser Part
upperParser = Word Upper <$> TL.pack <$> A.many1 char where
  char = do
    c <- A.satisfy C.isUpper
    ok <- maybe True (not . C.isLower) <$> A.peekChar
    if ok
      then return c
      else empty

lowerParser :: A.Parser Part
lowerParser = Word Lower <$> (TL.fromStrict <$> A.takeWhile1 C.isLower)

titleParser :: A.Parser Part
titleParser = Word Title <$> (TL.cons <$> headChar <*> remainder) where
  headChar = A.satisfy C.isUpper
  remainder = TL.fromStrict <$> A.takeWhile1 C.isLower

digitsParser :: A.Parser Part
digitsParser = Digits <$> (TL.fromStrict <$> A.takeWhile1 C.isDigit)

partParser :: A.Parser Part
partParser = titleParser <|> upperParser <|> lowerParser <|> digitsParser

-- |
-- A parser, which does in-place processing, using the supplied 'Folder'.
partsParser :: Monoid r => Folder r -> A.Parser r
partsParser fold = loop mempty where
  loop r = 
    (partParser >>= loop . fold r) <|> 
    (A.anyChar *> loop r) <|>
    (A.endOfInput *> pure r)


-- * Folders
-------------------------

type Folder r = r -> Part -> r

type Delimiter = Folder (Maybe TL.Text)

spinal :: Delimiter
spinal = 
  (. partToLazyText) . 
  fmap Just . 
  maybe id (\l r -> l <> "-" <> r)

snake :: Delimiter
snake = 
  (. partToLazyText) . 
  fmap Just . 
  maybe id (\l r -> l <> "_" <> r)

camel :: Delimiter
camel = 
  fmap Just .
  maybe partToLazyText (\l r -> l <> partToLazyText (title r))


-- * CaseTransformers
-------------------------

type CaseTransformer = Part -> Part

lower :: CaseTransformer
lower = \case
  Word c t -> Word Lower t' where
    t' = case c of
      Title -> TL.uncons t |> \case
        Nothing -> t
        Just (h, t) -> TL.cons (C.toLower h) t
      Upper -> TL.toLower t
      Lower -> t
  p -> p

upper :: CaseTransformer
upper = \case
  Word c t -> Word Upper t' where
    t' = case c of
      Title -> TL.uncons t |> \case
        Nothing -> t
        Just (h, t) -> TL.cons h (TL.toUpper t)
      Upper -> t
      Lower -> TL.toUpper t
  p -> p

title :: CaseTransformer
title = \case
  Word c t -> Word Title t' where
    t' = case c of
      Title -> t
      Upper -> TL.uncons t |> \case
        Nothing -> t  
        Just (h, t) -> TL.cons (C.toUpper h) (TL.toLower t)
      Lower -> TL.uncons t |> \case
        Nothing -> t
        Just (h, t) -> TL.cons (C.toUpper h) t
  p -> p


-- * API
-------------------------

-- |
-- Extract separate words from an arbitrary text using a smart parser and
-- produce a new text using case transformation and delimiter functions.
-- 
-- Note: to skip case transformation use the 'id' function.
process :: CaseTransformer -> Delimiter -> TL.Text -> TL.Text
process tr fo = 
  fromMaybe "" .
  either ($bug . ("Parse failure: " <>)) id .
  A.eitherResult . A.parse (partsParser $ (. tr) . fo)

-- |
-- Transform an arbitrary text into a lower spinal case.
-- 
-- Same as @('process' 'lower' 'spinal')@.
spinalize :: TL.Text -> TL.Text
spinalize = process lower spinal

-- |
-- Transform an arbitrary text into a lower snake case.
-- 
-- Same as @('process' 'lower' 'snake')@.
snakify :: TL.Text -> TL.Text
snakify = process lower snake

-- |
-- Transform an arbitrary text into a camel case, 
-- while preserving the case of the first character.
-- 
-- Same as @('process' 'id' 'camel')@.
camelize :: TL.Text -> TL.Text
camelize = process id camel

