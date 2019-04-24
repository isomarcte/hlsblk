module Data.BlockDevice.Internal.Parsers
  ( parseIntegralStringAsAeson
  , parseColonSeparatedText
  , parseIntegralWithSuffix
  , parseNonEmptyColonSeparatedIntegrals
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad.Fail (fail)
import Data.Either (either)
import Data.Eq (Eq(..))
import Data.Function (($))
import Data.Maybe (maybe)
import Data.Scientific (Scientific, toBoundedInteger)
import Data.Semigroup ((<>))
import Data.Traversable (Traversable(..))
import Prelude (Bounded, Integral)
import Text.Show (Show(..))

import qualified Data.Aeson as DA
import qualified Data.Aeson.Types as AesonT
import qualified Data.Attoparsec.Text as DAT
import qualified Data.Text as DT

parseIntegralStringAsAeson ::
     (Integral a, Bounded a) => DA.Value -> AesonT.Parser a
parseIntegralStringAsAeson (DA.String t) =
  either fail pure $ DAT.parseOnly parser t
  where
    parser :: (Integral b, Bounded b) => DAT.Parser b
    parser = do
      s <- DAT.scientific
      DAT.endOfInput
      maybe
        (fail $ "Value is not a bounded integer: " <> show s)
        pure
        (toBoundedInteger s)
parseIntegralStringAsAeson other = AesonT.typeMismatch "String" other

parseColonSeparatedText :: DAT.Parser [DT.Text]
parseColonSeparatedText =
  DAT.sepBy (DAT.takeWhile (/= ':')) (DAT.string ":") <* DAT.endOfInput

parseIntegralWithSuffix :: (Integral a, Bounded a) => DT.Text -> DAT.Parser a
parseIntegralWithSuffix suffix = do
  s <- DAT.scientific
  _ <- DAT.string suffix
  DAT.endOfInput
  maybe
    (fail $ "Value is not a bounded integer: " <> show s)
    pure
    (toBoundedInteger s)

parseNonEmptyColonSeparatedIntegrals ::
     (Integral a, Bounded a) => DAT.Parser [a]
parseNonEmptyColonSeparatedIntegrals = do
  is <- DAT.sepBy1 DAT.scientific (DAT.string ":")
  DAT.endOfInput
  traverse toBoundedInteger' is
  where
    toBoundedInteger' :: (Integral i, Bounded i) => Scientific -> DAT.Parser i
    toBoundedInteger' i =
      let error = (fail $ "JSON is not a valid integral value: " <> show i)
          i' = toBoundedInteger i
       in maybe error pure i'
