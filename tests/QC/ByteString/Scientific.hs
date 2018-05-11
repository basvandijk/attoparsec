{-# LANGUAGE OverloadedStrings #-}

module QC.ByteString.Scientific
    ( tests
    ) where

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)
import Test.QuickCheck (Property, property, (===))
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.ByteString.Lazy as PL
import qualified Data.ByteString.Builder as B
import qualified Data.Scientific as Sci
import qualified Data.ByteString.Builder.Scientific as Sci

sciP :: P.Parser Sci.Scientific
sciP = P.scientific <* P.endOfInput

eqParsePrint :: Integer -> Int -> Property
eqParsePrint c e =
    case PL.eitherResult $ PL.parse sciP lbs of
      Left _err -> property False
      Right s' -> s' === s
  where
    lbs = B.toLazyByteString $ Sci.scientificBuilder s
    s = Sci.scientific c e

tests :: [TestTree]
tests =
    [ testProperty "eqParsePrint" eqParsePrint
    , testCase "normalization" $
        case PL.eitherResult $ PL.parse sciP "10002300.00e4" of
          Left err -> assertFailure err
          Right s -> s @?= Sci.scientific 100023 6
    ]
