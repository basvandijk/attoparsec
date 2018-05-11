{-# LANGUAGE OverloadedStrings #-}

module QC.Text.Scientific
    ( tests
    ) where

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)
import Test.QuickCheck (Property, property, (===))
import qualified Data.Attoparsec.Text as P
import qualified Data.Attoparsec.Text.Lazy as PL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Scientific as Sci
import qualified Data.Text.Lazy.Builder.Scientific as Sci

sciP :: P.Parser Sci.Scientific
sciP = P.scientific <* P.endOfInput

eqParsePrint :: Integer -> Int -> Property
eqParsePrint c e =
    case PL.eitherResult $ PL.parse sciP txt of
      Left _err -> property False
      Right s' -> s' === s
  where
    txt = TLB.toLazyText $ Sci.scientificBuilder s
    s = Sci.scientific c e

tests :: [TestTree]
tests =
    [ testProperty "eqParsePrint" eqParsePrint
    , testCase "normalization" $
        case PL.eitherResult $ PL.parse sciP "10002300.00e4" of
          Left err -> assertFailure err
          Right s -> s @?= Sci.scientific 100023 6
    ]
