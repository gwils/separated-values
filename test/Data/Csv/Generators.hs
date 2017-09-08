module Data.Csv.Generators (
  genCsv
  , genNewline
  , genSep
  , genFinalRecord
  , genBetween
  , genEscaped
  , genQuote
  , genField
  , genRecord
  , genRecords
  , genNonEmptyRecord
  , genPesarated
  , genSeparated
) where

import Control.Applicative ((<$>), liftA2, liftA3)
import Data.Separated      (Pesarated (Pesarated), Separated (Separated))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Csv.Csv        (Csv (Csv))
import Data.Csv.Field      (Field (QuotedF, UnquotedF), downmix)
import Data.Csv.Record     (Record (Record), NonEmptyRecord (SingleFieldNER, MultiFieldNER), FinalRecord (FinalRecord), Records (Records))
import Text.Between        (Between (Between))
import Text.Escaped        (Escaped (SeparatedByEscapes))
import Text.Newline        (Newline (CRLF, LF))
import Text.Space          (Spaces, Spaced)
import Text.Quote          (Quote (SingleQuote, DoubleQuote), Quoted (Quoted))

genCsv :: Gen Char -> Gen Spaces -> Gen s1 -> Gen s2 -> Gen (Csv s1 s2)
genCsv sep spc s1 s2 =
  let rs = genRecords spc s2
      e  = genFinalRecord spc s1 s2
  in  liftA3 Csv sep rs e

genNewline :: Gen Newline
genNewline =
  -- TODO put CR back in
  Gen.element [CRLF, LF]

genSep :: Gen Char
genSep =
  Gen.element ['|', ',']

genFinalRecord :: Gen Spaces -> Gen s1 -> Gen s2 -> Gen (FinalRecord s1 s2)
genFinalRecord spc s1 s2 =
  FinalRecord <$> Gen.maybe (genNonEmptyRecord spc s1 s2)

genBetween :: Gen Spaces -> Gen str -> Gen (Spaced str)
genBetween spc str =
  liftA3 Between spc str spc

genQuote :: Gen Quote
genQuote =
  Gen.element [SingleQuote, DoubleQuote]

genEscaped :: Gen a -> Gen (Escaped a)
genEscaped a =
  SeparatedByEscapes <$> Gen.nonEmpty (Range.linear 1 5) a

genQuoted :: Gen a -> Gen (Quoted a)
genQuoted =
  liftA2 Quoted genQuote . genEscaped

genField :: Gen Spaces -> Gen s1 -> Gen s2 -> Gen (Field s1 s2)
genField spc s1 s2 =
  Gen.choice [
    UnquotedF <$> s1
  , QuotedF <$> genBetween spc (genQuoted s2)
  ]

genRecord :: Gen Spaces -> Gen s -> Gen (Record s)
genRecord spc s =
  Record <$> Gen.nonEmpty (Range.linear 1 10) (downmix <$> genField spc s s)

genNonEmptyRecord :: Gen Spaces -> Gen s1 -> Gen s2 -> Gen (NonEmptyRecord s1 s2)
genNonEmptyRecord spc s1 s2 =
  let f  = genField spc s1 s2
      f' = downmix <$> genField spc s2 s2
  in  Gen.choice [
    SingleFieldNER <$> f
  , MultiFieldNER <$> f' <*> Gen.nonEmpty (Range.linear 1 10) f'
  ]

genRecords :: Gen Spaces -> Gen str -> Gen (Records str)
genRecords spc str =
  Records <$> genPesarated spc str

genPesarated :: Gen Spaces -> Gen s -> Gen (Pesarated Newline (Record s))
genPesarated spc s =
  let r = genRecord spc s
  in  Pesarated <$> genSeparated r genNewline

genSeparated :: Gen a -> Gen b -> Gen (Separated a b)
genSeparated a b =
  Separated <$> Gen.list (Range.linear 0 100) (liftA2 (,) a b)

