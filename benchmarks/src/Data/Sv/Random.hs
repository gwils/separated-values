{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Sv.Random (
  Row (..)
  , BenchData (..)
  , rowDec
  , loadOrCreateTestFiles
) where

import Control.Applicative ((<$>), (<*>), (<|>), empty)
import Control.DeepSeq (NFData)
import Control.Lens (makeLenses, makePrisms)
import Control.Monad (replicateM)
import Data.Csv (FromRecord (..), FromField (..), (.!))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Functor.Alt ((<!>))
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Functor.Contravariant.Divisible (choose)
import Data.Semigroup (Semigroup ((<>)))
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Internal.Seed as Seed
import System.Directory (createDirectoryIfMissing, doesFileExist)

import Data.Sv (Decode)
import Data.Sv.Alien.Hedgehog (sample)
import qualified Data.Sv.Decode as D
import qualified Data.Sv.Encode as E

data Product =
  Product { i :: Int, f :: Float, d :: Double }
  deriving (Show, Generic)

instance NFData Product

data Coproduct
  = I Int
  | B ByteString
  | D Double
  deriving (Show, Generic)

instance NFData Coproduct

instance FromField Coproduct where
  parseField a = I <$> parseField a <|> B <$> parseField a <|> D <$> parseField a

data Row = Long LongRow | Short ShortRow deriving (Show, Generic)

instance NFData Row

data LongRow = LongRow {
    _lrB1 :: ByteString
  , _lrB2 :: ByteString
  , _lrI :: Int
  , _lrG :: Integer
  , _lrF :: Float
  , _lrD :: Double
  , _lrP :: Product
  , _lrC :: Coproduct 
  } deriving (Show, Generic)

instance NFData LongRow

instance FromRecord LongRow where
  parseRecord v = case V.length v of
    10 -> LongRow <$> v.!0 <*> v.!1 <*> v.!2 <*> v.!3 <*> v.!4 <*> v.!5 <*> (Product <$> v.!6 <*> v.!7 <*> v.!8) <*> v.!9
    _  -> empty

data ShortRow =
  ShortRow { _1 :: ByteString, _2 ::ByteString, _3 :: ByteString }
  deriving (Show, Generic)

instance NFData ShortRow

instance FromRecord ShortRow where
  parseRecord v = case V.length v of
    3  -> ShortRow <$> v.!0 <*> v.!1 <*> v.!2
    _  -> empty

instance FromRecord Row where
  parseRecord v = Long <$> parseRecord v <|> Short <$> parseRecord v 

makePrisms ''Coproduct

makeLenses ''LongRow

utf8Gen :: Gen ByteString
utf8Gen = fmap BS.pack . replicateM 30 $ Gen.choice
  [ Gen.word8 (Range.constant 65 90)
  , Gen.word8 (Range.constant 97 122)
  , Gen.word8 (Range.constant 48 57)
  ]

{-
utf8Gen :: Gen ByteString
utf8Gen = Gen.utf8 (Range.constantFrom 10 0 100) Gen.alphaNum
-}

numRange :: Num a => Range.Range a
numRange = Range.constantFrom 0 (-100000) 100000

productGen :: Gen Product
productGen = Product <$> Gen.int numRange <*> Gen.float numRange <*> Gen.double numRange

coproductGen :: Gen Coproduct
coproductGen =
  Gen.choice [
    I <$> Gen.int numRange
  , B <$> utf8Gen
  , D <$> Gen.double numRange
  ]

longRowGen :: Gen LongRow
longRowGen =
  LongRow
    <$> utf8Gen
    <*> utf8Gen
    <*> Gen.int numRange
    <*> Gen.integral numRange
    <*> Gen.float numRange
    <*> Gen.double numRange
    <*> productGen
    <*> coproductGen

shortRowGen :: Gen ShortRow
shortRowGen =
  ShortRow <$> utf8Gen <*> utf8Gen <*> utf8Gen

rowGen :: Gen Row
rowGen =
  Gen.frequency [
    (9, Long <$> longRowGen)
  , (1, Short <$> shortRowGen)
  ]

rowEnc :: E.Encode Row
rowEnc = choose (\x -> case x of {Long r -> Left r ; Short r -> Right r}) longRowEnc shortRowEnc

shortRowEnc :: E.Encode ShortRow
shortRowEnc =
  contramap _1 E.byteString <> contramap _2 E.byteString <> contramap _3 E.byteString

productEnc :: E.Encode Product
productEnc =
  contramap i E.int <> contramap f E.float <> contramap d E.double

coproductEnc :: E.Encode Coproduct
coproductEnc =
  E.encodeOf _I  E.int <> E.encodeOf _B E.byteString <> E.encodeOf _D E.double

longRowEnc :: E.Encode LongRow
longRowEnc = mconcat [
    E.encodeOf lrB1 E.byteString
  , E.encodeOf lrB2 E.byteString
  , E.encodeOf lrI E.int
  , E.encodeOf lrG E.integer
  , E.encodeOf lrF E.float
  , E.encodeOf lrD E.double
  , E.encodeOf lrP productEnc
  , E.encodeOf lrC coproductEnc
  ]

rowDec :: Decode ByteString Row
rowDec =  Long <$> longRowDec <!> Short <$> shortRowDec

shortRowDec :: Decode ByteString ShortRow
shortRowDec = ShortRow <$> D.byteString <*> D.byteString <*> D.byteString

productDec :: Decode ByteString Product
productDec = Product <$> D.int <*> D.float <*> D.double

coproductDec :: Decode ByteString Coproduct
coproductDec = I <$> D.int <!> B <$> D.byteString <!> D <$> D.double

longRowDec :: Decode ByteString LongRow
longRowDec = LongRow <$> D.byteString <*> D.byteString <*> D.int <*> D.integer <*> D.float <*> D.double <*> productDec <*> coproductDec

rows :: Int -> Gen [Row]
rows n = replicateM n rowGen

rowsSv :: Int -> Gen (LBS.ByteString)
rowsSv = fmap (E.encode rowEnc E.defaultEncodeOptions) . rows

data BenchData a =
  BenchData {
    f1 :: a
  , f10 :: a
  , f100 :: a
  , f500 :: a
  , f1000 :: a
  , f5000 :: a
  , f10000 :: a
  , f50000 :: a
  , f100000 :: a
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData a => NFData (BenchData a) where

instance Applicative BenchData where
  pure x = BenchData x x x x x x x x x
  BenchData g1 g2 g3 g4 g5 g6 g7 g8 g9
    <*> BenchData a1 a2 a3 a4 a5 a6 a7 a8 a9 =
      BenchData (g1 a1) (g2 a2) (g3 a3) (g4 a4) (g5 a5) (g6 a6) (g7 a7) (g8 a8) (g9 a9)

inds :: BenchData Int
inds = BenchData 1 10 100 500 1000 5000 10000 50000 100000

benchDataGen :: Gen (BenchData LBS.ByteString)
benchDataGen = traverse rowsSv inds

seed :: Seed
seed = Seed.from 42

benchData :: BenchData LBS.ByteString
benchData = sample seed benchDataGen

filenames :: BenchData FilePath
filenames =
  ("benchmarks/data/" <>) <$> BenchData
    "1.csv"
    "10.csv"
    "100.csv"
    "500.csv"
    "1000.csv"
    "5000.csv"
    "10000.csv"
    "50000.csv"
    "100000.csv"

loadOrCreateTestFiles :: IO (BenchData ByteString)
loadOrCreateTestFiles = do
  createDirectoryIfMissing True "benchmarks/data"
  b <- and <$> traverse doesFileExist filenames
  if   b
  then do
    putStrLn "Data files found. Loading..."
    traverse BS.readFile filenames
  else do
    putStrLn "Data files not found. Creating..."
    _ <- sequenceA (LBS.writeFile <$> filenames <*> benchData)
    traverse BS.readFile filenames
