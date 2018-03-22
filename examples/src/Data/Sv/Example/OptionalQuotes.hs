{-# LANGUAGE OverloadedStrings #-}

module Data.Sv.Example.OptionalQuotes where

import Control.Lens
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import System.Exit (exitFailure)

import Data.Sv
import qualified Data.Sv.Decode as D
import qualified Data.Sv.Encode as E
import Text.Escape (Unescaped (Unescaped))
import Text.Space (spacedValue)
import Text.Newline (Newline (LF))

data Two =
  Two (Maybe ByteString) (Maybe ByteString)
  deriving (Eq, Ord, Show)

twoDecode :: Decode' ByteString Two
twoDecode = Two <$> bs <*> bs
  where
    bs =
      flip fmap D.raw $ \sf ->
        case sf ^. spacedValue of
          Unquoted s -> if BS.null s then Nothing else Just s
          Quoted _ (Unescaped v) -> Just v

twoEncode :: Encode Two
twoEncode = divide (\ (Two x y) -> (x,y)) bsEncode bsEncode

bsEncode :: Encode (Maybe ByteString)
bsEncode =
  Encode $ \o mbs ->
    case mbs of
      Nothing -> pure mempty
      Just x -> E.getEncode E.byteString o x

subjectToDecode :: ByteString
subjectToDecode =
  BS.intercalate "\n"
    [ "\"hello\",\"goodbye\"" -- quoted
    , "yes,no" -- unquoted
    , "," -- blank
    , "\"\",\"\"" -- empty quotes
    ]

testValues :: [Two]
testValues =
  [ Two (Just "hello") (Just "goodbye")
  , Two (Just "yes") (Just "no")
  , Two Nothing Nothing
  , Two (Just "") (Just "")
  ]

expectedEncoding :: ByteString
expectedEncoding =
  BS.intercalate "\n"
    [ "\"hello\",\"goodbye\""
    , "\"yes\",\"no\""
    , ","
    , "\"\",\"\""
    ]

parseOpts :: ParseOptions ByteString
parseOpts = defaultParseOptions & headedness .~ Unheaded

encOpts :: EncodeOptions
encOpts = defaultEncodeOptions & newline .~ LF

main :: IO ()
main = do
  let decResult = parseDecode twoDecode parseOpts subjectToDecode
  print decResult
  unless (decResult == pure testValues) exitFailure
  let encResult = LBS.toStrict $ encode twoEncode encOpts testValues
  print encResult
  unless (encResult == expectedEncoding) exitFailure
