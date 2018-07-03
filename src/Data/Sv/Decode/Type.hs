{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : Data.Sv.Decode.Type
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable
-}

module Data.Sv.Decode.Type (
  Decode (..)
, buildDecode
, DecodeState (..)
, runDecodeState
, Ind (..)
, DecodeError (..)
, DecodeErrors (..)
, DecodeValidation
, Validation (..)
) where

import Control.DeepSeq (NFData)
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT), MonadReader, withReaderT)
import Control.Monad.State (State, runState, state, MonadState)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply (Apply)
import Data.Functor.Bind (Bind ((>>-)))
import Data.Functor.Compose (Compose (Compose))
import Data.List.NonEmpty
import Data.Semigroup (Semigroup)
import Data.Profunctor (Profunctor (lmap, rmap))
import Data.Validation (Validation (Success, Failure))
import Data.Vector (Vector)
import GHC.Generics (Generic)

-- | A @'Decode' e s a@ is for decoding some fields from a CSV row into our type 'a'.
--
-- The second type parameter (@s@) is the input string type
-- (usually 'ByteString' or 'Text').
-- The first type parameter (@e@) is the type of strings which occur in errors.
-- Under most circumstances you want these type paraters to coincide, but they
-- don't have to. They are two separate type parameters instead of one so that
-- 'Decode' can have a 'Data.Profunctor.Profunctor' instance.
--
-- There are primitive 'Decode's, and combinators for composing or
-- otherwise manipulating them. In particular, 'Decode' is an
-- 'Applicative' functor and an 'Alt' from the semigroupoids package.
--
-- 'Decode' is not a 'Monad', but we can perform monad-like operations on
-- it with 'Data.Sv.Decode.>>==' 'Data.Sv.Decode.bindDecode'
newtype Decode s a =
  Decode { unwrapDecode :: Compose (DecodeState s) (DecodeValidation s) a }
  deriving (Functor, Apply, Applicative)

instance Alt (Decode s) where
  Decode (Compose as) <!> Decode (Compose bs) =
    buildDecode $ \v i ->
      case runDecodeState as v i of
        (a, j) -> case runDecodeState bs v i of
          (b, k) ->
            let a' = fmap (,j) a
                b' = fmap (,k) b
            in  case a' <!> b' of
                  Failure e -> (Failure e, k)
                  Success (z, m) -> (Success z, m)

-- | As we decode a row of data, we walk through its fields. This 'Monad'
-- keeps track of our position.
newtype DecodeState s a =
  DecodeState { getDecodeState :: ReaderT (Vector s) (State Ind) a }
  deriving (Functor, Apply, Applicative, Monad, MonadReader (Vector s), MonadState Ind)

instance Bind (DecodeState s) where
  (>>-) = (>>=)

instance Profunctor DecodeState where
  lmap f (DecodeState s) = DecodeState (withReaderT (fmap f) s)
  rmap = fmap

-- | Convenient constructor for 'Decode' that handles all the newtype noise for you.
buildDecode :: (Vector s -> Ind -> (DecodeValidation s a, Ind)) -> Decode s a
buildDecode f = Decode . Compose . DecodeState . ReaderT $ \v -> state $ \i -> f v i

-- | Convenient function to run a DecodeState
runDecodeState :: DecodeState s a -> Vector s -> Ind -> (a, Ind)
runDecodeState = fmap runState . runReaderT . getDecodeState

-- | Newtype for indices into the field vector
newtype Ind = Ind Int

-- | 'DecodeError' is a value indicating what went wrong during a parse or
-- decode. Its constructor indictates the type of error which occured, and
-- there is usually an associated string with more finely-grained details.
data DecodeError e =
  -- | I was looking for another field, but I am at the end of the row
  UnexpectedEndOfRow
  -- | I should be at the end of the row, but I found extra fields
  | ExpectedEndOfRow (Vector e)
  -- | This decoder was built using the 'categorical' primitive for categorical data
  | UnknownCategoricalValue e [[e]]
  -- | The parser failed, meaning decoding proper didn't even begin
  | BadParse e
  -- | Some other kind of decoding failure occured
  | BadDecode e
  deriving (Eq, Ord, Show, Generic)

instance Functor DecodeError where
  fmap f d = case d of
    UnexpectedEndOfRow -> UnexpectedEndOfRow
    ExpectedEndOfRow v -> ExpectedEndOfRow (fmap f v)
    UnknownCategoricalValue e ess -> UnknownCategoricalValue (f e) (fmap (fmap f) ess)
    BadParse e -> BadParse (f e)
    BadDecode e -> BadDecode (f e)

instance NFData e => NFData (DecodeError e)

-- | 'DecodeErrors' is a 'Semigroup' full of 'DecodeError'. It is used as the
-- error side of a 'DecodeValidation'. When multiple errors occur, they will
-- be collected.
newtype DecodeErrors e =
  DecodeErrors (NonEmpty (DecodeError e))
  deriving (Eq, Ord, Show, Semigroup, Generic)

instance Functor DecodeErrors where
  fmap f (DecodeErrors nel) = DecodeErrors (fmap (fmap f) nel)

instance NFData e => NFData (DecodeErrors e)

-- | 'DecodeValidation' is the error-accumulating 'Applicative' underlying
-- 'Decode'
type DecodeValidation s = Validation (DecodeErrors s)
