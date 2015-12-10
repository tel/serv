{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Serv.Internal.RawText where

import           Data.Text   (Text)

-- | RawText extracts as, like the name suggests,
-- raw text from URI segments and header values.
--
-- It exists as a default value for extensibility of typeclasses like
-- HeaderDecode and HeaderEncode

newtype RawText =
  RawText { getRawText :: Text }
  deriving (Eq, Ord, Read, Show, Monoid)
