{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Common types useful for the "kinder" HTTP system.
module Network.HTTP.Kinder.Common where

import           Data.String

-- | 'Raw' is an identity-like newtype wrapper which is used as an
-- indicator that a serialization should return the "raw" underlying
-- values. For instance, an instance of 'HeaderDecode' for @'Raw' 'Text'@
-- would return exactly the text value of the header whereas one for 'Text'
-- would only parse if the values were quoted.
newtype Raw a = Raw { getRaw :: a }
  deriving (Eq, Ord, Read, Show, Semigroup, Monoid, IsString)
