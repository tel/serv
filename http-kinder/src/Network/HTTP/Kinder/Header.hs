
-- | Re-exports generally useful values from
-- "Network.HTTP.Kinder.Header.Definitions" and
-- "Network.HTTP.Kinder.Header.Serialization".
--
-- HTTP headers handled at the type level. This module provides types and
-- kinds (with @DataKinds@ enabled) to represent most headers as types and
-- serialize or desrialize header values accordingly.
--
-- For instance, these methods describe that we can interpret values at the
-- 'Network.HTTP.Kinder.Header.Definitions.ContentType' header as a set of
-- media types or at
-- 'Network.HTTP.Kinder.Header.Definitions.AccessControlMaxAge' as
-- a 'Data.Time.NominalDiffTime'. See the instances for
-- 'Network.HTTP.Kinder.Header.Serialization.HeaderEncode' and
-- 'Network.HTTP.Kinder.Header.Serialization.HeaderDecode' for more detail.
module Network.HTTP.Kinder.Header (

    module Network.HTTP.Kinder.Header.Definitions
  , module Network.HTTP.Kinder.Header.Serialization

) where

import qualified Network.HTTP.Kinder.Header.Definitions
import qualified Network.HTTP.Kinder.Header.Serialization (HeaderDecode (..),
                                                           HeaderEncode (..),
                                                           headerDecodeBS,
                                                           headerEncodeBS,
                                                           headerEncodePair)

