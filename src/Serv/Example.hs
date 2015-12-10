{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Serv.Example where

-- import Serv.Internal.Api
-- import qualified Serv.Internal.Header as Header


-- Types
-- ----------------------------------------------------------------------------

-- data JSON

-- data User = User { userName :: String }

-- newtype LogOutTime = LogOutTime Int




-- Example
-- ----------------------------------------------------------------------------

-- type TheApi
--   = 'OneOf
--     '[ 'Seg "log-out" ':> 'Endpoint LogOutResponses
--      , 'Seg "user"    ':> 'Endpoint UserResponses
--      ]

-- type UserResponses
--   = '[ 'Method 'GET
--        '[ '(Header.ETag, String) ]
--        ('Body '[ JSON ] User)

--      , 'Method 'DELETE '[] 'NoBody
--      ]


-- type LogOutResponses
--   = '[ 'Method
--        'GET
--        '[]
--        ('Body '[ JSON ] User)
--      ]
