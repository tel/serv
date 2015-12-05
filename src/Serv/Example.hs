
module Serv.Example where






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
--   = '[ 'Method
--        'GET
--        '[ 'ResponseHeader "ETag" S.ByteString ]
--        ('Body '[ 'ContentType JSON 0 ] User)

--      , 'Method 'DELETE '[] 'NoBody
--      ]


-- type LogOutResponses
--   = '[ 'Method
--        'GET
--        '[]
--        ('Body '[ 'ContentType JSON 0 ] User)
--      ]
