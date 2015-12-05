{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Serv.Example where

import Serv.Internal.Api
import Serv.Internal.Qualifier
import Serv.Internal.Response
import Serv.Internal.ContentType





-- Types
-- ----------------------------------------------------------------------------

data JSON

data User = User { userName :: String }

newtype LogOutTime = LogOutTime Int




-- Example
-- ----------------------------------------------------------------------------

type TheApi
  = 'OneOf
    '[ 'Seg "log-out" ':> 'Endpoint LogOutResponses
     , 'Seg "user"    ':> 'Endpoint UserResponses
     ]

type UserResponses
  = '[ 'Method
       'GET
       '[ 'ResponseHeader "ETag" String ]
       ('Body '[ 'ContentType JSON 0 ] User)

     , 'Method 'DELETE '[] 'NoBody
     ]


type LogOutResponses
  = '[ 'Method
       'GET
       '[]
       ('Body '[ 'ContentType JSON 0 ] User)
     ]
