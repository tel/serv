{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Serv.Example where

import Serv.Internal.Api
import Serv.Internal.Response


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
       ('Body '[ 'As JSON ] User)

     , 'Method 'DELETE '[] 'NoBody
     ]


type LogOutResponses
  = '[ 'Method
       'GET
       '[]
       ('Body '[ 'As JSON ] User)
     ]
