module Routing.Method where

import Prelude

import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))

foreign import kind Method

foreign import data Get :: Method

foreign import data Post :: Method

foreign import data Put :: Method

data MethodProxy (method :: Method) = MethodProxy

newtype MethodError = MethodError { expected :: Method, actual :: Method }

checkMethod :: Method -> Method -> Maybe MethodError
checkMethod expected actual =
    if expected == actual
    then Nothing
    else Just $ MethodError
    { expected: expected
    , actual: actual
    }

class MethodRouter (method :: Method) where
    methodRouter :: MethodProxy method -> Method -> Maybe MethodError

instance methodRouterGet :: MethodRouter Get where
    methodRouter _ actual = checkMethod GET actual

instance methodRouterPost :: MethodRouter Post where
    methodRouter _ actual = checkMethod POST actual

instance methodRouterPut :: MethodRouter Put where
    methodRouter _ actual = checkMethod PUT actual
