module Routing.Method where

import Prelude

import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..), Variant, inj)

foreign import kind Method

foreign import data Get :: Method

foreign import data Post :: Method

foreign import data Put :: Method

data MethodProxy (method :: Method) = MethodProxy

newtype MethodError = MethodError
    { expectedMethod :: Method
    , actualMethod :: Method
    }

checkMethod :: forall errors.
    Method -> Method -> Maybe (Variant (methodError :: MethodError | errors))
checkMethod expected actual =
    if expected == actual
    then Nothing
    else Just $ inj (SProxy :: SProxy "methodError") $ MethodError
    { expectedMethod: expected
    , actualMethod: actual
    }

class MethodRouter (method :: Method) where
    methodRouter
        :: forall errors
        .  MethodProxy method
        -> Method
        -> Maybe (Variant (methodError :: MethodError | errors))

instance methodRouterGet :: MethodRouter Get where
    methodRouter _ actual = checkMethod GET actual

instance methodRouterPost :: MethodRouter Post where
    methodRouter _ actual = checkMethod POST actual

instance methodRouterPut :: MethodRouter Put where
    methodRouter _ actual = checkMethod PUT actual
