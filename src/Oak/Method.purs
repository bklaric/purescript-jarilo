module Oak.Method where

import Prelude

import Data.Either (Either(..))
import Data.HTTP.Method (CustomMethod, Method(..))
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..), Variant, inj)

foreign import kind Method

foreign import data Get :: Method

foreign import data Post :: Method

foreign import data Put :: Method

data MethodProxy (method :: Method) = MethodProxy

newtype MethodError = MethodError
    { expectedMethod :: Either CustomMethod Method
    , actualMethod :: Either CustomMethod Method
    }

checkMethod :: forall errors
    .  Either CustomMethod Method
    -> Either CustomMethod Method
    -> Maybe (Variant (methodError :: MethodError | errors))
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
        -> Either CustomMethod Method
        -> Maybe (Variant (methodError :: MethodError | errors))

instance methodRouterGet :: MethodRouter Get where
    methodRouter _ actual = checkMethod (Right GET) actual

instance methodRouterPost :: MethodRouter Post where
    methodRouter _ actual = checkMethod (Right POST) actual

instance methodRouterPut :: MethodRouter Put where
    methodRouter _ actual = checkMethod (Right PUT) actual
