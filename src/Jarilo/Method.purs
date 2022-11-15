module Jarilo.Method where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (CustomMethod, Method(..)) as HM
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Variant (Variant, inj)
import Type.Proxy (Proxy(..))

foreign import data Method :: Type

foreign import data Options :: Method

foreign import data Head :: Method

foreign import data Get :: Method

foreign import data Post :: Type -> Method

foreign import data Put :: Type -> Method

foreign import data Patch :: Type -> Method

foreign import data Delete :: Method

newtype MethodError = MethodError
    { expectedMethod :: Either HM.CustomMethod HM.Method
    , actualMethod :: Either HM.CustomMethod HM.Method
    }

derive instance Generic MethodError _

instance Show MethodError where
    show = genericShow

checkMethod :: forall errors
    .  Either HM.CustomMethod HM.Method
    -> Either HM.CustomMethod HM.Method
    -> Maybe (Variant (methodError :: MethodError | errors))
checkMethod expected actual =
    if expected == actual
    then Nothing
    else Just $ inj (Proxy :: _ "methodError") $ MethodError
    { expectedMethod: expected
    , actualMethod: actual
    }

class MethodRouter (method :: Method) where
    methodRouter
        :: forall errors
        .  Proxy method
        -> Either HM.CustomMethod HM.Method
        -> Maybe (Variant (methodError :: MethodError | errors))

instance MethodRouter Options where
    methodRouter _ actual = checkMethod (Right HM.OPTIONS) actual

instance MethodRouter Head where
    methodRouter _ actual = checkMethod (Right HM.HEAD) actual

instance MethodRouter Get where
    methodRouter _ actual = checkMethod (Right HM.GET) actual

instance MethodRouter (Post request) where
    methodRouter _ actual = checkMethod (Right HM.POST) actual

instance MethodRouter (Put request) where
    methodRouter _ actual = checkMethod (Right HM.PUT) actual

instance MethodRouter (Patch request) where
    methodRouter _ actual = checkMethod (Right HM.PATCH) actual

instance MethodRouter Delete where
    methodRouter _ actual = checkMethod (Right HM.DELETE) actual
