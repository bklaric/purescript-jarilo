module Jarilo.FromComponent where

import Prelude

import Data.Either (Either(..), note)
import Data.Int as Int
import Data.String (toLower)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NonEmptyString

class FromComponent value where
    fromComponent :: String -> Either String value

instance fromComponentString :: FromComponent String where
    fromComponent = pure

instance fromComponentInt :: FromComponent Int where
    fromComponent =
        Int.fromString >>> note "Couldn't turn component into an integer."

instance fromComponentBoolean :: FromComponent Boolean where
    fromComponent = toLower >>>
        case _ of
        "true" -> Right true
        "false" -> Right false
        _ -> Left "Couldn't turn component into a boolean."

instance fromComponentNonEmptyString :: FromComponent NonEmptyString where
    fromComponent = NonEmptyString.fromString >>> note "Component cannot be empty."
