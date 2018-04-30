module Routing.Query where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Record.Builder (Builder, insert, passThrough)
import Data.StrMap (StrMap, delete, isEmpty, lookup)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Variant (Variant, inj)
import Routing.Segment (class FromSegment, fromSegment)
import Type.Row (class RowLacks)

foreign import kind Query

foreign import data Nil :: Query

foreign import data Optional :: Symbol -> Type -> Query -> Query

data QueryProxy (query :: Query) = QueryProxy

data QueryError
    = ExcessParametersError (StrMap String)
    | MissingParameterError String (StrMap String)
    | ParameterParseError
        { name :: String, message :: String, actual :: String }

class QueryRouter
    (query :: Query) (input :: # Type) (output :: # Type)
    | query -> input output where
    queryRouter
        :: forall errors
        .  QueryProxy query
        -> StrMap String
        -> Either
            (Variant (queryError :: QueryError | errors))
            (Builder (Record input) (Record output))

instance queryRouterNil :: QueryRouter Nil input input where
    queryRouter _ query =
        if isEmpty query
        then Right $ passThrough
        else Left
            $ inj (SProxy :: SProxy "queryError")
            $ ExcessParametersError query

instance queryRouterOptional ::
    ( IsSymbol name
    , FromSegment result
    , RowLacks name input
    , RowCons name (Maybe result) input midput
    , QueryRouter query midput output
    ) =>
    QueryRouter (Optional name result query) input output where
    queryRouter _ query = let
        key = (SProxy :: SProxy name)
        newQuery = delete (reflectSymbol key) query
        in do
        keyBuilder :: Builder (Record input) (Record midput) <-
            case lookup (reflectSymbol key) query of
            Nothing -> Right $ insert key Nothing
            Just value -> fromSegment value # bimap
                ({ name: reflectSymbol key, message: _, actual: value }
                    >>> ParameterParseError
                    >>> inj (SProxy :: SProxy "queryError"))
                (Just >>> insert key)
        queryBuilder :: Builder (Record midput) (Record output) <-
            queryRouter (QueryProxy :: QueryProxy query) newQuery
        pure $ keyBuilder >>> queryBuilder
