module Routing.Query where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(Tuple))
import Data.Variant (Variant, inj)
import Prim.Row (class Cons, class Lacks)
import Record.Builder (Builder, insert)
import Routing.FromComponent (class FromComponent, fromComponent)
import Routing.Query.QueryPairs (delete, find)
import URI.Extra.QueryPairs (Key, QueryPairs, Value, keyFromString, valueToString)

foreign import kind Query

foreign import data NoQuery :: Query

foreign import data Optional :: Symbol -> Type -> Query

foreign import data Mandatory :: Symbol -> Type -> Query

foreign import data Query :: Query -> Query -> Query

infixr 9 type Query as :?

data QueryProxy (query :: Query) = QueryProxy

data QueryError
    = MissingParameterError
        { parameterName :: String
        , query :: QueryPairs Key Value
        }
    | ParameterParseError
        { parameterName :: String
        , errorMessage :: String
        , actualValue :: Value
        }

parameterParseError :: forall parameterName errors. IsSymbol parameterName =>
    SProxy parameterName
    -> Value
    -> String
    -> Variant (queryError :: QueryError | errors)
parameterParseError nameProxy actualValue errorMessage =
    inj (SProxy :: SProxy "queryError") $ ParameterParseError
    { parameterName: reflectSymbol nameProxy
    , errorMessage: errorMessage
    , actualValue: actualValue
    }

fromValue :: forall value. FromComponent value => Value -> Either String value
fromValue = valueToString >>> fromComponent

class QueryRouter (query :: Query) (input :: # Type) (output :: # Type)
    | query -> input output where
    queryRouter
        :: forall errors
        .  QueryProxy query
        -> QueryPairs Key Value
        -> Either
            (Variant (queryError :: QueryError | errors))
            (Tuple
                (QueryPairs Key Value)
                (Builder (Record input) (Record output)))

instance queryRouterNoQuery :: QueryRouter NoQuery input input where
    queryRouter _ query = pure $ Tuple query identity

instance queryRouterOptional ::
    ( IsSymbol name
    , FromComponent result
    , Lacks name input
    , Cons name (Maybe result) input output
    ) =>
    QueryRouter (Optional name result) input output where
    queryRouter _ query = let
        nameProxy = (SProxy :: SProxy name)
        newQuery = delete (keyFromString $ reflectSymbol nameProxy) query
        in do
        Tuple newQuery <$>
            case find (keyFromString $ reflectSymbol nameProxy) query of
            Nothing -> Right $ insert nameProxy Nothing
            Just (Tuple key Nothing) -> Right $ insert nameProxy Nothing
            Just (Tuple key (Just value)) -> value # fromValue # bimap
                (parameterParseError nameProxy value)
                (Just >>> insert nameProxy)

instance queryRouterMandatory ::
    ( IsSymbol name
    , FromComponent result
    , Lacks name input
    , Cons name result input output
    ) =>
    QueryRouter (Mandatory name result) input output where
    queryRouter _ query = let
        nameProxy = (SProxy :: SProxy name)
        newQuery = delete (keyFromString $ reflectSymbol nameProxy) query
        missingParameterError = Left
            $ inj (SProxy :: SProxy "queryError")
            $ MissingParameterError
            { parameterName: reflectSymbol nameProxy, query: query }
        in
        Tuple newQuery <$>
            case find (keyFromString $ reflectSymbol nameProxy) query of
            Nothing -> missingParameterError
            Just (Tuple key Nothing) -> missingParameterError
            Just (Tuple key (Just value)) -> value # fromValue # bimap
                (parameterParseError nameProxy value)
                (insert nameProxy)

instance queryRouterQuery ::
    ( QueryRouter leftQuery input midput
    , QueryRouter rightQuery midput output
    ) => QueryRouter (Query leftQuery rightQuery) input output where
    queryRouter _ query = let
        leftQueryProxy = (QueryProxy :: QueryProxy leftQuery)
        rightQueryProxy = (QueryProxy :: QueryProxy rightQuery)
        in do
        Tuple leftQuery leftBuilder <- queryRouter leftQueryProxy query
        Tuple rightQuery rightBuilder <- queryRouter rightQueryProxy leftQuery
        pure $ Tuple rightQuery $ leftBuilder >>> rightBuilder
