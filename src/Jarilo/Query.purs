module Jarilo.Query where

import Prelude

import Data.Array (catMaybes)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple), snd)
import Data.Variant (Variant, inj)
import Jarilo.FromComponent (class FromComponent, fromComponent)
import Jarilo.Query.QueryPairs (delete, find, findAll)
import Prim.Row (class Cons, class Lacks)
import Record.Builder (Builder, insert)
import URI.Extra.QueryPairs (Key, QueryPairs(..), Value, keyFromString, valueToString)

foreign import kind Query

foreign import data NoQuery :: Query

foreign import data Optional :: Symbol -> Type -> Query

foreign import data Mandatory :: Symbol -> Type -> Query

foreign import data Many :: Symbol -> Type -> Query

foreign import data Rest :: Symbol -> Query

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

derive instance genericQueryError :: Generic QueryError _

instance showQueryError :: Show QueryError where
    show = genericShow

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

fromValue' :: forall value. FromComponent value =>
    Value -> Either { error :: String, value :: Value } value
fromValue' value = valueToString value # fromComponent # lmap { error: _, value }

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
    ) => QueryRouter (Optional name result) input output where
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
    ) => QueryRouter (Mandatory name result) input output where
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

instance queryRouterMany ::
    ( IsSymbol name
    , FromComponent result
    , Lacks name input
    , Cons name (Array result) input output
    ) => QueryRouter (Many name result) input output where
    queryRouter _ query = let
        nameProxy = (SProxy :: SProxy name)
        newQuery = delete (keyFromString $ reflectSymbol nameProxy) query
        foundPairs = findAll (keyFromString $ reflectSymbol nameProxy) query
        foundValues = foundPairs <#> snd # catMaybes # traverse fromValue'
        in
        bimap
            (\{ error, value } -> parameterParseError nameProxy value error)
            (\foundValues' -> Tuple newQuery $ insert nameProxy foundValues')
            foundValues

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

instance queryRouterRest ::
    ( IsSymbol name
    , Lacks name input
    , Cons name (QueryPairs Key Value) input output
    ) => QueryRouter (Rest name) input output where
    queryRouter _ query =
        pure $ Tuple (QueryPairs []) (insert (SProxy :: SProxy name) query)
