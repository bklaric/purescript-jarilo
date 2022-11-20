module Jarilo.Fetch where

import Prelude

import Async (Async, left)
import Browser.Async.Fetch (body, credentials, method)
import Browser.Async.Fetch as Fetch
import Browser.Fetch (Credentials)
import Browser.Fetch.Response as FetchRes
import Data.Either (Either)
import Data.HTTP.Method as Http
import Data.Maybe (Maybe(..), maybe)
import Data.Options ((:=))
import Data.Options as Options
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Effect (Effect)
import Error (Error)
import Jarilo.Method (Delete, Get, Head, Method, Options, Patch, Post, Put)
import Jarilo.Path (type (:>), Capture, Literal, Path)
import Jarilo.Query (type (:?), Mandatory, Many, NoQuery, Optional, Query, QueryChain, Rest)
import Jarilo.Route (FullRoute, Route)
import Jarilo.Shared.Component (class Component, toComponent)
import Prim.Row (class Cons)
import Record (get, insert, merge)
import Test.QuickCheck (assertEquals)
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Yoga.JSON (class WriteForeign, writeJSON)

-- fetch proxy
-- Option.Record
-- ( pathParameters :: { pathParams } -- from proxy
-- , queryParameters :: { queryParams } -- from proxy
-- , body :: { body } -- from proxy
-- )
-- ( host :: String -- e.g. https://www.teamtavern.net
-- , pathPrefix :: Proxy pathPrefix
-- , credentials :: Credentials
-- )

class RequestUrlPath (path :: Path) parameters | path -> parameters where
    requestUrlPath :: Proxy path -> Record parameters -> String

instance IsSymbol name => RequestUrlPath (Literal name) parameters where
    requestUrlPath _ _ = "/" <> reflectSymbol (Proxy :: _ name)

instance (IsSymbol name, Component value, Cons name value parameters' parameters) => RequestUrlPath (Capture name value) parameters where
    requestUrlPath _ parameters = "/" <> ((toComponent :: value -> String) $ get (Proxy :: _ name) parameters)

instance (RequestUrlPath leftPath parameters, RequestUrlPath rightPath parameters) => RequestUrlPath (leftPath :> rightPath) parameters where
    requestUrlPath _ parameters = requestUrlPath (Proxy :: _ leftPath) parameters <> requestUrlPath (Proxy :: _ rightPath) parameters



class RequestUrlQuery (query :: Query) parameters | query -> parameters where
    requestUrlQuery :: Proxy query -> Record parameters -> Maybe String

instance RequestUrlQuery NoQuery parameters where
    requestUrlQuery _ _ = Nothing

instance (IsSymbol name, Component value, Cons name (Maybe value) parameters' parameters) => RequestUrlQuery (Optional name value) parameters where
    requestUrlQuery _ parameters = get (Proxy :: _ name) parameters <#> toComponent <#> \value -> reflectSymbol (Proxy :: _ name) <> "=" <> value

instance (IsSymbol name, Component value, Cons name value parameters' parameters) => RequestUrlQuery (Mandatory name value) parameters where
    requestUrlQuery _ parameters = get (Proxy :: _ name) parameters # toComponent # \value -> Just $ reflectSymbol (Proxy :: _ name) <> "=" <> value

instance (IsSymbol name, Component value, Cons name (Array value) parameters' parameters) => RequestUrlQuery (Many name value) parameters where
    requestUrlQuery _ parameters = get (Proxy :: _ name) parameters <#> toComponent <#> (\value -> reflectSymbol (Proxy :: _ name) <> "=" <> value) # joinWith "&" # Just

instance (IsSymbol name,  Component name', Component value, Cons name (Array (Tuple name' value)) parameters' parameters) => RequestUrlQuery (Rest name) parameters where
    requestUrlQuery _ parameters = get (Proxy :: _ name) parameters <#> (\(Tuple name value) -> toComponent name <> "=" <> toComponent value) # joinWith "&" # Just

instance (RequestUrlQuery leftQuery parameters, RequestUrlQuery rightQuery parameters) => RequestUrlQuery (leftQuery :? rightQuery) parameters where
    requestUrlQuery _ parameters =
        case requestUrlQuery (Proxy :: _ leftQuery) parameters, requestUrlQuery (Proxy :: _ rightQuery) parameters of
        Nothing, Nothing -> Nothing
        Just left, Nothing -> Just left
        Nothing, Just right -> Just right
        Just left, Just right -> Just $ left <> "&" <> right

requestUrlQuery' :: forall parameters query. RequestUrlQuery query parameters => Proxy query -> Record parameters -> String
requestUrlQuery' proxy parameters = maybe "" ("?" <> _) (requestUrlQuery proxy parameters)



requestUrl :: forall pathParameters queryParameters path query.
    RequestUrlPath path pathParameters => RequestUrlQuery query queryParameters =>
    Proxy path -> Proxy query -> Record pathParameters -> Record queryParameters -> String
requestUrl pathProxy queryProxy pathParameters queryParameters = requestUrlPath pathProxy pathParameters <> requestUrlQuery' queryProxy queryParameters



class RequestMethod (method :: Method) body | method -> body where
    requestMethod :: Proxy method -> body -> Options.Options Fetch.FetchOptions

instance RequestMethod Get body where
    requestMethod _ _ = method := Http.GET

instance RequestMethod Head body where
    requestMethod _ _ = method := Http.HEAD

instance RequestMethod Options body where
    requestMethod _ _ = method := Http.OPTIONS

instance RequestMethod Delete body where
    requestMethod _ _ = method := Http.DELETE

instance WriteForeign body => RequestMethod (Post body) body where
    requestMethod _ body' = method := Http.POST <> body := writeJSON body'

instance WriteForeign body => RequestMethod (Put body) body where
    requestMethod _ body' = method := Http.PUT <> body := writeJSON body'

instance WriteForeign body => RequestMethod (Patch body) body where
    requestMethod _ body' = method := Http.PATCH <> body := writeJSON body'



type FetchOptions = { host :: Maybe String, pathPrefix :: Maybe String, credentials :: Maybe Credentials }

defaultOptions :: FetchOptions
defaultOptions = { host: Nothing, pathPrefix: Nothing, credentials: Nothing }



class Fetch (route :: Route) pathParameters queryParameters body responses | route -> pathParameters queryParameters body responses where
    fetch :: Proxy route -> { pathParameters :: Record pathParameters, queryParameters :: Record queryParameters, body :: body } -> FetchOptions -> Async Error FetchRes.Response -- Async Error (Variant responses)

instance (RequestMethod method body, RequestUrlPath path pathParameters, RequestUrlQuery query queryParameters) => Fetch (FullRoute method path query response) pathParameters queryParameters body responses where
    fetch _ { pathParameters, queryParameters, body } options = let
        host = maybe "" identity options.host
        pathPrefix = maybe "" identity options.pathPrefix
        url = host <> pathPrefix <> requestUrl (Proxy :: _ path) (Proxy :: _ query) pathParameters queryParameters
        fetchOptions = requestMethod (Proxy :: _ method) body <> maybe mempty (credentials := _) options.credentials
        in
        Fetch.fetch url fetchOptions
        -- Fetch.fetch url fetchOptions >>= receiveResponse (Proxy :: _ response)
