module Test.Main where

import Prelude

import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..), either, hush)
import Data.HTTP.Method (CustomMethod, Method(..))
import Data.List (List, (:))
import Data.List as List
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.MultiMap (MultiMap)
import Data.String.NonEmpty (NonEmptyString)
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Data.Variant (class VariantMatchCases, Variant, contract, inj, match)
import Effect (Effect)
import Effect.Console (log)
import Foreign (ForeignError(..))
import Jarilo.Junction (type (:=), type (:<|>))
import Jarilo.Method (Get, Post)
import Jarilo.Path (type (:>), Capture, Literal)
import Jarilo.Query (type (:?), Mandatory, NoQuery, Optional)
import Jarilo.Response (type (:!), BadRequest, Internal_, Ok, ResponseChain)
import Jarilo.Route (FullRoute, Route)
import Jarilo.Router.Junction (router)
import Jarilo.Router.Route (RouteResult, RouteError)
import Jarilo.Serve.SendResponse.Response (class SendResponse)
import Prim.Row (class Lacks, class Union)
import Prim.RowList (class RowToList)
import Record.Builder (Builder, build, insert)
import Test.Fetch (fetchTests, requestUrlPathTest, requestUrlQueryTest)
import Type.Proxy (Proxy(..))
import URI.Extra.QueryPairs (Key, QueryPairs(..), Value)
import URI.Path.Segment (PathSegment, unsafeSegmentFromString)
import Unsafe.Coerce (unsafeCoerce)
import Yoga.JSON (class ReadForeign, class ReadForeignVariant, class WriteForeign, readJSON, readJSON_, writeJSON)

type RegisterPlayer = FullRoute
    (Post { content :: String })
    (Literal "players")
    NoQuery
    (  Ok { response :: Int }
    :! BadRequest { error :: String }
    :! Internal_)

type ViewPlayer = FullRoute Get (Literal "players" :> Capture "nickname" NonEmptyString) NoQuery (Ok { huehue :: Int } :! BadRequest String)

type ViewPlayers = FullRoute Get (Literal "players") (Optional "game" NonEmptyString :? Mandatory "teamId" Int) (Internal_)

type PlayerRoutes
    =    "registerPlayer" := RegisterPlayer
    :<|> "viewPlayer"     := ViewPlayer
    :<|> "viewPlayers"    := ViewPlayers

-- junction :: forall t166.
--     Either
--         { viewPlayers :: RouteError
--         , viewPlayer :: RouteError
--         , registerPlayer :: RouteError
--         }
--         (Variant
--             ( registerPlayer :: RouteResult () () { content :: String }
--             , viewPlayer :: RouteResult (nickname :: NonEmptyString) () Unit
--             , viewPlayers :: RouteResult () (teamId :: Int, game :: Maybe NonEmptyString) Unit
--             | t166
--             )
--         )
-- junction :: forall registerResponses viewResponses internalResponses other.
--   SendResponse
--     (
--        Ok
--           { response :: Int
--           }
--        :!
--           BadRequest
--              { error :: String
--              }
--           :!
--           Internal_
--     )
--     registerResponses
--    => SendResponse
--         (ResponseChain
--            (Ok
--               { huehue :: Int
--               }
--            )
--            (BadRequest String)
--         )
--         viewResponses
--        => SendResponse Internal_ internalResponses => Either
--                                           { registerPlayer :: RouteError
--                                           , viewPlayer :: RouteError
--                                           , viewPlayers :: RouteError
--                                           }
--                                           (Variant
--                                              ( registerPlayer :: Tuple
--                                                                    { body :: { content :: String
--                                                                              }
--                                                                    , path :: Record ()
--                                                                    , query :: Record ()
--                                                                    }
--                                                                    (Variant registerResponses
--                                                                     -> { body :: String
--                                                                        , headers :: MultiMap String String
--                                                                        , statusCode :: Int
--                                                                        }
--                                                                    )
--                                              , viewPlayer :: Tuple
--                                                                { body :: Unit
--                                                                , path :: { nickname :: NonEmptyString
--                                                                          }
--                                                                , query :: Record ()
--                                                                }
--                                                                (Variant viewResponses
--                                                                 -> { body :: String
--                                                                    , headers :: MultiMap String String
--                                                                    , statusCode :: Int
--                                                                    }
--                                                                )
--                                              , viewPlayers :: Tuple
--                                                                 { body :: Unit
--                                                                 , path :: Record ()
--                                                                 , query :: { game :: Maybe NonEmptyString
--                                                                            , teamId :: Int
--                                                                            }
--                                                                 }
--                                                                 (Variant internalResponses
--                                                                  -> { body :: String
--                                                                     , headers :: MultiMap String String
--                                                                     , statusCode :: Int
--                                                                     }
--                                                                 )
--                                              | other
--                                              )
--                                           )
junction =
    router (Proxy :: _ PlayerRoutes) { method: Right POST, path: unsafeSegmentFromString "players" : List.Nil, query: QueryPairs [], body: """{"content": "huehue"}""", cookies: mempty, headers: mempty }

-- wut :: String
-- wut = case hush junction of
--     Nothing -> "nothing"
--     Just routeValues -> match
--         { registerPlayer: \_ -> "register player"
--         , viewPlayer: \{ path: { nickname } } -> "view player " <> show nickname
--         , viewPlayers: \{ query: { teamId, game } } -> "viewPlayers " <> show teamId <> " " <> show game
--         }
--         routeValues

-- class WriteRequest (route :: Route) request | route -> request where
--     writeRequest :: Proxy route -> request -> String

-- instance WriteForeign request => WriteRequest (FullRoute (Post request) path query response) request where
--     writeRequest _ = writeJSON

-- class ReadRequest (route :: Route) request | route -> request where
--     readRequest :: Proxy route -> String -> Maybe request

-- instance ReadForeign request => ReadRequest (FullRoute (Post request) path query response) request where
--     readRequest _ = readJSON_


-- class PrepareWriteResponse (route :: Route) input output | route -> input output where
--     prepareWriteResponse :: Proxy route -> Builder (Record input) (Record output)

-- instance (WriteForeign body, Lacks "ok" input) =>
--     PrepareWriteResponse (FullRoute method path query (Ok body)) input (ok :: body -> { code :: Int, body :: Maybe String } | input) where
--     prepareWriteResponse _ = insert (Proxy :: _ "ok") \body -> { code: 200, body: Just $ writeJSON body }

-- instance (WriteForeign body, Lacks "badRequest" input) =>
--     PrepareWriteResponse (FullRoute method path query (BadRequest body)) input (badRequest :: body -> { code :: Int, body :: Maybe String } | input) where
--     prepareWriteResponse _ = insert (Proxy :: _ "badRequest") \body -> { code: 400, body: Just $ writeJSON body }

-- instance (Lacks "internal" input) =>
--     PrepareWriteResponse (FullRoute method path query Internal_) input (internal :: body -> { code :: Int, body :: Maybe String } | input) where
--     prepareWriteResponse _ = insert (Proxy :: _ "internal") $ const { code: 500, body: Nothing }

-- instance
--     ( PrepareWriteResponse (FullRoute method path query leftResponse) input midput
--     , PrepareWriteResponse (FullRoute method path query rightResponse) midput output
--     ) =>
--     PrepareWriteResponse (FullRoute method path query (leftResponse :! rightResponse)) input output where
--     prepareWriteResponse _ = let
--         leftBuilder = prepareWriteResponse (Proxy :: _ (FullRoute method path query leftResponse))
--         rightBuilder = prepareWriteResponse (Proxy :: _ (FullRoute method path query rightResponse))
--         in
--         leftBuilder >>> rightBuilder

-- writeResponse :: forall route result outputList output wtf response.
--     PrepareWriteResponse route () output => RowToList output outputList => VariantMatchCases outputList wtf result => Union wtf () response =>
--     Proxy route -> Variant response -> result
-- writeResponse proxy response = let
--     responseHandlers = build (prepareWriteResponse proxy) {}
--     in
--     response # match responseHandlers


-- class PrepareReadResponse (route :: Route) input output | route -> input output where
--     prepareReadResponse :: Proxy route -> Builder (Record input) (Record output)

-- instance (ReadForeign body, Lacks "ok" input) =>
--     PrepareReadResponse (FullRoute method path query (Ok body)) input (ok :: Maybe String -> Variant (ok :: Either String (Maybe body) | other) | input) where
--     prepareReadResponse _ = insert (Proxy :: _ "ok") \body -> inj (Proxy :: _ "ok") (body # traverse readJSON # lmap show)

-- instance (ReadForeign body, Lacks "badRequest" input) =>
--     PrepareReadResponse (FullRoute method path query (BadRequest body)) input (badRequest :: Maybe String -> Variant (badRequest :: Either String (Maybe body) | other) | input) where
--     prepareReadResponse _ = insert (Proxy :: _ "badRequest") \body -> inj (Proxy :: _ "badRequest") (body # traverse readJSON # lmap show)

-- instance (Lacks "internal" input) =>
--     PrepareReadResponse (FullRoute method path query Internal_) input (internal :: Maybe String -> Variant (internal :: Unit | other) | input) where
--     prepareReadResponse _ = insert (Proxy :: _ "internal") $ const $ inj (Proxy :: _ "internal") unit

-- instance
--     ( PrepareReadResponse (FullRoute method path query leftResponse) input midput
--     , PrepareReadResponse (FullRoute method path query rightResponse) midput output
--     ) =>
--     PrepareReadResponse (FullRoute method path query (leftResponse :! rightResponse)) input output where
--     prepareReadResponse _ = let
--         leftBuilder = prepareReadResponse (Proxy :: _ (FullRoute method path query leftResponse))
--         rightBuilder = prepareReadResponse (Proxy :: _ (FullRoute method path query rightResponse))
--         in
--         leftBuilder >>> rightBuilder

-- readResponse proxy response = let
--     responseHandlers = build (prepareReadResponse proxy) {}
--     in
--     response # match responseHandlers

-- test = readResponse (Proxy :: _ RegisterPlayer) (inj (Proxy :: _ "ok") (Just """{ "response": "123" }"""))

-- test2 = test # match
--     { badRequest: either identity show
--     , internal: const "internal"
--     , ok: either identity show
--     }

-- main :: Effect Unit
-- main = do
--     log $ show $ readRequest (Proxy :: _ RegisterPlayer) $ writeRequest (Proxy :: _ RegisterPlayer) { content: "aoeu" }
--     log $ unsafeCoerce $ build (prepareWriteResponse (Proxy :: _ RegisterPlayer)) {}
--     log $ show $ writeResponse (Proxy :: _ RegisterPlayer) (inj (Proxy :: _ "badRequest") {error: "oh no"})
--     log $ unsafeCoerce $ build (prepareReadResponse (Proxy :: _ RegisterPlayer)) {}
--     -- log $ show $ writeJSON $ readResponse (Proxy :: _ RegisterPlayer) (inj (Proxy :: _ "badRequest") (Just """{ "error": "haha" }""")) -- $ writeResponse (Proxy :: _ RegisterPlayer) (inj (Proxy :: _ "badRequest") {error: "oh no"})
--     log test2
--     fetchTests
