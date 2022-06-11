module Jarilo.Path where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant, inj)
import Jarilo.Segment (class SegmentRouter, Capture, Literal, SegmentError, segmentRouter, Segment)
import Record.Builder (Builder)
import Type.Proxy (Proxy(..))
import URI.Path.Segment (PathSegment)

foreign import data Path :: Type

foreign import data End :: Path

foreign import data Sub :: Segment -> Path -> Path

infixr 9 type Sub as :>

data PathError
    = NotEndError { restOfPath :: List PathSegment }
    | SegmentEndError { expectedSegment :: String }

derive instance Generic PathError _

instance Show PathError where
    show = genericShow

type PathRouterErrors errors =
    ( pathError :: PathError
    , segmentError :: SegmentError
    | errors
    )

class PathRouter
    (path :: Path) (input :: Row Type) (output :: Row Type)
    | path -> input output where
    pathRouter
        :: forall errors
        .  Proxy path
        -> List PathSegment
        -> Either
            (Variant (PathRouterErrors errors))
            (Builder (Record input) (Record output))

instance PathRouter End input input where
    pathRouter _ Nil = Right identity
    pathRouter _ nonEmptyPath =
        Left
        $ inj (Proxy :: _ "pathError")
        $ NotEndError
        $ { restOfPath: nonEmptyPath }

instance
    ( IsSymbol literal
    , SegmentRouter (Literal literal) input midput
    , PathRouter path midput output
    ) =>
    PathRouter (Sub (Literal literal) path) input output where
    pathRouter _ Nil =
        Left
        $ inj (Proxy :: _ "pathError")
        $ SegmentEndError
        $ { expectedSegment: reflectSymbol (Proxy :: _ literal) }
    pathRouter _ (segment : path) = do
        segmentBuilder :: Builder (Record input) (Record midput) <-
            segmentRouter
                (Proxy :: _ (Literal literal))
                segment
        pathBuilder <- pathRouter (Proxy :: _ path) path
        pure $ segmentBuilder >>> pathBuilder

instance
    ( IsSymbol name
    , SegmentRouter (Capture name result) input midput
    , PathRouter path midput output
    ) =>
    PathRouter (Sub (Capture name result) path) input output where
    pathRouter _ Nil =
        Left
        $ inj (Proxy :: _ "pathError")
        $ SegmentEndError
        $ { expectedSegment: reflectSymbol (Proxy :: _ name) }
    pathRouter _ (segment : path) = do
        segmentBuilder :: Builder (Record input) (Record midput) <-
            segmentRouter
                (Proxy :: _ (Capture name result))
                segment
        pathBuilder :: Builder (Record midput) (Record output) <-
            pathRouter (Proxy :: _ path) path
        pure $ segmentBuilder >>> pathBuilder
