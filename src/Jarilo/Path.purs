module Jarilo.Path where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..), (:))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Variant (Variant, inj)
import Jarilo.Segment (class SegmentRouter, Capture, Literal, SegmentError, SegmentProxy(..), segmentRouter, kind Segment)
import Record.Builder (Builder)
import URI.Path.Segment (PathSegment)

foreign import kind Path

foreign import data End :: Path

foreign import data Sub :: Segment -> Path -> Path

infixr 9 type Sub as :>

data PathProxy (path :: Path) = PathProxy

data PathError
    = NotEndError { restOfPath :: List PathSegment }
    | SegmentEndError { expectedSegment :: String }

derive instance genericPathError :: Generic PathError _

instance showPathError :: Show PathError where
    show = genericShow

type PathRouterErrors errors =
    ( pathError :: PathError
    , segmentError :: SegmentError
    | errors
    )

class PathRouter
    (path :: Path) (input :: # Type) (output :: # Type)
    | path -> input output where
    pathRouter
        :: forall errors
        .  PathProxy path
        -> List PathSegment
        -> Either
            (Variant (PathRouterErrors errors))
            (Builder (Record input) (Record output))

instance pathRouterEnd :: PathRouter End input input where
    pathRouter _ Nil = Right identity
    pathRouter _ nonEmptyPath =
        Left
        $ inj (SProxy :: SProxy "pathError")
        $ NotEndError
        $ { restOfPath: nonEmptyPath }

instance pathRouterSubLiteral ::
    ( IsSymbol literal
    , SegmentRouter (Literal literal) input midput
    , PathRouter path midput output
    ) =>
    PathRouter (Sub (Literal literal) path) input output where
    pathRouter _ Nil =
        Left
        $ inj (SProxy :: SProxy "pathError")
        $ SegmentEndError
        $ { expectedSegment: reflectSymbol (SProxy :: SProxy literal) }
    pathRouter _ (segment : path) = do
        segmentBuilder :: Builder (Record input) (Record midput) <-
            segmentRouter
                (SegmentProxy :: SegmentProxy (Literal literal))
                segment
        pathBuilder <- pathRouter (PathProxy :: PathProxy path) path
        pure $ segmentBuilder >>> pathBuilder

instance pathRouterSubCapture ::
    ( IsSymbol name
    , SegmentRouter (Capture name result) input midput
    , PathRouter path midput output
    ) =>
    PathRouter (Sub (Capture name result) path) input output where
    pathRouter _ Nil =
        Left
        $ inj (SProxy :: SProxy "pathError")
        $ SegmentEndError
        $ { expectedSegment: reflectSymbol (SProxy :: SProxy name) }
    pathRouter _ (segment : path) = do
        segmentBuilder :: Builder (Record input) (Record midput) <-
            segmentRouter
                (SegmentProxy :: SegmentProxy (Capture name result))
                segment
        pathBuilder :: Builder (Record midput) (Record output) <-
            pathRouter (PathProxy :: PathProxy path) path
        pure $ segmentBuilder >>> pathBuilder
