module Routing.Path where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..), intercalate, (:))
import Data.Record.Builder (Builder, passThrough)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Variant (Variant, inj)
import Routing.Segment (class SegmentRouter, Capture, Literal, SegmentError, SegmentProxy(..), segmentRouter, kind Segment)

foreign import kind Path

foreign import data End :: Path

foreign import data Sub :: Segment -> Path -> Path

infixr 9 type Sub as :>

data PathProxy (path :: Path) = PathProxy

data PathError
    = NotEndError String
    | SegmentEndError String

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
        -> List String
        -> Either
            (Variant (PathRouterErrors errors))
            (Builder (Record input) (Record output))

instance pathRouterEnd :: PathRouter End input input where
    pathRouter _ Nil = Right passThrough
    pathRouter _ nonEmptyPath =
        Left
        $ inj (SProxy :: SProxy "pathError")
        $ NotEndError
        $ intercalate "/" nonEmptyPath

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
        $ reflectSymbol (SProxy :: SProxy literal)
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
        $ reflectSymbol (SProxy :: SProxy name)
    pathRouter _ (segment : path) = do
        segmentBuilder :: Builder (Record input) (Record midput) <-
            segmentRouter
                (SegmentProxy :: SegmentProxy (Capture name result))
                segment
        pathBuilder :: Builder (Record midput) (Record output) <-
            pathRouter (PathProxy :: PathProxy path) path
        pure $ segmentBuilder >>> pathBuilder
