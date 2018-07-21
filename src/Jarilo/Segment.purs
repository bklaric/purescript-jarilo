module Jarilo.Segment where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either(Left, Right))
import Record.Builder (Builder, insert)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Variant (Variant, inj)
import Prim.Row (class Cons, class Lacks)
import Jarilo.FromComponent (class FromComponent, fromComponent)
import URI.Path.Segment (PathSegment, segmentToString, unsafeSegmentFromString)

foreign import kind Segment

foreign import data Literal :: Symbol -> Segment

foreign import data Capture :: Symbol -> Type -> Segment

data SegmentError
    = LiteralError
        { expectedLiteral :: PathSegment
        , actualLiteral :: PathSegment
        }
    | CaptureError
        { segmentName :: String
        , errorMessage :: String
        , actualSegment :: PathSegment
        }

data SegmentProxy (segment :: Segment) = SegmentProxy

class SegmentRouter
    (segment :: Segment) (input :: # Type) (output :: # Type)
    | segment -> input output
    where
    segmentRouter
        :: forall errors
        .  SegmentProxy segment
        -> PathSegment
        -> Either
            (Variant (segmentError :: SegmentError | errors))
            (Builder (Record input) (Record output))

instance segmentRouterLiteral :: IsSymbol literal =>
    SegmentRouter (Literal literal) input input where
    segmentRouter _ actualLiteral = let
        expectedLiteral = reflectSymbol (SProxy :: SProxy literal) # unsafeSegmentFromString
        in
        if expectedLiteral == actualLiteral
        then Right identity
        else Left $ inj (SProxy :: SProxy "segmentError") $ LiteralError $
        { expectedLiteral: expectedLiteral
        , actualLiteral: actualLiteral
        }

instance segmentRouterCapture ::
    ( IsSymbol name
    , Lacks name input
    , Cons name value input output
    , FromComponent value
    ) =>
    SegmentRouter (Capture name value) input output where
    segmentRouter _ segmentToCapture =
        segmentToCapture # segmentToString # fromComponent # bimap
            (\message -> inj (SProxy :: SProxy "segmentError") $ CaptureError
                { segmentName: reflectSymbol (SProxy :: SProxy name)
                , errorMessage: message
                , actualSegment: segmentToCapture
                })
            (insert (SProxy :: SProxy name))
