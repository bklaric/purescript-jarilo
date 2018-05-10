module Routing.Segment where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either(Left, Right))
import Data.Record.Builder (Builder, insert, passThrough)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Variant (Variant, inj)
import Routing.FromComponent (class FromComponent, fromComponent)
import Type.Row (class RowLacks)

foreign import kind Segment

foreign import data Literal :: Symbol -> Segment

foreign import data Capture :: Symbol -> Type -> Segment

data SegmentError
    = LiteralError
        { expectedLiteral :: String
        , actualLiteral :: String
        }
    | CaptureError
        { segmentName :: String
        , errorMessage :: String
        , actualSegment :: String
        }

data SegmentProxy (segment :: Segment) = SegmentProxy

class SegmentRouter
    (segment :: Segment) (input :: # Type) (output :: # Type)
    | segment -> input output
    where
    segmentRouter
        :: forall errors
        .  SegmentProxy segment
        -> String
        -> Either
            (Variant (segmentError :: SegmentError | errors))
            (Builder (Record input) (Record output))

instance segmentRouterLiteral :: IsSymbol literal =>
    SegmentRouter (Literal literal) input input where
    segmentRouter _ actualLiteral = let
        expectedLiteral = reflectSymbol (SProxy :: SProxy literal)
        in
        if expectedLiteral == actualLiteral
        then Right $ passThrough
        else Left $ inj (SProxy :: SProxy "segmentError") $ LiteralError $
        { expectedLiteral: expectedLiteral
        , actualLiteral: actualLiteral
        }

instance segmentRouterCapture ::
    ( IsSymbol name
    , RowLacks name input
    , RowCons name value input output
    , FromComponent value
    ) =>
    SegmentRouter (Capture name value) input output where
    segmentRouter _ segmentToCapture =
        fromComponent segmentToCapture # bimap
            (\message -> inj (SProxy :: SProxy "segmentError") $ CaptureError
                { segmentName: reflectSymbol (SProxy :: SProxy name)
                , errorMessage: message
                , actualSegment: segmentToCapture
                })
            (insert (SProxy :: SProxy name))
