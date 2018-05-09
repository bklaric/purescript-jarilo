module Routing.Segment where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Record.Builder (Builder, insert, passThrough)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NonEmptyString
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Variant (Variant, inj)
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

class FromSegment value where
    fromSegment :: String -> Either String value

instance fromSegmentInt :: FromSegment Int where
    fromSegment =
        Int.fromString >>> note "Couldn't turn segment into an integer."

instance fromSegmentString :: FromSegment String where
    fromSegment = pure

instance fromSegmentNonEmptyString :: FromSegment NonEmptyString where
    fromSegment = NonEmptyString.fromString >>> note "Segment cannot be empty."

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
    , FromSegment value
    ) =>
    SegmentRouter (Capture name value) input output where
    segmentRouter _ segmentToCapture =
        fromSegment segmentToCapture # bimap
            (\message -> inj (SProxy :: SProxy "segmentError") $ CaptureError
                { segmentName: reflectSymbol (SProxy :: SProxy name)
                , errorMessage: message
                , actualSegment: segmentToCapture
                })
            (insert (SProxy :: SProxy name))
