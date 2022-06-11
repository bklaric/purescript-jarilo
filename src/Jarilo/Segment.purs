module Jarilo.Segment where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant, inj)
import Jarilo.FromComponent (class FromComponent, fromComponent)
import Prim.Row (class Cons, class Lacks)
import Record.Builder (Builder, insert)
import Type.Proxy (Proxy(..))
import URI.Path.Segment (PathSegment, segmentToString, unsafeSegmentFromString)

foreign import data Segment :: Type

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

derive instance Generic SegmentError _

instance Show SegmentError where
    show = genericShow

class SegmentRouter
    (segment :: Segment) (input :: Row Type) (output :: Row Type)
    | segment -> input output
    where
    segmentRouter
        :: forall errors
        .  Proxy segment
        -> PathSegment
        -> Either
            (Variant (segmentError :: SegmentError | errors))
            (Builder (Record input) (Record output))

instance IsSymbol literal =>
    SegmentRouter (Literal literal) input input where
    segmentRouter _ actualLiteral = let
        expectedLiteral = reflectSymbol (Proxy :: _ literal) # unsafeSegmentFromString
        in
        if expectedLiteral == actualLiteral
        then Right identity
        else Left $ inj (Proxy :: _ "segmentError") $ LiteralError $
        { expectedLiteral: expectedLiteral
        , actualLiteral: actualLiteral
        }

instance
    ( IsSymbol name
    , Lacks name input
    , Cons name value input output
    , FromComponent value
    ) =>
    SegmentRouter (Capture name value) input output where
    segmentRouter _ segmentToCapture =
        segmentToCapture # segmentToString # fromComponent # bimap
            (\message -> inj (Proxy :: _ "segmentError") $ CaptureError
                { segmentName: reflectSymbol (Proxy :: _ name)
                , errorMessage: message
                , actualSegment: segmentToCapture
                })
            (insert (Proxy :: _ name))
