module Jarilo.Router.Path where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Jarilo.Shared.Component (class Component, fromComponent)
import Jarilo.Path (Path, Literal, Capture, Sub)
import Prim.Row (class Cons, class Lacks)
import Record.Builder (Builder, insert)
import Type.Proxy (Proxy(..))
import URI.Path.Segment (PathSegment, segmentToString, unsafeSegmentFromString)

data PathError
    = NotEndError { restOfPath :: List PathSegment }
    | SegmentEndError { expectedSegment :: String }
    | LiteralError
        { expectedLiteral :: PathSegment
        , actualLiteral :: PathSegment
        }
    | CaptureError
        { segmentName :: String
        , errorMessage :: String
        , actualSegment :: PathSegment
        }
derive instance Generic PathError _

instance Show PathError where
    show = genericShow

type PathRouterErrors errors =
    ( pathError :: PathError
    | errors
    )

class PathRouter
    (path :: Path) (input :: Row Type) (output :: Row Type)
    | path -> input output
    where
    pathRouter
        :: forall errors
        .  Proxy path
        -> List PathSegment
        -> Either
            (Variant (PathRouterErrors errors))
            (Tuple
                (List PathSegment)
                (Builder (Record input) (Record output))
            )

instance IsSymbol literal => PathRouter (Literal literal) input input where
    pathRouter _ Nil =
        Left
        $ inj (Proxy :: _ "pathError")
        $ SegmentEndError
        $ { expectedSegment: reflectSymbol (Proxy :: _ literal) }
    pathRouter _ (actualLiteral : path) = do
        let expectedLiteral = reflectSymbol (Proxy :: _ literal) # unsafeSegmentFromString
        builder <- if expectedLiteral == actualLiteral
            then Right identity
            else Left $ inj (Proxy :: _ "pathError") $ LiteralError $
            { expectedLiteral: expectedLiteral
            , actualLiteral: actualLiteral
            }
        Right $ Tuple path builder

instance
    ( IsSymbol name
    , Lacks name input
    , Cons name value input output
    , Component value
    ) =>
    PathRouter (Capture name value) input output where
    pathRouter _ Nil =
        Left
        $ inj (Proxy :: _ "pathError")
        $ SegmentEndError
        $ { expectedSegment: reflectSymbol (Proxy :: _ name) }
    pathRouter _ (segmentToCapture : path) =
        segmentToCapture # segmentToString # (fromComponent :: _ -> _ _ value) # bimap
            (\message -> inj (Proxy :: _ "pathError") $ CaptureError
                { segmentName: reflectSymbol (Proxy :: _ name)
                , errorMessage: message
                , actualSegment: segmentToCapture
                })
            (Tuple path <<< insert (Proxy :: _ name))

instance
    ( PathRouter leftPath input midput
    , PathRouter rightPath midput output
    ) =>
    PathRouter (Sub leftPath rightPath) input output where
    pathRouter _ path = let
        leftProxy = (Proxy :: _ leftPath)
        rightProxy = (Proxy :: _ rightPath)
        in do
        Tuple leftPath leftBuilder <- pathRouter leftProxy path
        Tuple rightPath rightBuilder <- pathRouter rightProxy leftPath
        pure $ Tuple rightPath $ leftBuilder >>> rightBuilder
