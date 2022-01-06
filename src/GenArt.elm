module GenArt exposing (Dimensions, applyGenerator, applyRecursively, arc, circle, circleStencil, grid, rect, regularPolygon, square)

import Data.Point as Point
import Random exposing (Generator, Seed)


type alias Dimensions =
    { width : Float, height : Float }


{-|

    square =
        [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 0, 1 ) ]

-}
square : List ( number, number )
square =
    rect ( 0, 0 ) ( 1, 1 )


rect : ( number, number ) -> ( number, number ) -> List ( number, number )
rect ( x1, y1 ) ( x2, y2 ) =
    [ ( x1, y1 ), ( x1, y2 ), ( x2, y2 ), ( x2, y1 ) ]


{-| Constructs a circle by a given radius and point.
-}
circle : Float -> ( Float, Float ) -> List ( Float, Float )
circle radius p =
    regularPolygon { points = 2 * pi * radius / 0.05 |> round |> max 10, radius = radius } p


circleStencil : Float -> ( Float, Float ) -> List ( ( Float, Float ), List ( Float, Float ) )
circleStencil radius ( x, y ) =
    [ { angleOffset = 0, list = [ ( x, 1 ), ( 1, 1 ), ( 1, y ) ], p = ( 1, 1 ) }
    , { angleOffset = 1 / 4, list = [ ( -1, y ), ( -1, 1 ), ( x, 1 ) ], p = ( -1, 1 ) }
    , { angleOffset = 2 / 4, list = [ ( x, -1 ), ( -1, -1 ), ( -1, y ) ], p = ( -1, -1 ) }
    , { angleOffset = 3 / 4, list = [ ( 1, y ), ( 1, -1 ), ( x, -1 ) ], p = ( 1, -1 ) } --}
    ]
        |> List.map
            (\{ angleOffset, list, p } ->
                ( p
                , arc
                    { points = 20
                    , radius = radius
                    , angle = pi / 2
                    , angleOffset = angleOffset * 2 * pi
                    }
                    ( x, y )
                    ++ list
                )
            )


regularPolygon : { points : Int, radius : Float } -> ( Float, Float ) -> List ( Float, Float )
regularPolygon args =
    arc
        { points = args.points
        , radius = args.radius
        , angle = 2 * pi
        , angleOffset = 0
        }


arc : { points : Int, radius : Float, angle : Float, angleOffset : Float } -> ( Float, Float ) -> List ( Float, Float )
arc args p =
    List.range 0 args.points
        |> List.map
            (\i ->
                fromPolar ( args.radius, args.angleOffset + args.angle * toFloat i / toFloat args.points )
                    |> Point.toVec
                    |> Point.addTo p
            )


grid : { width : Int, height : Int } -> List ( Int, Int )
grid args =
    List.range 0 args.width
        |> List.concatMap (\x -> List.range 0 args.height |> List.map (Tuple.pair x))


applyGenerator : Seed -> Generator { model | seed : Seed } -> { model | seed : Seed }
applyGenerator seed generator =
    let
        ( newModel, newSeed ) =
            seed
                |> Random.step generator
    in
    { newModel | seed = newSeed }


applyRecursively : Int -> (Int -> a -> List a) -> List a -> List a
applyRecursively maxRecursions fun l =
    List.range 0 (maxRecursions - 1)
        |> List.foldl
            (\recursion ->
                List.concatMap (fun recursion)
            )
            l
