module GenArt exposing (Dimensions, applyGenerator, applyRecursively, circle, grid, rect, regularPolygon, square)

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


regularPolygon : { points : Int, radius : Float } -> ( Float, Float ) -> List ( Float, Float )
regularPolygon args p =
    List.range 0 args.points
        |> List.map
            (\i ->
                fromPolar ( args.radius, 2 * pi * toFloat i / toFloat args.points )
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
