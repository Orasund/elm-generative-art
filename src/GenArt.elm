module GenArt exposing (Dimensions, applyGenerator, applyRecursively, grid, rect, square)

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


grid : { width : Int, height : Int } -> List ( Int, Int )
grid args =
    List.range 0 (args.width - 1)
        |> List.concatMap (\x -> List.range 0 (args.height - 1) |> List.map (Tuple.pair x))


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
