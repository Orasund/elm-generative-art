module GenArt exposing (Dimensions, applyGenerator, applyRecursively, grid, randomGrid)

import Data.Point as Point
import Dict exposing (Dict)
import Random exposing (Generator, Seed)


type alias Dimensions =
    { width : Float, height : Float }


randomGrid : { columns : Int, rows : Int } -> Generator a -> Generator (Dict ( Int, Int ) a)
randomGrid args gen =
    Random.list (args.columns * args.rows) gen
        |> Random.map
            (\list ->
                list
                    |> List.indexedMap
                        (\int a ->
                            let
                                i =
                                    int // args.rows

                                j =
                                    modBy args.rows int
                            in
                            ( ( i, j ), a )
                        )
                    |> Dict.fromList
            )


{-| creates a grid of coordinates from 0 to (excluding) width / height.
-}
grid : { columns : Int, rows : Int } -> List ( Int, Int )
grid args =
    List.range 0 (args.columns - 1)
        |> List.concatMap (\x -> List.range 0 (args.rows - 1) |> List.map (Tuple.pair x))


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
