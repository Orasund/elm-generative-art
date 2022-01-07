module GenArt exposing (Dimensions, applyGenerator, applyRecursively, grid)

import Data.Point as Point
import Random exposing (Generator, Seed)


type alias Dimensions =
    { width : Float, height : Float }


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
