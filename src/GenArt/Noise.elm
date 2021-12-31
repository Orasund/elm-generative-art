module GenArt.Noise exposing (toFloat)

import Simplex exposing (PermutationTable)


{-| returns a value between 0 and 1
-}
toFloat : PermutationTable -> { x : Float, y : Float } -> Float
toFloat permutationTable { x, y } =
    0.5 + 0.5 * Simplex.noise2d permutationTable x y
