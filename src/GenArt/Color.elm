module GenArt.Color exposing (Palette, blendTo, create, fromCIELCH, toCIELCH, toVec, withLuminance, withOpacity, withSaturation)

import Color exposing (Color)
import Internal.Color as Color
import Math.Vector4 exposing (Vec4)


type alias Palette =
    { primary : Color
    , secondary : Color
    , trinary : Color
    }


toVec : Color -> Vec4
toVec color =
    let
        { red, green, blue, alpha } =
            color |> Color.toRgba
    in
    Math.Vector4.vec4 red green blue alpha


{-| creates a color.

  - hue wraps around (0.7 = 1.7 = -0.3)
  - luminance is set using the CIELCH color field

-}
create : { hue : Float, saturation : Float, luminance : Float, alpha : Float } -> Color
create args =
    let
        h =
            if args.hue > 1 then
                args.hue - toFloat (ceiling args.hue)

            else if args.hue < 0 then
                args.hue + toFloat (floor args.hue)

            else
                args.hue

        s =
            args.saturation
    in
    Color.hsla h s 0.5 args.alpha
        |> toCIELCH
        |> (\c1 -> { c1 | l = args.luminance })
        |> fromCIELCH


{-| luminance according to visual light level
-}
withLuminance : Float -> Color -> Color
withLuminance f c =
    c
        |> toCIELCH
        |> (\c1 -> { c1 | l = f })
        |> fromCIELCH


withSaturation : Float -> Color -> Color
withSaturation f c =
    c
        |> Color.toHsla
        |> (\record -> { record | saturation = f })
        |> Color.fromHsla


withOpacity : Float -> Color -> Color
withOpacity f color =
    color
        |> Color.toRgba
        |> (\record -> { record | alpha = f })
        |> Color.fromRgba


{-| Utility function to convert a color to CIELCH color space
-}
toCIELCH : Color -> { l : Float, c : Float, h : Float, alpha : Float }
toCIELCH =
    Color.toCIELCH


{-| Utility function to convert CIELCH color space back to a color
-}
fromCIELCH : { l : Float, c : Float, h : Float, alpha : Float } -> Color
fromCIELCH =
    Color.fromCIELCH


blendTo : Color -> Float -> Color -> Color
blendTo c2 amount c1 =
    let
        alpha =
            c1
                |> Color.toRgba
                |> .alpha

        h2 a b =
            a.h
                * (1 - amount)
                + hueCorrection a.h b.h
                * amount
                |> (\h ->
                        if h > 1 then
                            h - (h |> ceiling |> toFloat)

                        else if h < 0 then
                            h + (abs h |> ceiling |> toFloat)

                        else
                            h
                   )

        fun a b =
            { l = (a.l * (1 - amount) + b.l * amount) / 1
            , c = (a.c * (1 - amount) + b.c * amount) / 1
            , h = h2 a b
            , alpha = alpha
            }
    in
    fun (Color.toCIELCH c1) (Color.toCIELCH c2)
        |> Color.fromCIELCH


{-| (-2pi,pi) -> (-pi,pi)
-}
hueCorrection : Float -> Float -> Float
hueCorrection ah bh =
    if ah < bh && bh - ah > 0.5 then
        bh - (bh |> ceiling |> toFloat)

    else if ah - bh > 0.5 then
        bh + (bh |> ceiling |> toFloat)

    else
        bh
