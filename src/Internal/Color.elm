module Internal.Color exposing (CIELCHColor, fromCIELCH, toCIELCH)

{-| Functions taken from noahzgordon/elm-color-extra.

  - Added alpha values to convertion
  - normed the CIELCH values (hue: (-pi,pi) -> (0,1), chroma: 100 -> 1, luminance: 100 -> 1)
  - hsl constructor that wraps the h value and uses CIELCH for luminance

-}

import Color exposing (Color)


{-| Color in the CIELCH field.

  - luminance: 0-1 light level
  - chroma: 0-1 color dencity
  - hue: 0-1 hue

-}
type alias CIELCHColor =
    { l : Float, c : Float, h : Float, alpha : Float }


toCIELCH : Color -> CIELCHColor
toCIELCH color =
    color
        |> colorToXyz
        |> xyzToLab
        |> (\{ l, a, b, alpha } ->
                { l = l / 100
                , c = sqrt (a * a + b * b) / 100
                , h = 0.5 + atan2 b a / (pi * 2)
                , alpha = alpha
                }
           )


colorToXyz : Color -> { x : Float, y : Float, z : Float, alpha : Float }
colorToXyz cl =
    let
        c ch =
            let
                ch_ =
                    if ch > 4.045e-2 then
                        ((ch + 5.5e-2) / 1.055) ^ 2.4

                    else
                        ch / 12.92
            in
            ch_ * 100

        { red, green, blue, alpha } =
            Color.toRgba cl

        r =
            c red

        g =
            c green

        b =
            c blue
    in
    { x = r * 0.4124 + g * 0.3576 + b * 0.1805
    , y = r * 0.2126 + g * 0.7152 + b * 7.22e-2
    , z = r * 1.93e-2 + g * 0.1192 + b * 0.9505
    , alpha = alpha
    }


xyzToLab : { a | x : Float, y : Float, z : Float, alpha : d } -> { l : Float, a : Float, b : Float, alpha : d }
xyzToLab { x, y, z, alpha } =
    let
        c ch =
            if ch > 8.856e-3 then
                ch ^ (1 / 3)

            else
                (7.787 * ch) + (16 / 116)

        x_ =
            c (x / 95.047)

        y_ =
            c (y / 100)

        z_ =
            c (z / 108.883)
    in
    { l = (116 * y_) - 16
    , a = 500 * (x_ - y_)
    , b = 200 * (y_ - z_)
    , alpha = alpha
    }


fromCIELCH : CIELCHColor -> Color
fromCIELCH args =
    let
        l =
            args.l * 100

        c =
            args.c * 100

        h =
            (args.h - 0.5) * pi * 2
    in
    { l = l
    , a = c * cos h
    , b = c * sin h
    , alpha = args.alpha
    }
        |> labToXyz
        |> xyzToColor


labToXyz : { a | l : Float, a : Float, b : Float, alpha : d } -> { y : Float, x : Float, z : Float, alpha : d }
labToXyz { l, a, b, alpha } =
    let
        c ch =
            let
                ch_ =
                    ch * ch * ch
            in
            if ch_ > 8.856e-3 then
                ch_

            else
                (ch - 16 / 116) / 7.787

        y =
            (l + 16) / 116
    in
    { y = c y * 100
    , x = c (y + a / 500) * 95.047
    , z = c (y - b / 200) * 108.883
    , alpha = alpha
    }


xyzToColor : { a | x : Float, y : Float, z : Float, alpha : Float } -> Color
xyzToColor { x, y, z, alpha } =
    let
        x_ =
            x / 100

        y_ =
            y / 100

        z_ =
            z / 100

        r =
            x_ * 3.2404542 + y_ * -1.5371385 + z_ * -0.4986

        g =
            x_ * -0.969266 + y_ * 1.8760108 + z_ * 4.1556e-2

        b =
            x_ * 5.56434e-2 + y_ * -0.2040259 + z_ * 1.0572252

        c ch =
            let
                ch_ =
                    if ch > 3.1308e-3 then
                        1.055 * (ch ^ (1 / 2.4)) - 5.5e-2

                    else
                        12.92 * ch
            in
            clamp 0 1 ch_
    in
    Color.rgba (c r) (c g) (c b) alpha
