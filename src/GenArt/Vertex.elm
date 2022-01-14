module GenArt.Vertex exposing (arc, circle, circleStencil, rect, regularPolygon, square)

{-| A Vertex is a point where two lines lines. You can describe any shape as a list of vertices.

This module provices various ways to construct convex and star shapes as vertices.

-}

import Data.Point as Point


{-|

    square =
        rect ( 0, 0 ) ( 1, 1 )

-}
square : List ( number, number )
square =
    rect ( 0, 0 ) ( 1, 1 )


{-|

    rect ( x1, y1 ) ( x2, y2 ) =
        [ ( x1, y1 ), ( x1, y2 ), ( x2, y2 ), ( x2, y1 ) ]

-}
rect : ( number, number ) -> ( number, number ) -> List ( number, number )
rect ( x1, y1 ) ( x2, y2 ) =
    [ ( x1, y1 ), ( x1, y2 ), ( x2, y2 ), ( x2, y1 ) ]


{-| Constructs a circle by a given radius and point. It will construct a polygon with 40 sides.
-}
circle : Float -> ( Float, Float ) -> List ( Float, Float )
circle radius p =
    regularPolygon { points = 40, radius = radius, angleOffset = 0 } p


{-| Constructs a stencil of a circle using the circle function as a basis.
-}
circleStencil : { radius : Float, min : ( Float, Float ), max : ( Float, Float ) } -> ( Float, Float ) -> List ( ( Float, Float ), List ( Float, Float ) )
circleStencil args ( x, y ) =
    let
        ( minX, minY ) =
            args.min

        ( maxX, maxY ) =
            args.max
    in
    [ { angleOffset = 0, list = [ ( x, maxY ), ( maxX, maxY ), ( maxX, y ) ], p = ( maxX, maxY ) }
    , { angleOffset = 1 / 4, list = [ ( minX, y ), ( minX, maxY ), ( x, maxY ) ], p = ( minX, maxY ) }
    , { angleOffset = 2 / 4, list = [ ( x, minY ), ( minX, minY ), ( minX, y ) ], p = ( minX, minY ) }
    , { angleOffset = 3 / 4, list = [ ( maxX, y ), ( maxX, minY ), ( x, minY ) ], p = ( maxX, minY ) }
    ]
        |> List.map
            (\{ angleOffset, list, p } ->
                ( p
                , arc
                    { points = 35
                    , radius = args.radius
                    , angle = pi / 2
                    , angleOffset = angleOffset * 2 * pi
                    }
                    ( x, y )
                    ++ list
                )
            )


{-| Constructs a regular polygon. angleOffset is between 0 and 2\*pi.
-}
regularPolygon : { points : Int, radius : Float, angleOffset : Float } -> ( Float, Float ) -> List ( Float, Float )
regularPolygon args p =
    p
        |> arc
            { points = args.points
            , radius = args.radius
            , angle = 2 * pi
            , angleOffset = args.angleOffset
            }
        |> List.take args.points


{-| constructs an arc around a point
-}
arc : { points : Int, radius : Float, angle : Float, angleOffset : Float } -> ( Float, Float ) -> List ( Float, Float )
arc args p =
    List.range 0 args.points
        |> List.map
            (\i ->
                fromPolar ( args.radius, args.angleOffset + args.angle * toFloat i / toFloat args.points )
                    |> Point.toVec
                    |> Point.addTo p
            )
