module GenArt.Shape exposing
    ( Path
    , Shape
    , Triangle
    , Uniforms
    , Varyings
    , Vertex
    , convexVertex
    , fromPaths
    , fromTriangles
    , line
    , path
    , starVertex
    , toCanvas
    , toSvg
    , toWebGL
    , withPixelShader
    , withVertexShader
    )

{-| This modules is all about shapes. You can think of shapes as things that you can actually see on your seen. (Where as Vertices are very abstract)
Under the hood the module actually only knows of two types of shapes: triangles and lines. Shapes that are filled in are made out of triangles and outlines are made out of lines.
-}

import Canvas
import Canvas.Settings as Settings
import Color exposing (Color)
import Data.Point as Point
import Data.Vector as Vector
import GenArt exposing (Dimensions)
import GenArt.Color as Color exposing (Palette)
import Html exposing (Attribute)
import Internal.Shape
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector4 exposing (Vec4)
import Random
import Svg exposing (Svg)
import Svg.Attributes
import WebGL exposing (Shader)
import WebGL.Settings.Blend as Blend


{-| A path represented as a record. Usually you would pass it right along to `fromPaths`.
-}
type alias Path =
    { form : List ( Float, Float ), color : Color }


{-| A triangle represented as a record. Usually you would pass it onto `fromTriangles`
-}
type alias Triangle =
    { form : ( ( Float, Float ), ( Float, Float ), ( Float, Float ) ), color : Color }


{-| A shape. From here you can either pass it to `toCanvas`, `toWebGL`, `toSvg`
-}
type alias Shape =
    Internal.Shape.Shape


{-| Uniforms that can be used in WebGL shaders
-}
type alias Uniforms =
    { u_dimensions : Vec2
    , u_primary : Vec4
    , u_secondary : Vec4
    , u_trinary : Vec4
    , u_seed : Float
    , u_pixel_args : Vec4
    }


{-| Vertex for vertex shaders
-}
type alias Vertex =
    { position : Vec2
    , color : Vec4
    , args : Vec4
    }


{-| Varyings for webGL shader
-}
type alias Varyings =
    { v_color : Vec4
    }


{-| Adds a vertex shaders. This will only be used in combination with `toWebGL`
-}
withVertexShader : ( Shader Vertex Uniforms Varyings, Vec4 ) -> Shape -> Shape
withVertexShader ( vertexShader, vertexArguments ) shape =
    { shape | vertexShader = vertexShader, vertexArguments = vertexArguments }


{-| Adds a pixel shaders. This will only be used in combination with `toWebGL`
-}
withPixelShader : ( Shader {} Uniforms Varyings, Vec4 ) -> Shape -> Shape
withPixelShader ( pixelShader, pixelArguments ) shape =
    { shape | pixelShader = pixelShader, pixelArguments = pixelArguments }


{-| constructs a path from a list of points
-}
path : Color -> List ( Float, Float ) -> Path
path color list =
    { color = color
    , form = list
    }


{-| constructs a line from a list of points and returns a list of triangles.
It constructs roughly 4 triangles per line segment, so if a lot of lines need to be drawn it might be better to use a path instead.
-}
line : { width : Float, color : Color } -> List ( Float, Float ) -> List Triangle
line args list =
    (case list of
        head :: c :: tail ->
            let
                p =
                    head

                center =
                    c

                right =
                    center
                        |> Point.vecTo p
                        |> Vector.toPolar
                        |> .angle
                        |> (+) (pi / 2)
            in
            Internal.Shape.lineRec args.width
                { from = p
                , pLeft = p |> Point.add (Vector.fromPolar { angle = right + pi, length = args.width / 2 })
                , pRight = p |> Point.add (Vector.fromPolar { angle = right, length = args.width / 2 })
                }
                (c :: tail)
                []

        _ ->
            []
    )
        |> Internal.Shape.withColor args.color


{-| If the points are convex, then no center point is needed to convert into triangles.
-}
convexVertex : Color -> List ( Float, Float ) -> List Triangle
convexVertex color l =
    (case l of
        a :: b :: _ ->
            l
                |> List.foldl
                    (\c args ->
                        { first = args.first
                        , second = c
                        , list = ( args.first, args.second, c ) :: args.list
                        }
                    )
                    { first = a, second = b, list = [] }
                |> .list

        _ ->
            []
    )
        |> Internal.Shape.withColor color


{-| turns a list of points into a filled shape by creating triangles around a given point.
-}
starVertex : ( Float, Float ) -> Color -> List ( Float, Float ) -> List Triangle
starVertex a color l =
    (case l of
        [ x, y, z ] ->
            ( x, y, z ) |> List.singleton

        x :: _ :: _ ->
            let
                rec b list acc =
                    case list of
                        c :: tail ->
                            rec c tail (( a, b, c ) :: acc)

                        [] ->
                            ( a, b, x ) :: acc
            in
            rec x l []

        _ ->
            []
    )
        |> Internal.Shape.withColor color


{-| creates a Shape from a list of paths.
-}
fromPaths : List Path -> Shape
fromPaths list =
    list
        |> Internal.Shape.PathsForm
        |> Internal.Shape.new


{-| creates a shape from a list of triangles
-}
fromTriangles : List Triangle -> Shape
fromTriangles list =
    list
        |> Internal.Shape.TrianglesForm
        |> Internal.Shape.new


{-| Turns a shape into a WebGL entity
-}
toWebGL : { seed : Int, dimensions : Dimensions, palette : Palette } -> Shape -> WebGL.Entity
toWebGL { seed, dimensions, palette } shape =
    let
        toVertex color pixel =
            let
                ( x, y ) =
                    pixel
            in
            { position = Vec2.vec2 x y
            , color = color |> Color.toVec
            , args = shape.vertexArguments
            }

        mesh =
            case shape.form of
                Internal.Shape.PathsForm list ->
                    list
                        |> List.concatMap
                            (\{ form, color } ->
                                case form of
                                    _ :: tail ->
                                        List.map2 (\a b -> ( toVertex color a, toVertex color b )) form tail

                                    _ ->
                                        []
                            )
                        |> WebGL.lines

                Internal.Shape.TrianglesForm list ->
                    list
                        |> List.map
                            (\{ form, color } ->
                                let
                                    ( a, b, c ) =
                                        form
                                in
                                ( toVertex color a, toVertex color b, toVertex color c )
                            )
                        |> WebGL.triangles
    in
    WebGL.entityWith
        [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha ]
        shape.vertexShader
        shape.pixelShader
        mesh
        { u_dimensions = Vec2.vec2 dimensions.width dimensions.height
        , u_primary = palette.primary |> Color.toVec
        , u_secondary = palette.secondary |> Color.toVec
        , u_trinary = palette.trinary |> Color.toVec
        , u_seed =
            Random.step (Random.float 0 1) (Random.initialSeed seed)
                |> Tuple.first
        , u_pixel_args = shape.pixelArguments
        }


{-| Turns a Shape into a renderable Canvas element
-}
toCanvas : Dimensions -> Shape -> List Canvas.Renderable
toCanvas dimensions shape =
    let
        toPixel ( x, y ) =
            ( (1 + x) * dimensions.width / 2, (1 - y) * dimensions.height / 2 )
    in
    case shape.form of
        Internal.Shape.PathsForm list ->
            list
                |> List.concatMap
                    (\{ form, color } ->
                        case form of
                            head :: tail ->
                                Canvas.path (head |> toPixel)
                                    (tail |> List.map toPixel |> List.map Canvas.lineTo)
                                    |> List.singleton
                                    |> Canvas.shapes
                                        [ Settings.stroke color
                                        ]
                                    |> List.singleton

                            [] ->
                                []
                    )

        Internal.Shape.TrianglesForm list ->
            list
                |> List.map
                    (\{ form, color } ->
                        let
                            ( a, b, c ) =
                                form
                        in
                        Canvas.path (toPixel a)
                            [ toPixel b |> Canvas.lineTo
                            , toPixel c |> Canvas.lineTo
                            , toPixel a |> Canvas.lineTo
                            ]
                            |> List.singleton
                            |> Canvas.shapes
                                [ Settings.stroke color --(Color.rgba 0 0 0 0)
                                , Settings.fill color
                                ]
                    )


{-| Turns a Shape into a Svg element
-}
toSvg : Shape -> List (Svg msg)
toSvg shape =
    let
        fromPoint ( x, y ) =
            String.fromFloat x
                ++ " "
                ++ String.fromFloat y

        toPath : List ( Float, Float ) -> Maybe (Attribute msg)
        toPath list =
            case list of
                head :: tail ->
                    [ "M"
                        ++ fromPoint head
                        |> List.singleton
                    , tail
                        |> List.map (\p -> "L" ++ fromPoint p)
                    , List.singleton "Z"
                    ]
                        |> List.concat
                        |> String.join " "
                        |> Svg.Attributes.d
                        |> Just

                [] ->
                    Nothing
    in
    case shape.form of
        Internal.Shape.PathsForm list ->
            list
                |> List.filterMap
                    (\{ form, color } ->
                        form
                            |> toPath
                            |> Maybe.map
                                (\att ->
                                    Svg.path
                                        [ att
                                        , "none"
                                            |> Svg.Attributes.fill
                                        , color
                                            |> Color.toCssString
                                            |> Svg.Attributes.stroke
                                        ]
                                        []
                                )
                    )

        Internal.Shape.TrianglesForm list ->
            list
                |> List.filterMap
                    (\{ form, color } ->
                        let
                            ( a, b, c ) =
                                form
                        in
                        [ a, b, c ]
                            |> toPath
                            |> Maybe.map
                                (\att ->
                                    Svg.path
                                        [ att
                                        , color
                                            |> Color.toCssString
                                            |> Svg.Attributes.fill
                                        , color
                                            |> Color.toCssString
                                            |> Svg.Attributes.stroke
                                        ]
                                        []
                                )
                    )
