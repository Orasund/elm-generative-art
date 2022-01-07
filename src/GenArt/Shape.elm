module GenArt.Shape exposing
    ( Form
    , Path
    , Shape
    , Triangle
    , Uniforms
    , Varyings
    , Vertex
    , fromConvexVertex
    , fromPath
    , line
    , paths
    , toCanvas
    , toWebGL
    , triangles
    , trianglesAround
    , withPixelShader
    , withVertexShader
    )

import Canvas
import Canvas.Settings as Settings
import Color exposing (Color)
import Data.Point as Point
import Data.Vector as Vector
import GenArt exposing (Dimensions)
import GenArt.Color as Color exposing (Palette)
import Internal.Shape
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector4 exposing (Vec4)
import Random
import WebGL exposing (Shader)
import WebGL.Settings.Blend as Blend


type alias Path =
    { form : List ( Float, Float ), color : Color }


type alias Triangle =
    { form : ( ( Float, Float ), ( Float, Float ), ( Float, Float ) ), color : Color }


type alias Shape =
    Internal.Shape.Shape


type alias Form =
    Internal.Shape.Form


type alias Uniforms =
    { u_dimensions : Vec2
    , u_primary : Vec4
    , u_secondary : Vec4
    , u_trinary : Vec4
    , u_seed : Float
    , u_pixel_args : Vec4
    }


type alias Vertex =
    { position : Vec2
    , color : Vec4
    , args : Vec4
    }


type alias Varyings =
    { v_color : Vec4
    }


withVertexShader : ( Shader Vertex Uniforms Varyings, Vec4 ) -> Shape -> Shape
withVertexShader ( vertexShader, vertexArguments ) shape =
    { shape | vertexShader = vertexShader, vertexArguments = vertexArguments }


withPixelShader : ( Shader {} Uniforms Varyings, Vec4 ) -> Shape -> Shape
withPixelShader ( pixelShader, pixelArguments ) shape =
    { shape | pixelShader = pixelShader, pixelArguments = pixelArguments }


fromPath : Color -> List ( Float, Float ) -> Path
fromPath color list =
    { color = color
    , form = list
    }


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
fromConvexVertex : Color -> List ( Float, Float ) -> List { form : ( ( Float, Float ), ( Float, Float ), ( Float, Float ) ), color : Color }
fromConvexVertex color l =
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
trianglesAround : ( Float, Float ) -> Color -> List ( Float, Float ) -> List Triangle
trianglesAround a color l =
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


paths : List Path -> Shape
paths list =
    list
        |> Internal.Shape.PathsForm
        |> Internal.Shape.new


triangles : List { form : ( ( Float, Float ), ( Float, Float ), ( Float, Float ) ), color : Color } -> Shape
triangles list =
    list
        |> Internal.Shape.TrianglesForm
        |> Internal.Shape.new


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
