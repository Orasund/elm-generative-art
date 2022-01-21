module Internal.Shape exposing (Form(..), Shape, lineRec, new, withColor)

import Color exposing (Color)
import Data.Point as Point
import Data.Vector as Vector
import Math.Vector2 exposing (Vec2)
import Math.Vector4 as Vec4 exposing (Vec4)
import WebGL exposing (Shader)


type alias Vertex =
    { position : Vec2
    , color : Vec4
    , args : Vec4
    }


type alias Varyings =
    { v_color : Vec4
    }


type alias Uniforms =
    { u_dimensions : Vec2
    , u_primary : Vec4
    , u_secondary : Vec4
    , u_trinary : Vec4
    , u_seed : Float
    , u_pixel_args : Vec4
    }


type alias Path =
    { form : List ( Float, Float ), color : Color }


type alias Triangle =
    { form : ( ( Float, Float ), ( Float, Float ), ( Float, Float ) ), color : Color }


type alias Shape =
    { form : Form
    , vertexShader : Shader Vertex Uniforms Varyings
    , pixelShader : Shader {} Uniforms Varyings
    , vertexArguments : Vec4
    , pixelArguments : Vec4
    }


type Form
    = PathsForm (List Path)
    | TrianglesForm (List Triangle)


{-|

    rightTurning        leftTurning

                                      \ to\
    pLeft    cLeft    pc   pLeft  cLeft\   x p0
    x-------------x---x     x-----------x   |     ^ left
    from        center|     from      center|     |
    x-------------x   |     x-----------x---x     v right
    pRight cRight/   x p0   pRight cRight    pc
                / to/

-}
lineRec :
    Float
    ->
        { from : ( Float, Float )
        , pLeft : ( Float, Float )
        , pRight : ( Float, Float )
        }
    -> List ( Float, Float )
    -> List ( ( Float, Float ), ( Float, Float ), ( Float, Float ) )
    -> List ( ( Float, Float ), ( Float, Float ), ( Float, Float ) )
lineRec width { from, pLeft, pRight } list acc =
    case list of
        center :: tail ->
            let
                v1 =
                    center |> Point.vecTo from |> Vector.toPolar

                right =
                    v1.angle + (pi / 2)

                v2 to =
                    center |> Point.vecTo to |> Vector.toPolar

                {--angle from v1 to v2 --}
                angleBetween =
                    tail
                        |> List.head
                        |> Maybe.map (\to -> (v2 to).angle - v1.angle)
                        |> Maybe.withDefault pi

                isLeftTurning =
                    (if angleBetween > pi then
                        angleBetween - 2 * pi

                     else if angleBetween < -pi then
                        angleBetween + 2 * pi

                     else
                        angleBetween
                    )
                        <= 0

                newRight =
                    tail
                        |> List.head
                        |> Maybe.map (\to -> (v2 to).angle - pi / 2)
                        |> Maybe.withDefault right

                ( length2, angle2 ) =
                    let
                        tempLength =
                            (width / 2) / sin (angleBetween / 2)

                        tempAngle =
                            v1.angle + angleBetween / 2

                        maxLength =
                            tail
                                |> List.head
                                |> Maybe.map (\to -> min v1.length (v2 to).length)
                                |> Maybe.withDefault v1.length
                    in
                    if tempLength < 0 then
                        ( min -tempLength maxLength
                        , tempAngle + pi
                        )

                    else
                        ( min tempLength maxLength
                        , tempAngle
                        )

                {- Point left from `center` -}
                cLeft =
                    center
                        |> (if not isLeftTurning then
                                Point.add (Vector.fromPolar { angle = right + pi, length = width / 2 })

                            else
                                Point.add (Vector.fromPolar { angle = angle2 + pi, length = length2 })
                           )

                {- Point right of `center ` -}
                cRight =
                    center
                        |> (if isLeftTurning then
                                Point.add (Vector.fromPolar { angle = right, length = width / 2 })

                            else
                                Point.add (Vector.fromPolar { angle = angle2, length = length2 })
                           )

                {- outer point of the `center` -}
                pc =
                    center
                        |> Point.add
                            (Vector.fromPolar
                                { angle =
                                    if isLeftTurning then
                                        angle2

                                    else
                                        angle2 + pi
                                , length = width / 2
                                }
                            )

                {- new (normal) point to `center` (in relation to `to`) -}
                p0 =
                    center
                        |> Point.add
                            (Vector.fromPolar
                                { angle =
                                    if isLeftTurning then
                                        newRight

                                    else
                                        newRight + pi
                                , length = width / 2
                                }
                            )

                version1 =
                    [ ( pLeft, pRight, cRight )
                    , ( cRight, cLeft, pLeft )
                    , ( cRight, cLeft, pc )
                    , if isLeftTurning then
                        ( cLeft, pc, p0 )

                      else
                        ( cRight, pc, p0 )
                    ]
            in
            lineRec width
                { from = center
                , pLeft =
                    if isLeftTurning then
                        cLeft

                    else
                        p0
                , pRight =
                    if isLeftTurning then
                        p0

                    else
                        cRight
                }
                tail
                version1
                ++ acc

        [] ->
            acc


withColor : Color -> List form -> List { form : form, color : Color }
withColor color =
    List.map (\form -> { form = form, color = color })


new : Form -> Shape
new form =
    { form = form
    , vertexShader = [glsl|
            precision mediump float;
            attribute vec2 position;
            attribute vec4 color;
            attribute vec4 args;
            varying vec4 v_color;

            void main () {
                gl_Position = vec4(position,0.0, 1.0);
                v_color = color;
            }
        |]
    , pixelShader = [glsl|
            precision mediump float;
            varying vec4 v_color;
            uniform vec2 u_dimensions;

            void main () {
                gl_FragColor = v_color;
            }
        |]
    , vertexArguments = Vec4.vec4 0 0 0 0
    , pixelArguments = Vec4.vec4 0 0 0 0
    }
