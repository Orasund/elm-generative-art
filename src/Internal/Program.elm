module Internal.Program exposing (GenerativeProgram, Model(..), Msg, Settings, Target(..), defaultView, init, subscriptions, update, viewCanvas)

import Canvas
import Color
import GenArt exposing (Dimensions)
import GenArt.Blueprint exposing (Blueprint)
import GenArt.Color exposing (Palette)
import GenArt.Shape as Shape exposing (Shape)
import Html exposing (Html)
import Html.Attributes as Attributes
import Random exposing (Generator, Seed)
import Simplex exposing (PermutationTable)
import Task
import Time
import WebGL


type alias InternalProgram config model form =
    { palette : Generator Palette
    , blueprints : List (Blueprint config model form)
    , target : Target
    }


type Target
    = WebGL
    | Canvas


type alias Settings =
    { dimensions : Dimensions
    , view :
        Maybe { seed : Int, palette : Palette, progress : Maybe { recursion : Int, maxRecursions : Int } }
        -> Html Msg
        -> Html Msg
    }


type alias GenerativeProgram flags c m f =
    Program flags (Model c m f) Msg


type alias RunningModel c m f =
    { config : c
    , maxRecursions : Int
    , stack : List { state : m, recursion : Int }
    , current : { state : m, recursion : Int }
    , seed : Seed
    , animations : List (Blueprint c m f)
    , initialSeed : Int
    , permutationTable : PermutationTable
    , palette : Palette
    , shapes : List Shape
    , forms : List f
    , program : InternalProgram c m f
    }


type Model c m f
    = Idle (InternalProgram c m f)
    | Running (RunningModel c m f)
    | Done
        { program : InternalProgram c m f
        , initialSeed : Int
        , shapes : List Shape
        , palette : Palette
        }


type Msg
    = Frame
    | GotSeed Int


init :
    { palette : Generator Palette
    , blueprints : List (Blueprint c m f)
    , target : Target
    , fixedSeed : Maybe Int
    }
    -> ( Model c m f, Cmd Msg )
init args =
    ( Idle
        { palette = args.palette
        , blueprints = args.blueprints
        , target = args.target
        }
    , case args.fixedSeed of
        Nothing ->
            Random.generate GotSeed (Random.int (-2 ^ 31) (2 ^ 31 - 1))

        Just seed ->
            Task.perform GotSeed (Task.succeed seed)
    )


addShape : Target -> RunningModel c m f -> RunningModel c m f
addShape target model =
    model
        |> createShapes
        |> Random.map
            (\forms ->
                { model
                    | forms =
                        if target == WebGL then
                            model.forms ++ forms

                        else
                            forms
                }
            )
        |> GenArt.applyGenerator model.seed


initRunning :
    { program : InternalProgram c m f
    , palette : Palette
    , blueprints : List (Blueprint c m f)
    , initialSeed : Int
    }
    -> List Shape
    -> Model c m f
initRunning { program, palette, blueprints, initialSeed } shapes =
    let
        seed =
            Random.initialSeed initialSeed
    in
    case blueprints of
        head :: _ ->
            head.randomConfig
                |> Random.andThen
                    (\{ config, maxRecursions } ->
                        Random.map2
                            (\state permutationTable ->
                                { config = config
                                , current = { state = state, recursion = 1 }
                                , stack = []
                                , maxRecursions = maxRecursions
                                , seed = seed
                                , animations = blueprints
                                , initialSeed = initialSeed
                                , permutationTable = permutationTable
                                , palette = palette
                                , shapes = shapes
                                , forms = []
                                , program = program
                                }
                            )
                            (head.init config)
                            Simplex.permutationTableGenerator
                    )
                |> GenArt.applyGenerator seed
                |> addShape program.target
                |> Running

        [] ->
            Done { program = program, initialSeed = initialSeed, shapes = shapes, palette = palette }


createShapes : RunningModel c m f -> Generator (List f)
createShapes m =
    case m.animations of
        animation :: _ ->
            m.current.state
                |> animation.view
                    { config = m.config
                    , recursion = m.current.recursion
                    , maxRecursions = m.maxRecursions
                    , permutationTable = m.permutationTable
                    }
                    m.palette

        [] ->
            Random.constant []


viewCanvas : Dimensions -> Model c m f -> Html Msg
viewCanvas dimensions model =
    let
        drawing target forms shapes palette seed =
            case target of
                Canvas ->
                    forms
                        |> List.concatMap (Shape.toCanvas dimensions)
                        |> Canvas.toHtml
                            ( round dimensions.width, round dimensions.height )
                            [ Attributes.style "height" "100%"
                            , Attributes.style "object-fit" "contain"
                            ]

                WebGL ->
                    shapes
                        ++ forms
                        |> List.map
                            (Shape.toWebGL
                                { dimensions = dimensions
                                , palette = palette
                                , seed = seed
                                }
                            )
                        |> WebGL.toHtmlWith
                            [ WebGL.antialias
                            , WebGL.depth 1

                            --, WebGL.alpha True
                            ]
                            [ Attributes.width (round dimensions.width)
                            , Attributes.height (round dimensions.height)
                            , Attributes.style "display" "block"
                            , Attributes.style "height" "100%"
                            , Attributes.style "object-fit" "contain"
                            ]
    in
    case model of
        Idle _ ->
            Html.text ""

        Running m ->
            let
                forms =
                    case m.animations of
                        blueprint :: _ ->
                            blueprint.toShape m.config m.forms

                        [] ->
                            []
            in
            drawing m.program.target forms m.shapes m.palette m.initialSeed

        Done m ->
            drawing m.program.target [] m.shapes m.palette m.initialSeed


defaultView :
    Maybe { seed : Int, palette : Palette, progress : Maybe { recursion : Int, maxRecursions : Int } }
    -> Html Msg
    -> Html Msg
defaultView model canvas =
    let
        drawPalette palette =
            Html.div [ Attributes.style "display" "flex", Attributes.style "flex-direction" "row", Attributes.style "height" "30px" ]
                [ Html.div
                    [ Attributes.style "width" "30px"
                    , Attributes.style "height" "30px"
                    , Attributes.style "background-color" (palette.primary |> Color.toCssString)
                    ]
                    []
                , Html.div
                    [ Attributes.style "width" "30px"
                    , Attributes.style "height" "30px"
                    , Attributes.style "background-color" (palette.secondary |> Color.toCssString)
                    ]
                    []
                , Html.div
                    [ Attributes.style "width" "30px"
                    , Attributes.style "height" "30px"
                    , Attributes.style "background-color" (palette.trinary |> Color.toCssString)
                    ]
                    []
                ]
    in
    Html.div
        [ Attributes.style "display" "flex"
        , Attributes.style "justify-content" "center"
        , Attributes.style "align-items" "center"
        , Attributes.style "flex-direction" "column"
        ]
        [ canvas
        , Html.div
            [ Attributes.style "display" "flex"
            , Attributes.style "flex-direction" "column"
            ]
            (case model of
                Just m ->
                    [ "Seed:" ++ String.fromInt m.seed |> Html.text
                    , case m.progress of
                        Just progress ->
                            ("Progress:"
                                ++ String.fromInt progress.recursion
                                ++ "/"
                                ++ String.fromInt progress.maxRecursions
                            )
                                |> Html.text

                        Nothing ->
                            "Done" |> Html.text
                    , drawPalette m.palette
                    ]

                Nothing ->
                    []
            )
        ]


update : Msg -> Model c m f -> ( Model c m f, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Frame, Running m ) ->
            case m.animations of
                animation :: tailAnimations ->
                    if m.current.recursion >= m.maxRecursions then
                        case m.stack of
                            head :: tail ->
                                ( { m | current = head, stack = tail }
                                    |> addShape m.program.target
                                    |> Running
                                , Cmd.none
                                )

                            [] ->
                                ( initRunning
                                    { program = m.program
                                    , palette = m.palette
                                    , blueprints = tailAnimations
                                    , initialSeed = m.initialSeed
                                    }
                                    (m.shapes ++ (m.forms |> animation.toShape m.config))
                                , Cmd.none
                                )

                    else
                        ( Random.step
                            (m.current.state
                                |> animation.update
                                    { config = m.config
                                    , recursion = m.current.recursion
                                    , maxRecursions = m.maxRecursions
                                    , permutationTable = m.permutationTable
                                    }
                            )
                            m.seed
                            |> (\( list, seed ) ->
                                    case list of
                                        head :: tail ->
                                            { m
                                                | current =
                                                    { state = head
                                                    , recursion = m.current.recursion + 1
                                                    }
                                                , seed = seed
                                                , stack =
                                                    (tail
                                                        |> List.map
                                                            (\state ->
                                                                { state = state
                                                                , recursion = m.current.recursion + 1
                                                                }
                                                            )
                                                    )
                                                        ++ m.stack
                                            }
                                                |> addShape m.program.target
                                                |> Running

                                        [] ->
                                            case m.stack of
                                                head :: tail ->
                                                    Running
                                                        { m
                                                            | current = head
                                                            , seed = seed
                                                            , stack = tail
                                                        }

                                                [] ->
                                                    Idle m.program
                               )
                        , Cmd.none
                        )

                [] ->
                    ( Idle m.program, Cmd.none )

        ( GotSeed seed, Idle program ) ->
            ( Random.step
                (program.palette
                    |> Random.map
                        (\palette ->
                            initRunning
                                { program = program
                                , palette = palette
                                , blueprints = program.blueprints
                                , initialSeed = seed
                                }
                                []
                        )
                )
                (Random.initialSeed <| seed)
                |> Tuple.first
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


subscriptions : Model c m f -> Sub Msg
subscriptions model =
    let
        intervall : Float
        intervall =
            50
    in
    case model of
        Idle _ ->
            Sub.none

        Running _ ->
            Time.every intervall (\_ -> Frame)

        Done _ ->
            Sub.none
