module GenArt.Program exposing (GenerativeProgram, Target, canvas, fromList, settings, webGL, withView)

import Browser
import GenArt exposing (Dimensions)
import GenArt.Blueprint exposing (Blueprint)
import GenArt.Color exposing (Palette)
import Html exposing (Html)
import Internal.Program exposing (Model(..), Msg, Settings)
import Random exposing (Generator)


type alias Target =
    Internal.Program.Target


type alias GenerativeProgram flags c m f =
    Internal.Program.GenerativeProgram flags c m f


canvas : Target
canvas =
    Internal.Program.Canvas


webGL : Target
webGL =
    Internal.Program.WebGL


fromList :
    (flags
     ->
        { palette : Generator Palette
        , blueprints : List (Blueprint config model form)
        , target : Target
        , fixedSeed : Maybe Int
        }
    )
    -> Settings
    -> GenerativeProgram flags config model form
fromList l { view, dimensions } =
    Browser.element
        { init =
            \flags ->
                flags
                    |> l
                    |> Internal.Program.init
        , view =
            \model ->
                model
                    |> Internal.Program.viewCanvas dimensions
                    |> view
                        (case model of
                            Idle _ ->
                                Nothing

                            Running m ->
                                Just
                                    { seed = m.initialSeed
                                    , palette = m.palette
                                    , progress =
                                        { recursion = m.current.recursion
                                        , maxRecursions = m.maxRecursions
                                        }
                                            |> Just
                                    }

                            Done m ->
                                Just { seed = m.initialSeed, palette = m.palette, progress = Nothing }
                        )
        , update = Internal.Program.update
        , subscriptions = Internal.Program.subscriptions
        }


withView :
    (Maybe { seed : Int, palette : Palette, progress : Maybe { recursion : Int, maxRecursions : Int } }
     -> Html Msg
     -> Html Msg
    )
    -> Settings
    -> Settings
withView view setting =
    { setting | view = view }


settings : Dimensions -> Settings
settings dimensions =
    { dimensions = dimensions
    , view = Internal.Program.defaultView
    }
