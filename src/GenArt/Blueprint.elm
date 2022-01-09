module GenArt.Blueprint exposing
    ( Blueprint
    , BlueprintState
    , ImageProperties
    , IteratedState
    , RenderingState
    , Stateless
    , from1
    , from2
    , from3
    , from4
    , from5
    , fromShape
    , iterated
    , map
    , mapConfig
    , mapPalette
    , mapRandomConfig
    , rendering
    )

import GenArt.Color exposing (Palette)
import GenArt.Shape exposing (Shape)
import Internal.Union as Union exposing (Union1, Union2, Union3, Union4, Union5)
import Random exposing (Generator)
import Simplex exposing (PermutationTable)


type alias ImageProperties =
    { palette : Palette
    }


type alias BlueprintState config =
    { config : config
    , recursion : Int
    , maxRecursions : Int
    , permutationTable : PermutationTable
    }


type alias IteratedState config =
    { config : config
    , maxSteps : Int
    , step : Int
    , permutationTable : PermutationTable
    }


type alias RenderingState config =
    { config : config, permutationTable : PermutationTable }


type alias Blueprint config model form =
    { config : Generator { config : config, maxRecursions : Int }
    , init : config -> Generator model
    , update : BlueprintState config -> model -> Generator (List model)
    , view : BlueprintState config -> ImageProperties -> model -> Generator (List form)
    , toShape : config -> List form -> List Shape
    }


type alias Stateless config form =
    { config : Generator { config : config, maxSteps : Int }
    , view : IteratedState config -> ImageProperties -> Generator (List form)
    , toShape : config -> List form -> List Shape
    }


iterated :
    { config : Generator { config : config, maxSteps : Int }
    , init : config -> Generator model
    , update : IteratedState config -> model -> Generator model
    , view : IteratedState config -> ImageProperties -> model -> Generator (List form)
    , toShape : config -> List form -> List Shape
    }
    -> Blueprint config model form
iterated blueprint =
    { config =
        blueprint.config
            |> Random.map (\{ config, maxSteps } -> { config = config, maxRecursions = maxSteps })
    , init = blueprint.init
    , update =
        \state model ->
            blueprint.update
                { step = state.recursion
                , maxSteps = state.maxRecursions
                , config = state.config
                , permutationTable = state.permutationTable
                }
                model
                |> Random.map List.singleton
    , view =
        \state imageProperties ->
            blueprint.view
                { step = state.recursion
                , maxSteps = state.maxRecursions
                , config = state.config
                , permutationTable = state.permutationTable
                }
                imageProperties
    , toShape = blueprint.toShape
    }


rendering :
    { config : Generator config
    , view : RenderingState config -> ImageProperties -> Generator (List Shape)
    }
    -> Blueprint config {} Shape
rendering r =
    iterated
        { config = r.config |> Random.map (\config -> { config = config, maxSteps = 1 })
        , init = \_ -> Random.constant {}
        , update = \_ model -> Random.constant model
        , view =
            \state imageProperties _ ->
                r.view
                    { config = state.config
                    , permutationTable = state.permutationTable
                    }
                    imageProperties
        , toShape = \_ -> identity
        }


fromShape : (Palette -> Generator (List Shape)) -> Blueprint {} {} Shape
fromShape view =
    rendering
        { config = Random.constant {}
        , view = \_ { palette } -> view palette
        }


mapPalette : (Palette -> Palette) -> Blueprint config model form -> Blueprint config model form
mapPalette fun animation =
    { animation
        | view =
            \state imageProperties ->
                animation.view state
                    { imageProperties | palette = fun imageProperties.palette }
    }


mapConfig : (config -> config) -> Blueprint config model form -> Blueprint config model form
mapConfig fun animation =
    { animation
        | config =
            animation.config
                |> Random.map
                    (\{ config, maxRecursions } ->
                        { maxRecursions = maxRecursions
                        , config = fun config
                        }
                    )
    }


mapRandomConfig :
    (config -> Generator config)
    -> Blueprint config model form
    -> Blueprint config model form
mapRandomConfig fun animation =
    { animation
        | config =
            animation.config
                |> Random.andThen
                    (\{ config, maxRecursions } ->
                        fun config
                            |> Random.map
                                (\c ->
                                    { config = c, maxRecursions = maxRecursions }
                                )
                    )
    }


map :
    { configTo : config1 -> config2
    , configFrom : config2 -> Maybe config1
    , modelTo : model1 -> model2
    , modelFrom : model2 -> Maybe model1
    , formTo : form1 -> form2
    , formFrom : form2 -> Maybe form1
    }
    -> Blueprint config1 model1 form1
    -> Blueprint config2 model2 form2
map args anim =
    let
        maybeConfigToConfig : Maybe config1 -> config1
        maybeConfigToConfig c =
            case c of
                Just a ->
                    a

                Nothing ->
                    Random.step
                        anim.config
                        (Random.initialSeed 0)
                        |> Tuple.first
                        |> .config

        maybeModelToModel c =
            case c of
                Just a ->
                    a

                Nothing ->
                    Random.step
                        (anim.config
                            |> Random.map .config
                            |> Random.andThen anim.init
                        )
                        (Random.initialSeed 0)
                        |> Tuple.first
    in
    { config =
        anim.config
            |> Random.map (\a -> { config = args.configTo a.config, maxRecursions = a.maxRecursions })
    , init =
        \config ->
            args.configFrom config
                |> maybeConfigToConfig
                |> anim.init
                |> Random.map args.modelTo
    , update =
        \state model ->
            anim.update
                { config = args.configFrom state.config |> maybeConfigToConfig
                , maxRecursions = state.maxRecursions
                , recursion = state.recursion
                , permutationTable = state.permutationTable
                }
                (args.modelFrom model |> maybeModelToModel)
                |> Random.map (List.map args.modelTo)
    , view =
        \state imageProperties model ->
            anim.view
                { config = args.configFrom state.config |> maybeConfigToConfig
                , maxRecursions = state.maxRecursions
                , recursion = state.recursion
                , permutationTable = state.permutationTable
                }
                imageProperties
                (args.modelFrom model |> maybeModelToModel)
                |> Random.map (List.map args.formTo)
    , toShape =
        \config list ->
            list
                |> List.filterMap args.formFrom
                |> anim.toShape
                    (args.configFrom config |> maybeConfigToConfig)
    }



-------------------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------------------


from5 :
    Blueprint configA modelA formA
    -> Blueprint configB modelB formB
    -> Blueprint configC modelC formC
    -> Blueprint configD modelD formD
    -> Blueprint configE modelE formE
    ->
        List
            (Blueprint
                (Union5 configA configB configC configD configE {})
                (Union5 modelA modelB modelC modelD modelE {})
                (Union5 formA formB formC formD formE {})
            )
from5 a b c d e =
    let
        empty =
            Union.empty5
    in
    [ a |> map (Union.mapperFirst empty empty empty)
    , b |> map (Union.mapperSecond empty empty empty)
    , c |> map (Union.mapperThird empty empty empty)
    , d |> map (Union.mapperFourth empty empty empty)
    , e |> map (Union.mapperFifth empty empty empty)
    ]


from4 :
    Blueprint configA modelA formA
    -> Blueprint configB modelB formB
    -> Blueprint configC modelC formC
    -> Blueprint configD modelD formD
    ->
        List
            (Blueprint
                (Union4 configA configB configC configD {})
                (Union4 modelA modelB modelC modelD {})
                (Union4 formA formB formC formD {})
            )
from4 a b c d =
    let
        empty =
            Union.empty4
    in
    [ a |> map (Union.mapperFirst empty empty empty)
    , b |> map (Union.mapperSecond empty empty empty)
    , c |> map (Union.mapperThird empty empty empty)
    , d |> map (Union.mapperFourth empty empty empty)
    ]


from3 :
    Blueprint configA modelA formA
    -> Blueprint configB modelB formB
    -> Blueprint configC modelC formC
    ->
        List
            (Blueprint
                (Union3 configA configB configC {})
                (Union3 modelA modelB modelC {})
                (Union3 formA formB formC {})
            )
from3 a b c =
    let
        empty =
            Union.empty3
    in
    [ a |> map (Union.mapperFirst empty empty empty)
    , b |> map (Union.mapperSecond empty empty empty)
    , c |> map (Union.mapperThird empty empty empty)
    ]


from2 :
    Blueprint configA modelA formA
    -> Blueprint configB modelB formB
    ->
        List
            (Blueprint
                (Union2 configA configB {})
                (Union2 modelA modelB {})
                (Union2 formA formB {})
            )
from2 a b =
    let
        empty =
            Union.empty2
    in
    [ a |> map (Union.mapperFirst empty empty empty)
    , b |> map (Union.mapperSecond empty empty empty)
    ]


from1 :
    Blueprint configA modelA formA
    ->
        List
            (Blueprint
                (Union1 configA {})
                (Union1 modelA {})
                (Union1 formA {})
            )
from1 a =
    let
        empty =
            Union.empty1
    in
    [ a |> map (Union.mapperFirst empty empty empty)
    ]
