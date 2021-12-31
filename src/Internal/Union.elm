module Internal.Union exposing (Union1, Union2, Union3, Union4, Union5, empty1, empty2, empty3, empty4, empty5, mapperFifth, mapperFirst, mapperFourth, mapperSecond, mapperThird)


type alias Union1 a record =
    { record | first : Maybe a }


type alias Union2 a b record =
    Union1 a { record | second : Maybe b }


type alias Union3 a b c record =
    Union2 a b { record | third : Maybe c }


type alias Union4 a b c d record =
    Union3 a b c { record | fourth : Maybe d }


type alias Union5 a b c d e record =
    Union4 a b c d { record | fifth : Maybe e }


empty1 : Union1 a {}
empty1 =
    { first = Nothing }


empty2 : Union2 a b {}
empty2 =
    { first = Nothing
    , second = Nothing
    }


empty3 : Union3 a b c {}
empty3 =
    { first = Nothing
    , second = Nothing
    , third = Nothing
    }


empty4 : Union4 a b c d {}
empty4 =
    { first = Nothing
    , second = Nothing
    , third = Nothing
    , fourth = Nothing
    }


empty5 : Union5 a b c d e {}
empty5 =
    { first = Nothing
    , second = Nothing
    , third = Nothing
    , fourth = Nothing
    , fifth = Nothing
    }


mapperFirst :
    { recordC | first : Maybe config }
    -> { recordM | first : Maybe model }
    -> { recordF | first : Maybe form }
    ->
        { configTo : config -> { recordC | first : Maybe config }
        , configFrom : { recordC | first : Maybe config } -> Maybe config
        , modelTo : model -> { recordM | first : Maybe model }
        , modelFrom : { recordM | first : Maybe model } -> Maybe model
        , formTo : form -> { recordF | first : Maybe form }
        , formFrom : { recordF | first : Maybe form } -> Maybe form
        }
mapperFirst recordC recordM recordF =
    { configTo = \a -> { recordC | first = Just a }
    , configFrom = .first
    , modelTo = \a -> { recordM | first = Just a }
    , modelFrom = .first
    , formTo = \a -> { recordF | first = Just a }
    , formFrom = .first
    }


mapperSecond :
    { recordC | second : Maybe config }
    -> { recordM | second : Maybe model }
    -> { recordF | second : Maybe form }
    ->
        { configTo : config -> { recordC | second : Maybe config }
        , configFrom : { recordC | second : Maybe config } -> Maybe config
        , modelTo : model -> { recordM | second : Maybe model }
        , modelFrom : { recordM | second : Maybe model } -> Maybe model
        , formTo : form -> { recordF | second : Maybe form }
        , formFrom : { recordF | second : Maybe form } -> Maybe form
        }
mapperSecond recordC recordM recordF =
    { configTo = \a -> { recordC | second = Just a }
    , configFrom = .second
    , modelTo = \a -> { recordM | second = Just a }
    , modelFrom = .second
    , formTo = \a -> { recordF | second = Just a }
    , formFrom = .second
    }


mapperThird :
    { recordC | third : Maybe config }
    -> { recordM | third : Maybe model }
    -> { recordF | third : Maybe form }
    ->
        { configTo : config -> { recordC | third : Maybe config }
        , configFrom : { recordC | third : Maybe config } -> Maybe config
        , modelTo : model -> { recordM | third : Maybe model }
        , modelFrom : { recordM | third : Maybe model } -> Maybe model
        , formTo : form -> { recordF | third : Maybe form }
        , formFrom : { recordF | third : Maybe form } -> Maybe form
        }
mapperThird recordC recordM recordF =
    { configTo = \a -> { recordC | third = Just a }
    , configFrom = .third
    , modelTo = \a -> { recordM | third = Just a }
    , modelFrom = .third
    , formTo = \a -> { recordF | third = Just a }
    , formFrom = .third
    }


mapperFourth :
    { recordC | fourth : Maybe config }
    -> { recordM | fourth : Maybe model }
    -> { recordF | fourth : Maybe form }
    ->
        { configTo : config -> { recordC | fourth : Maybe config }
        , configFrom : { recordC | fourth : Maybe config } -> Maybe config
        , modelTo : model -> { recordM | fourth : Maybe model }
        , modelFrom : { recordM | fourth : Maybe model } -> Maybe model
        , formTo : form -> { recordF | fourth : Maybe form }
        , formFrom : { recordF | fourth : Maybe form } -> Maybe form
        }
mapperFourth recordC recordM recordF =
    { configTo = \a -> { recordC | fourth = Just a }
    , configFrom = .fourth
    , modelTo = \a -> { recordM | fourth = Just a }
    , modelFrom = .fourth
    , formTo = \a -> { recordF | fourth = Just a }
    , formFrom = .fourth
    }


mapperFifth :
    { recordC | fifth : Maybe config }
    -> { recordM | fifth : Maybe model }
    -> { recordF | fifth : Maybe form }
    ->
        { configTo : config -> { recordC | fifth : Maybe config }
        , configFrom : { recordC | fifth : Maybe config } -> Maybe config
        , modelTo : model -> { recordM | fifth : Maybe model }
        , modelFrom : { recordM | fifth : Maybe model } -> Maybe model
        , formTo : form -> { recordF | fifth : Maybe form }
        , formFrom : { recordF | fifth : Maybe form } -> Maybe form
        }
mapperFifth recordC recordM recordF =
    { configTo = \a -> { recordC | fifth = Just a }
    , configFrom = .fifth
    , modelTo = \a -> { recordM | fifth = Just a }
    , modelFrom = .fifth
    , formTo = \a -> { recordF | fifth = Just a }
    , formFrom = .fifth
    }
