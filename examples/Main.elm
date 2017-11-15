module Main exposing (main)

import BeautifulExample
import Color
import Element exposing (Element)
import Element.Attributes as Attrs
import Element.Events as Events
import Element.Input as Input exposing (Text)
import Html exposing (Html)
import Http
    exposing
        ( Error
            ( BadUrl
            , Timeout
            , NetworkError
            , BadStatus
            , BadPayload
            )
        )
import RemoteData
    exposing
        ( RemoteData
            ( NotAsked
            , Loading
            , Failure
            , Success
            )
        , WebData
        )
import Style exposing (StyleSheet)
import Style.Font as Font
import Style.Shadow as Shadow
import ThreeWords exposing (Key, Position, ThreeWords)


type alias Model =
    { fromPosition : FromPosition
    , toPosition : ToPosition
    }


type alias FromPosition =
    { key : Input String
    , latitude : Input Float
    , longitude : Input Float
    , result : WebData ThreeWords
    }


type Input a
    = Empty
    | Error String String
    | Correct a


type alias ToPosition =
    { key : Input String
    , threeWords : Input ( ThreeWords, Char )
    , result : WebData Position
    }


type Msg
    = FromPositionKeyChange (Input String)
    | LatitudeChange (Input Float)
    | LongitudeChange (Input Float)
    | FromPositionClicked
    | FromPositionResult (WebData ThreeWords)
    | ToPositionKeyChange (Input String)
    | ThreeWordsChange (Input ( ThreeWords, Char ))
    | ToPositionClicked
    | ToPositionResult (WebData Position)


main : Program Never Model Msg
main =
    BeautifulExample.program
        { title = "Three Words"
        , details = Just """
        what3words is the simplest way to talk about any precise location. This
        system has divided the world into a grid of three meters by three meters
        squares and assigned each one a unique address made of just 3 words. Now
        everyone and everywhere has a reliable address. Before you get started,
        you'll need to register at https://accounts.what3words.com/register.
        This will give you a unique key which is needed for using the API.
        """
        , color = Just Color.blue
        , maxWidth = 1000
        , githubUrl = Just "https://github.com/cjduncana/three-words"
        , documentationUrl = Nothing
        }
        { init = ( initModel, Cmd.none )
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


initModel : Model
initModel =
    { fromPosition = FromPosition Empty Empty Empty NotAsked
    , toPosition = ToPosition Empty Empty NotAsked
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FromPositionKeyChange key ->
            { model
                | fromPosition =
                    FromPosition
                        key
                        model.fromPosition.latitude
                        model.fromPosition.longitude
                        model.fromPosition.result
            }
                |> flip (,) Cmd.none

        LatitudeChange latitude ->
            { model
                | fromPosition =
                    FromPosition
                        model.fromPosition.key
                        latitude
                        model.fromPosition.longitude
                        model.fromPosition.result
            }
                |> flip (,) Cmd.none

        LongitudeChange longitude ->
            { model
                | fromPosition =
                    FromPosition
                        model.fromPosition.key
                        model.fromPosition.latitude
                        longitude
                        model.fromPosition.result
            }
                |> flip (,) Cmd.none

        FromPositionClicked ->
            let
                maybeCmd =
                    Maybe.map2 Position
                        (imputToMaybe model.fromPosition.latitude)
                        (imputToMaybe model.fromPosition.longitude)
                        |> Maybe.map2 ThreeWords.fromPosition
                            (imputToMaybe model.fromPosition.key)
                        |> Maybe.map
                            (RemoteData.sendRequest
                                >> Cmd.map FromPositionResult
                            )
            in
                case maybeCmd of
                    Just cmd ->
                        { model
                            | fromPosition =
                                FromPosition
                                    model.fromPosition.key
                                    model.fromPosition.latitude
                                    model.fromPosition.longitude
                                    Loading
                        }
                            |> flip (,) cmd

                    Nothing ->
                        { model
                            | fromPosition =
                                FromPosition
                                    (missingInput
                                        "You must provide a key"
                                        model.fromPosition.key
                                    )
                                    (missingInput
                                        "You must provide a latitude"
                                        model.fromPosition.latitude
                                    )
                                    (missingInput
                                        "You must provide a longitude"
                                        model.fromPosition.longitude
                                    )
                                    NotAsked
                        }
                            |> flip (,) Cmd.none

        FromPositionResult result ->
            { model
                | fromPosition =
                    FromPosition
                        model.fromPosition.key
                        model.fromPosition.latitude
                        model.fromPosition.longitude
                        result
            }
                |> flip (,) Cmd.none

        ToPositionKeyChange key ->
            { model
                | toPosition =
                    ToPosition
                        key
                        model.toPosition.threeWords
                        model.toPosition.result
            }
                |> flip (,) Cmd.none

        ThreeWordsChange threeWords ->
            { model
                | toPosition =
                    ToPosition
                        model.toPosition.key
                        threeWords
                        model.toPosition.result
            }
                |> flip (,) Cmd.none

        ToPositionClicked ->
            let
                maybeCmd =
                    Maybe.map2 ThreeWords.toPosition
                        (imputToMaybe model.toPosition.key)
                        (imputToMaybe model.toPosition.threeWords
                            |> Maybe.map Tuple.first
                        )
                        |> Maybe.map
                            (RemoteData.sendRequest
                                >> Cmd.map ToPositionResult
                            )
            in
                case maybeCmd of
                    Just cmd ->
                        { model
                            | toPosition =
                                ToPosition
                                    model.toPosition.key
                                    model.toPosition.threeWords
                                    Loading
                        }
                            |> flip (,) cmd

                    Nothing ->
                        { model
                            | toPosition =
                                ToPosition
                                    (missingInput
                                        "You must provide a key"
                                        model.toPosition.key
                                    )
                                    (missingInput
                                        "You must provide three words"
                                        model.toPosition.threeWords
                                    )
                                    NotAsked
                        }
                            |> flip (,) Cmd.none

        ToPositionResult result ->
            { model
                | toPosition =
                    ToPosition
                        model.toPosition.key
                        model.toPosition.threeWords
                        result
            }
                |> flip (,) Cmd.none


view : Model -> Html Msg
view model =
    Element.layout styleSheet
        (Element.row NoStyle
            [ Attrs.spacing 16 ]
            [ fromPositionColumn model.fromPosition
                |> Element.column NoStyle [ Attrs.spacing 16 ]
                |> Element.el NoStyle [ Attrs.width Attrs.fill ]
            , toPositionColumn model.toPosition
                |> Element.column NoStyle [ Attrs.spacing 16 ]
                |> Element.el NoStyle [ Attrs.width Attrs.fill ]
            ]
        )


fromPositionColumn : FromPosition -> List (Element Styles Variations Msg)
fromPositionColumn { key, latitude, longitude, result } =
    [ Element.text "Position → Three Words"
        |> Element.h2 Header []
    , Input.text InputBox [ Attrs.vary ErrorOutline (isError key) ] <|
        case key of
            Empty ->
                fromPositionKeyOptions

            Error input error ->
                { fromPositionKeyOptions
                    | value = input
                    , options =
                        [ Element.text error
                            |> Input.errorBelow
                        ]
                }

            Correct value ->
                { fromPositionKeyOptions | value = value }
    , Input.text InputBox [ Attrs.vary ErrorOutline (isError latitude) ] <|
        case latitude of
            Empty ->
                latitudeOptions

            Error input error ->
                { latitudeOptions
                    | value = input
                    , options =
                        [ Element.text error
                            |> Input.errorBelow
                        ]
                }

            Correct value ->
                { latitudeOptions | value = toString value }
    , Input.text InputBox [ Attrs.vary ErrorOutline (isError longitude) ] <|
        case longitude of
            Empty ->
                longitudeOptions

            Error input error ->
                { longitudeOptions
                    | value = input
                    , options =
                        [ Element.text error
                            |> Input.errorBelow
                        ]
                }

            Correct value ->
                { longitudeOptions | value = toString value }
    , Element.text "Get Three Words"
        |> Element.button NoStyle [ Events.onClick FromPositionClicked ]
    , Element.paragraph NoStyle [] <|
        List.singleton <|
            case result of
                NotAsked ->
                    Element.empty

                Loading ->
                    Element.text "Loading..."

                Failure err ->
                    Element.text <|
                        case err of
                            BadUrl url ->
                                "The URL provided \"" ++ url ++ "\" does not work."

                            Timeout ->
                                "Too much time has passed."

                            NetworkError ->
                                "There's no internet connection."

                            BadStatus { status } ->
                                "A " ++ toString status.code ++ " error was returned by the server."

                            BadPayload error _ ->
                                error

                Success ( a, b, c ) ->
                    [ a, b, c ]
                        |> String.join "."
                        |> (++) "///"
                        |> Element.text
    ]


toPositionColumn : ToPosition -> List (Element Styles Variations Msg)
toPositionColumn { key, threeWords, result } =
    [ Element.text "Three Words → Position"
        |> Element.h2 Header []
    , Input.text InputBox [ Attrs.vary ErrorOutline (isError key) ] <|
        case key of
            Empty ->
                toPositionKeyOptions

            Error input error ->
                { toPositionKeyOptions
                    | value = input
                    , options =
                        [ Element.text error
                            |> Input.errorBelow
                        ]
                }

            Correct value ->
                { toPositionKeyOptions | value = value }
    , Input.text InputBox [ Attrs.vary ErrorOutline (isError threeWords) ] <|
        case threeWords of
            Empty ->
                threeWordsOptions

            Error input error ->
                { threeWordsOptions
                    | value = input
                    , options =
                        [ Element.text error
                            |> Input.errorBelow
                        ]
                }

            Correct ( ( a, b, c ), glue ) ->
                { threeWordsOptions
                    | value = String.join (String.fromChar glue) [ a, b, c ]
                }
    , Element.text "Get Position"
        |> Element.button NoStyle [ Events.onClick ToPositionClicked ]
    , Element.paragraph NoStyle [] <|
        List.singleton <|
            case result of
                NotAsked ->
                    Element.empty

                Loading ->
                    Element.text "Loading..."

                Failure err ->
                    Element.text <|
                        case err of
                            BadUrl url ->
                                "The URL provided \"" ++ url ++ "\" does not work."

                            Timeout ->
                                "Too much time has passed."

                            NetworkError ->
                                "There's no internet connection."

                            BadStatus { status } ->
                                "A " ++ toString status.code ++ " error was returned by the server."

                            BadPayload error _ ->
                                error

                Success { latitude, longitude } ->
                    Element.text (toString latitude ++ ", " ++ toString longitude)
    ]


type Styles
    = Header
    | InputBox
    | NoStyle


type Variations
    = ErrorOutline


styleSheet : StyleSheet Styles Variations
styleSheet =
    Style.styleSheet
        [ Style.style Header
            [ Font.size 24
            , Font.center
            ]
        , Style.style InputBox
            [ Style.variation ErrorOutline
                [ Shadow.glow Color.red 2
                ]
            ]
        , Style.style NoStyle []
        ]


fromPositionKeyOnChange : String -> Msg
fromPositionKeyOnChange input =
    if String.isEmpty input then
        FromPositionKeyChange Empty
    else
        FromPositionKeyChange (Correct input)


fromPositionKeyOptions : Text Styles Variations Msg
fromPositionKeyOptions =
    { onChange = fromPositionKeyOnChange
    , value = ""
    , label =
        Element.text "Key"
            |> Input.labelAbove
    , options = []
    }


latitudeOnChange : String -> Msg
latitudeOnChange input =
    if String.isEmpty input then
        LatitudeChange Empty
    else
        case String.toFloat input of
            Ok latitude ->
                LatitudeChange (Correct latitude)

            Err error ->
                LatitudeChange (Error input error)


latitudeOptions : Text Styles Variations Msg
latitudeOptions =
    { onChange = latitudeOnChange
    , value = ""
    , label =
        Element.text "Latitude"
            |> Input.labelAbove
    , options = []
    }


longitudeOnChange : String -> Msg
longitudeOnChange input =
    if String.isEmpty input then
        LongitudeChange Empty
    else
        case String.toFloat input of
            Ok longitude ->
                LongitudeChange (Correct longitude)

            Err error ->
                LongitudeChange (Error input error)


longitudeOptions : Text Styles Variations Msg
longitudeOptions =
    { onChange = longitudeOnChange
    , value = ""
    , label =
        Element.text "Longitude"
            |> Input.labelAbove
    , options = []
    }


toPositionKeyOnChange : String -> Msg
toPositionKeyOnChange input =
    if String.isEmpty input then
        ToPositionKeyChange Empty
    else
        ToPositionKeyChange (Correct input)


toPositionKeyOptions : Text Styles Variations Msg
toPositionKeyOptions =
    { onChange = toPositionKeyOnChange
    , value = ""
    , label =
        Element.text "Key"
            |> Input.labelAbove
    , options = []
    }


threeWordsOnChange : String -> Msg
threeWordsOnChange input =
    if String.isEmpty input then
        ThreeWordsChange Empty
    else
        case String.split "." input of
            [ a, b, c ] as list ->
                if List.any String.isEmpty list then
                    ThreeWordsChange (Error input "Invalid three-word address")
                else
                    ThreeWordsChange (Correct ( ( a, b, c ), '.' ))

            _ ->
                case String.split " " input of
                    [ a, b, c ] as list ->
                        if List.any String.isEmpty list then
                            ThreeWordsChange (Error input "Invalid three-word address")
                        else
                            ThreeWordsChange (Correct ( ( a, b, c ), ' ' ))

                    _ ->
                        ThreeWordsChange (Error input "Invalid three-word address")


threeWordsOptions : Text Styles Variations Msg
threeWordsOptions =
    { onChange = threeWordsOnChange
    , value = ""
    , label =
        Element.text "what3words"
            |> Input.labelAbove
    , options = []
    }


imputToMaybe : Input a -> Maybe a
imputToMaybe input =
    case input of
        Empty ->
            Nothing

        Error _ _ ->
            Nothing

        Correct value ->
            Just value


missingInput : String -> Input a -> Input a
missingInput message input =
    case input of
        Empty ->
            Error "" message

        Error _ _ ->
            input

        Correct _ ->
            input


isError : Input a -> Bool
isError input =
    case input of
        Empty ->
            False

        Error _ _ ->
            True

        Correct _ ->
            False
