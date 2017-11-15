port module Test exposing (main)

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
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import ThreeWords exposing (Key, Position, ThreeWords)


type Msg
    = FromPosition Key Position
    | FromPositionResult (Result Error ThreeWords)
    | ToPosition Key ThreeWords
    | ToPositionResult (Result Error Position)
    | NoOp


main : Program Never () Msg
main =
    Platform.program
        { init = ( (), Cmd.none )
        , update = update
        , subscriptions = \_ -> inbound inboundSub
        }


update : Msg -> () -> ( (), Cmd Msg )
update msg _ =
    case msg of
        FromPosition key position ->
            ThreeWords.fromPosition key position
                |> Http.send FromPositionResult
                |> resp

        FromPositionResult result ->
            let
                value =
                    case result of
                        Ok threeWords ->
                            threeWordsEncoder threeWords

                        Err error ->
                            errorEncoder error
            in
                outbound value
                    |> resp

        ToPosition key threeWords ->
            ThreeWords.toPosition key threeWords
                |> Http.send ToPositionResult
                |> resp

        ToPositionResult result ->
            let
                value =
                    case result of
                        Ok position ->
                            positionEncoder position

                        Err error ->
                            errorEncoder error
            in
                outbound value
                    |> resp

        NoOp ->
            Cmd.none
                |> resp


resp : Cmd msg -> ( (), Cmd msg )
resp =
    (,) ()


port outbound : Value -> Cmd msg


port inbound : (Value -> msg) -> Sub msg


errorEncoder : Error -> Value
errorEncoder error =
    let
        message =
            case error of
                BadUrl reason ->
                    "Bad URL: " ++ reason

                Timeout ->
                    "Timeout"

                NetworkError ->
                    "Network Error"

                BadStatus { status } ->
                    "Bad Status: " ++ status.message

                BadPayload reason _ ->
                    "Bad Payload: " ++ reason
    in
        Encode.object
            [ ( "error"
              , Encode.string message
              )
            ]


positionEncoder : Position -> Value
positionEncoder { latitude, longitude } =
    Encode.object
        [ ( "position"
          , Encode.object
                [ ( "latitude", Encode.float latitude )
                , ( "longitude", Encode.float longitude )
                ]
          )
        ]


threeWordsEncoder : ThreeWords -> Value
threeWordsEncoder ( w1, w2, w3 ) =
    Encode.object
        [ ( "threeWords"
          , String.join "." [ w1, w2, w3 ]
                |> Encode.string
          )
        ]


inboundSub : Value -> Msg
inboundSub =
    Decode.decodeValue inboundDecoder
        >> Result.withDefault NoOp


inboundDecoder : Decoder Msg
inboundDecoder =
    Decode.oneOf
        [ Decode.map2 FromPosition
            (Decode.field "key" Decode.string)
            (Decode.field "position" positionDecoder)
        , Decode.map2 ToPosition
            (Decode.field "key" Decode.string)
            (Decode.field "threeWords" threeWordsDecoder)
        ]


positionDecoder : Decoder Position
positionDecoder =
    Decode.map2 Position
        (Decode.field "lat" Decode.float)
        (Decode.field "lng" Decode.float)


threeWordsDecoder : Decoder ThreeWords
threeWordsDecoder =
    Decode.string
        |> Decode.andThen
            (\address ->
                case String.split "." address of
                    [ a, b, c ] ->
                        Decode.succeed ( a, b, c )

                    _ ->
                        Decode.fail "Invalid three-word address"
            )
