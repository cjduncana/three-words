module ThreeWords exposing (Key, Position, ThreeWords, fromPosition, toPosition)

{-| With this library, you can create HTTP requests that will convert three-word
addresses to latitude, longitude coordinates and vice-versa.


# Type Aliases

@docs ThreeWords, Position, Key


# Converters

@docs fromPosition, toPosition

-}

import Http exposing (Request)
import HttpBuilder exposing (RequestBuilder)
import Json.Decode as Decode exposing (Decoder)


{-| Before you get started, you'll need to register at
[what3words](https://accounts.what3words.com/register). This will give you a
unique key which is needed for using the API.
-}
type alias Key =
    String


{-| Use this record to express latitude and longitude coordinates.
-}
type alias Position =
    { latitude : Float
    , longitude : Float
    }


{-| Use this three string tuple to express the three-word address.
-}
type alias ThreeWords =
    ( String, String, String )


{-| Create a request that returns a 3 word address when successful. If given a
valid position and validation key, the command will result in a successful
address.

    Position 51.521251 -0.203586
        |> ThreeWords.fromPosition "Example Key"
        |> Http.send ThreeWordsReceived

    -- Ok ("index", "home", "raft")

-}
fromPosition : Key -> Position -> Request ThreeWords
fromPosition key { latitude, longitude } =
    HttpBuilder.get (baseApi ++ "reverse")
        |> HttpBuilder.withQueryParams
            [ ( "coords"
              , toString latitude ++ "," ++ toString longitude
              )
            ]
        |> commonRequest key fromPositionDecoder


{-| Create a request that given a 3 word address return coordinates when
successful. If given a valid address and validation key, the command will result
in successful coordinates.

    ("index", "home", "raft")
        |> ThreeWords.toPosition "Example Key"
        |> Http.send PositionReceived

    -- Ok { latitude = 51.521251, longitude = -0.203586 }

-}
toPosition : Key -> ThreeWords -> Request Position
toPosition key address =
    HttpBuilder.get (baseApi ++ "forward")
        |> HttpBuilder.withQueryParams [ ( "addr", convertThreeWords address ) ]
        |> commonRequest key toPositionDecoder


baseApi : String
baseApi =
    "https://api.what3words.com/v2/"


commonRequest : Key -> Decoder a -> RequestBuilder b -> Request a
commonRequest key decoder =
    HttpBuilder.withHeader "X-Api-Key" key
        >> HttpBuilder.withExpect (Http.expectJson <| errorDecoder decoder)
        >> HttpBuilder.toRequest


errorDecoder : Decoder a -> Decoder a
errorDecoder decoder =
    Decode.map2 (,)
        (Decode.at [ "status", "code" ] Decode.int)
        (Decode.at [ "status", "message" ] Decode.string)
        |> Decode.andThen
            (\( code, message ) ->
                if code /= 200 then
                    Decode.fail message
                else
                    decoder
            )


fromPositionDecoder : Decoder ThreeWords
fromPositionDecoder =
    Decode.field "words" Decode.string
        |> Decode.andThen threeWordsDecoder


toPositionDecoder : Decoder Position
toPositionDecoder =
    Decode.map2 Position
        (Decode.at [ "geometry", "lat" ] Decode.float)
        (Decode.at [ "geometry", "lng" ] Decode.float)


threeWordsDecoder : String -> Decoder ThreeWords
threeWordsDecoder address =
    case String.split "." address of
        [ a, b, c ] ->
            Decode.succeed ( a, b, c )

        _ ->
            Decode.fail "Invalid three-word address"


convertThreeWords : ThreeWords -> String
convertThreeWords ( a, b, c ) =
    a ++ "." ++ b ++ "." ++ c
