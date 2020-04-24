module Main exposing (main)

import Browser
import Html exposing (Html, a, blockquote, button, div, footer, h3, p, text)
import Html.Attributes exposing (attribute, class, href, id, rel, target)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Json.Decode exposing (Decoder, field, map2, string)
import Url.Builder exposing (crossOrigin)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    Result String Quote


type alias Quote =
    { quote : String
    , author : String
    }


quoteDecoder : Decoder Quote
quoteDecoder =
    map2 Quote
        (field "content" string)
        (field "author" string)


quotesApiUrl : String
quotesApiUrl =
    "https://api.quotable.io/random"


getRandomQuote : Cmd Msg
getRandomQuote =
    Http.get
        { url = quotesApiUrl
        , expect = Http.expectJson GotQuote quoteDecoder
        }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( Err "Loading..."
    , getRandomQuote
    )


type Msg
    = GetQuote
    | GotQuote (Result Http.Error Quote)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetQuote ->
            ( model
            , getRandomQuote
            )

        GotQuote quoteRes ->
            case quoteRes of
                Ok quote ->
                    ( Ok quote
                    , Cmd.none
                    )

                Err err ->
                    ( Err <| "Error getting quote: " ++ httpErrorToString err
                    , Cmd.none
                    )


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        BadUrl url ->
            "The URL " ++ url ++ " was invalid"

        Timeout ->
            "Unable to reach the server, try again"

        NetworkError ->
            "Unable to reach the server, check your network connection"

        BadStatus 500 ->
            "The server had a problem, try again later"

        BadStatus 400 ->
            "Verify your information and try again"

        BadStatus _ ->
            "Unknown error"

        BadBody errorMessage ->
            errorMessage


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div
        [ class "container mt-5"
        , id "quote-box"
        ]
        (case model of
            Err err ->
                [ div
                    [ class "row" ]
                    [ div
                        [ class "col-md text-center" ]
                        [ h3
                            []
                            [ text err ]
                        ]
                    ]
                , getQuoteRow
                ]

            Ok { quote, author } ->
                [ div
                    [ class "row" ]
                    [ div
                        [ class "col-md" ]
                        [ blockquote
                            [ class "blockquote text-center" ]
                            [ p
                                [ id "text"
                                , class "mb-0"
                                ]
                                [ text quote ]
                            , footer
                                [ id "author"
                                , class "blockquote-footer"
                                ]
                                [ text author ]
                            ]
                        ]
                    ]
                , getQuoteRow
                , div
                    [ class "row" ]
                    [ div
                        [ class "col-md" ]
                        [ a
                            [ href <| buildTwitterShareUrl quote author
                            , rel "noopener noreferrer"
                            , target "_blank"
                            , class "btn btn-primary"
                            , id "tweet-quote"
                            , attribute "role" "button"
                            ]
                            [ text "Tweet this quote" ]
                        ]
                    ]
                ]
        )


getQuoteRow : Html Msg
getQuoteRow =
    div
        [ class "row" ]
        [ div
            [ class "col-md" ]
            [ button
                [ onClick GetQuote
                , class "btn btn-primary"
                , id "new-quote"
                ]
                [ text "Get New Quote" ]
            ]
        ]


{-| Builds a URL to share the quote to Twitter using proper URL encoding techniques.
-}
buildTwitterShareUrl : String -> String -> String
buildTwitterShareUrl quote author =
    crossOrigin "https://twitter.com"
        [ "intent"
        , "tweet"
        ]
        [ Url.Builder.string "text" <| "\"" ++ quote ++ "\" - " ++ author ]
