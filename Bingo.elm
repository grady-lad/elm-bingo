module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random


-- Model


type alias Model =
    { name : String, gameNumber : Int, entries : List Entry }


type alias Entry =
    { id : Int, phrase : String, points : Int, marked : Bool }


initialModel : Model
initialModel =
    { name = "Martin"
    , gameNumber = 1
    , entries = initialEntries
    }


initialEntries : List Entry
initialEntries =
    [ Entry 1 "Future-Proof" 100 False
    , Entry 2 "Doing Agile" 200 False
    , Entry 3 "In the Cloud" 300 False
    , Entry 4 "Rock Star Ninja" 400 False
    ]



-- Messages


type Msg
    = NewGame
    | Mark Int
    | NewRandom Int



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRandom randomNumber ->
            ( { model | gameNumber = randomNumber }, Cmd.none )

        NewGame ->
            ( { model
                | entries = initialEntries
              }
            , generateRandomNumber
            )

        Mark id ->
            let
                markEntry e =
                    if e.id == id then
                        { e | marked = not (e.marked) }
                    else
                        e
            in
                ( { model | entries = List.map markEntry model.entries }, Cmd.none )



-- Commands


generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate (\num -> NewRandom num) (Random.int 1 100)



-- View


sumMarkedPoints : List Entry -> Int
sumMarkedPoints entries =
    entries
        |> List.filter .marked
        |> List.map .points
        |> List.sum


viewScore : Int -> Html msg
viewScore sum =
    div
        [ class "score" ]
        [ span [ class "label" ] [ text "Score" ]
        , span [ class "value" ] [ text (toString sum) ]
        ]


playerInfo : String -> Int -> String
playerInfo name gameNumber =
    name ++ " - Game " ++ (toString gameNumber)


viewPlayer : String -> Int -> Html msg
viewPlayer name gameNumber =
    let
        playInfoText =
            playerInfo name gameNumber
                |> String.toUpper
                |> text
    in
        h2 [ id "info", class "classy" ]
            [ playInfoText ]


viewHeader : String -> Html msg
viewHeader title =
    header []
        [ h1 [] [ text title ] ]


viewFooter : Html Msg
viewFooter =
    footer []
        [ a [ href "http://elm-lang.org" ]
            [ text "Powered By elm" ]
        ]


viewEntryItem : Entry -> Html Msg
viewEntryItem entryItem =
    li [ classList [ ( "marked", entryItem.marked ) ], onClick (Mark entryItem.id) ]
        [ span [ class "phrase" ] [ text entryItem.phrase ]
        , span [ class "points" ] [ text (toString entryItem.points) ]
        ]


viewEntryList : List Entry -> Html Msg
viewEntryList entries =
    let
        listOfEntries =
            List.map viewEntryItem entries
    in
        ul [] listOfEntries


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BING"
        , viewPlayer model.name model.gameNumber
        , viewEntryList model.entries
        , viewScore (sumMarkedPoints model.entries)
        , button [ onClick NewGame ] [ text "New Game" ]
        , div [ class "debug" ] [ text (toString model) ]
        , viewFooter
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, generateRandomNumber )
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.none)
        }
