module Main exposing (..)

import Html exposing (Html, h1, div, p, text, button, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value, placeholder)
import Html.App as Html
import Random
import List
import String
import Time exposing (Time, millisecond)


main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { number : Int
    , sides : Int
    , rolls : List Int
    , total : Int
    , rollsLeft : Int
    }


model : Model
model =
    { number = 2
    , sides = 6
    , rolls = []
    , total = 0
    , rollsLeft = 0
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



-- UPDATE


type Msg
    = IncrementDice
    | DecrementDice
    | UpdateSides String
    | StartRolling
    | Roll Int
    | UpdateRolls (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IncrementDice ->
            ( { model | number = model.number + 1 }, Cmd.none )

        DecrementDice ->
            ( { model | number = max (model.number - 1) 1 }, Cmd.none )

        UpdateSides s ->
            let
                validate : String -> Int -> Int
                validate str fallback =
                    case (String.toInt str) of
                        Ok val ->
                            max val 1

                        Err _ ->
                            fallback
            in
                ( { model | sides = validate s model.sides }, Cmd.none )

        StartRolling ->
            if model.rollsLeft == 0 then
                ( model, Random.generate Roll (Random.int 10 15) )
            else
                ( model, Cmd.none )

        Roll times ->
            ( { model | rollsLeft = Debug.log "rolls left" times }
            , Random.generate
                UpdateRolls
                (Random.list model.number <|
                    Random.int 1 model.sides
                )
            )

        UpdateRolls rolls ->
            ( { model | rolls = rolls, total = List.sum rolls }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.rollsLeft > 0 then
        Time.every (100 * millisecond) (\t -> Roll (model.rollsLeft - 1))
    else
        Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Dice roll" ]
        , mainDiv model
        ]


mainDiv : Model -> Html Msg
mainDiv model =
    let
        stringRepresentation =
            toString model.number ++ "d" ++ toString model.sides
    in
        div []
            [ p []
                [ text "Current selection: "
                , text stringRepresentation
                ]
            , controls model
            , p [] [ button [ onClick StartRolling ] [ text "Roll" ] ]
            , rollRepresentation model
            ]


controls : Model -> Html Msg
controls model =
    p []
        [ button [ onClick DecrementDice ] [ text "Fewer Dice" ]
        , button [ onClick IncrementDice ] [ text "More Dice" ]
        , input [ onInput UpdateSides, placeholder "Update sides" ] []
        ]


rollRepresentation : Model -> Html a
rollRepresentation model =
    div []
        [ p [] [ text ("Total: " ++ toString model.total) ]
        , p [] [ text ("Rolls" ++ toString model.rolls) ]
        ]
