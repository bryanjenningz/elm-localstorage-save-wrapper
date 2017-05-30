port module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Array exposing (Array)


type Msg
    = AddCounter
    | Increment Int
    | Decrement Int
    | Reset Int
    | Remove Int


type alias Model =
    { counts : Array Int }


type alias Flags =
    Model


init : Flags -> ( Model, Cmd Msg )
init { counts } =
    Model counts ! []


view : Model -> Html Msg
view model =
    div []
        [ div [] [ button [ onClick AddCounter ] [ text "Add Counter" ] ]
        , div []
            (model.counts
                |> Array.indexedMap
                    (\i count ->
                        div []
                            [ button [ onClick (Increment i) ] [ text "+" ]
                            , button [ onClick (Decrement i) ] [ text "-" ]
                            , span [] [ text (toString count) ]
                            , button [ onClick (Reset i) ] [ text "Reset" ]
                            , button [ onClick (Remove i) ] [ text "X" ]
                            ]
                    )
                |> Array.toList
            )
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddCounter ->
            { model | counts = model.counts |> Array.push 0 }

        Increment i ->
            { model | counts = model.counts |> Array.set i (model.counts |> Array.get i |> Maybe.withDefault -1 |> ((+) 1)) }

        Decrement i ->
            { model | counts = model.counts |> Array.set i (model.counts |> Array.get i |> Maybe.withDefault -1 |> (\x -> x - 1)) }

        Reset i ->
            { model | counts = model.counts |> Array.set i 0 }

        Remove i ->
            { model | counts = Array.append (Array.slice 0 i model.counts) (Array.slice (i + 1) (Array.length model.counts) model.counts) }


save : (Msg -> Model -> Model) -> Msg -> Model -> ( Model, Cmd Msg )
save update msg model =
    let
        newModel =
            update msg model
    in
        newModel ! [ saveModel newModel ]


port saveModel : Model -> Cmd msg


main : Program Model Model Msg
main =
    programWithFlags { init = init, view = view, update = save update, subscriptions = (\_ -> Sub.none) }
