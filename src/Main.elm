module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { persons : List Person
    , name : Name
    , job : Job
    , editName : Name
    , editJob : Job
    , editID : EditID
    , error : ErrorMsg
    }


init : ( Model, Cmd Msg )
init =
    ( { persons = []
      , name = ""
      , job = ""
      , editName = ""
      , editJob = ""
      , editID = Nothing
      , error = Nothing
      }
    , getPersons
    )


type alias Person =
    { id : ID, name : Name, job : Job }


type alias ID =
    Int


type alias PostedPerson =
    { name : Name, job : Job }


type alias Name =
    String


type alias Job =
    String


type alias ErrorMsg =
    Maybe String


type alias EditID =
    Maybe ID



-- UPDATE


type Msg
    = GetPersonsResponse (Result Http.Error (List Person))
    | PostPersonResponse (Result Http.Error ())
    | PutPersonResponse (Result Http.Error ())
    | DeletePersonResponse (Result Http.Error ())
    | NewName Name
    | NewJob Job
    | NewPerson
    | EditStart ID Name Job
    | EditName Name
    | EditJob Job
    | EditEnd
    | DeletePerson ID


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ persons, name, job, editID, editName, editJob } as model) =
    case msg of
        NewName name ->
            ( { model | name = name }, Cmd.none )

        NewJob job ->
            ( { model | job = job }, Cmd.none )

        NewPerson ->
            if String.isEmpty name || String.isEmpty job then
                ( { model | error = Just "名前と職業を入力してから「追加」ボタンを押してください。" }, Cmd.none )
            else
                ( { model | name = "", job = "", error = Nothing }, postPerson <| PostedPerson name job )

        EditStart id n j ->
            ( { model | editID = Just id, editName = n, editJob = j, error = Nothing }, Cmd.none )

        EditName n ->
            ( { model | editName = n }, Cmd.none )

        EditJob j ->
            ( { model | editJob = j }, Cmd.none )

        EditEnd ->
            case editID of
                Just eid ->
                    if String.isEmpty editName || String.isEmpty editJob then
                        ( { model | error = Just "名前と職業を入力してから「確定」ボタンを押してください。" }, Cmd.none )
                    else
                        ( { model | editName = "", editJob = "", editID = Nothing }, putPerson <| Person eid editName editJob )

                Nothing ->
                    Debug.crash "Can not edit."

        DeletePerson id ->
            ( { model | error = Nothing }, deletePerson id )

        -- HTTP Response
        GetPersonsResponse (Ok persons) ->
            ( { model | persons = persons }, Cmd.none )

        GetPersonsResponse (Err error) ->
            Debug.crash <| toString error

        PostPersonResponse (Ok _) ->
            ( model, getPersons )

        PostPersonResponse (Err error) ->
            Debug.crash <| toString error

        PutPersonResponse (Ok _) ->
            ( model, getPersons )

        PutPersonResponse (Err error) ->
            Debug.crash <| toString error

        DeletePersonResponse (Ok _) ->
            ( model, getPersons )

        DeletePersonResponse (Err error) ->
            Debug.crash <| toString error



-- VIEW


view : Model -> Html Msg
view ({ persons, name, job, editID, editName, editJob, error } as model) =
    div [ class "container" ]
        [ addForm name job
        , errorMsg error
        , personList persons editID editName editJob
        ]


errorMsg : ErrorMsg -> Html Msg
errorMsg addError =
    case addError of
        Just e ->
            span [] [ text e ]

        Nothing ->
            empty


addForm : Name -> Job -> Html Msg
addForm name job =
    div [ class "addForm" ]
        [ input [ class "name", value name, placeholder "名前", onInput NewName ] []
        , input [ class "job", value job, placeholder "職業", onInput NewJob ] []
        , input [ type_ "button", class "add", value "追加", onClick NewPerson ] []
        ]


personList : List Person -> EditID -> Name -> Job -> Html Msg
personList persons editId editName editJob =
    ul [] <|
        List.map
            (\person ->
                personItem editId editName editJob person
            )
            persons


personItem : EditID -> Name -> Job -> Person -> Html Msg
personItem editID editName editJob { id, name, job } =
    let
        item =
            li []
                [ span [] [ text <| toString id ]
                , span [] [ text name ]
                , span [] [ text job ]
                , input [ type_ "button", class "edit", value "編集", onClick <| EditStart id name job ] []
                , input [ type_ "button", class "delete", value "削除", onClick <| DeletePerson id ] []
                ]
    in
        case editID of
            Just eid ->
                if id == eid then
                    li []
                        [ span [] [ text <| toString id ]
                        , input [ value editName, onInput EditName ] []
                        , input [ value editJob, onInput EditJob ] []
                        , input [ type_ "button", class "edit", value "確定", onClick EditEnd ] []
                        , input [ type_ "button", class "delete", value "削除", onClick <| DeletePerson id ] []
                        ]
                else
                    item

            Nothing ->
                item


empty : Html Msg
empty =
    text ""



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getPersons : Cmd Msg
getPersons =
    let
        url =
            "http://localhost:8000/persons"
    in
        Http.send GetPersonsResponse (Http.get url decodePersons)


decodePerson : Decode.Decoder Person
decodePerson =
    Decode.map3 Person
        (Decode.at [ "id" ] Decode.int)
        (Decode.at [ "name" ] Decode.string)
        (Decode.at [ "job" ] Decode.string)


decodePersons : Decode.Decoder (List Person)
decodePersons =
    Decode.list decodePerson


postPerson : PostedPerson -> Cmd Msg
postPerson person =
    let
        url =
            "http://localhost:8000/persons"

        jsonBody =
            Http.jsonBody <| encodePostedPerson person

        request =
            Http.request
                { method = "POST"
                , headers = []
                , url = url
                , body = jsonBody
                , expect = Http.expectStringResponse (\_ -> Ok ())
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send PostPersonResponse request


encodePostedPerson : PostedPerson -> Encode.Value
encodePostedPerson { name, job } =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "job", Encode.string job )
        ]


putPerson : Person -> Cmd Msg
putPerson person =
    let
        url =
            "http://localhost:8000/persons/" ++ toString person.id

        jsonBody =
            Http.jsonBody <| encodePostedPerson <| PostedPerson person.name person.job

        request =
            Http.request
                { method = "PUT"
                , headers = []
                , url = url
                , body = jsonBody
                , expect = Http.expectStringResponse (\_ -> Ok ())
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send PutPersonResponse request


deletePerson : ID -> Cmd Msg
deletePerson id =
    let
        url =
            "http://localhost:8000/persons/" ++ toString id

        request =
            Http.request
                { method = "Delete"
                , headers = []
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectStringResponse (\_ -> Ok ())
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send DeletePersonResponse request
