module Page.User exposing (Model, Msg, init, update, view)

import Github exposing (Repo)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Url.Builder
import ViewUtil exposing (viewLink)


type alias Model =
    { usename : String
    , state : State
    }


type State
    = Init
    | Loaded (List Repo)
    | Error Http.Error


init : String -> ( Model, Cmd Msg )
init userName =
    --- ページの初期化
    --- 最初のModelを作ると同時に、ページの表示に必要なデータをHTTPで取得
    ( Model userName Init
    , Github.getRepos GotRepos userName
    )


type Msg
    = GotRepos (Result Http.Error (List Repo))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    --- init で HTTP リクエストの結果が得られたら Model を更新します
    case msg of
        GotRepos (Ok repos) ->
            ( { model | state = Loaded repos }, Cmd.none )

        GotRepos (Err error) ->
            ( { model | state = Error error }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.state of
        Init ->
            text "Loadong..."

        Loaded repoList ->
            ul [] (List.map viewRepo repoList)

        Error errorHttp ->
            text (Debug.toString errorHttp)


viewRepo : Repo -> Html msg
viewRepo repo =
    viewLink
        (Url.Builder.absolute [ repo.owner, repo.name ] [])
