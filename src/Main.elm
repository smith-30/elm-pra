module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Route exposing (Route)
import Url
import Url.Builder



--MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , page : Page
    }


type Page
    = NotFound
    | ErrorPage Http.Error
    | TopPage
    | UserPage (List Repo)


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    Model key TopPage
        |> goTo (Route.parse url)



--- UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | Loaded (Result Http.Error.Page)


update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            -- ページ初期化処理をヘルパー関数に移譲
            goTo (Route.parse url) model

        --- ページの内容を非同期で取得したときの共通処理
        Loaded result ->
            ( { model
                | page =
                    case result of
                        Ok page ->
                            Page

                        Err e ->
                            ErrorPage e
              }
            , Cmd.none
            )


goTo : Maybe Route -> Model -> ( Model, Cmd Msg )
goTo routeMaybe model =
    case routeMaybe of
        Just Route.Top ->
            --- TopPageは即座にページ更新
            ( { model | page = NotFound }, Cmd.None )

        Just (Route.User userName) ->
            ( model
            , Http.get
                { url =
                    Url.Builder.crossOrigin "https://api.github.com"
                        [ "users", userName, "repos" ]
                        []
                , expect =
                    Http.expectJson
                        (Result.map UserPage >> Loaded)
                        reposDecoder
                }
            )

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )



--- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "My Github Viewer"
    , body =
        [ a [ href "/" ] [ h1 [] [ text "My Github Viewer" ] ]

        -- 場合分けしてページを表示する
        , case model.page of
            NotFound ->
                viewNotFound

            ErrorPage ->
                viewError error

            TopPage ->
                viewTopPage

            UserPage repoList ->
                viewUserPage repos
        ]
    }
