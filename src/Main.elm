module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Github exposing (Repo, getRepos)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as D exposing (Decoder)
import Page.User as User
import Route exposing (Route)
import Url
import Url.Builder
import ViewUtil exposing (viewLink)



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
    | UserPage User.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    Model key TopPage
        |> goTo (Route.parse url)



--- UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | UserMsg User.Msg


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

        UserMsg userMsg ->
            case model.page of
                UserPage userModel ->
                    let
                        ( newUserModel, usrCmd ) =
                            User.update userMsg userModel
                    in
                    ( { model | page = UserPage newUserModel }
                    , Cmd.map UserMsg usrCmd
                    )

                _ ->
                    ( model, Cmd.none )


goTo : Maybe Route -> Model -> ( Model, Cmd Msg )
goTo routeMaybe model =
    case routeMaybe of
        Just Route.Top ->
            --- TopPageは即座にページ更新
            ( { model | page = TopPage }, Cmd.none )

        Just (Route.User userName) ->
            let
                ( userModel, usrCmd ) =
                    User.init userName
            in
            ( { model | page = UserPage userModel }
            , Cmd.map UserMsg usrCmd
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

            ErrorPage error ->
                viewError error

            TopPage ->
                viewTopPage

            UserPage userModel ->
                User.view userModel
                    |> Html.map UserMsg
        ]
    }


viewNotFound : Html Msg
viewNotFound =
    text "not found"


viewError : Http.Error -> Html Msg
viewError error =
    case error of
        Http.BadBody message ->
            pre [] [ text message ]

        _ ->
            text (Debug.toString error)


viewTopPage : Html Msg
viewTopPage =
    ul []
        [ viewLink (Url.Builder.absolute [ "elm" ] [])
        , viewLink (Url.Builder.absolute [ "evancz" ] [])
        ]


viewUserPage : List Repo -> Html msg
viewUserPage repoList =
    ul []
        (repoList
            |> List.map
                (\repo ->
                    ViewUtil.viewLink
                        (Url.Builder.absolute [ repo.owner, repo.name ] [])
                )
        )
