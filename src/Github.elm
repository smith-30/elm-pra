module Github exposing (Repo, getRepos)

import Http
import Json.Decode as D exposing (Decoder)
import Url
import Url.Builder



-- Types


type alias Repo =
    { name : String
    , description : Maybe String
    , language : Maybe String
    , owner : String
    , fork : Int
    , star : Int
    , watch : Int
    }



-- Decoder (private)


reposDecoder : Decoder (List Repo)
reposDecoder =
    D.list repoDecoder


repoDecoder : Decoder Repo
repoDecoder =
    D.map7 Repo
        (D.field "name" D.string)
        (D.maybe (D.field "description" D.string))
        (D.maybe (D.field "language" D.string))
        (D.at [ "owner", "login" ] D.string)
        (D.field "forks_count" D.int)
        (D.field "stargazers_count" D.int)
        (D.field "watchers_count" D.int)



-- Api


getRepos : (Result Http.Error (List Repo) -> msg) -> String -> Cmd msg
getRepos toMsg userName =
    Http.get
        { url =
            Url.Builder.crossOrigin "https://api.github.com"
                [ "users", userName, "repos" ]
                []
        , expect =
            Http.expectJson
                toMsg
                reposDecoder
        }
