module Example exposing (suite)

import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)
import Url exposing (Url)


route_suite : Test
route_suite =
    describe "Route"
        [ test "should parse URL" <|
            \_ ->
                Url.fromString "http://example.com"
                    |> Maybe.andThen Route.parse
                    |> Expect.equal (Just Route.Top)
        ]
