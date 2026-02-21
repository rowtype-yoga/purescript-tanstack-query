module Test.UseQueryExtendedTest where

import Prelude

import Beta.DOM as DOM
import Effect.Aff (Milliseconds(..), delay)
import Effect.Unsafe (unsafePerformEffect)
import Network.RemoteData (RemoteData(..))
import React.Basic (fragment)
import React.Basic.Hooks as React
import React.TestingLibrary (cleanup, render)
import TanStack.Query (newQueryClient, queryClientProvider, queryKey, useQuery)
import ViTest (ViTest, afterEach, describe, test, viTest)
import ViTest.Expect ((====))
import ViTest.Expect.DOM (elemText)

useQueryWithOptionsComponent :: React.ReactComponent {}
useQueryWithOptionsComponent = unsafePerformEffect $ React.reactComponent "UseQueryExtended" \_ -> React.do
  q <- useQuery @String
    { queryKey: queryKey "extended-test"
    , queryFn: do
        delay (20.0 # Milliseconds)
        pure "extended-result"
    , staleTime: 60000
    , gcTime: 300000
    , retry: 0
    , refetchOnWindowFocus: false
    }
  pure $ fragment
    [ DOM.div { "data-testid": "data" } case q.data of
        Loading -> "Loading"
        Failure _ -> "Error"
        Success d -> d
        _ -> "Unknown"
    , DOM.div { "data-testid": "is-fetched" } (show q.isFetched)
    ]

main :: ViTest
main = viTest do
  queryClient <- newQueryClient
  describe "useQuery extended options" do
    afterEach cleanup
    test "supports staleTime and other options" do
      { findByTestId, findByText } <- render $ queryClientProvider { client: queryClient }
        [ React.element useQueryWithOptionsComponent {} ]
      loadingEl <- findByTestId "data"
      text <- elemText loadingEl
      text ==== "Loading"
      void $ findByText "extended-result"
