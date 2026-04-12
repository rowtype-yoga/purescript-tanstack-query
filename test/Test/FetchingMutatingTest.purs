module Test.FetchingMutatingTest where

import Prelude

import Beta.DOM as DOM
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks as React
import React.TestingLibrary (cleanup, render)
import TanStack.Query (newQueryClient, queryClientProvider, queryKey, useIsFetching, useIsMutating)
import ViTest (ViTest, afterEach, describe, test, viTest)
import ViTest.Expect ((====))
import ViTest.Expect.DOM (elemText)

fetchingMutatingComponent :: React.ReactComponent {}
fetchingMutatingComponent = unsafePerformEffect $ React.reactComponent "FetchMutTest" \_ -> React.do
  fetching <- useIsFetching { queryKey: queryKey "no-such-key" }
  mutating <- useIsMutating { mutationKey: "no-such-mutation" }
  pure $ DOM.div {}
    [ DOM.div { "data-testid": "fetching" } (show fetching)
    , DOM.div { "data-testid": "mutating" } (show mutating)
    ]

main :: ViTest
main = viTest do
  queryClient <- newQueryClient
  describe "useIsFetching and useIsMutating with filters" do
    afterEach cleanup
    test "both return 0 when nothing is active" do
      { findByTestId } <- render $ queryClientProvider { client: queryClient }
        [ React.element fetchingMutatingComponent {} ]
      fetchEl <- findByTestId "fetching"
      fetchText <- elemText fetchEl
      fetchText ==== "0"
      mutEl <- findByTestId "mutating"
      mutText <- elemText mutEl
      mutText ==== "0"
