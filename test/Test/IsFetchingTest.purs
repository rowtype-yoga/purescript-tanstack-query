module Test.IsFetchingTest where

import Prelude

import Beta.DOM as DOM
import Effect.Aff (Milliseconds(..), delay)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks as React
import React.TestingLibrary (cleanup, render)
import TanStack.Query (newQueryClient, queryClientProvider, queryKey, useIsFetchingAll, useQuery)
import ViTest (ViTest, afterEach, describe, test, viTest)
import ViTest.Expect ((====))
import ViTest.Expect.DOM (elemText)

isFetchingComponent :: React.ReactComponent {}
isFetchingComponent = unsafePerformEffect $ React.reactComponent "IsFetchingTest" \_ -> React.do
  _ <- useQuery @String { queryKey: queryKey "is-fetching-test", queryFn: do
    delay (200.0 # Milliseconds)
    pure "done"
  }
  count <- useIsFetchingAll
  pure $ DOM.div { "data-testid": "fetching-count" } (show count)

main :: ViTest
main = viTest do
  queryClient <- newQueryClient
  describe "useIsFetching" do
    afterEach cleanup
    test "reports fetching count" do
      { findByTestId } <- render $ queryClientProvider { client: queryClient }
        [ React.element isFetchingComponent {} ]
      el <- findByTestId "fetching-count"
      text <- elemText el
      text ==== "1"
