module Test.InfiniteQueryTest where

import Prelude

import Beta.DOM as DOM
import Data.Array as Array
import Data.String (joinWith)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Unsafe (unsafePerformEffect)
import Network.RemoteData (RemoteData(..))
import React.Basic.Hooks as React
import React.TestingLibrary (cleanup, render)
import TanStack.Query (newQueryClient, queryClientProvider, queryKey, useInfiniteQuery)
import Unsafe.Coerce (unsafeCoerce)
import ViTest (ViTest, afterEach, describe, test, viTest)
import ViTest.Expect ((====))
import ViTest.Expect.DOM (elemText)

infiniteQueryComponent :: React.ReactComponent {}
infiniteQueryComponent = unsafePerformEffect $ React.reactComponent "InfiniteQueryTest" \_ -> React.do
  q <- useInfiniteQuery @(Array String) @Int
    { queryKey: queryKey "infinite-test"
    , queryFn: \ctx -> do
        delay (10.0 # Milliseconds)
        let page = ctx.pageParam
        pure [ "item-" <> show (page * 3 + 1), "item-" <> show (page * 3 + 2), "item-" <> show (page * 3 + 3) ]
    , initialPageParam: 0
    , getNextPageParam: \_ allPages ->
        if Array.length allPages >= 3 then (unsafeCoerce unit :: Int)
        else Array.length allPages
    }
  pure $ DOM.div {} case q.data of
    Loading -> DOM.div { "data-testid": "status" } "Loading"
    Failure _ -> DOM.div { "data-testid": "status" } "Error"
    Success d -> DOM.div { "data-testid": "status" } (joinWith "," (join d.pages))
    _ -> DOM.div { "data-testid": "status" } "Unknown"

main :: ViTest
main = viTest do
  queryClient <- newQueryClient
  describe "useInfiniteQuery" do
    afterEach cleanup
    test "loads paginated data" do
      { findByTestId } <- render $ queryClientProvider { client: queryClient }
        [ React.element infiniteQueryComponent {} ]
      statusEl <- findByTestId "status"
      statusText <- elemText statusEl
      statusText ==== "Loading"
