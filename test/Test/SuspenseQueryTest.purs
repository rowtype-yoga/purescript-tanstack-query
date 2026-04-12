module Test.SuspenseQueryTest where

import Prelude

import Beta.DOM as DOM
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic (JSX)
import React.Basic.Hooks as React
import React.TestingLibrary (cleanup, render)
import TanStack.Query (newQueryClient, queryClientProvider, queryKey, useSuspenseQuery, useSuspenseInfiniteQuery)
import Unsafe.Coerce (unsafeCoerce)
import ViTest (ViTest, afterEach, describe, test, viTest)
import ViTest.Expect ((====))
import ViTest.Expect.DOM (elemText)

foreign import suspenseBoundary :: { fallback :: JSX, children :: Array JSX } -> JSX

suspenseQueryComponent :: React.ReactComponent {}
suspenseQueryComponent = unsafePerformEffect $ React.reactComponent "SuspenseQueryTest" \_ -> React.do
  q <- useSuspenseQuery @String
    { queryKey: queryKey "suspense-test"
    , queryFn: do
        delay (10.0 # Milliseconds)
        pure "suspense-result"
    }
  pure $ DOM.div { "data-testid": "data" } q.data

suspenseInfiniteComponent :: React.ReactComponent {}
suspenseInfiniteComponent = unsafePerformEffect $ React.reactComponent "SuspenseInfiniteTest" \_ -> React.do
  q <- useSuspenseInfiniteQuery @(Array String) @Int
    { queryKey: queryKey "suspense-infinite-test"
    , queryFn: \ctx -> do
        delay (10.0 # Milliseconds)
        let page = ctx.pageParam
        pure [ "item-" <> show page ]
    , initialPageParam: 0
    , getNextPageParam: \_ allPages _ _ ->
        if Array.length allPages >= 2 then Nothing
        else Just (Array.length allPages)
    }
  pure $ DOM.div { "data-testid": "data" } (joinWith "," (join q.data.pages))

main :: ViTest
main = viTest do
  queryClient <- newQueryClient
  _ <- describe "useSuspenseQuery" do
    afterEach cleanup
    test "resolves data through suspense" do
      { findByText } <- render $ queryClientProvider { client: queryClient }
        [ suspenseBoundary
            { fallback: DOM.div { "data-testid": "fallback" } "Loading..."
            , children: [ React.element suspenseQueryComponent {} ]
            }
        ]
      void $ findByText "suspense-result"

  describe "useSuspenseInfiniteQuery" do
    afterEach cleanup
    test "resolves first page through suspense" do
      { findByText } <- render $ queryClientProvider { client: queryClient }
        [ suspenseBoundary
            { fallback: DOM.div {} "Loading..."
            , children: [ React.element suspenseInfiniteComponent {} ]
            }
        ]
      void $ findByText "item-0"
