module Test.InfiniteQueryTest where

import Prelude

import Beta.DOM as DOM
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Effect.Aff (Milliseconds(..), delay, throwError)
import Effect.Exception (error)
import Effect.Unsafe (unsafePerformEffect)
import Network.RemoteData (RemoteData(..))
import React.Basic.Hooks as React
import React.TestingLibrary (cleanup, render)
import TanStack.Query (newQueryClient, queryClientProvider, queryKey, useInfiniteQuery)
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
    , getNextPageParam: \_ allPages _ _ ->
        if Array.length allPages >= 3 then Nothing
        else Just (Array.length allPages)
    }
  pure $ DOM.div {} case q.data of
    Loading -> DOM.div { "data-testid": "status" } "Loading"
    Failure _ -> DOM.div { "data-testid": "status" } "Error"
    Success d -> DOM.div { "data-testid": "status" } (joinWith "," (join d.pages))
    _ -> DOM.div { "data-testid": "status" } "Unknown"

failingInfiniteComponent :: React.ReactComponent {}
failingInfiniteComponent = unsafePerformEffect $ React.reactComponent "FailInfiniteQuery" \_ -> React.do
  q <- useInfiniteQuery @(Array String) @Int
    { queryKey: queryKey "infinite-error-test"
    , queryFn: \_ -> do
        delay (10.0 # Milliseconds)
        throwError (error "infinite-boom")
    , initialPageParam: 0
    , getNextPageParam: \_ _ _ _ -> Nothing
    , retry: 0
    }
  pure $ DOM.div { "data-testid": "status" } case q.data of
    Loading -> "loading"
    Failure _ -> "error"
    Success _ -> "success"
    _ -> "unknown"

main :: ViTest
main = viTest do
  queryClient <- newQueryClient
  _ <- describe "useInfiniteQuery" do
    afterEach cleanup
    test "loads paginated data" do
      { findByTestId, findByText } <- render $ queryClientProvider { client: queryClient }
        [ React.element infiniteQueryComponent {} ]
      statusEl <- findByTestId "status"
      statusText <- elemText statusEl
      statusText ==== "Loading"
      void $ findByText "item-1,item-2,item-3"

  describe "useInfiniteQuery error" do
    afterEach cleanup
    test "shows Failure when queryFn throws" do
      { findByText } <- render $ queryClientProvider { client: queryClient }
        [ React.element failingInfiniteComponent {} ]
      void $ findByText "error"
