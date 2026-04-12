module Test.ErrorPathTest where

import Prelude

import Beta.DOM as DOM
import Effect.Aff (Milliseconds(..), delay, throwError, catchError)
import Effect.Aff as Aff
import Effect.Exception (error)
import Effect.Unsafe (unsafePerformEffect)
import Network.RemoteData (RemoteData(..))
import React.Basic.Events (handler_)
import React.Basic.Hooks as React
import React.TestingLibrary (cleanup, fireEventClick, render)
import TanStack.Query (newQueryClient, queryClientProvider, queryKey, useQuery, useMutation)
import ViTest (ViTest, afterEach, describe, test, viTest)
import ViTest.Expect ((====))
import ViTest.Expect.DOM (elemText)

failingQueryComponent :: React.ReactComponent {}
failingQueryComponent = unsafePerformEffect $ React.reactComponent "FailingQuery" \_ -> React.do
  q <- useQuery @String
    { queryKey: queryKey "error-test"
    , queryFn: do
        delay (10.0 # Milliseconds)
        throwError (error "boom")
    , retry: 0
    }
  pure $ DOM.div { "data-testid": "status" } case q.data of
    Loading -> "loading"
    Failure _ -> "error"
    Success d -> d
    _ -> "unknown"

failingMutationComponent :: React.ReactComponent {}
failingMutationComponent = unsafePerformEffect $ React.reactComponent "FailingMutation" \_ -> React.do
  m <- useMutation @String
    { mutationFn: \(_ :: String) -> do
        delay (10.0 # Milliseconds)
        throwError (error "mut-boom")
    , retry: 0
    }
  pure $ DOM.div {}
    [ DOM.div { "data-testid": "status" } case m.data of
        NotAsked -> "idle"
        Loading -> "loading"
        Failure _ -> "error"
        Success d -> d
    , DOM.button
        { "data-testid": "trigger"
        , onClick: handler_ do
            void $ Aff.launchAff $ m.mutateAsync "fail" `catchError` \_ -> pure unit
        }
        "Mutate"
    ]

main :: ViTest
main = viTest do
  queryClient <- newQueryClient
  _ <- describe "useQuery error path" do
    afterEach cleanup
    test "shows Failure when queryFn throws" do
      { findByText } <- render $ queryClientProvider { client: queryClient }
        [ React.element failingQueryComponent {} ]
      void $ findByText "error"

  describe "useMutation error path" do
    afterEach cleanup
    test "shows Failure when mutationFn throws" do
      { findByTestId, findByText } <- render $ queryClientProvider { client: queryClient }
        [ React.element failingMutationComponent {} ]
      statusEl <- findByTestId "status"
      text <- elemText statusEl
      text ==== "idle"
      btn <- findByTestId "trigger"
      fireEventClick btn
      void $ findByText "error"
