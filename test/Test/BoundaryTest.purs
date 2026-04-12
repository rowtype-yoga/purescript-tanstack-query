module Test.BoundaryTest where

import Prelude

import Beta.DOM as DOM
import Effect.Aff (Milliseconds(..), delay)
import Effect.Unsafe (unsafePerformEffect)
import Network.RemoteData (RemoteData(..))
import React.Basic.Hooks as React
import React.TestingLibrary (cleanup, render)
import TanStack.Query (dehydrate, hydrate, hydrationBoundary, newQueryClient, queryClientProvider, queryErrorResetBoundary, queryKey, setQueryData, skipToken, useQuery, useQueryErrorResetBoundary)
import ViTest (ViTest, afterEach, describe, test, viTest)
import ViTest.Expect ((====))
import ViTest.Expect.DOM (elemText)
import Effect.Class (liftEffect)

errorBoundaryComponent :: React.ReactComponent {}
errorBoundaryComponent = unsafePerformEffect $ React.reactComponent "ErrorBoundaryTest" \_ -> React.do
  { reset } <- useQueryErrorResetBoundary
  pure $ queryErrorResetBoundary \_ ->
    DOM.div { "data-testid": "boundary" } "inside-boundary"

skipTokenComponent :: React.ReactComponent {}
skipTokenComponent = unsafePerformEffect $ React.reactComponent "SkipTokenTest" \_ -> React.do
  q <- useQuery @String
    { queryKey: queryKey "skip-test"
    , queryFn: skipToken
    , enabled: false
    }
  pure $ DOM.div { "data-testid": "status" } case q.data of
    Loading -> "pending"
    Failure _ -> "error"
    Success d -> d
    _ -> "not-asked"

hydrationComponent :: React.ReactComponent { state :: _ }
hydrationComponent = unsafePerformEffect $ React.reactComponent "HydrationTest" \props -> React.do
  pure $ hydrationBoundary { state: props.state }
    [ DOM.div { "data-testid": "hydrated" } "hydrated-content" ]

main :: ViTest
main = viTest do
  queryClient <- newQueryClient
  _ <- describe "error reset boundary" do
    afterEach cleanup
    test "queryErrorResetBoundary renders children" do
      { findByTestId } <- render $ queryClientProvider { client: queryClient }
        [ React.element errorBoundaryComponent {} ]
      el <- findByTestId "boundary"
      text <- elemText el
      text ==== "inside-boundary"

  _ <- describe "skipToken" do
    afterEach cleanup
    test "prevents query from fetching" do
      { findByTestId } <- render $ queryClientProvider { client: queryClient }
        [ React.element skipTokenComponent {} ]
      el <- findByTestId "status"
      text <- elemText el
      text ==== "pending"

  describe "hydrationBoundary" do
    afterEach cleanup
    test "renders with dehydrated state" do
      qc <- newQueryClient # liftEffect
      let qk = queryKey "hydration-test"
      setQueryData qk "data" qc # liftEffect
      state <- dehydrate qc # liftEffect
      { findByTestId } <- render $ queryClientProvider { client: qc }
        [ React.element hydrationComponent { state } ]
      el <- findByTestId "hydrated"
      text <- elemText el
      text ==== "hydrated-content"
