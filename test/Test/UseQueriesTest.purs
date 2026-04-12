module Test.UseQueriesTest where

import Prelude

import Beta.DOM as DOM
import Data.String (joinWith)
import Effect.Aff (Milliseconds(..), delay, throwError)
import Effect.Aff as Effect.Aff
import Effect.Exception (error)
import Effect.Unsafe (unsafePerformEffect)
import Network.RemoteData (RemoteData(..))
import React.Basic.Hooks as React
import React.TestingLibrary (cleanup, render)
import TanStack.Query (newQueryClient, queryClientProvider, queryKey, useQueries)
import TanStack.Query as TQ
import Unsafe.Coerce (unsafeCoerce)
import ViTest (ViTest, afterEach, describe, test, viTest)
import ViTest.Expect ((====))
import ViTest.Expect.DOM (elemText)

mkQueryOpt :: forall output. { queryKey :: TQ.QueryKey, queryFn :: Effect.Aff.Aff output } -> TQ.UseQueryOptions output
mkQueryOpt = unsafeCoerce

useQueriesComponent :: React.ReactComponent {}
useQueriesComponent = unsafePerformEffect $ React.reactComponent "UseQueriesTest" \_ -> React.do
  results <- useQueries @String
    [ mkQueryOpt { queryKey: queryKey "q1", queryFn: delay (10.0 # Milliseconds) *> pure "alpha" }
    , mkQueryOpt { queryKey: queryKey "q2", queryFn: delay (10.0 # Milliseconds) *> pure "beta" }
    ]
  let
    texts = results <#> \r -> case r.data of
      Loading -> "loading"
      Failure _ -> "error"
      Success d -> d
      _ -> "unknown"
  pure $ DOM.div { "data-testid": "status" } (joinWith "," texts)

useQueriesErrorComponent :: React.ReactComponent {}
useQueriesErrorComponent = unsafePerformEffect $ React.reactComponent "UseQueriesErrorTest" \_ -> React.do
  results <- useQueries @String
    [ unsafeCoerce { queryKey: queryKey "qerr1", queryFn: delay (1.0 # Milliseconds) *> throwError (error "fail1"), retry: 0 }
    ]
  let
    texts = results <#> \r -> case r.data of
      Loading -> "loading"
      Failure _ -> "error"
      Success d -> d
      _ -> "unknown"
  pure $ DOM.div { "data-testid": "status" } (joinWith "," texts)

main :: ViTest
main = viTest do
  queryClient <- newQueryClient
  _ <- describe "useQueries" do
    afterEach cleanup
    test "loads multiple queries in parallel" do
      { findByTestId, findByText } <- render $ queryClientProvider { client: queryClient }
        [ React.element useQueriesComponent {} ]
      statusEl <- findByTestId "status"
      text <- elemText statusEl
      text ==== "loading,loading"
      void $ findByText "alpha,beta"

  describe "useQueries error" do
    afterEach cleanup
    test "shows Failure when a query throws" do
      { findByText } <- render $ queryClientProvider { client: queryClient }
        [ React.element useQueriesErrorComponent {} ]
      void $ findByText "error"
