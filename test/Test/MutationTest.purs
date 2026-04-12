module Test.MutationTest where

import Prelude

import Beta.DOM as DOM
import Effect.Aff (Milliseconds(..), delay)
import Effect.Unsafe (unsafePerformEffect)
import Network.RemoteData (RemoteData(..))
import Effect.Aff as Aff
import React.Basic.Events (handler_)
import React.Basic.Hooks as React
import React.TestingLibrary (cleanup, fireEventClick, render)
import TanStack.Query (newQueryClient, queryClientProvider, useMutation)
import ViTest (ViTest, afterEach, describe, test, viTest)
import ViTest.Expect ((====))
import ViTest.Expect.DOM (elemText)

mutationComponent :: React.ReactComponent {}
mutationComponent = unsafePerformEffect $ React.reactComponent "MutationTest" \_ -> React.do
  m <- useMutation @String
    { mutationFn: \(input :: String) -> do
        delay (10.0 # Milliseconds)
        pure ("result:" <> input)
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
            void $ Aff.launchAff $ m.mutateAsync "hello"
        }
        "Mutate"
    ]

main :: ViTest
main = viTest do
  queryClient <- newQueryClient
  describe "useMutation" do
    afterEach cleanup
    test "starts idle, transitions to success after trigger" do
      { findByTestId, findByText } <- render $ queryClientProvider { client: queryClient }
        [ React.element mutationComponent {} ]
      statusEl <- findByTestId "status"
      text <- elemText statusEl
      text ==== "idle"
      btn <- findByTestId "trigger"
      fireEventClick btn
      void $ findByText "result:hello"
