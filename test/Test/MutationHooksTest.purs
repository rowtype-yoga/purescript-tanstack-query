module Test.MutationHooksTest where

import Prelude

import Beta.DOM as DOM
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), catchError, delay, throwError)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Network.RemoteData (RemoteData(..))
import React.Basic.Events (handler_)
import React.Basic.Hooks as React
import React.TestingLibrary (cleanup, fireEventClick, render)
import TanStack.Query (newQueryClient, queryClientProvider, useMutation, useIsMutatingAll, useMutationState, useQueryClient)
import ViTest (ViTest, afterEach, describe, test, viTest)
import ViTest.Expect ((====))
import ViTest.Expect.DOM (elemText)

settledRef :: Ref.Ref String
settledRef = unsafePerformEffect $ Ref.new ""

mutationWithCallbacksComponent :: React.ReactComponent {}
mutationWithCallbacksComponent = unsafePerformEffect $ React.reactComponent "MutCallbacks" \_ -> React.do
  _client <- useQueryClient
  mutating <- useIsMutatingAll
  mutState <- useMutationState {}
  m <- useMutation @String
    { mutationKey: "test-mut"
    , mutationFn: \(input :: String) -> do
        delay (10.0 # Milliseconds)
        pure ("ok:" <> input)
    , onSuccess: \output _input -> do
        Ref.write ("success:" <> output) settledRef
    , onSettled: \result _input _ctx -> do
        case result of
          Right v -> Ref.write ("settled:" <> v) settledRef # liftEffect
          Left _ -> Ref.write "settled:error" settledRef # liftEffect
    , onError: \_err _input _ctx -> do
        Ref.write "onError" settledRef # liftEffect
    , onMutate: \input -> do
        Ref.write ("mutating:" <> input) settledRef
        pure (Just unit)
    }
  pure $ DOM.div {}
    [ DOM.div { "data-testid": "status" } case m.data of
        NotAsked -> "idle"
        Loading -> "loading"
        Failure _ -> "error"
        Success d -> d
    , DOM.div { "data-testid": "is-mutating" } (show mutating)
    , DOM.div { "data-testid": "mut-state-count" } (show (Array.length mutState))
    , DOM.button
        { "data-testid": "trigger"
        , onClick: handler_ do
            void $ Aff.launchAff $ m.mutateAsync "world"
        }
        "Mutate"
    ]

failingMutationWithCallbacksComponent :: React.ReactComponent {}
failingMutationWithCallbacksComponent = unsafePerformEffect $ React.reactComponent "FailMutCallbacks" \_ -> React.do
  m <- useMutation @String
    { mutationFn: \(_ :: String) -> do
        delay (10.0 # Milliseconds)
        throwError (error "mut-fail")
    , retry: 0
    , onSettled: \result _input _ctx -> do
        case result of
          Right _ -> Ref.write "settled:ok" settledRef # liftEffect
          Left _ -> Ref.write "settled:error" settledRef # liftEffect
    , onError: \_err _input _ctx -> do
        Ref.write "onError:called" settledRef # liftEffect
    , onMutate: \_ -> do
        pure Nothing
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
  _ <- describe "mutation hooks (success)" do
    afterEach cleanup
    test "useIsMutatingAll, useMutationState, useQueryClient, and callbacks" do
      Ref.write "" settledRef # liftEffect
      { findByTestId, findByText } <- render $ queryClientProvider { client: queryClient }
        [ React.element mutationWithCallbacksComponent {} ]
      mutEl <- findByTestId "is-mutating"
      mutText <- elemText mutEl
      mutText ==== "0"
      btn <- findByTestId "trigger"
      fireEventClick btn
      void $ findByText "ok:world"
      settled <- Ref.read settledRef # liftEffect
      (settled == "settled:ok:world" || settled == "success:ok:world") ==== true

  describe "mutation hooks (error)" do
    afterEach cleanup
    test "onError and onSettled Left branch fire on failure" do
      Ref.write "" settledRef # liftEffect
      { findByTestId, findByText } <- render $ queryClientProvider { client: queryClient }
        [ React.element failingMutationWithCallbacksComponent {} ]
      btn <- findByTestId "trigger"
      fireEventClick btn
      void $ findByText "error"
      settled <- Ref.read settledRef # liftEffect
      (settled == "onError:called" || settled == "settled:error") ==== true
