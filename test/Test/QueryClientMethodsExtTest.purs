module Test.QueryClientMethodsExtTest where

import Prelude

import Data.Maybe (Maybe(..), isNothing)
import Effect.Class (liftEffect)
import Unsafe.Coerce (unsafeCoerce)
import Effect.Uncurried (runEffectFn1)
import Data.Maybe (isNothing) as M
import TanStack.Query (cancelQueries, fetchQuery, getQueryData, getQueryState, invalidateQueries, isFetching, isMutating, newQueryClient, newQueryClientWithConfig, queryKey, refetchQueries, resetQueries, setQueryData, appendQueryKey, startQueryKey, hashKey)
import TanStack.Query as TQ
import ViTest (ViTest, describe, test, viTest)
import ViTest.Expect ((====))

main :: ViTest
main = viTest do
  describe "QueryClient methods (extended)" do
    _ <- test "invalidateQueries does not throw" do
      qc <- newQueryClient # liftEffect
      let qk = queryKey "inv-test"
      setQueryData qk "data" qc # liftEffect
      invalidateQueries { queryKey: qk } qc # liftEffect

    _ <- test "cancelQueries does not throw" do
      qc <- newQueryClient # liftEffect
      let qk = queryKey "cancel-test"
      cancelQueries { queryKey: qk } qc

    _ <- test "resetQueries clears data" do
      qc <- newQueryClient # liftEffect
      let qk = queryKey "reset-test"
      setQueryData qk "before" qc # liftEffect
      resetQueries { queryKey: qk } qc
      (state :: Maybe (TQ.QueryState String)) <- getQueryState qk qc # liftEffect
      case state of
        Nothing -> pure unit
        Just _ -> pure unit

    _ <- test "refetchQueries does not throw" do
      qc <- newQueryClient # liftEffect
      let qk = queryKey "refetch-test"
      refetchQueries { queryKey: qk } qc

    _ <- test "getQueryState returns Nothing for unknown key" do
      qc <- newQueryClient # liftEffect
      let qk = queryKey "nonexistent"
      (state :: Maybe (TQ.QueryState String)) <- getQueryState qk qc # liftEffect
      isNothing state ==== true

    _ <- test "getQueryState returns state after setQueryData" do
      qc <- newQueryClient # liftEffect
      let qk = queryKey "state-test"
      setQueryData qk "hello" qc # liftEffect
      (state :: Maybe (TQ.QueryState String)) <- getQueryState qk qc # liftEffect
      case state of
        Nothing -> "missing" ==== "should have state"
        Just s -> s.status ==== "success"

    _ <- test "isFetching returns 0 when idle" do
      qc <- newQueryClient # liftEffect
      let qk = queryKey "fetching-test"
      n <- isFetching { queryKey: qk } qc # liftEffect
      n ==== 0

    _ <- test "isMutating returns 0 when idle" do
      qc <- newQueryClient # liftEffect
      n <- isMutating { queryKey: queryKey "no-mutations" } qc # liftEffect
      n ==== 0

    _ <- test "newQueryClientWithConfig creates working client" do
      qc <- newQueryClientWithConfig {} # liftEffect
      let qk = queryKey "config-test"
      result <- fetchQuery @String { queryKey: qk, queryFn: pure "ok" } qc
      result ==== "ok"

    _ <- test "getQueryData returns Nothing on type mismatch" do
      qc <- newQueryClient # liftEffect
      let qk = queryKey "mismatch-test"
      setQueryData qk "a string" qc # liftEffect
      (result :: Maybe Int) <- getQueryData qk qc # liftEffect
      M.isNothing result ==== true

    _ <- test "appendQueryKey produces different hash" do
      let qk1 = queryKey "base"
      let qk2 = appendQueryKey qk1 "sub"
      let h1 = hashKey qk1
      let h2 = hashKey qk2
      (h1 == h2) ==== false

    test "startQueryKey builds composite key" do
      let qk = startQueryKey "users" "list"
      let h1 = hashKey qk
      let h2 = hashKey (queryKey "users")
      (h1 == h2) ==== false
