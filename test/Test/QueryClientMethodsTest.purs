module Test.QueryClientMethodsTest where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import TanStack.Query (dehydrate, fetchQuery, getQueryData, hashKey, hydrate, newQueryClient, prefetchQuery, queryKey, removeQueries, setQueryData)
import ViTest (ViTest, describe, test, viTest)
import ViTest.Expect ((====))

main :: ViTest
main = viTest do
  describe "QueryClient methods" do
    _ <- test "setQueryData and getQueryData round-trip" do
      qc <- newQueryClient # liftEffect
      let qk = queryKey "test-data"
      setQueryData qk "hello" qc # liftEffect
      result :: Maybe String <- getQueryData qk qc # liftEffect
      (fromMaybe "" result) ==== "hello"

    _ <- test "removeQueries removes data" do
      qc <- newQueryClient # liftEffect
      let qk = queryKey "to-remove"
      setQueryData qk "data" qc # liftEffect
      removeQueries { queryKey: qk } qc # liftEffect
      result :: Maybe String <- getQueryData qk qc # liftEffect
      (show result) ==== "Nothing"

    _ <- test "prefetchQuery populates cache" do
      qc <- newQueryClient # liftEffect
      let qk = queryKey "prefetched"
      prefetchQuery @String { queryKey: qk, queryFn: pure "prefetch-result" } qc
      delay (50.0 # Milliseconds)
      result :: Maybe String <- getQueryData qk qc # liftEffect
      (fromMaybe "" result) ==== "prefetch-result"

    _ <- test "fetchQuery returns data" do
      qc <- newQueryClient # liftEffect
      let qk = queryKey "fetched"
      result <- fetchQuery @String { queryKey: qk, queryFn: pure "fetch-result" } qc
      result ==== "fetch-result"

    _ <- test "hashKey returns consistent string" do
      let qk = queryKey "hash-test"
      let h1 = hashKey qk
      let h2 = hashKey qk
      h1 ==== h2

    test "dehydrate and hydrate round-trip" do
      qc1 <- newQueryClient # liftEffect
      let qk = queryKey "hydration-test"
      setQueryData qk "hydrated-data" qc1 # liftEffect
      state <- dehydrate qc1 # liftEffect
      qc2 <- newQueryClient # liftEffect
      hydrate qc2 state # liftEffect
      result :: Maybe String <- getQueryData qk qc2 # liftEffect
      (fromMaybe "" result) ==== "hydrated-data"
