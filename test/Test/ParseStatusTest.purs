module Test.ParseStatusTest where

import Prelude

import Data.Either (isLeft)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (try, throwException, error)
import TanStack.Query (parseFetchStatus, parseQueryStatus)
import ViTest (ViTest, describe, test, viTest)
import ViTest.Expect ((====))

foreign import evalThunk :: forall a. (Unit -> a) -> Effect a

main :: ViTest
main = viTest do
  _ <- describe "parseFetchStatus" do
    _ <- test "parses all valid statuses" do
      void $ liftEffect $ evalThunk \_ -> parseFetchStatus "fetching"
      void $ liftEffect $ evalThunk \_ -> parseFetchStatus "paused"
      void $ liftEffect $ evalThunk \_ -> parseFetchStatus "idle"
    test "throws on invalid input" do
      result <- liftEffect $ try $ evalThunk \_ -> parseFetchStatus "garbage"
      isLeft result ==== true

  describe "parseQueryStatus" do
    _ <- test "parses all valid statuses" do
      void $ liftEffect $ evalThunk \_ -> parseQueryStatus "pending"
      void $ liftEffect $ evalThunk \_ -> parseQueryStatus "error"
      void $ liftEffect $ evalThunk \_ -> parseQueryStatus "success"
    test "throws on invalid input" do
      result <- liftEffect $ try $ evalThunk \_ -> parseQueryStatus "garbage"
      isLeft result ==== true
