module Test.TanstackQueryTest2 where

import Prelude

import Effect.Aff as Aff
import React.TestingLibrary (render)
import TanStack.Query (newQueryClient, queryClientProvider)
import Test.UseQueryExample (useQueryExample)
import ViTest (ViTest, describe, test, viTest)

main :: ViTest
main = viTest do
  queryClient <- newQueryClient
  describe "The Tanstack Query Bindings" do
    test "catches errors that are thrown in aff" do
        { findByText } <- renderWithClientProvider queryClient $ useQueryExample {
            loadData: do
              Aff.throwError (Aff.error "Fake server error")
        }
        void $ findByText "Fake server error"
  where
  renderWithClientProvider client x = render $ queryClientProvider { client } [ x ]
