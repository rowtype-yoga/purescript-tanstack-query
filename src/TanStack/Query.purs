module TanStack.Query where

import Prelude

import Data.Array (snoc) as Array
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable (toMaybe) as Nullable
import Data.Semigroup.Foldable (intercalateMap)
import Data.Undefined.NoProblem (Opt, pseudoMap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (error) as Console
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, mkEffectFn1, mkEffectFn2, mkEffectFn3, mkEffectFn4, runEffectFn1, runEffectFn2, runEffectFn3)
import Foreign (Foreign)
import ForgetMeNot (Id)
import Literals.Undefined (undefined)
import Network.RemoteData (RemoteData(..)) as RD
import Network.RemoteData (RemoteData)
import Partial.Unsafe (unsafeCrashWith)
import Promise (Promise)
import Promise (class Flatten) as Promise
import Promise.Aff (fromAff, toAffE) as Promise
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent)
import React.Basic.Hooks (bind, element) as React
import React.Basic.Hooks.Internal (Hook, unsafeHook)
import Record (insert) as Record
import Record.Builder (build, modify) as RB
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Yoga.JSON (class ReadForeign, class WriteForeign, unsafeStringify)
import Yoga.JSON (read, write) as JSON
import Yoga.JSON.Error (renderHumanError)

-- | QueryKey

foreign import data QueryKey :: Type

unsafeToQueryKey :: Array Foreign -> QueryKey
unsafeToQueryKey = unsafeCoerce

queryKey :: forall a. WriteForeign a => a -> QueryKey
queryKey x = unsafeToQueryKey [ JSON.write x ]

startQueryKey :: forall a b. WriteForeign a => WriteForeign b => a -> b -> QueryKey
startQueryKey x y = queryKey x % y

infixl 4 startQueryKey as !%

appendQueryKey :: forall a. WriteForeign a => QueryKey -> a -> QueryKey
appendQueryKey qk segment = unsafeToQueryKey (Array.snoc (unsafeCoerce qk) (JSON.write segment))

infixl 4 appendQueryKey as %

foreign import hashKeyImpl :: Fn2 (QueryKey -> String) QueryKey String

hashKey :: QueryKey -> String
hashKey = runFn2 hashKeyImpl hashKeyPure
  where
  hashKeyPure :: QueryKey -> String
  hashKeyPure = unsafeCoerce >>> unsafeStringify

-- | FetchStatus / QueryStatus

data FetchStatus = Fetching | Paused | Idle

parseFetchStatus :: String -> FetchStatus
parseFetchStatus = case _ of
  "fetching" -> Fetching
  "paused" -> Paused
  "idle" -> Idle
  _ -> unsafeCrashWith "Invalid fetchStatus"

data QueryStatus = QueryPending | QueryError | QuerySuccess

parseQueryStatus :: String -> QueryStatus
parseQueryStatus = case _ of
  "pending" -> QueryPending
  "error" -> QueryError
  "success" -> QuerySuccess
  _ -> unsafeCrashWith "Invalid query status"

-- | QueryClient

foreign import data QueryClient :: Type

foreign import newQueryClient :: Effect QueryClient

foreign import newQueryClientWithConfigImpl :: forall config. EffectFn1 { | config } QueryClient

type QueryClientConfig =
  ( defaultOptions ::
      { queries ::
          { staleTime :: Opt Int
          , gcTime :: Opt Int
          , retry :: Opt Int
          , refetchOnWindowFocus :: Opt Boolean
          , refetchOnReconnect :: Opt Boolean
          , refetchOnMount :: Opt Boolean
          }
      , mutations ::
          { retry :: Opt Int
          , retryDelay :: Opt Int
          }
      }
  )

newQueryClientWithConfig :: forall opts opts_. Union opts opts_ QueryClientConfig => { | opts } -> Effect QueryClient
newQueryClientWithConfig opts = runEffectFn1 newQueryClientWithConfigImpl opts

foreign import data UseQueryClient :: Type -> Type

foreign import useQueryClientImpl :: Effect QueryClient

useQueryClient :: Hook UseQueryClient QueryClient
useQueryClient = unsafeHook useQueryClientImpl

foreign import queryClientProviderImpl :: forall a. ReactComponent { | a }

queryClientProvider :: { client :: QueryClient } -> (Array JSX) -> JSX
queryClientProvider props children =
  React.element queryClientProviderImpl (props # Record.insert (Proxy :: Proxy "children") children)

-- | QueryClient methods

foreign import invalidateQueries :: { queryKey :: QueryKey } -> QueryClient -> Effect Unit

invalidateAllQueries :: QueryClient -> Effect Unit
invalidateAllQueries = invalidateQueries (unsafeCoerce undefined)

foreign import getQueryDataImpl :: QueryKey -> QueryClient -> Effect (Nullable Foreign)

getQueryData :: forall a. ReadForeign a => QueryKey -> QueryClient -> Effect (Maybe a)
getQueryData qk qc = do
  mqd <- getQueryDataImpl qk qc
  case Nullable.toMaybe mqd of
    Nothing -> pure Nothing
    Just qd -> case JSON.read qd of
      Right r -> pure $ Just r
      Left e -> do
        Console.error ("Broken JSON reader for " <> unsafeStringify qk <> ": " <> intercalateMap "\n" renderHumanError e)
        pure Nothing

foreign import setQueryDataImpl :: forall a. EffectFn3 QueryKey a QueryClient Unit

setQueryData :: forall a. WriteForeign a => QueryKey -> a -> QueryClient -> Effect Unit
setQueryData theQueryKey theData theQueryClient =
  runEffectFn3 setQueryDataImpl theQueryKey (JSON.write theData) theQueryClient

foreign import removeQueriesImpl :: EffectFn2 { queryKey :: QueryKey } QueryClient Unit

removeQueries :: { queryKey :: QueryKey } -> QueryClient -> Effect Unit
removeQueries = runEffectFn2 removeQueriesImpl

foreign import prefetchQueryImpl :: forall a. EffectFn2 { queryKey :: QueryKey, queryFn :: Effect (Promise a) } QueryClient (Promise Unit)

prefetchQuery :: forall @output. Promise.Flatten output output => { queryKey :: QueryKey, queryFn :: Aff output } -> QueryClient -> Aff Unit
prefetchQuery args qc = do
  let jsArgs = { queryKey: args.queryKey, queryFn: Promise.fromAff args.queryFn }
  Promise.toAffE (runEffectFn2 prefetchQueryImpl jsArgs qc)

foreign import fetchQueryImpl :: forall a. EffectFn2 { queryKey :: QueryKey, queryFn :: Effect (Promise a) } QueryClient (Promise a)

fetchQuery :: forall @output. Promise.Flatten output output => { queryKey :: QueryKey, queryFn :: Aff output } -> QueryClient -> Aff output
fetchQuery args qc = do
  let jsArgs = { queryKey: args.queryKey, queryFn: Promise.fromAff args.queryFn }
  Promise.toAffE (runEffectFn2 fetchQueryImpl jsArgs qc)

foreign import cancelQueriesImpl :: EffectFn2 { queryKey :: QueryKey } QueryClient (Promise Unit)

cancelQueries :: { queryKey :: QueryKey } -> QueryClient -> Aff Unit
cancelQueries args qc =
  Promise.toAffE (runEffectFn2 cancelQueriesImpl args qc)

foreign import resetQueriesImpl :: EffectFn2 { queryKey :: QueryKey } QueryClient (Promise Unit)

resetQueries :: { queryKey :: QueryKey } -> QueryClient -> Aff Unit
resetQueries args qc =
  Promise.toAffE (runEffectFn2 resetQueriesImpl args qc)

foreign import refetchQueriesImpl :: EffectFn2 { queryKey :: QueryKey } QueryClient (Promise Unit)

refetchQueries :: { queryKey :: QueryKey } -> QueryClient -> Aff Unit
refetchQueries args qc =
  Promise.toAffE (runEffectFn2 refetchQueriesImpl args qc)

foreign import isFetchingImpl :: EffectFn2 { queryKey :: QueryKey } QueryClient Int

isFetching :: { queryKey :: QueryKey } -> QueryClient -> Effect Int
isFetching = runEffectFn2 isFetchingImpl

foreign import isMutatingImpl :: EffectFn2 { queryKey :: QueryKey } QueryClient Int

isMutating :: { queryKey :: QueryKey } -> QueryClient -> Effect Int
isMutating = runEffectFn2 isMutatingImpl

type QueryState output =
  { data :: Nullable output
  , dataUpdatedAt :: Nullable Number
  , error :: Nullable Error
  , errorUpdatedAt :: Nullable Number
  , fetchStatus :: String
  , status :: String
  }

foreign import getQueryStateImpl :: forall output. EffectFn2 QueryKey QueryClient (Nullable (QueryState output))

getQueryState :: forall output. QueryKey -> QueryClient -> Effect (Maybe (QueryState output))
getQueryState qk qc = do
  result <- runEffectFn2 getQueryStateImpl qk qc
  pure (Nullable.toMaybe result)

-- | useQuery

foreign import data UseQuery :: Type -> Type

type UseQueryArgs :: forall k. (Type -> k) -> Type -> Row k
type UseQueryArgs f output =
  ( queryKey :: f QueryKey
  , enabled :: f Boolean
  , queryFn :: f (Aff output)
  , staleTime :: f Int
  , gcTime :: f Int
  , refetchOnWindowFocus :: f Boolean
  , refetchOnReconnect :: f Boolean
  , refetchOnMount :: f Boolean
  , refetchInterval :: f Int
  , retry :: f Int
  , retryDelay :: f Int
  , placeholderData :: f output
  , select :: f (output -> output)
  , initialData :: f output
  , initialDataUpdatedAt :: f Number
  , structuralSharing :: f Boolean
  , networkMode :: f String
  )

type UseQueryImplResult output =
  { data :: output
  , isError :: Boolean
  , error :: Error
  , isPending :: Boolean
  , fetchStatus :: String
  , status :: String
  , isRefetching :: Boolean
  , isStale :: Boolean
  , dataUpdatedAt :: Nullable Number
  , errorUpdatedAt :: Nullable Number
  , isFetched :: Boolean
  , isRefetchError :: Boolean
  , isPlaceholderData :: Boolean
  , refetch :: Effect (Promise Unit)
  }

foreign import useQueryImpl :: forall input output. EffectFn1 input (UseQueryImplResult output)

type UseQueryResult output =
  { data :: RemoteData Error output
  , fetchStatus :: FetchStatus
  , status :: QueryStatus
  , refetch :: Aff Unit
  , isRefetching :: Boolean
  , isStale :: Boolean
  , dataUpdatedAt :: Maybe Number
  , errorUpdatedAt :: Maybe Number
  , isFetched :: Boolean
  , isRefetchError :: Boolean
  , isPlaceholderData :: Boolean
  }

useQuery :: forall opts opts_ @output. Promise.Flatten output output => Union opts opts_ (UseQueryArgs Id output) => { | opts } -> Hook UseQuery (UseQueryResult output)
useQuery args' = React.do
  let
    args :: { | UseQueryArgs Opt output }
    args = unsafeCoerce args'
    argsImpl =
      args # RB.build
        ( RB.modify (Proxy :: Proxy "queryFn")
            (pseudoMap Promise.fromAff)
        )
  result <- unsafeHook (runEffectFn1 useQueryImpl argsImpl)
  pure
    { data:
        if result.isPending then RD.Loading
        else if result.isError then RD.Failure result.error
        else RD.Success result.data
    , fetchStatus: parseFetchStatus result.fetchStatus
    , status: parseQueryStatus result.status
    , refetch: Promise.toAffE result.refetch
    , isRefetching: result.isRefetching
    , isStale: result.isStale
    , dataUpdatedAt: Nullable.toMaybe result.dataUpdatedAt
    , errorUpdatedAt: Nullable.toMaybe result.errorUpdatedAt
    , isFetched: result.isFetched
    , isRefetchError: result.isRefetchError
    , isPlaceholderData: result.isPlaceholderData
    }

-- | useMutation

foreign import data UseMutation :: Type -> Type

type UseMutationArgs :: forall k. (Type -> k) -> Type -> Type -> Type -> Row k
type UseMutationArgs f input output ctx =
  ( mutationKey :: f String
  , mutationFn :: f (input -> Aff output)
  , onSettled :: f (Either Error output -> input -> ctx -> Aff Unit)
  , onSuccess :: f (output -> input -> Effect Unit)
  , onError :: f (Error -> input -> ctx -> Aff Unit)
  , onMutate :: f (input -> Effect (Maybe ctx))
  , retry :: f Int
  , retryDelay :: f Int
  )

type UseMutationImplResult input output =
  { data :: output
  , isIdle :: Boolean
  , isError :: Boolean
  , error :: Error
  , isPending :: Boolean
  , mutateAsync :: EffectFn1 input (Promise Unit)
  , variables :: Nullable input
  , reset :: Effect Unit
  , submittedAt :: Nullable Instant
  }

foreign import useMutationImpl ::
  forall input output ctx.
  EffectFn1
    { mutationFn :: Opt (EffectFn1 input (Promise output))
    , mutationKey :: Opt String
    , onSuccess :: Opt (EffectFn2 output input Unit)
    , onSettled :: Opt (EffectFn4 output (Nullable Error) input ctx (Promise Unit))
    , onError :: Opt (EffectFn3 Error input ctx (Promise Unit))
    , onMutate :: Opt (EffectFn1 input (Nullable ctx))
    , retry :: Opt Int
    , retryDelay :: Opt Int
    }
    (UseMutationImplResult input output)

type UseMutationResult input output =
  { data :: RemoteData Error output
  , mutateAsync :: input -> Aff Unit
  , variables :: Maybe input
  , reset :: Effect Unit
  , submittedAt :: Maybe Instant
  }

useMutation ::
  forall opts opts_ input @output ctx.
  Promise.Flatten output output =>
  Union opts opts_ (UseMutationArgs Id input output ctx) =>
  { | opts } ->
  Hook UseMutation (UseMutationResult input output)
useMutation args' = React.do
  let
    args :: { | UseMutationArgs Opt input output ctx }
    args = unsafeCoerce args'
    argsImpl =
      args # RB.build
        ( ( RB.modify (Proxy :: Proxy "mutationFn")
              (pseudoMap (\fn -> mkEffectFn1 \i -> Promise.fromAff (fn i)))
          )
            >>>
              ( RB.modify (Proxy :: Proxy "onSuccess")
                  (pseudoMap mkEffectFn2)
              )
            >>>
              ( RB.modify (Proxy :: Proxy "onSettled")
                  ( pseudoMap
                      \fn -> mkEffectFn4 \result err variables ctx -> do
                        let
                          val = case Nullable.toMaybe err of
                            Nothing -> Right result
                            Just e -> Left e
                        Promise.fromAff (fn val variables ctx)
                  )
              )
            >>>
              ( RB.modify (Proxy :: Proxy "onError")
                  ( pseudoMap
                      \fn -> mkEffectFn3 \err input ctx ->
                        Promise.fromAff (fn err input ctx)
                  )
              )
            >>>
              ( RB.modify (Proxy :: Proxy "onMutate")
                  ( pseudoMap
                      \fn -> mkEffectFn1 \input -> do
                        result <- fn input
                        pure case result of
                          Nothing -> unsafeCoerce undefined
                          Just ctx -> unsafeCoerce ctx
                  )
              )
        )
  implResult :: (UseMutationImplResult input output) <- unsafeHook (runEffectFn1 useMutationImpl argsImpl)
  let
    mutate :: input -> Aff Unit
    mutate input =
      Promise.toAffE (runEffectFn1 implResult.mutateAsync input)

    result :: UseMutationResult input output
    result =
      { data:
          if implResult.isIdle then RD.NotAsked
          else if implResult.isPending then RD.Loading
          else if implResult.isError then RD.Failure implResult.error
          else RD.Success implResult.data
      , mutateAsync: mutate
      , reset: implResult.reset
      , variables: Nullable.toMaybe implResult.variables
      , submittedAt: Nullable.toMaybe implResult.submittedAt
      }
  pure result

-- | useInfiniteQuery

foreign import data UseInfiniteQuery :: Type -> Type

type UseInfiniteQueryArgs :: forall k. (Type -> k) -> Type -> Type -> Row k
type UseInfiniteQueryArgs f output pageParam =
  ( queryKey :: f QueryKey
  , queryFn :: f ({ pageParam :: pageParam } -> Aff output)
  , initialPageParam :: f pageParam
  , getNextPageParam :: f (output -> Array output -> pageParam)
  , getPreviousPageParam :: f (output -> Array output -> pageParam)
  , enabled :: f Boolean
  , staleTime :: f Int
  , gcTime :: f Int
  , refetchOnWindowFocus :: f Boolean
  , refetchInterval :: f Int
  , retry :: f Int
  , maxPages :: f Int
  )

type UseInfiniteQueryImplResult output pageParam =
  { data :: { pages :: Array output, pageParams :: Array pageParam }
  , isError :: Boolean
  , error :: Error
  , isPending :: Boolean
  , fetchStatus :: String
  , hasNextPage :: Boolean
  , hasPreviousPage :: Boolean
  , fetchNextPage :: Effect (Promise Unit)
  , fetchPreviousPage :: Effect (Promise Unit)
  , isFetchingNextPage :: Boolean
  , isFetchingPreviousPage :: Boolean
  , refetch :: Effect (Promise Unit)
  , isRefetching :: Boolean
  }

foreign import useInfiniteQueryImpl :: forall input output pageParam. EffectFn1 input (UseInfiniteQueryImplResult output pageParam)

type InfiniteData output pageParam =
  { pages :: Array output
  , pageParams :: Array pageParam
  }

type UseInfiniteQueryResult output pageParam =
  { data :: RemoteData Error (InfiniteData output pageParam)
  , fetchStatus :: FetchStatus
  , hasNextPage :: Boolean
  , hasPreviousPage :: Boolean
  , fetchNextPage :: Aff Unit
  , fetchPreviousPage :: Aff Unit
  , isFetchingNextPage :: Boolean
  , isFetchingPreviousPage :: Boolean
  , refetch :: Aff Unit
  , isRefetching :: Boolean
  }

useInfiniteQuery ::
  forall opts opts_ @output @pageParam.
  Promise.Flatten output output =>
  Union opts opts_ (UseInfiniteQueryArgs Id output pageParam) =>
  { | opts } ->
  Hook UseInfiniteQuery (UseInfiniteQueryResult output pageParam)
useInfiniteQuery args' = React.do
  let
    args :: { | UseInfiniteQueryArgs Opt output pageParam }
    args = unsafeCoerce args'
    argsImpl =
      args # RB.build
        ( RB.modify (Proxy :: Proxy "queryFn")
            (pseudoMap (\fn -> mkEffectFn1 \ctx -> Promise.fromAff (fn ctx)))
        )
  result <- unsafeHook (runEffectFn1 useInfiniteQueryImpl argsImpl)
  pure
    { data:
        if result.isPending then RD.Loading
        else if result.isError then RD.Failure result.error
        else RD.Success result.data
    , fetchStatus: parseFetchStatus result.fetchStatus
    , hasNextPage: result.hasNextPage
    , hasPreviousPage: result.hasPreviousPage
    , fetchNextPage: Promise.toAffE result.fetchNextPage
    , fetchPreviousPage: Promise.toAffE result.fetchPreviousPage
    , isFetchingNextPage: result.isFetchingNextPage
    , isFetchingPreviousPage: result.isFetchingPreviousPage
    , refetch: Promise.toAffE result.refetch
    , isRefetching: result.isRefetching
    }

-- | useSuspenseQuery

foreign import data UseSuspenseQuery :: Type -> Type

type UseSuspenseQueryArgs :: forall k. (Type -> k) -> Type -> Row k
type UseSuspenseQueryArgs f output =
  ( queryKey :: f QueryKey
  , queryFn :: f (Aff output)
  , staleTime :: f Int
  , gcTime :: f Int
  , refetchOnWindowFocus :: f Boolean
  , refetchOnReconnect :: f Boolean
  , refetchOnMount :: f Boolean
  , refetchInterval :: f Int
  , retry :: f Int
  , retryDelay :: f Int
  , select :: f (output -> output)
  , initialData :: f output
  , initialDataUpdatedAt :: f Number
  , structuralSharing :: f Boolean
  , networkMode :: f String
  )

type UseSuspenseQueryResult output =
  { data :: output
  , fetchStatus :: FetchStatus
  , status :: QueryStatus
  , dataUpdatedAt :: Maybe Number
  , isRefetching :: Boolean
  , refetch :: Aff Unit
  }

foreign import useSuspenseQueryImpl :: forall input output. EffectFn1 input { data :: output, fetchStatus :: String, status :: String, dataUpdatedAt :: Nullable Number, isRefetching :: Boolean, refetch :: Effect (Promise Unit) }

useSuspenseQuery ::
  forall opts opts_ @output.
  Promise.Flatten output output =>
  Union opts opts_ (UseSuspenseQueryArgs Id output) =>
  { | opts } ->
  Hook UseSuspenseQuery (UseSuspenseQueryResult output)
useSuspenseQuery args' = React.do
  let
    args :: { | UseSuspenseQueryArgs Opt output }
    args = unsafeCoerce args'
    argsImpl =
      args # RB.build
        ( RB.modify (Proxy :: Proxy "queryFn")
            (pseudoMap Promise.fromAff)
        )
  result <- unsafeHook (runEffectFn1 useSuspenseQueryImpl argsImpl)
  pure
    { data: result.data
    , fetchStatus: parseFetchStatus result.fetchStatus
    , status: parseQueryStatus result.status
    , dataUpdatedAt: Nullable.toMaybe result.dataUpdatedAt
    , isRefetching: result.isRefetching
    , refetch: Promise.toAffE result.refetch
    }

-- | useSuspenseInfiniteQuery

foreign import data UseSuspenseInfiniteQuery :: Type -> Type

type UseSuspenseInfiniteQueryArgs :: forall k. (Type -> k) -> Type -> Type -> Row k
type UseSuspenseInfiniteQueryArgs f output pageParam =
  ( queryKey :: f QueryKey
  , queryFn :: f ({ pageParam :: pageParam } -> Aff output)
  , initialPageParam :: f pageParam
  , getNextPageParam :: f (output -> Array output -> pageParam)
  , getPreviousPageParam :: f (output -> Array output -> pageParam)
  , staleTime :: f Int
  , gcTime :: f Int
  , refetchInterval :: f Int
  , retry :: f Int
  , maxPages :: f Int
  )

type UseSuspenseInfiniteQueryResult output pageParam =
  { data :: InfiniteData output pageParam
  , fetchStatus :: FetchStatus
  , hasNextPage :: Boolean
  , hasPreviousPage :: Boolean
  , fetchNextPage :: Aff Unit
  , fetchPreviousPage :: Aff Unit
  , isFetchingNextPage :: Boolean
  , isFetchingPreviousPage :: Boolean
  , refetch :: Aff Unit
  , isRefetching :: Boolean
  }

foreign import useSuspenseInfiniteQueryImpl :: forall input output pageParam. EffectFn1 input { data :: { pages :: Array output, pageParams :: Array pageParam }, fetchStatus :: String, hasNextPage :: Boolean, hasPreviousPage :: Boolean, fetchNextPage :: Effect (Promise Unit), fetchPreviousPage :: Effect (Promise Unit), isFetchingNextPage :: Boolean, isFetchingPreviousPage :: Boolean, refetch :: Effect (Promise Unit), isRefetching :: Boolean }

useSuspenseInfiniteQuery ::
  forall opts opts_ @output @pageParam.
  Promise.Flatten output output =>
  Union opts opts_ (UseSuspenseInfiniteQueryArgs Id output pageParam) =>
  { | opts } ->
  Hook UseSuspenseInfiniteQuery (UseSuspenseInfiniteQueryResult output pageParam)
useSuspenseInfiniteQuery args' = React.do
  let
    args :: { | UseSuspenseInfiniteQueryArgs Opt output pageParam }
    args = unsafeCoerce args'
    argsImpl =
      args # RB.build
        ( RB.modify (Proxy :: Proxy "queryFn")
            (pseudoMap (\fn -> mkEffectFn1 \ctx -> Promise.fromAff (fn ctx)))
        )
  result <- unsafeHook (runEffectFn1 useSuspenseInfiniteQueryImpl argsImpl)
  pure
    { data: result.data
    , fetchStatus: parseFetchStatus result.fetchStatus
    , hasNextPage: result.hasNextPage
    , hasPreviousPage: result.hasPreviousPage
    , fetchNextPage: Promise.toAffE result.fetchNextPage
    , fetchPreviousPage: Promise.toAffE result.fetchPreviousPage
    , isFetchingNextPage: result.isFetchingNextPage
    , isFetchingPreviousPage: result.isFetchingPreviousPage
    , refetch: Promise.toAffE result.refetch
    , isRefetching: result.isRefetching
    }

-- | useQueries

foreign import data UseQueries :: Type -> Type

type UseQueryOptions output =
  { queryKey :: QueryKey
  , queryFn :: Aff output
  , enabled :: Opt Boolean
  , staleTime :: Opt Int
  , gcTime :: Opt Int
  }

type UseQueriesImplResult output =
  { data :: output
  , isError :: Boolean
  , error :: Error
  , isPending :: Boolean
  , fetchStatus :: String
  }

foreign import useQueriesImpl :: forall input output. EffectFn1 { queries :: input } (Array (UseQueriesImplResult output))

useQueries :: forall @output. Promise.Flatten output output => Array (UseQueryOptions output) -> Hook UseQueries (Array { data :: RemoteData Error output, fetchStatus :: FetchStatus })
useQueries queries = React.do
  let
    jsQueries = queries <#> \q ->
      { queryKey: q.queryKey
      , queryFn: Promise.fromAff q.queryFn
      , enabled: q.enabled
      , staleTime: q.staleTime
      , gcTime: q.gcTime
      }
  results <- unsafeHook (runEffectFn1 useQueriesImpl { queries: jsQueries })
  pure $ results <#> \r ->
    { data:
        if r.isPending then RD.Loading
        else if r.isError then RD.Failure r.error
        else RD.Success r.data
    , fetchStatus: parseFetchStatus r.fetchStatus
    }

-- | useIsFetching

foreign import data UseIsFetching :: Type -> Type

foreign import useIsFetchingImpl :: forall input. EffectFn1 input Int

useIsFetching :: forall opts opts_. Union opts opts_ (queryKey :: QueryKey) => { | opts } -> Hook UseIsFetching Int
useIsFetching filters = unsafeHook (runEffectFn1 useIsFetchingImpl filters)

useIsFetchingAll :: Hook UseIsFetching Int
useIsFetchingAll = unsafeHook (runEffectFn1 useIsFetchingImpl (unsafeCoerce undefined))

-- | useIsMutating

foreign import data UseIsMutating :: Type -> Type

foreign import useIsMutatingImpl :: forall input. EffectFn1 input Int

useIsMutating :: forall opts opts_. Union opts opts_ (mutationKey :: String) => { | opts } -> Hook UseIsMutating Int
useIsMutating filters = unsafeHook (runEffectFn1 useIsMutatingImpl filters)

useIsMutatingAll :: Hook UseIsMutating Int
useIsMutatingAll = unsafeHook (runEffectFn1 useIsMutatingImpl (unsafeCoerce undefined))

-- | useMutationState

foreign import data UseMutationState :: Type -> Type

type MutationStateEntry =
  { status :: String
  , variables :: Nullable Foreign
  , data :: Nullable Foreign
  , error :: Nullable Error
  , submittedAt :: Nullable Number
  }

foreign import useMutationStateImpl :: forall input. EffectFn1 input (Array MutationStateEntry)

type UseMutationStateFilters =
  ( filters :: { mutationKey :: Opt String, status :: Opt String }
  )

useMutationState :: forall opts opts_. Union opts opts_ UseMutationStateFilters => { | opts } -> Hook UseMutationState (Array MutationStateEntry)
useMutationState opts = unsafeHook (runEffectFn1 useMutationStateImpl opts)

-- | QueryErrorResetBoundary

foreign import data UseQueryErrorResetBoundary :: Type -> Type

foreign import useQueryErrorResetBoundaryImpl :: Effect { reset :: Effect Unit }

useQueryErrorResetBoundary :: Hook UseQueryErrorResetBoundary { reset :: Effect Unit }
useQueryErrorResetBoundary = unsafeHook useQueryErrorResetBoundaryImpl

foreign import queryErrorResetBoundaryImpl :: forall a. ReactComponent { | a }

queryErrorResetBoundary :: ({ reset :: Effect Unit } -> JSX) -> JSX
queryErrorResetBoundary renderFn =
  React.element queryErrorResetBoundaryImpl { children: unsafeCoerce renderFn }

-- | HydrationBoundary

foreign import data DehydratedState :: Type

foreign import hydrationBoundaryImpl :: forall a. ReactComponent { | a }

hydrationBoundary :: { state :: DehydratedState } -> Array JSX -> JSX
hydrationBoundary props children =
  React.element hydrationBoundaryImpl (props # Record.insert (Proxy :: Proxy "children") children)

foreign import dehydrateImpl :: EffectFn1 QueryClient DehydratedState

dehydrate :: QueryClient -> Effect DehydratedState
dehydrate = runEffectFn1 dehydrateImpl

foreign import hydrateImpl :: EffectFn2 QueryClient DehydratedState Unit

hydrate :: QueryClient -> DehydratedState -> Effect Unit
hydrate = runEffectFn2 hydrateImpl

-- | Utilities

foreign import skipTokenImpl :: forall a. a

skipToken :: forall output. Aff output
skipToken = unsafeCoerce skipTokenImpl
