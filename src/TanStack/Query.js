import {
    useQuery,
    useMutation,
    useQueryClient,
    QueryClient,
    QueryClientProvider,
    useInfiniteQuery,
    useSuspenseQuery,
    useSuspenseInfiniteQuery,
    useQueries,
    useIsFetching,
    useIsMutating,
    useMutationState,
    useQueryErrorResetBoundary,
    QueryErrorResetBoundary,
    HydrationBoundary,
    dehydrate,
    hydrate,
    hashKey,
    skipToken,
} from '@tanstack/react-query'

export const useQueryImpl = useQuery
export const useMutationImpl = useMutation
export const useQueryClientImpl = useQueryClient
export const useSuspenseQueryImpl = useSuspenseQuery
export const useSuspenseInfiniteQueryImpl = useSuspenseInfiniteQuery
export const useInfiniteQueryImpl = useInfiniteQuery
export const useQueriesImpl = useQueries
export const useIsFetchingImpl = useIsFetching
export const useIsMutatingImpl = useIsMutating
export const useMutationStateImpl = useMutationState
export const useQueryErrorResetBoundaryImpl = useQueryErrorResetBoundary

export const newQueryClient = () => new QueryClient()
export const newQueryClientWithConfigImpl = (config) => new QueryClient(config)
export const queryClientProviderImpl = QueryClientProvider

export const invalidateQueries = args => client => () => client.invalidateQueries(args)
export const removeQueriesImpl = (args, client) => client.removeQueries(args)
export const prefetchQueryImpl = (args, client) => client.prefetchQuery(args)
export const fetchQueryImpl = (args, client) => client.fetchQuery(args)
export const cancelQueriesImpl = (args, client) => client.cancelQueries(args)
export const resetQueriesImpl = (args, client) => client.resetQueries(args)
export const refetchQueriesImpl = (args, client) => client.refetchQueries(args)
export const isFetchingImpl = (filters, client) => client.isFetching(filters)
export const isMutatingImpl = (filters, client) => client.isMutating(filters)
export const getQueryStateImpl = (queryKey, client) => client.getQueryState(queryKey) ?? null

export const getQueryDataImpl = args => client => () => client.getQueryData(args)
export const setQueryDataImpl = (args, data, client) => client.setQueryData(args, data)

export const queryErrorResetBoundaryImpl = QueryErrorResetBoundary
export const hydrationBoundaryImpl = HydrationBoundary

export const dehydrateImpl = (client) => dehydrate(client)
export const hydrateImpl = (client, state) => hydrate(client, state)

export const hashKeyImpl = (_, queryKey) => hashKey(queryKey)
export const skipTokenImpl = skipToken
