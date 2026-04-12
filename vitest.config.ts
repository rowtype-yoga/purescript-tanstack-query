import {defineConfig} from 'vitest/config'
import react from '@vitejs/plugin-react'

// https://vitejs.dev/config/
export default defineConfig({
    // plugins: [react()],
    test: {
        environment: 'jsdom',
        include: ['output/Test.*Test/index.js'],
        coverage: {
            provider: 'v8',
            include: ['output/TanStack.Query/index.js'],
            reporter: ['text'],
        },
    },
})
