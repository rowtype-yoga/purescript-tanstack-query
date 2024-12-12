import {defineConfig} from 'vitest/config'
import react from '@vitejs/plugin-react'

// https://vitejs.dev/config/
export default defineConfig({
    // plugins: [react()],
    test: {
        // environment: 'happy-dom',
        environment: 'jsdom',
        // browser: {
        //     provider: 'playwright',
        //     name: 'chromium',
        //     enabled: true,
        // },
        // setupFiles: ['setupTests.ts'],
        include: ['output/Test.*Test/index.js'],
    },
})
