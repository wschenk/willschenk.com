import { defineConfig } from 'tsup'

export default defineConfig({
    entry: ['render.tsx', 'resend.tsx'],
    target: 'es2024'
})
