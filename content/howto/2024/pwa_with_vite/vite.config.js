import { defineConfig } from 'vite'
import { VitePWA } from 'vite-plugin-pwa'

const pwa =  VitePWA({
    devOptions: { enabled: true },
    includeAssets: ['*/*png', 'favicon.ico'],
    pwaAssets: {
        image: 'public/logo.png'
    },
    workbox: {
        globPatterns: ['**/*.{js,css,html,ico,png,svg}'],
        runtimeCaching: [ {
            urlPattern: /\/images.*/,
            handler: 'NetworkFirst'
        }]
    },
    registerType: 'autoupdate',
    manifest: {
        name: 'Electric APP',
        short_name: 'EAPP',
        description: 'This is really really fun',
        theme_color: '#ffffff',
        icons: [
          {
            src: 'pwa-192x192.png',
            sizes: '192x192',
            type: 'image/png'
          },
          {
            src: 'pwa-512x512.png',
            sizes: '512x512',
            type: 'image/png'
          }
        ]
    }
})

export default defineConfig({
    plugins: [
        pwa
    ]
})
