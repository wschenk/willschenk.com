// uno.config.ts
import {
    defineConfig,
    presetAttributify,
    presetTypography,
    presetUno
} from 'unocss'

import presetWebFonts from '@unocss/preset-web-fonts';

const fonts = presetWebFonts({
    provider: 'google', // default provider
    fonts: {
        sans: [ { name: 'Quicksand', weights: [ '300', '700'] } ] // Quicksand
    }
})

export default defineConfig({
  presets: [
      presetAttributify(), // required when using attributify mode
      presetUno(), // required
      presetTypography(),
      fonts,
  ],
})
