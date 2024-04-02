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
        header: [ {
            name: "Montserrat",
            weights: ['400', '700']
        } ],
        sans: [ { name: 'Inter' } ]
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
