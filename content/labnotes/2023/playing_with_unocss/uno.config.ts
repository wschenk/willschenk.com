// uno.config.ts
import { defineConfig, presetAttributify, presetUno, presetTypography } from 'unocss'
import presetWebFonts from '@unocss/preset-web-fonts';

const fonts = presetWebFonts({
  provider: 'google', // default provider
  fonts: {
    header: "Averia Serif Libre",
  }
})

export default defineConfig({
  presets: [
    presetAttributify({ /* preset options */}),
    presetUno(),
    fonts,
    presetTypography()
    // ...custom presets
  ],
})
