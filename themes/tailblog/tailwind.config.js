/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ['layouts/**/*.html'],
  theme: {
    extend: {
      fontFamily: {
        sans: ['Atkinson Hyperlegible', 'sans-serif'],
        mono: ['Fira Code', 'monospace']
      },
    },
  },
  plugins: [],
}
