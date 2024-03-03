/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ['layouts/**/*.html'],
  theme: {
    extend: {
      fontFamily: {
        sans: ['Inter', 'sans-serif'],
        mono: ['Fira Code', 'monospace']
      },
    },
  },
    plugins: [
        require('@tailwindcss/typography'),
    ],
}
