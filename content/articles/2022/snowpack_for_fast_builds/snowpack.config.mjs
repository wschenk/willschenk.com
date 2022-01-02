// snowpack.config.mjs
export default {
    mount: {
        src: '/dist',
        public: '/',
    },
    devOptions: {
        tailwindConfig: './tailwind.config.js',
    },
    plugins: [
        '@snowpack/plugin-postcss',
    ],
};
