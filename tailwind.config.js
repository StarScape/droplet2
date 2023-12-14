/** @type {import('tailwindcss').Config} */

const jsContent = process.env.NODE_ENV == 'production' ? "./public/js/main.js" : "./public/js/cljs-runtime/*.js";

module.exports = {
  content: [jsContent, "./public/index.html"],
  theme: {
    extend: {
      colors: {
        "light-blue": "#b4ddff",
        "dark-blue": "#008cff",
      },
    },
  },
  darkMode: "class",
  plugins: [],
}
