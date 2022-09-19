import { resolve } from 'path'
import { defineConfig } from 'vite'
import { plugin } from 'vite-plugin-elm'

export default defineConfig({
  root: './public/',
  plugins: [plugin()],
  server: {
    port: 3001,
    strictPort: true,
    proxy: {
      "^(/oauth2|/query)": {
        target: "http://localhost:3000",
        changeOrigin: true,
      },
    },
  },
  build: {
    rollupOptions: {
      input: {
        index: resolve(__dirname + '/public/', 'index.html'),
      },
    },
  },
})
