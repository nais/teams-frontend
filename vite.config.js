import { resolve } from 'path'
import { defineConfig } from 'vite'
import { plugin } from 'vite-plugin-elm'

export default defineConfig({
  root: './public/',
  plugins: [plugin()],
  server: {
    port: 3001,
  },
  build: {
    rollupOptions: {
      input: {
        index: resolve(__dirname + '/public/', 'index.html'),
      },
    },
  },
})
