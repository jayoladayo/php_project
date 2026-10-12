import react from '@vitejs/plugin-react';
import { defineConfig } from 'vite';

export default defineConfig({
  root: 'src/web',
  plugins: [react()],
  define: { global: 'globalThis' },
  build: { outDir: '../../dist', emptyOutDir: true },
  server: {
    port: 5173,
    host: '127.0.0.1',
    // Trailing slash matters: '/api' would also capture the dashboard's own /api.ts module.
    proxy: { '/api/': 'http://127.0.0.1:8787' },
  },
});
