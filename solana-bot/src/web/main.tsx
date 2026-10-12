import { Buffer } from 'buffer';
(globalThis as unknown as { Buffer: typeof Buffer }).Buffer ??= Buffer;

import { WalletProvider } from '@solana/wallet-adapter-react';
import { WalletModalProvider } from '@solana/wallet-adapter-react-ui';
import '@solana/wallet-adapter-react-ui/styles.css';
import { StrictMode } from 'react';
import { createRoot } from 'react-dom/client';
import { App } from './App.js';
import './styles.css';

// wallets=[]: modern wallets (Phantom, Solflare, Backpack, ...) register
// themselves through the Wallet Standard and are detected automatically.
createRoot(document.getElementById('root')!).render(
  <StrictMode>
    <WalletProvider wallets={[]} autoConnect={false}>
      <WalletModalProvider>
        <App />
      </WalletModalProvider>
    </WalletProvider>
  </StrictMode>,
);
