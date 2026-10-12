// Local command-line access to the bot wallet:
//   npm run wallet:address   print the bot wallet's public address
//   npm run wallet:export    print its private key (base58, importable into Phantom/Solflare)
// The key is never available through the web API.
import 'dotenv/config';
import { join } from 'node:path';
import bs58 from 'bs58';
import { BotWallet } from './botWallet.js';

const wallet = new BotWallet(process.env.BOT_WALLET_PATH ?? join('data', 'bot-wallet.json'));

function askHidden(question: string): Promise<string> {
  return new Promise((resolve) => {
    const stdin = process.stdin;
    process.stdout.write(question);
    if (!stdin.isTTY) {
      let buf = '';
      stdin.setEncoding('utf8');
      stdin.on('data', (c) => (buf += c));
      stdin.on('end', () => resolve(buf.split(/\r?\n/)[0] ?? ''));
      return;
    }
    stdin.setRawMode(true);
    stdin.resume();
    stdin.setEncoding('utf8');
    let s = '';
    const onData = (chunk: string) => {
      for (const ch of chunk) {
        if (ch === '\r' || ch === '\n') {
          stdin.setRawMode(false);
          stdin.pause();
          stdin.off('data', onData);
          process.stdout.write('\n');
          resolve(s);
          return;
        }
        if (ch === '\u0003') {
          process.stdout.write('\n');
          process.exit(1);
        }
        if (ch === '\u007f' || ch === '\b') s = s.slice(0, -1);
        else s += ch;
      }
    };
    stdin.on('data', onData);
  });
}

async function main() {
  const cmd = process.argv[2];
  if (!wallet.exists()) {
    console.log('No bot wallet yet. Create one in the dashboard (Bot wallet tab).');
    process.exit(1);
  }
  if (cmd === 'address') {
    console.log(wallet.publicKey());
    return;
  }
  if (cmd === 'export') {
    console.log('WARNING: anyone who sees the private key can take everything in this wallet.');
    console.log('Only do this on your own computer, and do not paste it into websites or chats.\n');
    const pass = await askHidden('Bot wallet passphrase: ');
    const kp = wallet.decrypt(pass);
    console.log(`\nAddress:     ${kp.publicKey.toBase58()}`);
    console.log(`Private key: ${bs58.encode(kp.secretKey)}`);
    console.log('\nIn Phantom: Add / Connect Wallet → Import Private Key → paste the line above.');
    return;
  }
  console.log('Usage: npm run wallet:address | npm run wallet:export');
  process.exit(1);
}

main().catch((e) => {
  console.error((e as Error).message);
  process.exit(1);
});
