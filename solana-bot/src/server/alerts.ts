import { randomUUID } from 'node:crypto';
import { redactString } from './audit.js';
import { env } from './env.js';
import type { Store } from './store.js';
import type { Alert } from '../shared/types.js';

// Fire-and-forget delivery; notification failures must never affect trading decisions.
function post(url: string, body: unknown): void {
  fetch(url, { method: 'POST', headers: { 'content-type': 'application/json' }, body: JSON.stringify(body), signal: AbortSignal.timeout(5_000) }).catch(() => undefined);
}

export class Alerts {
  constructor(private readonly store: Store) {}

  raise(level: Alert['level'], message: string): Alert {
    const a: Alert = { id: randomUUID(), at: new Date().toISOString(), level, message: redactString(message), acknowledged: false };
    this.store.update((s) => s.alerts.push(a));
    const line = `[alert:${level}] ${a.message}`;
    if (level === 'error') console.error(line);
    else console.warn(line);
    const text = `[solana-bot ${level}] ${a.message}`;
    // Slack uses "text", Discord uses "content".
    if (env.alertWebhookUrl) post(env.alertWebhookUrl, { text, content: text });
    if (env.telegramBotToken && env.telegramChatId) post(`https://api.telegram.org/bot${env.telegramBotToken}/sendMessage`, { chat_id: env.telegramChatId, text });
    return a;
  }

  acknowledge(id: string): void {
    this.store.update((s) => {
      const a = s.alerts.find((x) => x.id === id);
      if (a) a.acknowledged = true;
    });
  }
}
