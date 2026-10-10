import { randomUUID } from 'node:crypto';
import { redactString } from './audit.js';
import { env } from './env.js';
import type { Store } from './store.js';
import type { Alert } from '../shared/types.js';

export class Alerts {
  constructor(private readonly store: Store) {}

  raise(level: Alert['level'], message: string): Alert {
    const a: Alert = { id: randomUUID(), at: new Date().toISOString(), level, message: redactString(message), acknowledged: false };
    this.store.update((s) => s.alerts.push(a));
    const line = `[alert:${level}] ${a.message}`;
    if (level === 'error') console.error(line);
    else console.warn(line);
    if (env.alertWebhookUrl) {
      // Fire-and-forget; webhook failures must never affect trading decisions.
      fetch(env.alertWebhookUrl, {
        method: 'POST',
        headers: { 'content-type': 'application/json' },
        body: JSON.stringify({ text: `[solana-bot ${level}] ${a.message}` }),
        signal: AbortSignal.timeout(5_000),
      }).catch(() => undefined);
    }
    return a;
  }

  acknowledge(id: string): void {
    this.store.update((s) => {
      const a = s.alerts.find((x) => x.id === id);
      if (a) a.acknowledged = true;
    });
  }
}
