import { useEffect, useState } from 'react';
import { api } from '../api.js';

interface Entry {
  seq: number;
  ts: string;
  event: string;
  data: unknown;
  hash: string;
}

export function Audit() {
  const [entries, setEntries] = useState<Entry[]>([]);
  const [broken, setBroken] = useState<number | null>(null);
  const load = () =>
    api<{ entries: Entry[]; chainBrokenAt: number | null }>('/audit?limit=300').then((r) => {
      setEntries(r.entries);
      setBroken(r.chainBrokenAt);
    });
  useEffect(() => {
    void load();
  }, []);
  return (
    <section>
      <div className="row">
        <p className="grow muted">Append-only log of research inputs, decisions, quotes, approvals and transaction signatures. Each entry is hash-chained to the previous one; secrets are redacted before writing.</p>
        <button onClick={load}>Refresh</button>
      </div>
      <p className={broken === null ? 'pos' : 'neg'}>{broken === null ? 'Hash chain intact.' : `Hash chain broken at entry ${broken} — the log file was modified.`}</p>
      <table className="list small">
        <thead><tr><th>#</th><th>Time</th><th>Event</th><th>Data</th></tr></thead>
        <tbody>
          {entries.map((e) => (
            <tr key={e.seq}>
              <td>{e.seq}</td>
              <td>{new Date(e.ts).toLocaleString()}</td>
              <td>{e.event}</td>
              <td><details><summary>{JSON.stringify(e.data).slice(0, 120)}</summary><pre>{JSON.stringify(e.data, null, 2)}</pre></details></td>
            </tr>
          ))}
        </tbody>
      </table>
    </section>
  );
}
