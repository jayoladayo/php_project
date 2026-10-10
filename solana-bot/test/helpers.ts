import { DEFAULT_SETTINGS } from '../src/shared/defaults.js';
import type { Fill, Settings } from '../src/shared/types.js';

export const settings = (patch: Partial<Settings> = {}): Settings => structuredClone({ ...DEFAULT_SETTINGS, ...patch });

let n = 0;
export function fill(p: Partial<Fill> & Pick<Fill, 'side' | 'inputRaw' | 'outputRaw'>): Fill {
  n += 1;
  return {
    id: `f${n}`,
    at: '2026-10-10T12:00:00.000Z',
    mode: 'paper',
    reason: 'manual',
    quotedOutRaw: p.outputRaw,
    requestedInRaw: p.inputRaw,
    networkFeeLamports: '0',
    priorityFeeLamports: '0',
    platformFeeLamports: '0',
    rentLamports: '0',
    ...p,
  };
}

export const META = { id: 'pos1', mint: 'MintAddr1111111111111111111111111111111111', symbol: 'TEST', decimals: 6, wallet: null };
