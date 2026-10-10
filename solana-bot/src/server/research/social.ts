// Pure analysis of X/Twitter posts about a token. Social activity is context
// only: it is never a reason to buy and never evidence that a token is safe.
import type { RiskFlag, SocialPost } from '../../shared/types.js';

const DAY = 86_400_000;
const PAID = /(#ad\b|#sponsored|\bsponsored\b|paid (promo|partnership)|\bpromo(tion)?\b|#partner)/i;
const HYPE = /(\b\d{2,5}x\b|guaranteed|can'?t lose|next (gem|pepe|bonk)|to the moon|\bmoon(ing)?\b|presale|ape in|don'?t miss|last chance|financial freedom)/i;

export function normalizeText(t: string): string {
  return t
    .toLowerCase()
    .replace(/https?:\/\/\S+/g, '')
    .replace(/[@$#][\w]+/g, '')
    .replace(/[1-9A-HJ-NP-Za-km-z]{32,44}/g, '') // base58 addresses
    .replace(/[^a-z\s]/g, '')
    .replace(/\s+/g, ' ')
    .trim();
}

export function analyzeSocial(posts: SocialPost[], now: Date): { posts: SocialPost[]; signals: RiskFlag[] } {
  const signals: RiskFlag[] = [];
  const annotated = posts.map((p) => {
    const s: string[] = [];
    const ageDays = p.author.createdAt ? (now.getTime() - new Date(p.author.createdAt).getTime()) / DAY : null;
    if (ageDays === null) s.push('account age unknown');
    else if (ageDays < 30) s.push(`new account (${ageDays.toFixed(0)} days old)`);
    if (p.author.followers < 100) s.push('fewer than 100 followers');
    const engagement = p.likes + p.reposts + p.replies;
    if (p.author.followers < 500 && engagement > p.author.followers * 2) s.push('engagement unusually high for follower count');
    if (p.author.following > 0 && p.author.followers / p.author.following < 0.1 && p.author.following > 500) s.push('follows many, followed by few');
    if (PAID.test(p.text)) s.push('paid/sponsored promotion disclosed or implied');
    if (HYPE.test(p.text)) s.push('hype language (not evidence)');
    return { ...p, signals: s };
  });

  // Copied text across different accounts.
  const byText = new Map<string, Set<string>>();
  for (const p of annotated) {
    const n = normalizeText(p.text);
    if (n.length < 20) continue;
    const set = byText.get(n) ?? new Set<string>();
    set.add(p.author.id);
    byText.set(n, set);
  }
  for (const [text, authors] of byText) {
    if (authors.size >= 3) {
      signals.push({
        code: 'social_copied_text',
        kind: 'signal',
        severity: 'high',
        message: `${authors.size} different accounts posted near-identical text ("${text.slice(0, 60)}…"). Copy-paste campaigns often indicate paid or coordinated promotion.`,
        source: 'X',
      });
      for (const p of annotated) if (normalizeText(p.text) === text) p.signals.push('near-identical text posted by other accounts');
    }
  }

  // Burst: many posts from many accounts in a short window.
  const times = annotated.map((p) => new Date(p.createdAt).getTime()).sort((a, b) => a - b);
  const windowMs = 10 * 60_000;
  let maxInWindow = 0;
  for (let i = 0, j = 0; j < times.length; j++) {
    while ((times[j] ?? 0) - (times[i] ?? 0) > windowMs) i++;
    maxInWindow = Math.max(maxInWindow, j - i + 1);
  }
  if (times.length >= 6 && maxInWindow >= 5 && maxInWindow / times.length >= 0.5) {
    signals.push({
      code: 'social_burst',
      kind: 'signal',
      severity: 'warn',
      message: `${maxInWindow} of ${times.length} posts appeared within 10 minutes — consistent with a coordinated push.`,
      source: 'X',
    });
  }

  const newAccts = annotated.filter((p) => p.signals.some((s) => s.startsWith('new account') || s === 'fewer than 100 followers'));
  if (annotated.length >= 4 && newAccts.length / annotated.length >= 0.5) {
    signals.push({
      code: 'social_low_credibility_majority',
      kind: 'signal',
      severity: 'warn',
      message: `${newAccts.length} of ${annotated.length} posts come from new or very small accounts.`,
      source: 'X',
    });
  }
  const paid = annotated.filter((p) => p.signals.some((s) => s.startsWith('paid')));
  if (paid.length) {
    signals.push({ code: 'social_paid_promotion', kind: 'signal', severity: 'warn', message: `${paid.length} post(s) look like paid promotion.`, source: 'X' });
  }
  return { posts: annotated, signals };
}
