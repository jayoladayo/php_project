"""doe_anova.py - small, dependency-light ANOVA helper used by every verification script
in this replication pack (needs only numpy, pandas, scipy).

anova(df, response, terms, ss_type=1)
    terms: list of model terms; a term is a column name ("Block") or an interaction
    written with ':' ("Mode:Zinc"). Every factor is treated as categorical
    (sum-to-zero coding). ss_type=1 gives sequential SS in the order given
    (identical to Type III when the design is balanced); ss_type=3 gives
    partial (adjusted) SS.
Returns a pandas DataFrame with Source, df, SS, MS, F, p.
"""
import numpy as np
import pandas as pd
from scipy import stats


def _codes(series):
    lv = sorted(series.unique(), key=lambda v: (str(type(v)), v))
    if len(lv) < 2:
        return []
    last = lv[-1]
    return [np.where(series == l, 1.0, np.where(series == last, -1.0, 0.0)) for l in lv[:-1]]


def _term_columns(df, term):
    parts = term.split(":")
    cols = [np.ones(len(df))]
    for p in parts:
        c = _codes(df[p])
        cols = [a * b for a in cols for b in c]
    return cols


def anova(df, response, terms, ss_type=1, error_name="Error"):
    y = df[response].to_numpy(float)
    n = len(y)
    blocks = {t: _term_columns(df, t) for t in terms}

    def fit(keys):
        X = np.column_stack([np.ones(n)] + [c for k in keys for c in blocks[k]])
        beta, *_ = np.linalg.lstsq(X, y, rcond=None)
        r = y - X @ beta
        return float(r @ r), int(np.linalg.matrix_rank(X))

    sse_full, rank_full = fit(terms)
    dfe = n - rank_full
    mse = sse_full / dfe if dfe > 0 else np.nan
    rows = []
    prev_sse, prev_rank = fit([])
    for i, t in enumerate(terms):
        if ss_type == 1:
            s, r = fit(terms[: i + 1])
            ss, d = prev_sse - s, r - prev_rank
            prev_sse, prev_rank = s, r
        else:
            s, r = fit([u for u in terms if u != t])
            ss, d = s - sse_full, rank_full - r
        ms = ss / d if d else np.nan
        F = ms / mse if dfe > 0 else np.nan
        p = stats.f.sf(F, d, dfe) if dfe > 0 else np.nan
        rows.append([t.replace(":", " x "), d, ss, ms, F, p])
    rows.append([error_name, dfe, sse_full, mse, np.nan, np.nan])
    rows.append(["Total", n - 1, float(((y - y.mean()) ** 2).sum()), np.nan, np.nan, np.nan])
    return pd.DataFrame(rows, columns=["Source", "df", "SS", "MS", "F", "p"])


def show(title, table, fmt="{:,.4f}"):
    print(f"\n=== {title}")
    with pd.option_context("display.float_format", fmt.format, "display.width", 160):
        print(table.to_string(index=False))


def compare(label, ours, paper, tol_rel=0.02):
    """Print our value beside the paper's and flag relative differences above tol_rel."""
    if paper is None or (isinstance(paper, float) and np.isnan(paper)):
        flag = "not printed"
    else:
        diff = abs(ours - paper) / max(abs(paper), 1e-12)
        flag = "MATCH" if diff <= tol_rel else f"DIFFERS ({diff:.1%})"
    print(f"  {label:<42s} ours={ours:>14,.4f}   paper={paper if paper is not None else '-':>14}   {flag}")
