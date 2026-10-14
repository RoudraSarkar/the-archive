"""Compare kinship.py against DemoKin 1.0.3 on DemoKin's Swedish example data."""
import sys, os
import numpy as np, pandas as pd
sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
from kinship import kin_time_invariant, kin_time_variant, KIN
V = sys.argv[1]
px = pd.read_csv(f"{V}/swe_px.csv", index_col=0); fx = pd.read_csv(f"{V}/swe_asfr.csv", index_col=0); nx = pd.read_csv(f"{V}/swe_pop.csv", index_col=0)
years = [int(c) for c in px.columns]
# 1) time-invariant, year 2000
ref = pd.read_csv(f"{V}/demokin_swe_2000_invariant.csv")
K = kin_time_invariant(px["2000"].values, fx["2000"].values)
ref["py"] = [K[k][ak, af] for k, ak, af in zip(ref.kin, ref.age_kin, ref.age_focal)]
e1 = (ref.py - ref.living).abs()
print(f"time-invariant 2000: max abs diff {e1.max():.2e} over {len(ref):,} cells")
# 2) time-variant, cohorts 1930 and 1950
ref = pd.read_csv(f"{V}/demokin_swe_cohorts.csv")
need = set(ref.year.unique())
out = kin_time_variant(px.values, fx.values, years, n=nx.values, keep_years=need)
ref["py"] = [out[y][k][ak, af] if y in out else np.nan for k, ak, af, y in zip(ref.kin, ref.age_kin, ref.age_focal, ref.year)]
e2 = (ref.py - ref.living).abs()
print(f"time-variant cohorts 1930/1950: max abs diff {e2.max():.2e} over {ref.py.notna().sum():,} cells (missing {ref.py.isna().sum()})")
tot = ref[ref.age_focal == 50].groupby(["cohort", "kin"])[["living", "py"]].sum().round(4)
print("\nliving kin at focal age 50 (sum over kin ages), DemoKin vs Python:")
print(tot.unstack(0).to_string())
