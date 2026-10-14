"""A_analysis.py — all data engineering, methods and models for
'Why Should We Care About Low Fertility?'.

Contract (same as names~fertility): A writes one tidy CSV per chart to out/;
B_figures.py reads out/ and nothing else. Any aggregation belongs here.

Usage:  python3 A_analysis.py [c01 c02 ...]   (no args = run everything)
Paths are relative to the project root (the folder holding data/ and out/).
"""
import sys, os, gzip
import numpy as np
import pandas as pd

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
DATA = os.path.join(ROOT, "data")
OUT = os.path.join(ROOT, "out")
os.makedirs(OUT, exist_ok=True)
WPP = os.path.join(DATA, "shared_UN_WPP_2024")

FOCUS = ["JPN", "KOR", "ITA", "DEU", "GBR", "USA"]   # anchor countries (agreed 3 Oct)


def save(df, name, note=""):
    p = os.path.join(OUT, name)
    df.to_csv(p, index=False)
    print(f"  wrote out/{name}  ({len(df):,} rows) {note}")


# ---------------------------------------------------------------- C1
def c01_dependency():
    """65+ per 100 working-age adults, 1950-2100, by UN WPP 2024 scenario.
    Two working-age definitions are written so the choice can be made in B:
      wa2064 = 20-64 (OECD convention), wa1564 = 15-64 (UN convention).
    OECD-38 aggregate = population-summed (not an average of country ratios)."""
    use = ["ISO3_code", "Location", "Variant", "Time", "AgeGrpStart", "PopTotal"]
    med = pd.read_csv(os.path.join(WPP, "WPP2024_PopulationByAge5GroupSex_Medium_OECD38.csv.gz"), usecols=use)
    oth = pd.read_csv(os.path.join(WPP, "WPP2024_PopulationByAge5GroupSex_OtherVariants_OECD38.csv.gz"), usecols=use)
    keep = ["Low", "High", "Instant replacement", "Zero migration", "Constant fertility"]
    pop = pd.concat([med, oth[oth.Variant.isin(keep)]], ignore_index=True)
    a = pop.AgeGrpStart
    pop["old"] = np.where(a >= 65, pop.PopTotal, 0.0)
    pop["wa2064"] = np.where((a >= 20) & (a < 65), pop.PopTotal, 0.0)
    pop["wa1564"] = np.where((a >= 15) & (a < 65), pop.PopTotal, 0.0)
    g = pop.groupby(["ISO3_code", "Location", "Variant", "Time"], as_index=False)[["old", "wa2064", "wa1564"]].sum()
    oecd = g.groupby(["Variant", "Time"], as_index=False)[["old", "wa2064", "wa1564"]].sum()
    oecd["ISO3_code"], oecd["Location"] = "OECD", "OECD-38 (population-weighted)"
    g = pd.concat([g, oecd], ignore_index=True)
    g["ratio_65_per100_2064"] = 100 * g.old / g.wa2064
    g["ratio_65_per100_1564"] = 100 * g.old / g.wa1564
    g["workers2064_per_65plus"] = g.wa2064 / g.old
    g = g.rename(columns={"ISO3_code": "iso3", "Location": "country", "Variant": "variant", "Time": "year",
                          "old": "pop65plus_thousands", "wa2064": "pop2064_thousands", "wa1564": "pop1564_thousands"})
    g["focus"] = g.iso3.isin(FOCUS + ["OECD"])
    save(g.sort_values(["iso3", "variant", "year"]), "c01_dependency_ratio_by_variant.csv")
    return g


# ---------------------------------------------------------------- C2
EU_ANNEX = os.path.join(DATA, "C02_fiscal_pension_projections", "EU_2024_Ageing_Report_statistical_annex.xlsx")

def _eu_table(rows, table_no):
    """Parse one 'Table II.1.<n>' block of the EU 2024 Ageing Report cross-country sheet into long form."""
    import re
    tag = f"Table II.1.{table_no}:"
    start = next(i for i, r in enumerate(rows) if r[0] and str(r[0]).startswith(tag))
    title = str(rows[start][0]).split(":", 1)[1].strip()
    h = start + 1
    while not (rows[h][0] and str(rows[h][0]).startswith("Ch")):   # skip blank rows to the header
        h += 1
    years = [int(c) for c in rows[h][1:] if isinstance(c, (int, float))]
    out = []
    for r in rows[h + 1:]:
        if r[0] is None:
            if out: break          # blank row after the data ends the block
            continue
        if str(r[0]).startswith("Table"):
            break
        code = str(r[0]).strip()
        # r[1] = change 2022-70; r[2:] = one value per year
        for y, v in zip(years, r[2:2 + len(years)]):
            if isinstance(v, (int, float)):
                out.append((table_no, title, code, y, float(v)))
    return pd.DataFrame(out, columns=["table", "title", "geo", "year", "value"])

def c02_fiscal():
    """Total cost of ageing (% GDP), EU 2024 Ageing Report: baseline vs lower-fertility (-20%) and migration scenarios, 2022-2070."""
    import openpyxl
    wb = openpyxl.load_workbook(EU_ANNEX, read_only=True, data_only=True)
    rows = []
    for r in wb["cross-country_tables"].iter_rows(values_only=True):
        r = list(r)
        while r and r[0] is None:      # sheet has leading blank columns
            r.pop(0)
        rows.append(tuple(r) if r else (None,))
    scen = {135: "baseline", 140: "lower_fertility_minus20", 138: "higher_migration_plus33", 139: "lower_migration_minus33"}
    df = pd.concat([_eu_table(rows, t) for t in scen], ignore_index=True)
    df["scenario"] = df.table.map(scen)
    wide = df.pivot_table(index=["geo", "year"], columns="scenario", values="value").reset_index()
    wide["fertility_gap_pp"] = wide.lower_fertility_minus20 - wide.baseline
    wide["lowmig_gap_pp"] = wide.lower_migration_minus33 - wide.baseline
    save(wide.sort_values(["geo", "year"]), "c02_eu_cost_of_ageing_scenarios.csv",
         "(EU/EA aggregates + 27 MS + NO; % of GDP; gap = scenario minus baseline)")
    return wide


# ---------------------------------------------------------------- C3
def _wpp_matrix(files, value, iso, variant=None, ages=range(101)):
    frames = [pd.read_csv(os.path.join(WPP, f), usecols=lambda c: c in
              ["ISO3_code", "Variant", "Time", "AgeGrpStart", value]) for f in files]
    d = pd.concat(frames, ignore_index=True)
    d = d[d.ISO3_code == iso]
    if variant is not None:
        d = d[d.Variant == variant]
    m = d.pivot_table(index="AgeGrpStart", columns="Time", values=value).reindex(list(ages)).fillna(0.0)
    return m

def c03_kinship():
    """Elderly kin (65+) per potential family carer (20-64) at focal age 50, cohorts 1950/70/90/2010 (kinship model)."""
    sys.path.insert(0, os.path.dirname(__file__))
    from kinship import kin_time_variant
    LT_F = ["WPP2024_Life_Table_Complete_Medium_Female_1950-2023_OECD38.csv.gz",
            "WPP2024_Life_Table_Complete_Medium_Female_2024-2100_OECD38.csv.gz"]
    POP = ["WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023_OECD38.csv.gz",
           "WPP2024_Population1JanuaryBySingleAgeSex_Medium_2024-2100_OECD38.csv.gz"]
    FERT = ["WPP2024_Fertility_by_Age1_OECD38.csv.gz"]
    cohorts, focal_age = [1950, 1970, 1990, 2010], 50
    years = list(range(1950, 2100))                    # rate years; kin observed 1 Jan of year+1
    keep = set(c + a for c in cohorts for a in range(30, 81))
    rows, prof = [], []
    for iso in FOCUS:
        px = _wpp_matrix(LT_F, "px", iso)[years].values
        fx = (_wpp_matrix(FERT, "ASFR", iso, "Medium")[years] / 1000.0).values
        nx = _wpp_matrix(POP, "PopFemale", iso)[years].values
        # Androgynous approximation (Whiz, 8 Oct): men share women's rates. Running with birth_female=1
        # counts children of both sexes on the maternal side; paternal side mirrors it (x2 for parents,
        # aunts/uncles, cousins). Siblings are shared, so not doubled.
        out = kin_time_variant(px, fx, years, n=nx, birth_female=1.0, keep_years=keep, stable_birth_female=1 / 2.05)
        for c in cohorts:
            for age in range(30, 81):
                y = c + age
                if y not in out:
                    continue
                K = out[y]
                v = lambda k: K[k][:, age]
                parents = 2 * v("m"); aunts = 2 * (v("oa") + v("ya"))
                sibs = v("os") + v("ys"); cousins = 2 * (v("coa") + v("cya"))
                old = slice(65, 101); wa = slice(20, 65)
                rec = dict(iso3=iso, cohort=c, focal_age=age, year=y,
                           parents_65p=parents[old].sum(), aunts_uncles_65p=aunts[old].sum(),
                           siblings_2064=sibs[wa].sum(), cousins_2064=cousins[wa].sum(),
                           parents_alive=parents.sum(), aunts_uncles_alive=aunts.sum(),
                           siblings_alive=sibs.sum(), cousins_alive=cousins.sum(),
                           children_alive=v("d").sum(), grandchildren_alive=v("gd").sum())
                rec["elderly_kin_65p"] = rec["parents_65p"] + rec["aunts_uncles_65p"]
                rec["carers_2064"] = (1.0 if 20 <= age <= 64 else 0.0) + rec["siblings_2064"] + rec["cousins_2064"]
                rec["elderly_per_carer"] = rec["elderly_kin_65p"] / rec["carers_2064"] if rec["carers_2064"] > 0 else np.nan
                rec["carers_per_elderly"] = rec["carers_2064"] / rec["elderly_kin_65p"] if rec["elderly_kin_65p"] > 0 else np.nan
                prof.append(rec)
                if age == focal_age:
                    rows.append(rec)
        print(f"    {iso} done")
    save(pd.DataFrame(rows), "c03_kin_burden_age50.csv", "(headline: focal age 50)")
    save(pd.DataFrame(prof), "c03_kin_profiles_age30to80.csv", "(full focal-age profiles, for checking)")


# ---------------------------------------------------------------- C4
def c04_living_alone():
    """Share of people aged 65+ living alone: Japan (1980-2050 incl. projections), Korea (2000-2025), Eurostat EU (2003-2024)."""
    D4 = os.path.join(DATA, "C04_elderly_living_alone")
    # --- Japan: Cabinet Office white paper fig 1-1-9 gives % of 65+ men and of 65+ women living alone.
    raw = pd.read_csv(os.path.join(D4, "JP_CabinetOffice_AgeingWhitePaper2025_fig1-1-9.csv"), encoding="shift_jis", header=None)
    jp = []
    for _, r in raw.iterrows():
        yr = pd.Series(r[2]).astype(str).str.extract(r"(\d{4})")[0].iloc[0]
        if pd.notna(yr) and pd.notna(r[9]) and pd.notna(r[10]):
            jp.append(dict(year=int(yr), men=float(r[9]), women=float(r[10]),
                           projected=str(r[0]).startswith("（推計")))
    jp = pd.DataFrame(jp)
    # Total share = sex shares weighted by WPP 65+ population by sex (the white paper does not print a total %)
    use = ["ISO3_code", "Time", "AgeGrpStart", "PopMale", "PopFemale"]
    pop = pd.concat([pd.read_csv(os.path.join(WPP, f), usecols=use) for f in
                     ["WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023_OECD38.csv.gz",
                      "WPP2024_Population1JanuaryBySingleAgeSex_Medium_2024-2100_OECD38.csv.gz"]])
    pop = pop[(pop.ISO3_code == "JPN") & (pop.AgeGrpStart >= 65)].groupby("Time")[["PopMale", "PopFemale"]].sum()
    jp = jp.join(pop, on="year")
    jp["total"] = (jp.men * jp.PopMale + jp.women * jp.PopFemale) / (jp.PopMale + jp.PopFemale)
    jp_long = jp.melt(id_vars=["year", "projected"], value_vars=["total", "men", "women"], var_name="sex", value_name="pct_65plus_alone")
    jp_long["geo"], jp_long["source"] = "JP", "Cabinet Office Ageing White Paper 2025 fig 1-1-9 (census; 2025+ IPSS projection); total weighted by WPP 65+ pop"
    jp_long["series_break"] = False
    # --- Korea: national indicator 4233 (census; register-based from 2015)
    kr = pd.read_csv(os.path.join(D4, "KR_elderly_living_alone_ratio_census.csv"))
    kr = kr.rename(columns={"pct_65plus_living_alone": "pct_65plus_alone"})[["year", "pct_65plus_alone"]]
    kr["geo"], kr["sex"], kr["projected"] = "KR", "total", False
    kr["series_break"] = kr.year == 2015
    kr["source"] = "Statistics Korea census via index.go.kr 4233; register-based census from 2015 (series break)"
    # --- Eurostat: share of 65+ population in 'one adult 65 years or over' households
    eu = pd.read_csv(os.path.join(D4, "eurostat_ilc_lvps30_65plus_household_type.csv"))
    eu = eu[(eu.hhcomp == "A1_GE65")].rename(columns={"TIME_PERIOD": "year", "OBS_VALUE": "pct_65plus_alone"})
    eu["sex"] = eu.sex.map({"T": "total", "M": "men", "F": "women"})
    eu["projected"], eu["series_break"] = False, False
    eu["source"] = "Eurostat ilc_lvps30 (EU-SILC)"
    eu = eu[["geo", "year", "sex", "pct_65plus_alone", "projected", "series_break", "source"]]
    out = pd.concat([jp_long, kr, eu], ignore_index=True)[["geo", "year", "sex", "pct_65plus_alone", "projected", "series_break", "source"]]
    EU27 = set("AT BE BG CY CZ DE DK EE EL ES FI FR HR HU IE IT LT LU LV MT NL PL PT RO SE SI SK".split())
    out["eu27_member"] = out.geo.isin(EU27)          # for the EU reference band (Eurostat file also has EFTA/candidates)
    save(out.sort_values(["geo", "sex", "year"]), "c04_living_alone_65plus.csv")
    # EU member-state spread for the chart band (agreed: middle 80% = 10th-90th percentile, not min-max,
    # because SK (break 2022) and LT (60% by 2025) would drive a min-max band). Years with all 27 members only.
    m = out[(out.sex == "total") & out.eu27_member]
    band = (m.groupby("year").pct_65plus_alone
            .agg(n="count", p10=lambda x: x.quantile(0.10), p25=lambda x: x.quantile(0.25),
                 median="median", p75=lambda x: x.quantile(0.75), p90=lambda x: x.quantile(0.90), min="min", max="max")
            .reset_index())
    band = band[band.n == 27]
    eu = out[(out.geo == "EU27_2020") & (out.sex == "total")][["year", "pct_65plus_alone"]].rename(columns={"pct_65plus_alone": "eu27_aggregate"})
    band = band.merge(eu, on="year", how="left")
    save(band, "c04_eu_member_spread.csv", "(10th-90th percentile band of the 27 members)")
    return out


# ---------------------------------------------------------------- C5
def _euls_load(sample):
    """Read and clean the EU Loneliness Survey (JRC 2022). sample = 'EU27' or 'EU4'."""
    import zipfile, io, tempfile, pyreadstat
    z = zipfile.ZipFile(os.path.join(DATA, "C05_loneliness_EU_LS", f"Data_{sample}_sample.zip"))
    name = [n for n in z.namelist() if n.endswith(".dta")][0]
    with tempfile.NamedTemporaryFile(suffix=".dta") as tmp:
        tmp.write(z.read(name)); tmp.flush()
        d, meta = pyreadstat.read_dta(tmp.name, encoding="latin1")
    lab = meta.variable_value_labels
    out = pd.DataFrame(index=d.index)
    out["sample"] = sample
    out["country"] = d.country.map(lab["country"])
    age = d.age.where(d.age.between(16, 100))                       # EU27 has impossible ages (e.g. 997)
    out["age"] = age
    out["age_band"] = pd.cut(age, [15, 34, 49, 64, 101], labels=["16-34", "35-49", "50-64", "65+"])
    out["female"] = d.gender.map({1: 0.0, 2: 1.0})                  # other/prefer not -> missing (n~1%)
    rel = d.relationship.where(d.relationship < 900)
    out["partnered"] = rel.isin([2, 3]).astype(float).where(rel.notna())   # Whiz: married/cohabiting OR in a relationship
    kids = d.children_hh.fillna(0) + d.children_out.fillna(0)
    out["parent"] = (kids >= 1).astype(float).where(d.children_hh.notna() | d.children_out.notna())
    ld = d.loneliness_direct.where(d.loneliness_direct < 900)
    out["lonely_direct"] = ld.isin([1, 2]).astype(float).where(ld.notna())  # most/all of the time
    ucla = d[["loneliness_ucla_a", "loneliness_ucla_b", "loneliness_ucla_c"]]
    ucla = ucla.where(ucla < 900)
    out["ucla3"] = ucla.sum(axis=1, min_count=3)
    out["lonely_ucla"] = (out.ucla3 >= 6).astype(float).where(out.ucla3.notna())
    fe = d[["feelings_nerv", "feelings_hopel", "feelings_restl", "feelings_depr", "feelings_worthl"]]
    fe = fe.where(fe < 900)
    out["distress"] = (6 - fe).sum(axis=1, min_count=5)            # 0 (never) .. 25 (always on all five)
    if sample == "EU27":
        out["income"] = d.income_decile.where(d.income_decile < 900).fillna(0).astype(int).astype(str)  # '0' = not given
        out["w"] = d.w_eu27
    else:
        out["income"] = "na"                                          # EU4 has no decile; models omit income
        out["w"] = d.w_country
    out["w"] = out.w / out.w.mean()
    fam = {(1, 1): "Partner + children", (1, 0): "Partner, no children", (0, 1): "No partner, children", (0, 0): "No partner, no children"}
    out["family"] = [fam.get((a, b)) if pd.notna(a) and pd.notna(b) else None for a, b in zip(out.partnered, out.parent)]
    return out

def _wmean_ci(y, w):
    m = np.average(y, weights=w)
    neff = w.sum() ** 2 / (w ** 2).sum()                            # Kish effective sample size
    se = np.sqrt(m * (1 - m) / neff)
    return m, m - 1.96 * se, m + 1.96 * se, neff

def c05_loneliness():
    """Loneliness and distress by partnership x parenthood x age (EU Loneliness Survey 2022), plus weighted logit models."""
    import statsmodels.api as sm, statsmodels.formula.api as smf
    rng = np.random.default_rng(20261008)
    frames = {s: _euls_load(s) for s in ["EU27", "EU4"]}
    # high distress = top 20% of the weighted EU27 distribution (threshold applied to both samples)
    e = frames["EU27"].dropna(subset=["distress"])
    srt = e.sort_values("distress"); cw = srt.w.cumsum() / srt.w.sum()
    thr = srt.distress[cw >= 0.80].iloc[0]
    print(f"    high-distress threshold (EU27 weighted p80): score >= {thr}")
    for d in frames.values():
        d["high_distress"] = (d.distress >= thr).astype(float).where(d.distress.notna())
    # ---- 1) weighted prevalence by group
    rows = []
    for s, d in frames.items():
        for outcome in ["lonely_direct", "lonely_ucla", "high_distress"]:
            dd = d.dropna(subset=[outcome, "family", "age_band"])
            for keys, g in list(dd.groupby(["age_band", "family"], observed=True)) + \
                           [(("All ages", f), g) for f, g in dd.groupby("family")] + \
                           [(("All ages", "Everyone"), dd)]:
                m, lo, hi, neff = _wmean_ci(g[outcome].values, g.w.values)
                rows.append(dict(sample=s, outcome=outcome, age_band=keys[0], family=keys[1], pct=100 * m,
                                 ci_lo=100 * lo, ci_hi=100 * hi, n=len(g), n_eff=round(neff, 1)))
    prev = pd.DataFrame(rows)
    prev["small_cell"] = prev.n < 50
    save(prev, "c05_loneliness_prevalence_by_group.csv", f"(high distress = score >= {thr})")
    # ---- 2) models: predicted probability by family type, standardised over everyone's own covariates
    fams = ["Partner + children", "Partner, no children", "No partner, children", "No partner, no children"]
    def design(d, outcome, extra):
        import patsy
        ctrl = "C(age_band) + female + C(country)" + (" + C(income)" if d["sample"].iloc[0] == "EU27" else "")
        rhs = f"C(family, Treatment('Partner + children'), levels=fams){extra} + {ctrl}"
        y, X = patsy.dmatrices(f"{outcome} ~ {rhs}", d, return_type="dataframe", NA_action="raise",
                               eval_env=patsy.EvalEnvironment([{"fams": fams}]))
        famcols = [c for c in X.columns if c.startswith("C(family")]
        assert sorted(c.split("[T.")[1][:-1] for c in famcols) == sorted(fams[1:]), famcols
        return y.values.ravel(), X, famcols
    def irls_logit(y, X, w, beta=None, iters=25):
        beta = np.zeros(X.shape[1]) if beta is None else beta.copy()
        for _ in range(iters):
            eta = X @ beta; mu = 1 / (1 + np.exp(-eta)); W = w * mu * (1 - mu)
            z = eta + (y - mu) / np.clip(mu * (1 - mu), 1e-10, None)
            XtW = X.T * W
            new = np.linalg.solve(XtW @ X + 1e-9 * np.eye(X.shape[1]), XtW @ z)
            if np.max(np.abs(new - beta)) < 1e-8:
                beta = new; break
            beta = new
        return beta
    def predict_all(beta, X, famcols, w, extra):
        res = {}
        Xv = X.values
        for fam_ in fams:
            Xc = Xv.copy()
            for c in famcols:
                Xc[:, X.columns.get_loc(c)] = 1.0 if c.endswith(f"[T.{fam_}]") else 0.0   # exact level match
            res[fam_] = np.average(1 / (1 + np.exp(-(Xc @ beta))), weights=w)
        if extra:
            j = X.columns.get_loc("lonely_direct")
            X1 = Xv.copy(); X1[:, j] = 1.0; X0 = Xv.copy(); X0[:, j] = 0.0
            res["AME_lonely"] = np.average(1 / (1 + np.exp(-(X1 @ beta))) - 1 / (1 + np.exp(-(X0 @ beta))), weights=w)
        return res
    mrows = []
    B = 200
    for s, d in frames.items():
        specs = [("lonely_direct", ""), ("lonely_ucla", ""), ("high_distress", " + lonely_direct")]
        for outcome, extra in specs:
            need = [outcome, "family", "age_band", "female", "country", "w"] + (["lonely_direct"] if extra else [])
            dd = d.dropna(subset=need).reset_index(drop=True)
            y, X, famcols = design(dd, outcome, extra)
            w = dd.w.values; Xv = X.values
            beta = irls_logit(y, Xv, w)
            # cross-check the hand-rolled IRLS against statsmodels once per model
            ref = sm.GLM(y, Xv, family=sm.families.Binomial(), freq_weights=w).fit().params
            assert np.max(np.abs(ref - beta)) < 1e-5, "IRLS disagrees with statsmodels"
            est = predict_all(beta, X, famcols, w, extra)
            boots = []
            for b in range(B):
                idx = rng.integers(0, len(dd), len(dd))
                bb = irls_logit(y[idx], Xv[idx], w[idx], beta)
                Xb = X.iloc[idx].reset_index(drop=True)
                boots.append(predict_all(bb, Xb, famcols, w[idx], extra))
            bt = pd.DataFrame(boots)
            for k, v in est.items():
                lo, hi = np.percentile(bt[k], [2.5, 97.5])
                mrows.append(dict(sample=s, outcome=outcome, controls_for_loneliness=bool(extra), quantity=k,
                                  estimate_pct=100 * v, ci_lo=100 * lo, ci_hi=100 * hi, n=len(dd), boot_reps=len(bt)))
            print(f"    {s} {outcome}{' (+lonely)' if extra else ''}: n={len(dd):,}")
    # ---- 3) age x family interaction (Whiz, 8 Oct): does childlessness matter more in old age?
    import patsy
    irows = []
    bands = ["16-34", "35-49", "50-64", "65+"]
    for s, d in frames.items():
        for outcome in ["lonely_direct", "lonely_ucla"]:
            dd = d.dropna(subset=[outcome, "family", "age_band", "female", "country", "w"]).reset_index(drop=True)
            dd["age_band"] = dd.age_band.astype(str)
            ctrl = "female + C(country)" + (" + C(income)" if s == "EU27" else "")
            f = f"{outcome} ~ C(family, Treatment('Partner + children'), levels=fams) * C(age_band) + {ctrl}"
            y, X = patsy.dmatrices(f, dd, return_type="dataframe", NA_action="raise",
                                   eval_env=patsy.EvalEnvironment([{"fams": fams}]))
            y = y.values.ravel(); Xv = X.values; w = dd.w.values
            Xcf = {}
            for fam_ in fams:                      # counterfactual design: everyone assigned family fam_
                dm = dd.copy(); dm["family"] = fam_
                Xcf[fam_] = np.asarray(patsy.build_design_matrices([X.design_info], dm)[0])
            band_idx = {b: (dd.age_band.values == b) for b in bands}
            def preds(beta, rows):
                out = {}
                for fam_ in fams:
                    pr = 1 / (1 + np.exp(-(Xcf[fam_][rows] @ beta)))
                    ww = w[rows]
                    for b in bands:
                        m = band_idx[b][rows]
                        out[(b, fam_)] = np.average(pr[m], weights=ww[m])
                for b in bands:   # key contrast: childless minus parent, among the unpartnered
                    out[(b, "GAP no-partner: childless minus parent")] = out[(b, "No partner, no children")] - out[(b, "No partner, children")]
                    out[(b, "GAP partnered: childless minus parent")] = out[(b, "Partner, no children")] - out[(b, "Partner + children")]
                return out
            allrows = np.arange(len(dd))
            beta = irls_logit(y, Xv, w)
            ref = sm.GLM(y, Xv, family=sm.families.Binomial(), freq_weights=w).fit().params
            assert np.max(np.abs(ref - beta)) < 1e-5, "IRLS disagrees with statsmodels (interaction)"
            est = preds(beta, allrows)
            boots = []
            for b_ in range(B):
                idx = rng.integers(0, len(dd), len(dd))
                bb = irls_logit(y[idx], Xv[idx], w[idx], beta)
                boots.append(preds(bb, idx))
            bt = pd.DataFrame(boots)
            for k, v in est.items():
                lo, hi = np.percentile(bt[k], [2.5, 97.5])
                cell_n = int(((dd.age_band == k[0]) & (dd.family == k[1])).sum()) if not k[1].startswith("GAP") else None
                irows.append(dict(sample=s, outcome=outcome, age_band=k[0], quantity=k[1], estimate_pct=100 * v,
                                  ci_lo=100 * lo, ci_hi=100 * hi, cell_n=cell_n, n=len(dd), boot_reps=len(bt)))
            print(f"    {s} {outcome} age x family: n={len(dd):,}")
    save(pd.DataFrame(irows), "c05_loneliness_age_by_family.csv",
         "(adjusted % by age band x family; GAP rows = childless minus parent, in pp)")
    save(pd.DataFrame(mrows), "c05_loneliness_models.csv",
         "(predicted % by family type standardised over sample covariates; AME_lonely = pp change in high distress)")


# ---------------------------------------------------------------- C6
WHO_CODES = {"JPN": 3160, "KOR": 3325, "ITA": 4180, "DEU": 4085, "GBR": 4308, "USA": 2450}

def c06_suicide():
    """Suicide by marital status (Japan, MHLW 2019) and suicide rates 65+ vs all ages (WHO MDB) with Korea's living-alone share."""
    import subprocess, re, zipfile
    D6 = os.path.join(DATA, "C06_suicide_family_status")
    # ---- A) Japan, MHLW white paper (Reiwa 3 ed.): 2019 deaths / 2015 census, per 100,000, by marital status x sex x age
    txt = subprocess.run(["pdftotext", "-layout", os.path.join(D6, "MHLW_suicide_whitepaper_r3h-1-1-07.pdf"), "-"],
                         capture_output=True, text=True, check=True).stdout
    status = {"有配偶者": "married", "未婚": "never married", "死別": "widowed", "離別": "divorced"}
    ages = ["total", "20-29", "30-39", "40-49", "50-59", "60+"]
    rows, block = [], 0
    for line in txt.splitlines():
        lab = line.strip().split(" ")[0]
        if lab in status:
            nums = [float(x) for x in re.findall(r"\d+\.\d", line)]
            if len(nums) == 6:
                sex = "men" if block < 4 else "women"
                for a, v in zip(ages, nums):
                    rows.append(dict(sex=sex, marital_status=status[lab], age_band=a, suicides_per_100k=v))
                block += 1
    jp = pd.DataFrame(rows)
    assert len(jp) == 48, f"expected 2 sexes x 4 statuses x 6 ages, got {len(jp)}"
    # spot-check against values read off the PDF by eye (men, total)
    chk = jp[(jp.sex == "men") & (jp.age_band == "total")].set_index("marital_status").suicides_per_100k.to_dict()
    assert chk == {"married": 16.8, "never married": 32.9, "widowed": 48.3, "divorced": 101.0}, chk
    jp["ratio_to_married"] = jp.suicides_per_100k / jp.groupby(["sex", "age_band"]).suicides_per_100k.transform(
        lambda x: x[jp.loc[x.index, "marital_status"] == "married"].iloc[0])
    jp["source"] = "MHLW Suicide Countermeasures White Paper (R3 ed.), fig. on 2019 suicide rate by marital status; denominators 2015 census"
    save(jp, "c06a_japan_suicide_by_marital_status.csv")
    # ---- B) WHO Mortality Database: national suicide rates, all ages and 65+
    d = pd.read_csv(os.path.join(D6, "WHO_MDB_suicide_ICD10_all_countries.csv"),
                    dtype={"Cause": str, "List": str, "Frmat": str, "Admin1": str, "SubDiv": str}, low_memory=False)
    d = d[d.Country.isin(WHO_CODES.values()) & d.Admin1.isna() & d.SubDiv.isna() & d.Sex.isin([1, 2])]
    # avoid double counting: drop a 3-char code when its 4-char sub-codes are also present (none for JPN/KOR)
    kids = d[d.Cause.str.len() == 4].assign(parent=lambda x: x.Cause.str[:3])[["Country", "Year", "List", "parent"]].drop_duplicates()
    d = d.merge(kids.rename(columns={"parent": "Cause"}).assign(has_kids=True), on=["Country", "Year", "List", "Cause"], how="left")
    d = d[~((d.Cause.str.len() == 3) & d.has_kids.fillna(False).astype(bool))]
    dcols = [f"Deaths{i}" for i in range(19, 26)]                     # 65-69 ... 95+ in every WHO age format
    d["d_all"] = d.Deaths1; d["d_65p"] = d[dcols].sum(axis=1, min_count=1)
    deaths = d.groupby(["Country", "Year", "Sex"], as_index=False)[["d_all", "d_65p"]].sum()
    z = zipfile.ZipFile(os.path.join(D6, "WHO_mortality_database_raw", "mort_pop.zip"))
    pop = pd.read_csv(z.open(z.namelist()[0]), dtype={"Admin1": str, "SubDiv": str, "Frmat": str})
    pop = pop[pop.Country.isin(WHO_CODES.values()) & pop.Admin1.isna() & pop.SubDiv.isna() & pop.Sex.isin([1, 2])]
    pop["p_all"] = pop.Pop1; pop["p_65p"] = pop[[f"Pop{i}" for i in range(19, 26)]].sum(axis=1, min_count=1)
    m = deaths.merge(pop[["Country", "Year", "Sex", "p_all", "p_65p"]], on=["Country", "Year", "Sex"], how="left")
    # WHO population file has gaps (USA ends 2007; GBR 2021-22; ITA 2022): fill from UN WPP 2024 mid-year estimates
    wpp = pd.read_csv(os.path.join(WPP, "WPP2024_PopulationByAge5GroupSex_Medium_OECD38.csv.gz"),
                      usecols=["ISO3_code", "Time", "AgeGrpStart", "PopMale", "PopFemale"])
    wpp = wpp[wpp.ISO3_code.isin(WHO_CODES)]
    wl = wpp.melt(id_vars=["ISO3_code", "Time", "AgeGrpStart"], value_vars=["PopMale", "PopFemale"], var_name="s", value_name="pop")
    wl["Sex"] = wl.s.map({"PopMale": 1, "PopFemale": 2}); wl["pop"] *= 1000
    wa = wl.groupby(["ISO3_code", "Time", "Sex"]).pop.sum().rename("w_all")
    wo = wl[wl.AgeGrpStart >= 65].groupby(["ISO3_code", "Time", "Sex"]).pop.sum().rename("w_65p")
    wp = pd.concat([wa, wo], axis=1).reset_index()
    wp["Country"] = wp.ISO3_code.map(WHO_CODES)
    m = m.merge(wp.rename(columns={"Time": "Year"})[["Country", "Year", "Sex", "w_all", "w_65p"]], on=["Country", "Year", "Sex"], how="left")
    m["pop_source"] = np.where(m.p_all.isna(), "UN WPP 2024 (WHO pop missing)", "WHO MDB")
    m["p_all"] = m.p_all.fillna(m.w_all); m["p_65p"] = m.p_65p.fillna(m.w_65p)
    both = m.groupby(["Country", "Year"], as_index=False).agg(d_all=("d_all", "sum"), d_65p=("d_65p", "sum"),
                                                             p_all=("p_all", "sum"), p_65p=("p_65p", "sum"), pop_source=("pop_source", "first"))
    both["Sex"] = 0
    m = pd.concat([m, both], ignore_index=True)
    m["sex"] = m.Sex.map({0: "total", 1: "men", 2: "women"})
    # completeness check: a year with < 50% of the prior-3-year median deaths is a coverage break, not a real fall
    # (e.g. GBR 2021+ holds only a fraction of UK deaths). Such rows keep counts but get NaN rates.
    m = m.sort_values(["Country", "Sex", "Year"])
    prior = m.groupby(["Country", "Sex"]).d_all.transform(lambda x: x.shift(1).rolling(3, min_periods=2).median())
    m["incomplete"] = (m.d_all < 0.5 * prior)
    m["incomplete"] = m.groupby(["Country", "Sex"]).incomplete.cummax()   # once broken, stays flagged
    print("    flagged incomplete:", sorted(set(zip(m[m.incomplete].Country.map({v: k for k, v in WHO_CODES.items()}), m[m.incomplete].Year)))[:12])
    m["rate_all_ages"] = np.where(m.incomplete, np.nan, 1e5 * m.d_all / m.p_all)
    m["rate_65plus"] = np.where(m.incomplete, np.nan, 1e5 * m.d_65p / m.p_65p)
    inv = {v: k for k, v in WHO_CODES.items()}
    m["iso3"] = m.Country.map(inv)
    kr = pd.read_csv(os.path.join(DATA, "C04_elderly_living_alone", "KR_elderly_living_alone_ratio_census.csv"))
    m = m.merge(kr[["year", "pct_65plus_living_alone"]].assign(iso3="KOR").rename(columns={"year": "Year"}),
                on=["iso3", "Year"], how="left")
    m = m.rename(columns={"Year": "year", "pct_65plus_living_alone": "kr_pct_65plus_living_alone"})
    m["note"] = "crude rates per 100,000 (not age-standardised); WHO MDB ICD-10 X60-X84, Y87.0"
    out = m[["iso3", "year", "sex", "d_all", "p_all", "rate_all_ages", "d_65p", "p_65p", "rate_65plus", "incomplete", "pop_source", "kr_pct_65plus_living_alone", "note"]]
    save(out.sort_values(["iso3", "sex", "year"]), "c06b_suicide_rates_65plus_who.csv")


# ---------------------------------------------------------------- C7
def _c7_inputs(iso):
    """WPP 2024 medium inputs for the chart 7 projection, single ages 0..100, years 2024..2070."""
    yrs = list(range(2024, 2071))
    ltf = ["WPP2024_Life_Table_Complete_Medium_{}_2024-2100_OECD38.csv.gz"]
    S = np.stack([_wpp_matrix([ltf[0].format(sx)], "Sx", iso)[yrs].values for sx in ["Male", "Female"]])     # (2, A, T)
    L0 = np.stack([_wpp_matrix([ltf[0].format(sx)], "Lx", iso)[yrs].values[0] / 1e5 for sx in ["Male", "Female"]])  # (2, T)
    asfr = _wpp_matrix(["WPP2024_Fertility_by_Age1_OECD38.csv.gz"], "ASFR", iso, "Medium")[yrs].values            # (A, T)
    popf = ["WPP2024_Population1JanuaryBySingleAgeSex_Medium_2024-2100_OECD38.csv.gz"]
    pm = _wpp_matrix(popf, "PopMale", iso)[list(range(2024, 2072))].values
    pf = _wpp_matrix(popf, "PopFemale", iso)[list(range(2024, 2072))].values
    wpp_pop = np.stack([pm, pf])                                                            # (2, A, T+1) 1 Jan, thousands
    di = pd.read_csv(os.path.join(WPP, "WPP2024_Demographic_Indicators_Medium.csv.gz"),
                     usecols=["ISO3_code", "Time", "NetMigrations", "SRB", "TPopulation1July"], low_memory=False)
    di = di[di.ISO3_code == iso].set_index("Time")
    return yrs, S, L0, asfr, wpp_pop, di

def _migrant_profile(iso):
    """Age-sex shape of net migrants = WPP Medium minus Zero-migration population, mid-2025, 5-year groups
    spread evenly over single ages; negative cells set to 0; normalised to sum 1."""
    use = ["ISO3_code", "Variant", "Time", "AgeGrpStart", "PopMale", "PopFemale"]
    med = pd.read_csv(os.path.join(WPP, "WPP2024_PopulationByAge5GroupSex_Medium_OECD38.csv.gz"), usecols=use)
    oth = pd.read_csv(os.path.join(WPP, "WPP2024_PopulationByAge5GroupSex_OtherVariants_OECD38.csv.gz"), usecols=use)
    def get(df, iso_list, var):
        x = df[df.ISO3_code.isin(iso_list) & (df.Variant == var) & (df.Time == 2025)]
        return x.groupby("AgeGrpStart")[["PopMale", "PopFemale"]].sum()
    diff = get(med, [iso], "Medium") - get(oth, [iso], "Zero migration")
    prof = np.zeros((2, 101))
    for a0, r in diff.iterrows():
        a0 = int(a0); span = 1 if a0 == 100 else 5
        for k, col in enumerate(["PopMale", "PopFemale"]):
            prof[k, a0:a0 + span] = max(r[col], 0) / span
    return prof / prof.sum(), diff.values.sum()

def c07_replacement_migration():
    """Net migration needed 2025-2070 to hold (a) the 20-64 population and (b) the 65+/20-64 ratio at 2024 levels."""
    sys.path.insert(0, os.path.dirname(__file__))
    from projection import step, oadr, wa
    rows, val = [], []
    pooled = None
    for iso in FOCUS:
        yrs, S, L0, asfr, wpp_pop, di = _c7_inputs(iso)
        prof, net_gap = _migrant_profile(iso)
        P0 = wpp_pop[:, :, 0].copy()
        # ---- validation runs: WPP medium migration totals x profile, and zero migration
        P_med, P_zero = P0.copy(), P0.copy()
        for t, y in enumerate(yrs[:-1]):
            P_med, _ = step(P_med, S[:, :, t], asfr[:, t], di.SRB[y], L0[:, t], di.NetMigrations[y] * prof)
            P_zero, _ = step(P_zero, S[:, :, t], asfr[:, t], di.SRB[y], L0[:, t])
            if y + 1 in (2050, 2070):
                W = wpp_pop[:, :, t + 1]
                val.append(dict(iso3=iso, year=y + 1, model_pop=P_med.sum(), wpp_pop=W.sum(),
                                pop_err_pct=100 * (P_med.sum() / W.sum() - 1),
                                model_oadr=oadr(P_med), wpp_oadr=oadr(W), model_oadr_zero_mig=oadr(P_zero)))
        # ---- solve for required migration each year.
        # Anchored design (8 Oct, after validation): the baseline path is WPP Medium itself (exact, incl. its own
        # migration assumptions); the model projects only the ADDITIONAL migrants (and their descendants) needed on
        # top of it, so survival/migration-convention differences only affect that extra stock.
        # third target (Whiz, 8 Oct, sensitivity for the text): retirement threshold raised to 70 -
        # hold 70+ per 100 aged 20-69 at TODAY's 65+/20-64 level
        for target in ["working_age_2064", "oadr_65_2064", "oadr70_at_2024_65_level"]:
            X = np.zeros_like(P0)                          # extra migrants + descendants, 1 Jan
            W_star, r_star = wa(P0), oadr(P0)
            cum_total = cum_extra = 0.0
            m_w, m_o = prof[:, 20:65].sum(), prof[:, 65:].sum()
            for t, y in enumerate(yrs[:-1]):
                base = wpp_pop[:, :, t + 1]
                Xn, _ = step(X, S[:, :, t], asfr[:, t], di.SRB[y], L0[:, t])
                O = base[:, 65:].sum() + Xn[:, 65:].sum()
                Wk = base[:, 20:65].sum() + Xn[:, 20:65].sum()
                if target == "working_age_2064":
                    E = (W_star - Wk) / m_w
                elif target == "oadr_65_2064":
                    r = r_star / 100
                    E = (O - r * Wk) / (r * m_w - m_o)
                else:
                    r = r_star / 100
                    O7 = base[:, 70:].sum() + Xn[:, 70:].sum(); W7 = base[:, 20:70].sum() + Xn[:, 20:70].sum()
                    E = (O7 - r * W7) / (r * prof[:, 20:70].sum() - prof[:, 70:].sum())
                    E = max(E, 0.0)   # a ceiling, not an equality: never force emigration to RAISE the ratio
                X = Xn + E * prof
                P = base + X
                assumed = di.NetMigrations.get(y, np.nan)
                M = assumed + E
                cum_total += M; cum_extra += E
                rows.append(dict(iso3=iso, target=target, year=y + 1,
                                 required_net_migrants_k=M, extra_over_wpp_k=E, wpp_assumed_net_migrants_k=assumed,
                                 population_k=P.sum(), required_per_1000=1000 * M / P.sum(),
                                 wpp_assumed_per_1000=1000 * assumed / base.sum(),
                                 cumulative_required_k=cum_total, cumulative_extra_k=cum_extra,
                                 extra_origin_share_pct=100 * X.sum() / P.sum(),
                                 oadr=oadr(P), working_age_k=wa(P), oadr70=100 * P[:, 70:].sum() / P[:, 20:70].sum(),
                                 observed_net_migr_2015_2023_avg_k=di.NetMigrations.loc[2015:2023].mean(),
                                 observed_per_1000_2015_2023=1000 * (di.NetMigrations.loc[2015:2023] / di.TPopulation1July.loc[2015:2023]).mean()))
            chk = rows[-1]
            assert (target != "oadr70_at_2024_65_level" or chk["oadr70"] <= r_star + 1e-6) and \
                   (target != "oadr_65_2064" or abs(chk["oadr"] - r_star) < 1e-6) and \
                   (target != "working_age_2064" or abs(chk["working_age_k"] - W_star) < 1e-6), "target not held"
        print(f"    {iso}: migrant profile 20-64 share {prof[:, 20:65].sum():.2f}, 65+ share {prof[:, 65:].sum():.3f} (Medium-Zero gap {net_gap:,.0f}k)")
    v = pd.DataFrame(val)
    save(v, "c07_projection_validation.csv", "(model vs WPP medium: population and 65+/20-64 ratio)")
    res = pd.DataFrame(rows)
    save(res, "c07_replacement_migration.csv")
    # chart summary: average net migration per 1,000 residents per year, 2025-2070, by target, vs observed and WPP-assumed
    summ = (res.groupby(["iso3", "target"])
            .agg(required_per_1000_avg=("required_per_1000", "mean"), wpp_assumed_per_1000_avg=("wpp_assumed_per_1000", "mean"),
                 observed_per_1000_2015_2023=("observed_per_1000_2015_2023", "first"),
                 cumulative_required_k_2070=("cumulative_required_k", "last"), population_k_2070=("population_k", "last"),
                 first_year=("year", "min"), last_year=("year", "max"))
            .reset_index())
    save(summ, "c07_summary_per_1000.csv", "(2025-2070 averages, for the chart)")


# ---------------------------------------------------------------- C8
def c08_employment_origin():
    """Denmark: age-standardised (Danish-origin age mix) employment rate 25-64 by origin and sex, 2008 vs 2024."""
    D8 = os.path.join(DATA, "C08_employment_fiscal_by_origin")
    # RAS204 publishes country x age employment rates only to 2021, so rates are rebuilt from RAS208
    # (end-November population by origin x employment status x single age x sex, 2008-2024):
    # rate = employed / (employed + unemployed + outside labour force). Validated against RAS204 for 2021.
    r = pd.read_csv(os.path.join(D8, "DST_RAS208_population_by_origin_status_age_sex.csv"), sep=";", encoding="utf-8-sig")
    r["age"] = r.ALDER.str.extract(r"(\d+)").astype(int)
    r["band"] = ((r.age // 5) * 5).astype(str) + "-" + ((r.age // 5) * 5 + 4).astype(str) + " years"
    piv = r.pivot_table(index=["OPRINDLAND", "KØN", "TID", "band"], columns="SOCIO", values="INDHOLD", aggfunc="sum", fill_value=0)
    piv["pop"] = piv.sum(axis=1)
    piv["rate"] = 100 * piv["Employed"] / piv["pop"]
    rates = piv.reset_index().rename(columns={"OPRINDLAND": "origin", "KØN": "sex", "TID": "year"})
    # validation vs published RAS204 band rates, 2021
    pub = pd.read_csv(os.path.join(D8, "DST_RAS204_employment_by_country_of_origin.csv"), sep=";", encoding="utf-8-sig")
    pub = pub[(pub.BEREGNING == "Employment rate") & (pub.TID == 2021)].rename(columns={"OPRINDLAND": "origin", "KØN": "sex", "ALDER": "band", "INDHOLD": "pub"})
    v = rates[rates.year == 2021].merge(pub[["origin", "sex", "band", "pub"]], on=["origin", "sex", "band"])
    err = (v.rate - v.pub).abs()
    print(f"    RAS208-derived vs published RAS204 rates (2021, n={len(v)} cells): max abs diff {err.max():.2f} pp, mean {err.mean():.3f}")
    assert err.max() < 1.0, "derived employment rates do not match published RAS204"
    # add a Total-sex row
    tot = piv.groupby(level=["OPRINDLAND", "TID", "band"]).sum()
    tot["rate"] = 100 * tot["Employed"] / tot["pop"]
    tot = tot.reset_index().rename(columns={"OPRINDLAND": "origin", "TID": "year"}); tot["sex"] = "Total"
    rates = pd.concat([rates, tot], ignore_index=True)
    # standard population: persons of Danish origin, 1 Jan 2024, by sex and 5-year band (FOLK1C)
    f = pd.read_csv(os.path.join(DATA, "C09_crime_by_origin_DK", "DST_FOLK1C_population_by_origin_age_sex_2008-2026Q1.csv"),
                    sep=";", encoding="utf-8-sig")
    bands = [f"{a}-{a + 4} years" for a in range(25, 65, 5)]
    std = f[(f.HERKOMST == "Persons of Danish origin") & (f.IELAND == "Total") & (f.TID == "2024Q1") & f.ALDER.isin(bands)]
    std = std.rename(columns={"KØN": "sex", "ALDER": "band", "INDHOLD": "w"})[["sex", "band", "w"]]
    rates = rates.merge(std, on=["sex", "band"], how="left")
    assert rates.w.notna().all()
    def agg(x):
        ok = len(x) == 8 and (x["pop"] > 0).all()
        return pd.Series({"rate_std_2564": np.average(x.rate, weights=x.w) if ok else np.nan,
                          "rate_crude_2564": 100 * x.Employed.sum() / x["pop"].sum(),
                          "pop_2564": x["pop"].sum(), "min_band_pop": x["pop"].min()})
    out = rates.groupby(["origin", "sex", "year"]).apply(agg, include_groups=False).reset_index()
    aggregates = ["Total", "Denmark, total", "Immigrants from western countries", "Immigrants from non-western countries",
                  "Descendants from western countries", "Descendants from non-western countries"]
    out["is_aggregate"] = out.origin.isin(aggregates)
    out["small_group"] = out.min_band_pop < 100                 # flag: some 5-year band has < 100 people
    wide = out.pivot_table(index=["origin", "sex", "is_aggregate"], columns="year", values="rate_std_2564").reset_index()
    wide.columns = [str(c) for c in wide.columns]
    wide = wide.rename(columns={"2008": "rate_std_2008", "2021": "rate_std_2021", "2024": "rate_std_2024"})
    wide["change_2008_2024_pp"] = wide.rate_std_2024 - wide.rate_std_2008
    dk = wide[wide.origin == "Denmark, total"].set_index("sex").rate_std_2024
    wide["gap_to_danish_origin_2024"] = wide.rate_std_2024 - wide.sex.map(dk)
    o24 = out[out.year == 2024][["origin", "sex", "rate_crude_2564", "pop_2564", "small_group"]]
    o08 = out[out.year == 2008][["origin", "sex", "pop_2564"]].rename(columns={"pop_2564": "pop_2564_2008"})
    wide = wide.merge(o24, on=["origin", "sex"], how="left").merge(o08, on=["origin", "sex"], how="left")
    wide["composition_changed"] = wide.pop_2564 >= 2 * wide.pop_2564_2008      # Whiz: hollow arrow + footnote
    # Statistics Denmark's definition: 'Western' = EU-27, Iceland, Norway, Switzerland, Liechtenstein, UK, Andorra,
    # Monaco, San Marino, Vatican, USA, Canada, Australia, New Zealand; every other country is 'non-Western'.
    # Used only to group rows in the figure (Whiz, 11 Oct: India had read as if it sat under the Western aggregate).
    western = {"Netherlands", "Lithuania", "Hungary", "Sweden", "Poland", "Latvia", "Italy", "United Kingdom", "France",
               "Spain", "Norway", "Iceland", "Romania", "Bulgaria", "Germany", "USA"}
    non_western = {"Philippines", "Nepal", "Thailand", "Vietnam", "India", "China", "Russia", "Sri Lanka", "Ukraine",
                   "Bosnia and Herzegovina", "Pakistan", "Iran", "Yugoslavia", "Turkey", "Morocco", "Afghanistan",
                   "Iraq", "Syria", "Somalia", "Lebanon"}
    countries = set(wide.origin[~wide.is_aggregate])
    assert countries == western | non_western and not western & non_western, countries ^ (western | non_western)
    wide["dst_group"] = np.where(wide.is_aggregate, "aggregate", np.where(wide.origin.isin(western), "western", "non-western"))
    wide["note"] = "age-standardised to Danish-origin 1 Jan 2024 age mix (5-yr bands 25-64, by sex); RAS208 end-Nov; country = immigrants + descendants"
    save(wide.sort_values(["sex", "rate_std_2024"]), "c08_employment_by_origin_2008_2024.csv")


# ---------------------------------------------------------------- C9
def c09_crime_index():
    """Denmark: Criminal-Code conviction index by origin, raw vs age-sex standardised (indirect), pooled 2022-2024."""
    from scipy import stats
    import re
    D9 = os.path.join(DATA, "C09_crime_by_origin_DK")
    rd = lambda f: pd.read_csv(os.path.join(D9, f), sep=";", encoding="utf-8-sig")
    BANDS = {"15-29 years": range(15, 30), "30-49 years": range(30, 50), "50-79 years": range(50, 80)}
    # ---- convictions (persons found guilty, all offence types) by sex x age band
    s3 = rd("DST_STRAFNA3_guilty_by_sex_age_origin.csv").rename(columns={"IELAND": "group", "KOEN": "sex", "ALDER": "band", "TID": "year", "INDHOLD": "n"})
    s9 = rd("DST_STRAFNA9_guilty_by_sex_age_ancestry.csv").rename(columns={"HERKOMST": "group", "KOEN": "sex", "ALDER": "band", "TID": "year", "INDHOLD": "n"})
    s9 = s9[~s9.group.isin(["Total"])]                       # 'Total' comes from STRAFNA3
    # ---- Criminal-Code share of guilty persons, by group and year
    s4 = rd("DST_STRAFNA4_guilty_by_offence_origin.csv").rename(columns={"IELAND": "group", "TID": "year"})
    s10 = rd("DST_STRFNA10_guilty_by_offence_ancestry.csv").rename(columns={"HERKOMST": "group", "TID": "year"})
    def share(df):
        w = df[df.OVERTRÆD.isin(["Criminal decisions total", "Criminal code, total"])].pivot_table(
            index=["group", "year"], columns="OVERTRÆD", values="INDHOLD").reset_index()
        w["cc_share"] = w["Criminal code, total"] / w["Criminal decisions total"]
        return w[["group", "year", "cc_share"]]
    sh = pd.concat([share(s4), share(s10[~s10.group.isin(["Total"])])], ignore_index=True)
    # ---- population 1 Jan of the conviction year, by sex x age band
    f1c = rd("DST_FOLK1C_population_by_origin_age_sex_2008-2026Q1.csv")
    f1c = f1c[(f1c.HERKOMST == "Total") & f1c["KØN"].isin(["Men", "Women"])]
    def band5(a):
        m = re.match(r"(\d+)-\d+ years", a)
        lo = int(m.group(1)) if m else None
        return next((b for b, r in BANDS.items() if lo is not None and lo in r), None)
    f1c["band"] = f1c.ALDER.map(band5)
    f1c = f1c[f1c.band.notna()]
    f1c["year"] = f1c.TID.str[:4].astype(int)
    pc = f1c.groupby(["IELAND", "KØN", "band", "year"]).INDHOLD.sum().reset_index().rename(columns={"IELAND": "group", "KØN": "sex", "INDHOLD": "pop"})
    f1e = rd("DST_FOLK1E_population_by_ancestry_age_sex_2022-2024Q1.csv")
    f1e["age"] = f1e.ALDER.str.extract(r"(\d+)").astype(int)
    f1e["band"] = f1e.age.map(lambda a: next(b for b, r in BANDS.items() if a in r))
    f1e["year"] = f1e.TID.str[:4].astype(int)
    pe = f1e[f1e.HERKOMST != "Total"].groupby(["HERKOMST", "KØN", "band", "year"]).INDHOLD.sum().reset_index().rename(
        columns={"HERKOMST": "group", "KØN": "sex", "INDHOLD": "pop"})
    pop = pd.concat([pc, pe], ignore_index=True)
    # national age-sex shape of the Criminal-Code share (decisions, 2022-2024 pooled)
    s40 = rd("DST_STRAF40_decisions_by_offence_age_sex_2022-2024.csv")
    s40["lo"] = s40.ALDER.str.extract(r"(\d+)").astype(int)
    s40["band"] = s40.lo.map(lambda a: next((b for b, r in BANDS.items() if a in r), None))
    s40 = s40[s40.band.notna()].rename(columns={"KØN": "sex"})
    fw = s40.pivot_table(index=["sex", "band"], columns="OVERTRÆD", values="INDHOLD", aggfunc="sum")
    fw["f"] = fw["Criminal code, total"] / fw["Nature of the offence, total"]
    fshape = fw[["f"]].reset_index()
    print("    Criminal-Code share by sex x band (decisions):", {f"{a[0][0]} {a[1][:5]}": round(b, 3) for a, b in fw.f.items()})
    conv = pd.concat([s3, s9], ignore_index=True)
    conv = conv[conv.band.isin(BANDS)]
    def build(years):
        d = conv[conv.year.isin(years)].merge(pop, on=["group", "sex", "band", "year"], how="inner")
        d = d.merge(sh, on=["group", "year"], how="left")
        # Age-adjusted rescaling (8 Oct, after the flat rescale failed validation vs DST 2022: 188 vs 144, 370 vs 237):
        # within a group, the Criminal-Code share follows the NATIONAL age-sex pattern f[s,a] (STRAF40 decisions),
        # scaled by k_g so the group's total matches its own Criminal-Code count:
        #   n_cc[g,s,a] = n[g,s,a] * f[s,a] * CC_g / sum_{s,a} n[g,s,a] * f[s,a],  CC_g = cc_share_g * sum n[g,.,.]
        d = d.merge(fshape, on=["sex", "band"], how="left")
        tot_n = d.groupby(["group", "year"]).n.transform("sum")
        denom = (d.n * d.f).groupby([d.group, d.year]).transform("sum")
        d["n_cc"] = d.n * d.f * (d.cc_share * tot_n) / denom
        return d
    def index_table(d, years_label, scopes=(("men and women", ["Men", "Women"]), ("men", ["Men"]))):
        rows = []
        ref = d[d.group == "Total"].groupby(["sex", "band"])[["n", "n_cc", "pop"]].sum()
        for scope, sexes in scopes:
            for measure, col in [("criminal_code", "n_cc"), ("all_offences", "n")]:
                rr = ref.loc[sexes]
                ref_rate = (rr[col] / rr["pop"])
                ref_crude = rr[col].sum() / rr["pop"].sum()
                for g, x in d[d.sex.isin(sexes)].groupby("group"):
                    x = x.groupby(["sex", "band"])[list(dict.fromkeys([col, "pop", "n"]))].sum()
                    obs, popg = x[col].sum(), x["pop"].sum()
                    exp_ = (x["pop"] * ref_rate.reindex(x.index)).sum()
                    k = x["n"].sum()                      # underlying integer count, for the interval
                    lo, hi = stats.chi2.ppf(0.025, 2 * k) / 2, stats.chi2.ppf(0.975, 2 * k + 2) / 2
                    scale = obs / k if k else np.nan
                    rows.append(dict(years=years_label, scope=scope, measure=measure, group=g,
                                     guilty_persons=obs, population=popg, expected=exp_,
                                     raw_index=100 * (obs / popg) / ref_crude, adjusted_index=100 * obs / exp_,
                                     adj_ci_lo=100 * lo * scale / exp_, adj_ci_hi=100 * hi * scale / exp_))
        return pd.DataFrame(rows)
    # validation against DST Indvandrere i Danmark 2023, Table 6.7 (2022, ALL decisions, 15-79, index all men/women = 100)
    pub = pd.DataFrame([
        ("men", "Immigrants from western countries", 96, 84), ("men", "Descendants from western countries", 123, 107),
        ("men", "Immigrants from non-western countries", 156, 144), ("men", "Descendants from non-western countries", 325, 237),
        ("men", "Persons of Danish origin", 89, 92),
        ("women", "Immigrants from western countries", 100, 86), ("women", "Descendants from western countries", 134, 124),
        ("women", "Immigrants from non-western countries", 125, 108), ("women", "Descendants from non-western countries", 289, 240),
        ("women", "Persons of Danish origin", 93, 96)], columns=["scope", "group", "dst_raw_2022", "dst_adjusted_2022"])
    v = index_table(build([2022]), "2022", scopes=[("men", ["Men"]), ("women", ["Women"])])
    v = v[v.measure == "all_offences"].merge(pub, on=["scope", "group"])
    print(v[["scope", "group", "raw_index", "dst_raw_2022", "adjusted_index", "dst_adjusted_2022"]].round(0).to_string(index=False))
    save(v, "c09_validation_vs_dst_2022.csv", "(our all-offence indices vs DST Table 6.7, 2022)")
    out = index_table(build([2022, 2023, 2024]), "2022-2024")
    aggregates = ["Total", "Persons of Danish origin", "Immigrants from western countries", "Immigrants from non-western countries",
                  "Descendants from western countries", "Descendants from non-western countries", "Immigrants, total", "Descendants, total"]
    out["is_aggregate"] = out.group.isin(aggregates)
    out = out[~out.group.str.startswith("Other countries")]
    out["small_group"] = out.population < 3 * 5000          # < ~5,000 people per pooled year
    # Statistics Denmark's Western/non-Western split (same definition as c08), used to group rows in the figure
    # (Whiz, 12 Oct). Only the groups the figure shows (>= ~5,000 people) are classified; small groups are left blank.
    western = {"Bulgaria", "France", "Germany", "Hungary", "Iceland", "Italy", "Latvia", "Lithuania", "Netherlands",
               "Norway", "Poland", "Romania", "Spain", "Sweden", "USA", "United Kingdom"}
    non_western = {"Afghanistan", "Bosnia and Herzegovina", "China", "India", "Iran", "Iraq", "Lebanon", "Morocco",
                   "Pakistan", "Philippines", "Republic of North Macedonia", "Russia", "Somalia", "Sri Lanka", "Syria",
                   "Thailand", "Turkey", "Ukraine", "Vietnam", "Yugoslavia"}
    shown = set(out.group[~out.small_group & ~out.is_aggregate & (out.group != "Denmark")])
    assert shown == western | non_western and not western & non_western, shown ^ (western | non_western)
    out["dst_group"] = np.select([out.is_aggregate, out.group.isin(western), out.group.isin(non_western)],
                                 ["aggregate", "western", "non-western"], default="")
    common = ("indirect standardisation on total-population rates by sex x age band (15-29/30-49/50-79); 100 = population average; "
              "country groups = immigrants + descendants; 'Denmark' = Danish origin")
    out["note"] = np.where(out.measure == "all_offences",
                           "persons found guilty of any offence incl. traffic (STRAFNA3/9; CHART MEASURE, validated vs DST Table 6.7); " + common,
                           "persons found guilty (STRAFNA3/9) x Criminal-Code share for that group-year (STRAFNA4/STRFNA10) - "
                           "did NOT validate, kept for reference only; " + common)
    save(out.sort_values(["scope", "measure", "adjusted_index"]), "c09_crime_index_by_origin.csv")


# ---------------------------------------------------------------- C10
C10 = os.path.join(DATA, "C10_political_strain")
C10_COUNTRIES = {"DEU": "Germany", "ITA": "Italy", "SWE": "Sweden", "NLD": "Netherlands", "AUT": "Austria", "DNK": "Denmark"}
EB_ISO = {"BE": "BEL", "DK": "DNK", "DE": "DEU", "EL": "GRC", "ES": "ESP", "FR": "FRA", "IE": "IRL", "IT": "ITA", "LU": "LUX",
          "NL": "NLD", "AT": "AUT", "FI": "FIN", "SE": "SWE", "UK": "GBR", "PT": "PRT", "CZ": "CZE", "EE": "EST", "CY": "CYP",
          "LV": "LVA", "LT": "LTU", "HU": "HUN", "MT": "MLT", "PL": "POL", "SK": "SVK", "BG": "BGR", "RO": "ROU", "SI": "SVN",
          "HR": "HRV", "TR": "TUR", "MK": "MKD", "ME": "MNE", "RS": "SRB", "AL": "ALB", "IS": "ISL", "BA": "BIH", "XK": "XKX",
          "MD": "MDA", "CH": "CHE", "NO": "NOR", "GE": "GEO"}


def c10_political_strain():
    """Immigration salience (Eurobarometer, 2004-2026) and far-right vote share (PopuList 4.0 x ParlGov + official results)."""
    import glob, zipfile
    sys.path.insert(0, os.path.dirname(__file__))
    from eb_parse import parse_round

    # ---- (a) Eurobarometer: % naming immigration among the (up to) two most important issues facing their country
    ebd = os.path.join(C10, "Eurobarometer_volume_A")
    src = pd.read_csv(os.path.join(ebd, "_eurobarometer_volA_sources.csv"))
    fw = dict(zip(src.std_round, src.fieldwork_start))                                    # round -> fieldwork start
    rows = []
    for f in sorted(glob.glob(os.path.join(ebd, "STD*"))):
        rnd = int(os.path.basename(f)[3:6])
        if rnd < 62:                 # agreed: start autumn 2004 (rounds 57-61 use an older layout)
            continue
        o = parse_round(f)
        assert o is not None, f"round {rnd} not parsed"
        o["round"], o["fieldwork_start"] = rnd, fw[rnd]
        rows.append(o)
    eb = pd.concat(rows, ignore_index=True).rename(columns={"geo_raw": "geo"})
    eb = eb[eb.geo != "NC"]                                        # Turkish Cypriot Community (one round)
    eb["is_eu_aggregate"] = eb.geo.str.match(r"^EU\d+$")
    eb["iso3"] = eb.geo.map(EB_ISO)
    assert eb.loc[~eb.is_eu_aggregate, "iso3"].notna().all(), eb.loc[eb.iso3.isna() & ~eb.is_eu_aggregate, "geo"].unique()
    assert not eb.duplicated(["round", "geo"]).any()
    assert eb.immigration_pct.between(0, 100).all()
    for iso in C10_COUNTRIES:
        assert eb[eb.iso3 == iso]["round"].nunique() == 44, iso
    assert eb[eb.is_eu_aggregate]["round"].nunique() == 44
    # spot checks against the published Volume A tables
    chk = eb.set_index(["round", "geo"]).immigration_pct.round()
    assert chk[(84, "DE")] == 76 and chk[(84, "EU28")] == 36 and chk[(62, "DK")] == 25, "EB spot check failed"
    eb["chart_country"] = eb.iso3.isin(list(C10_COUNTRIES))
    eb["note"] = ("Standard Eurobarometer QA3a/QA3: 'What do you think are the two most important issues facing (OUR COUNTRY) "
                  "at the moment?' (up to two answers); share mentioning Immigration. EU aggregate membership changes "
                  "(EU25 -> EU27 -> EU28 -> EU27). UK surveyed as a non-member after 2020.")
    save(eb[["round", "fieldwork_start", "geo", "iso3", "is_eu_aggregate", "chart_country", "immigration_pct", "note"]]
         .sort_values(["round", "geo"]), "c10a_immigration_salience_eurobarometer.csv")

    # ---- (b) far-right vote share in national (lower-house) elections
    pl = pd.read_csv(os.path.join(C10, "PopuList_4.0.csv"), sep=";", encoding="utf-8-sig")
    fr = pl[(pl.farright == 1) & pl.country_name.isin(C10_COUNTRIES.values())].copy()
    # PopuList parties without a ParlGov id: Denmark Democrats = ParlGov 2888 (checked: 8.1% in 2022 = DST FVPANDEL).
    # Borgernes Parti first ran in 2026 (manual file). LD-FT (Italy 2008) is recorded in ParlGov under MSFT (176), which is linked.
    fr.loc[(fr.country_name == "Denmark") & (fr.party_name_short == "DD"), "parlgov_id"] = 2888
    z = zipfile.ZipFile(os.path.join(C10, "parlgov_development_csv.zip"))
    el = pd.read_csv(z.open([n for n in z.namelist() if n.endswith("view_election.csv")][0]))
    el = el[(el.election_type == "parliament") & el.country_name.isin(C10_COUNTRIES.values()) & (el.election_date >= "2002-01-01")]
    elections = el[["country_name", "election_date"]].drop_duplicates()
    m = el.merge(fr[["country_name", "parlgov_id", "party_name_short", "farright_start", "farright_end"]]
                 .rename(columns={"party_name_short": "popu_list_party"}),
                 left_on=["country_name", "party_id"], right_on=["country_name", "parlgov_id"])
    yr = m.election_date.str[:4].astype(int)
    m = m[(yr >= m.farright_start) & (yr <= m.farright_end) & m.vote_share.notna()]
    m = m.assign(source="ParlGov development snapshot (Nov 2023)", source_url="https://www.parlgov.org/")
    party = m[["country_name", "election_date", "party_name_short", "popu_list_party", "vote_share", "source", "source_url"]]

    man = pd.read_csv(os.path.join(C10, "official_results", "manual_election_results_after_parlgov.csv"))
    assert (man.election_date > el.election_date.max()).all(), "manual rows must post-date ParlGov"
    assert set(man.popu_list_party) <= set(fr.party_name_short), "manual party not classified far right by PopuList"
    # Denmark 2026 cross-check against the downloaded DST table
    dst = pd.read_csv(os.path.join(C10, "official_results", "DST_FVPANDEL_party_vote_share_all_Denmark_2007-2026.csv"), sep=";")
    d26 = dst[(dst.TID == 2026)].set_index("VALRES").INDHOLD
    assert abs(d26.filter(regex=r"^(O\.|Æ\.|H\.)").sum() - man[man.iso3 == "DNK"].vote_share.sum()) < 1e-9
    # ParlGov vs DST for Denmark 2007-2022 (DF+NB+DD), as a check on the ParlGov join
    party = pd.concat([party, man[party.columns]], ignore_index=True)
    party["iso3"] = party.country_name.map({v: k for k, v in C10_COUNTRIES.items()})
    elections = pd.concat([elections, man[["country_name", "election_date"]].drop_duplicates()]).drop_duplicates()
    tot = (party.groupby(["country_name", "election_date"]).agg(far_right_vote_share=("vote_share", "sum"),
                                                                 parties=("popu_list_party", lambda x: "+".join(sorted(x))),
                                                                 source=("source", "first"))
           .reset_index())
    tot = elections.merge(tot, how="left").fillna({"far_right_vote_share": 0.0, "parties": "", "source": "ParlGov development snapshot (Nov 2023)"})
    tot["iso3"] = tot.country_name.map({v: k for k, v in C10_COUNTRIES.items()})
    tot["manual_addition"] = tot.election_date > el.election_date.max()
    for (c, y), v in {("DNK", "2015"): 21.1, ("DNK", "2022"): 14.4, ("AUT", "2024"): 28.85, ("DEU", "2025"): 20.8}.items():
        got = tot[(tot.iso3 == c) & tot.election_date.str.startswith(y)].far_right_vote_share.round(2).item()
        assert abs(got - v) < 0.051, (c, y, got)
    dk = tot[tot.iso3 == "DNK"].assign(y=lambda d: d.election_date.str[:4].astype(int)).set_index("y").far_right_vote_share
    dst_fr = dst[dst.VALRES.str.match(r"^(O\.|Æ\.|H\.|D\.)")].groupby("TID").INDHOLD.sum()
    gap = (dk.reindex(dst_fr.index) - dst_fr).abs().max()
    print(f"    Denmark ParlGov/manual vs DST FVPANDEL (DF+NB+DD+BP), 2007-2026: max abs gap {gap:.2f}pp")
    assert gap < 0.15
    tot["note"] = ("Sum of vote shares of parties classified far right by PopuList 4.0 (within its start/end years), national "
                   "lower-house elections; ParlGov to June 2023, official results after (data/C10_political_strain/official_results/). "
                   "NL includes BBB per PopuList.")
    save(party.sort_values(["iso3", "election_date", "vote_share"], ascending=[True, True, False]), "c10b_far_right_vote_by_party.csv")
    save(tot[["iso3", "country_name", "election_date", "far_right_vote_share", "parties", "manual_addition", "source", "note"]]
         .sort_values(["iso3", "election_date"]), "c10b_far_right_vote_share.csv")
    print(tot.pivot_table(index=tot.election_date.str[:4], columns="iso3", values="far_right_vote_share").round(1).to_string())


STEPS = {"c01": c01_dependency, "c02": c02_fiscal, "c03": c03_kinship, "c04": c04_living_alone, "c05": c05_loneliness, "c06": c06_suicide, "c07": c07_replacement_migration, "c08": c08_employment_origin, "c09": c09_crime_index, "c10": c10_political_strain}

if __name__ == "__main__":
    todo = sys.argv[1:] or list(STEPS)
    for k in todo:
        print(f"[{k}] {STEPS[k].__doc__.strip().splitlines()[0]}")
        STEPS[k]()
