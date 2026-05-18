"""
Build the analysis-ready panel for the educational-hypergamy article.

Sources merged (all keyed on ISO3 + Year):
  - females_.csv / males.csv : Eurostat EDAT_LFSE_03 (tertiary attainment 25-34,
        both sexes are in the SAME file; the two uploads are identical)
  - non_eu_oecd.csv          : OECD attainment for non-EU OECD countries
  - wpp2024_clean.csv        : UN WPP (TFR, mean age at childbearing, sex ratios)
  - controls.csv             : World Bank WDI (GDP/cap, urban %, female LFP)
  - eu_marriagerate.csv      : Eurostat DEMO_NIND (crude marriage rate, mean age
        at first marriage) -- ONLY 2015-2024
  - global_marriage.xlsx     : UN World Marriage Data 2019, SMAM sheet
        (singulate mean age at marriage; irregular/census years, ends 2019)

Output: panel_hypergamy.csv  (ISO3, Year, + variables)
"""

import warnings
import numpy as np
import pandas as pd
import country_converter as coco

warnings.filterwarnings("ignore")
import logging
logging.getLogger("country_converter").setLevel(logging.CRITICAL)

UP = "/mnt/user-data/uploads"
cc = coco.CountryConverter()

# Explicit overrides for names coco mis-handles or matches ambiguously
OVERRIDES = {
    "china, taiwan province of china": "TWN",
    "taiwan province of china": "TWN",
    "korea, rep.": "KOR",
    "republic of korea": "KOR",
    "korea (republic of)": "KOR",
    "russian federation": "RUS",
    "turkiye": "TUR",
    "türkiye": "TUR",
    "czechia": "CZE",
    "slovak republic": "SVK",
}


def _one_iso3(name):
    """Return a single ISO3 string or np.nan. Never a list."""
    if not isinstance(name, str):
        return np.nan
    key = name.strip().lower()
    if key in OVERRIDES:
        return OVERRIDES[key]
    res = cc.convert(name, to="ISO3", not_found=None)
    if isinstance(res, list):          # ambiguous match -> reject
        return np.nan
    if isinstance(res, str) and len(res) == 3:
        return res
    return np.nan


def to_iso3(names):
    """Convert a series of country names/codes to ISO3, NaN if not a country."""
    uniq = {n: _one_iso3(n) for n in pd.Series(names).dropna().unique()}
    return pd.Series(names).map(uniq)


# Target country universe (50: 38 OECD + 12 comparators)
UNIVERSE = {
    "AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST","FIN","FRA",
    "DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN","KOR","LVA","LTU","LUX",
    "MEX","NLD","NZL","NOR","POL","PRT","SVK","SVN","ESP","SWE","CHE","TUR",
    "GBR","USA","CHN","IND","RUS","BRA","ARG","ZAF","SGP","TWN","IRN","UKR",
    "MYS","THA",
}

# ---------------------------------------------------------------------------
# 1. EDUCATION: Eurostat (EU) + OECD (non-EU) -> hypergamy gap
# ---------------------------------------------------------------------------
eu = pd.read_csv(f"{UP}/females_.csv")  # contains Females, Males, Total
eu = eu[(eu["age"] == "From 25 to 34 years") &
        (eu["isced11"] == "Tertiary education (levels 5-8)") &
        (eu["sex"].isin(["Females", "Males"]))].copy()
eu["ISO3"] = to_iso3(eu["geo"])
eu = eu.dropna(subset=["ISO3"])
eu = eu.rename(columns={"TIME_PERIOD": "Year", "OBS_VALUE": "val"})
eu_w = eu.pivot_table(index=["ISO3", "Year"], columns="sex",
                       values="val", aggfunc="first").reset_index()
eu_w = eu_w.rename(columns={"Females": "tert_F", "Males": "tert_M"})
eu_w["edu_src"] = "Eurostat"

oecd = pd.read_csv(f"{UP}/non_eu_oecd.csv")
oecd = oecd[(oecd["AGE"] == "Y25T34") &
            (oecd["EDUCATION_LEV"] == "ISCED11_5T8") &
            (oecd["SEX"].isin(["F", "M"]))].copy()
oecd["ISO3"] = oecd["REF_AREA"]
oecd = oecd.rename(columns={"TIME_PERIOD": "Year", "OBS_VALUE": "val"})
oecd_w = oecd.pivot_table(index=["ISO3", "Year"], columns="SEX",
                          values="val", aggfunc="first").reset_index()
oecd_w = oecd_w.rename(columns={"F": "tert_F", "M": "tert_M"})
oecd_w["edu_src"] = "OECD"

# Japan: separate OECD export (DSD_EAG_LSO). Uses ATTAINMENT_LEV column name
# and ISCED11A_5T8 code (note the 'A'), distinct from the non_eu_oecd file.
jp = pd.read_csv(f"{UP}/japan.csv")
jp = jp[(jp["AGE"] == "Y25T34") &
        (jp["ATTAINMENT_LEV"] == "ISCED11A_5T8") &
        (jp["SEX"].isin(["F", "M"]))].copy()
jp["ISO3"] = jp["REF_AREA"]
jp = jp.rename(columns={"TIME_PERIOD": "Year", "OBS_VALUE": "val"})
jp_w = jp.pivot_table(index=["ISO3", "Year"], columns="SEX",
                      values="val", aggfunc="first").reset_index()
jp_w = jp_w.rename(columns={"F": "tert_F", "M": "tert_M"})
jp_w["edu_src"] = "OECD-JPN"

# Prefer Eurostat where a country exists in both (e.g. GBR); OECD fills the
# rest; Japan comes only from its dedicated file.
eu_countries = set(eu_w["ISO3"])
oecd_only = oecd_w[~oecd_w["ISO3"].isin(eu_countries)]
covered = eu_countries | set(oecd_only["ISO3"])
jp_only = jp_w[~jp_w["ISO3"].isin(covered)]
edu = pd.concat([eu_w, oecd_only, jp_only], ignore_index=True)
edu = edu.dropna(subset=["tert_F", "tert_M"])
edu["hypergamy_gap"] = edu["tert_F"] - edu["tert_M"]
edu = edu[edu["ISO3"].isin(UNIVERSE)]

# ---------------------------------------------------------------------------
# 2. WPP outcomes (already clean ISO3 + Year)
# ---------------------------------------------------------------------------
wpp = pd.read_csv(f"{UP}/wpp2024_clean.csv")[
    ["ISO3", "Year", "TFR", "MAC", "SRB"]
]

# ---------------------------------------------------------------------------
# 3. World Bank controls (wide -> long -> wide-by-series)
# ---------------------------------------------------------------------------
wb = pd.read_csv(f"{UP}/controls.csv")
year_cols = [c for c in wb.columns if c.endswith("]")]
wb = wb.melt(
    id_vars=["Country Code", "Series Name"],
    value_vars=year_cols, var_name="yr", value_name="v",
)
wb["Year"] = wb["yr"].str.extract(r"(\d{4})").astype(int)
wb["v"] = pd.to_numeric(wb["v"], errors="coerce")
series_map = {
    "GDP per capita (constant 2015 US$)": "gdp_pc",
    "Urban population (% of total population)": "urban_pct",
    "Labor force participation rate, female (% of female population ages 15+) "
    "(national estimate)": "flfp",
}
wb["var"] = wb["Series Name"].map(series_map)
wb = wb.dropna(subset=["var"])
wb["ISO3"] = wb["Country Code"]  # WB codes == ISO3 for our universe
wb_w = wb.pivot_table(index=["ISO3", "Year"], columns="var",
                      values="v", aggfunc="first").reset_index()

# ---------------------------------------------------------------------------
# 4. Marriage: Eurostat crude rate + mean age (2015-2024 only)
# ---------------------------------------------------------------------------
mar = pd.read_csv(f"{UP}/marriage_rate.csv")
mar["ISO3"] = to_iso3(mar["geo"])
mar = mar.dropna(subset=["ISO3"]).rename(
    columns={"TIME_PERIOD": "Year", "OBS_VALUE": "v"}
)
mmap = {
    "Crude marriage rate": "cmr",
    "Mean age at first marriage - females": "mafm_F",
    "Mean age at first marriage - males": "mafm_M",
}
mar = mar[mar["indic_de"].isin(mmap)].copy()
mar["var"] = mar["indic_de"].map(mmap)
mar_w = mar.pivot_table(index=["ISO3", "Year"], columns="var",
                        values="v", aggfunc="first").reset_index()

# ---------------------------------------------------------------------------
# 5. UN SMAM (supplementary, non-EU coverage; women's SMAM)
# ---------------------------------------------------------------------------
sm = pd.read_excel(f"{UP}/global_marriage.xlsx", sheet_name="SMAM", header=2)
sm = sm[sm["Sex"] == "Women"].copy()
sm["ISO3"] = to_iso3(sm["Country or area"])
sm = sm.dropna(subset=["ISO3"])
sm["Year"] = sm["YearStart"].astype(int)
sm["smam_F"] = pd.to_numeric(sm["DataValue"], errors="coerce")
sm = (sm.dropna(subset=["smam_F"])
        .sort_values("Year")
        .groupby(["ISO3", "Year"], as_index=False)["smam_F"].mean())

# ---------------------------------------------------------------------------
# 5b. WVS gender-norms (country-level; slow-moving -> merged as country mean)
#     C001 -> norm_jobs (share agreeing men get job priority when scarce)
#     D059 -> norm_lead (men make better political leaders, 0..1 traditional)
#     norm_index = mean of the two. Higher = more traditional hypergamy norms.
# ---------------------------------------------------------------------------
wvs = pd.read_csv(f"{UP}/wvs_filtered.csv")

wj = wvs[wvs["C001"].isin([1, 2, 3])].copy()
wj["trad_jobs"] = (wj["C001"] == 1).astype(float)
wl = wvs[wvs["D059"].isin([1, 2, 3, 4])].copy()
wl["trad_lead"] = (5 - wl["D059"]) / 3.0


def _wmean(g, val):
    d = g.dropna(subset=[val])
    return np.average(d[val], weights=d["S017"]) if len(d) else np.nan


nj = (wj.groupby("S003").apply(lambda g: _wmean(g, "trad_jobs"))
        .rename("norm_jobs"))
nl = (wl.groupby("S003").apply(lambda g: _wmean(g, "trad_lead"))
        .rename("norm_lead"))
norms = pd.concat([nj, nl], axis=1).reset_index()
norms["ISO3"] = cc.pandas_convert(norms["S003"], src="ISOnumeric",
                                  to="ISO3", not_found=None)
norms = norms[norms["ISO3"].apply(
    lambda x: isinstance(x, str) and len(x) == 3)]
norms["norm_index"] = norms[["norm_jobs", "norm_lead"]].mean(axis=1)
norms = norms[["ISO3", "norm_jobs", "norm_lead", "norm_index"]]

# ---------------------------------------------------------------------------
# 6. MERGE: education is the spine (defines the analytic sample)
# ---------------------------------------------------------------------------
panel = (edu[["ISO3", "Year", "tert_F", "tert_M", "hypergamy_gap", "edu_src"]]
         .merge(wpp,   on=["ISO3", "Year"], how="left")
         .merge(wb_w,  on=["ISO3", "Year"], how="left")
         .merge(mar_w, on=["ISO3", "Year"], how="left")
         .merge(sm,    on=["ISO3", "Year"], how="left")
         .merge(norms, on="ISO3",          how="left"))  # country-level

_iso2name = {i: cc.convert(i, src="ISO3", to="name_short", not_found=i)
             for i in panel["ISO3"].unique()}
panel["country"] = panel["ISO3"].map(_iso2name)
panel = panel.sort_values(["ISO3", "Year"]).reset_index(drop=True)
panel = panel[panel["Year"].between(1990, 2024)]

cols = ["ISO3", "country", "Year", "tert_F", "tert_M", "hypergamy_gap",
        "TFR", "MAC", "SRB", "gdp_pc", "urban_pct", "flfp",
        "cmr", "mafm_F", "mafm_M", "smam_F",
        "norm_jobs", "norm_lead", "norm_index", "edu_src"]
panel = panel[cols]
panel.to_csv("/mnt/user-data/outputs/panel_hypergamy.csv", index=False)

print("Panel written.")
print("Shape:", panel.shape)
print("Countries:", panel["ISO3"].nunique())
print("Year range:", panel["Year"].min(), "-", panel["Year"].max())
