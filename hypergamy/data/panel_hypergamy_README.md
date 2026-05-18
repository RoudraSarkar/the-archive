# panel_hypergamy.csv — Data Dictionary

Analysis-ready country-year panel for the educational-hypergamy / fertility article.

**Shape:** 942 rows · 38 countries · 1992–2024 · keyed on (`ISO3`, `Year`), no duplicates.

## Columns

| Column | Description | Source | Coverage |
|---|---|---|---|
| `ISO3` | ISO 3166-1 alpha-3 country code | — | 100% |
| `country` | Country short name | — | 100% |
| `Year` | Calendar year | — | 100% |
| `tert_F` | % of women 25–34 with tertiary education (ISCED 5–8) | Eurostat EDAT_LFSE_03 / OECD | 100% |
| `tert_M` | % of men 25–34 with tertiary education (ISCED 5–8) | Eurostat / OECD | 100% |
| `hypergamy_gap` | `tert_F − tert_M` (pp). The key predictor. Positive = women more educated | derived | 100% |
| `TFR` | Total fertility rate (births/woman). **Primary outcome.** | UN WPP 2024 | 100% |
| `MAC` | Mean age at childbearing (years) | UN WPP 2024 | 100% |
| `SRB` | Sex ratio at birth (males per 100 female births) | UN WPP 2024 | 100% |
| `gdp_pc` | GDP per capita, constant 2015 US$ | World Bank WDI | 100% |
| `urban_pct` | Urban population (% of total) | World Bank WDI | 100% |
| `flfp` | Female labour force participation, % of women 15+ | World Bank WDI | 100% |
| `cmr` | Crude marriage rate (per 1,000 pop). Secondary outcome. | Eurostat DEMO_NIND | 81% |
| `mafm_F` | Mean age at first marriage, women | Eurostat DEMO_NIND | 71% |
| `mafm_M` | Mean age at first marriage, men | Eurostat DEMO_NIND | 71% |
| `smam_F` | Singulate mean age at marriage, women (supplementary, irregular years) | UN World Marriage Data 2019 | 39% |
| `norm_jobs` | Share agreeing "men should have more right to a job than women" (C001). Higher = more traditional | WVS Time-Series 1981–2022 | 74% |
| `norm_lead` | "Men make better political leaders" recoded 0–1, 1 = most traditional (D059) | WVS Time-Series | 74% |
| `norm_index` | Mean of `norm_jobs` & `norm_lead`. The hypergamy-norms moderator. Country-level (time-invariant) | derived | 74% |
| `edu_src` | Provenance of education figures: `Eurostat`, `OECD`, `OECD-JPN` | — | 100% |

## Recommended analysis samples

- **Main TFR model:** all 942 rows (full coverage on gap, TFR, and all controls). 38 countries.
- **Interaction model (norms):** 700 rows / 29 countries. Drops 9 countries with no WVS data (AUT, BEL, CRI, DNK, IRL, ISL, ISR, LUX, PRT — these never fielded the C001/D059 items consistently).
- **Marriage-rate model:** 761 rows / 27 countries.
- **Marriage-age model:** ~670 rows / 71% coverage.

## Known caveats (carry into the article honestly)

1. **Uneven panel.** Eurostat countries have long series (≤33 yrs); some non-EU OECD (Australia, Chile, Korea) have only ~5 points from a 5-yearly OECD export. Use country fixed effects + cluster-robust SEs; consider reporting with/without short-series countries.
2. **Japan.** Sourced from a separate OECD export (DSD_EAG_LSO). Female tertiary slightly *exceeds* male (gap +3 to +7), unlike most of OECD where the gap is much wider — a useful contrast case, not an error. No Eurostat marriage data for Japan (`cmr` blank).
3. **Marriage outcomes thinner than TFR.** Treat TFR as primary; marriage variables as secondary/illustrative.
4. **Non-OECD comparators absent.** No by-sex-and-age tertiary data for China/India/Brazil/etc. Scope is "OECD + a few", not global. State this explicitly.
5. **Norms are country-level and time-invariant.** WVS waves are sparse, so `norm_index` is a single per-country mean across all available waves, not a time series. This is standard for slow-moving cultural measures but means the interaction identifies *cross-country* moderation, not within-country change. State this in the methods.

## Rebuild

`build_panel.py` regenerates this file from the seven raw inputs in `/mnt/user-data/uploads`.
