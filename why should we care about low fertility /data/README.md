# Data: "Why Should We Care About Low Fertility?"

Fetched 7 Oct 2026 by `scripts/00_fetch_data.py` (+ `scripts/filter_local_wpp.py` for the four WPP files that needed a resumable download).
Every file's source URL, fetch time and any filtering note is in `_manifest.csv`. Rows marked FAILED are dead links and are covered under "Still manual" below.

## Folder → chart map

| Folder | Chart(s) | What's inside |
|---|---|---|
| `shared_UN_WPP_2024/` | **C1** dependency ratio, **C3** kinship (DemoKin), **C7** replacement migration | UN World Population Prospects 2024. Everything except `Demographic_Indicators_Medium` (kept whole, all countries) is **filtered to the 38 OECD members** to keep the size down. `OtherVariants` files hold Low/High, **Instant replacement**, **Zero migration**, Constant fertility, etc. `Fertility_by_Age1` = single-year ASFRs, all variants. `Life_Table_Complete_*` = single-year life tables by sex (DemoKin inputs). |
| `C02_fiscal_pension_projections/` | C2 | OECD Pensions at a Glance SDMX extract (`MEASURE == "PPEP"` = projected public pension spending % GDP, 2023–2060, 52 countries incl. JPN/KOR; `OAWAR` = old-age to working-age ratio). OECD SOCX old-age spending history. EU 2024 Ageing Report: full report PDF + statistical annex xlsx (pensions, health, long-term care to 2070, EU only). |
| `C04_elderly_living_alone/` | C4 | Eurostat `ilc_lvps30` (65+ by household type, incl. "one adult 65+"). OECD Affordable Housing DB HM1.4 (PDF + xlsx; 65+, **no Japan**). |
| `C05_loneliness_EU_LS/` | C5 | EU Loneliness Survey microdata, EU4 (probability sample, use for prevalence) and EU27 (non-probability panel) as Stata .dta + label/value CSVs, plus both questionnaires. Has marital status, children ever had, grandchildren, loneliness items and a distress battery. |
| `C06_suicide_family_status/` | C6 | Japan MHLW suicide white-paper chapters (r1h, r2h, **r3h** = 2019 deaths by marital status × sex × age). `WHO_MDB_suicide_ICD10_all_countries.csv` = every ICD-10 self-harm row (X60–X84, Y87.0, list code 1101) extracted from the WHO Mortality Database; Japan = 3160, Korea = 3325 (both 1995–2023). Raw WHO zips + population file + country codes + docs in `WHO_mortality_database_raw/`. |
| `C08_employment_fiscal_by_origin/` | C8 | Statistics Denmark `RAS204` (employment rate × 42 origin countries × age × sex, 2008–2024) and `RAS200` (by ancestry). OECD immigrant labour-market outcomes. Eurostat `lfsa_ergacob`. van de Beek et al. 2024 (IZA DP 17569). |
| `C09_crime_by_origin_DK/` | C9 | DST `STRAFNA3` (guilty persons × sex × age band × 75 origins, 2000–2025; **all offence types incl. traffic**), `STRAFNA4` (× offence type, no age), `STRAFNA9`/`STRFNA10` (by ancestry). Denominators: `FOLK1C` (Q1 2008–2026, 5-year ages) and `FOLK2` (2000–2007, single ages). DST *Indvandrere i Danmark 2023* (age-standardised index, 2022 data) and the 2015 release (age + SES standardised, 2014 data). |
| `C10_political_strain/` | C10 | PopuList 4.0 (CSV), ParlGov development snapshot (zip; `view_election.csv` has vote shares). |

DST CSVs are **semicolon-delimited**, UTF-8 with BOM, labels in English.

## Added 7 Oct 2026 (second pass)
- `C10_political_strain/Eurobarometer_volume_A/`: "Volume A" country tables for **every Standard Eurobarometer round 57-105 (spring 2002 to spring 2026, 49 rounds)**, pulled from the EU open-data portal (data.europa.eu) via the Eurobarometer API. The national "most important issues facing (OUR COUNTRY)" question is the sheet named QA3/QA30-style (number varies by round), with an "Immigration" row and one column per country; each answer has a count row followed by a share row. 2002-2020 rounds are zipped old-format .xls (read with readxl / xlrd); 2021+ are .xlsx. Sources in `_eurobarometer_volA_sources.csv`.
- `C04_elderly_living_alone/KR_elderly_living_alone_ratio_census.csv`: Korea's national indicator 4233 "독거노인비율" = share of people aged 65+ living alone, 2000, 2005, 2010, 2015-2025 (Population and Housing Census). Note the 2010 -> 2015 dip (20.2 -> 19.1) coincides with the switch to a register-based census in 2015, so treat it as a break in the series.
- `C04_elderly_living_alone/JP_CabinetOffice_AgeingWhitePaper2025_fig1-1-9.csv`: Japan 65+ living alone, 1980-2050 incl. projections (Shift-JIS).

## Still manual / not done
1. **Danish Finance Ministry net fiscal contribution (C8, text only, optional).** fm.dk restructured and the old PDF links now 404. Parliamentary answer quoting the 2018 projection to 2100: https://ft.dk/samling/20211/almdel/fiu/spm/8/svar/1826050/2475227.pdf
2. **Germany and Norway WHO suicide data** is already downloaded (morticd10_add.zip holds Germany.xlsx and Norway.xlsx); it just isn't merged into the combined CSV yet.
3. **Newer MHLW white papers** (2022+) moved to opaque URLs; r3h already has the marital-status chart.
