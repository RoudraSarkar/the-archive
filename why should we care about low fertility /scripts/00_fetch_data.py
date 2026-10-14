#!/usr/bin/env python3
"""Fetch all raw data for 'Why Should We Care About Low Fertility?'.
Usage: python3 00_fetch_data.py <data_dir> <group>   (group: small | dst | who | wpp | all)
Every file's source URL + fetch time is appended to <data_dir>/_manifest.csv.
"""
import sys, os, json, csv, gzip, io, time, urllib.request, urllib.error, datetime

DATA = sys.argv[1]; GROUP = sys.argv[2] if len(sys.argv) > 2 else "all"
UA = {"User-Agent": "Mozilla/5.0 (archives-data-fetch)"}
OECD38 = set("AUS AUT BEL CAN CHL COL CRI CZE DNK EST FIN FRA DEU GRC HUN ISL IRL ISR ITA JPN KOR LVA LTU LUX MEX NLD NZL NOR POL PRT SVK SVN ESP SWE CHE TUR GBR USA".split())

def log(folder, fname, url, note=""):
    p = os.path.join(DATA, "_manifest.csv"); new = not os.path.exists(p)
    with open(p, "a", newline="") as f:
        w = csv.writer(f)
        if new: w.writerow(["folder", "file", "source_url", "fetched_utc", "note"])
        w.writerow([folder, fname, url, datetime.datetime.utcnow().isoformat(timespec="seconds"), note])

def get(url, folder, fname, data=None, headers=None, note="", tries=3):
    d = os.path.join(DATA, folder); os.makedirs(d, exist_ok=True); out = os.path.join(d, fname)
    if os.path.exists(out) and os.path.getsize(out) > 0:
        print("skip (exists)", folder, fname); return out
    h = dict(UA); h.update(headers or {})
    for i in range(tries):
        try:
            req = urllib.request.Request(url, data=data, headers=h)
            with urllib.request.urlopen(req, timeout=600) as r, open(out + ".part", "wb") as f:
                while True:
                    b = r.read(1 << 20)
                    if not b: break
                    f.write(b)
            os.replace(out + ".part", out)
            print("ok", folder, fname, os.path.getsize(out)); log(folder, fname, url, note); return out
        except Exception as e:
            print("retry", i, fname, e); time.sleep(5)
    print("FAILED", folder, fname, url); log(folder, fname, url, "FAILED"); return None

def dst(table, variables, folder, fname, note=""):
    body = json.dumps({"table": table, "format": "BULK", "lang": "en", "variables": variables}).encode()
    return get("https://api.statbank.dk/v1/data", folder, fname, data=body,
               headers={"Content-Type": "application/json"}, note=f"DST {table} POST {json.dumps(variables)} {note}")

def wpp_filter(fname, folder, keep_variants=None):
    """Stream a WPP gz CSV, keep OECD-38 rows only, write gz."""
    url = "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/" + fname
    d = os.path.join(DATA, folder); os.makedirs(d, exist_ok=True)
    out = os.path.join(d, fname.replace(".csv.gz", "_OECD38.csv.gz"))
    if os.path.exists(out): print("skip", out); return
    req = urllib.request.Request(url, headers=UA)
    n = 0
    with urllib.request.urlopen(req, timeout=1800) as r, gzip.open(out + ".part", "wt", newline="") as fo:
        rd = csv.DictReader(io.TextIOWrapper(gzip.GzipFile(fileobj=r), encoding="utf-8-sig"))
        w = csv.DictWriter(fo, fieldnames=rd.fieldnames); w.writeheader()
        for row in rd:
            if row["ISO3_code"] in OECD38 and (keep_variants is None or row.get("Variant") in keep_variants):
                w.writerow(row); n += 1
    os.replace(out + ".part", out); print("ok", out, n, "rows")
    log(folder, os.path.basename(out), url, f"filtered to OECD-38 ISO3 codes; {n} rows")

def small():
    # C02 fiscal projections
    f = "C02_fiscal_pension_projections"
    get("https://economy-finance.ec.europa.eu/document/download/403cc04f-9487-406b-a48a-22538e0d461c_en?filename=2024_Ageing_Report-Statistical_annex_all_horizontal_tables.xlsx", f, "EU_2024_Ageing_Report_statistical_annex.xlsx")
    get("https://economy-finance.ec.europa.eu/document/download/971dd209-41c2-425d-94f8-e3c3c3459af9_en?filename=ip279_en.pdf", f, "EU_2024_Ageing_Report_ip279.pdf")
    get("https://sdmx.oecd.org/public/rest/data/OECD.ELS.SPD,DSD_PAG@DF_PAG,1.0/all?format=csvfilewithlabels", f, "OECD_PensionsAtAGlance_DF_PAG.csv", note="incl. measure PPEP = projections of public pension expenditure, OAWAR old-age to working-age ratio")
    get("https://sdmx.oecd.org/public/rest/data/OECD.ELS.SPD,DSD_SOCX_AGG@DF_PUB_OLD,1.0/all?format=csvfilewithlabels", f, "OECD_SOCX_public_old_age_spending.csv")
    # C04 living alone
    f = "C04_elderly_living_alone"
    get("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/ilc_lvps30?format=SDMX-CSV", f, "eurostat_ilc_lvps30_65plus_household_type.csv")
    get("https://webfs.oecd.org/Els-com/Affordable_Housing_Database/HM1-4-Living-arrangements-age-groups.pdf", f, "OECD_AHD_HM1-4_living_arrangements.pdf")
    get("https://webfs.oecd.org/Els-com/Affordable_Housing_Database/HM1-4-Living-arrangements-age-groups.xlsx", f, "OECD_AHD_HM1-4_living_arrangements.xlsx")
    # C05 loneliness
    f = "C05_loneliness_EU_LS"; base = "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EU-Loneliness-Survey/"
    for fn in ["Data%20EU4%20sample.zip", "Data%20EU27%20sample.zip", "eu_loneliness_survey_quest_eu4.pdf", "eu_loneliness_survey_quest_eu27.pdf", "copyright.txt"]:
        get(base + fn, f, fn.replace("%20", "_"))
    # C06 suicide by marital status (Japan MHLW white-paper chapters)
    f = "C06_suicide_family_status"
    for tag in ["r1h-1-7", "r2h-1-1-07", "r3h-1-1-07"]:  # later editions moved to opaque URLs; r3h has the marital-status chart
        get(f"https://www.mhlw.go.jp/content/{tag}.pdf", f, f"MHLW_suicide_whitepaper_{tag}.pdf", tries=1)
    # C08 employment / fiscal by origin
    f = "C08_employment_fiscal_by_origin"
    get("https://sdmx.oecd.org/public/rest/data/OECD.ELS.IMD,DSD_MIG@DF_MIG_NUP_SEX,1.0/all?format=csvfilewithlabels", f, "OECD_immigrant_labour_outcomes_by_sex.csv")
    get("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/lfsa_ergacob?format=SDMX-CSV", f, "eurostat_lfsa_ergacob_employment_by_country_of_birth.csv")
    get("https://docs.iza.org/dp17569.pdf", f, "vandeBeek_etal_2024_IZA_DP17569.pdf")
    # Danish FinMin net-contribution PDFs: fm.dk links now 404 -> manual download (see README)
    # C09 crime docs
    f = "C09_crime_by_origin_DK"
    get("https://www.dst.dk/pubfile/47883/Indv%202023", f, "DST_Indvandrere_i_Danmark_2023.pdf")
    get("https://www.dst.dk/nyt/20403", f, "DST_release_2015_crime_index_age_SES.html")
    # C10 political strain
    f = "C10_political_strain"
    get("https://popu-list.github.io/Data/The%20PopuList%204.0.csv", f, "PopuList_4.0.csv")
    get("https://www.parlgov.org/data/parlgov-development_csv-utf-8.zip", f, "parlgov_development_csv.zip")

def dstgroup():
    f8 = "C08_employment_fiscal_by_origin"; f9 = "C09_crime_by_origin_DK"
    star = lambda *c: [{"code": x, "values": ["*"]} for x in c]
    dst("RAS204", star("OPRINDLAND", "BEREGNING", "ALDER", "KØN", "Tid"), f8, "DST_RAS204_employment_by_country_of_origin.csv")
    dst("RAS200", [{"code": "OMRÅDE", "values": ["000"]}] + star("HERKOMST", "ALDER", "KØN", "BEREGNING", "Tid"), f8, "DST_RAS200_employment_by_ancestry.csv")
    dst("STRAFNA3", star("KOEN", "ALDER", "IELAND", "Tid"), f9, "DST_STRAFNA3_guilty_by_sex_age_origin.csv", "all offence types incl. traffic/special laws")
    dst("STRAFNA4", star("OVERTRÆD", "IELAND", "Tid"), f9, "DST_STRAFNA4_guilty_by_offence_origin.csv")
    dst("STRAFNA9", star("KOEN", "ALDER", "HERKOMST", "Tid"), f9, "DST_STRAFNA9_guilty_by_sex_age_ancestry.csv")
    dst("STRFNA10", star("OVERTRÆD", "HERKOMST", "Tid"), f9, "DST_STRFNA10_guilty_by_offence_ancestry.csv")
    q1 = [f"{y}K1" for y in range(2008, 2027)]  # DST quarter codes use K
    dst("FOLK1C", [{"code": "OMRÅDE", "values": ["000"]}, {"code": "KØN", "values": ["*"]}, {"code": "ALDER", "values": ["*"]},
                   {"code": "HERKOMST", "values": ["*"]}, {"code": "IELAND", "values": ["*"]}, {"code": "Tid", "values": q1}],
        f9, "DST_FOLK1C_population_by_origin_age_sex_2008-2026Q1.csv", "denominators for standardised index")
    dst("FOLK2", star("ALDER", "KØN", "HERKOMST", "STATSB", "IELAND") + [{"code": "Tid", "values": ["2000", "2002", "2004", "2005", "2006", "2007"]}],
        f9, "DST_FOLK2_population_by_origin_single_age_sex_2000-2007.csv", "denominators for pre-2008 years")

def who():
    f = "C06_suicide_family_status/WHO_mortality_database_raw"
    b = "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/"
    for fn in ["morticd10_part1.zip", "morticd10_part2.zip", "morticd10_part3.zip", "morticd10_part4.zip", "morticd10_part5.zip",
               "morticd10_part6.zip", "morticd10_add.zip", "mort_pop.zip", "mort_country_codes.zip", "mort_notes.zip", "mort_availability.zip",
               "mort_documentation71f9e29d-7e3f-41e6-aafc-c4c1775c7aa3.zip"]:
        get(b + fn, f, fn.replace("71f9e29d-7e3f-41e6-aafc-c4c1775c7aa3", ""))
    who_filter_suicide(os.path.join(DATA, f), os.path.join(DATA, "C06_suicide_family_status"))

def who_filter_suicide(rawdir, outdir):
    """Extract intentional self-harm rows (ICD-10 X60-X84, Y87.0; condensed list code 1101) from all ICD-10 parts."""
    import zipfile, re
    pat = re.compile(r"^(X6\d|X7\d|X8[0-4]|Y870)")
    out = os.path.join(outdir, "WHO_MDB_suicide_ICD10_all_countries.csv"); n = 0; hdr = None
    with open(out, "w", newline="") as fo:
        w = csv.writer(fo)
        for fn in sorted(os.listdir(rawdir)):
            if not fn.startswith("morticd10_part"): continue  # _add.zip is xlsx (DE, NO), handled manually
            with zipfile.ZipFile(os.path.join(rawdir, fn)) as z:
                for inner in z.namelist():
                    with z.open(inner) as fi:
                        rd = csv.reader(io.TextIOWrapper(fi, encoding="latin-1"))
                        h = next(rd)
                        if hdr is None: hdr = h; w.writerow(h + ["source_file"])
                        ic = h.index("Cause"); il = h.index("List")
                        for row in rd:
                            c = row[ic]
                            if pat.match(c) or (c == "1101" and row[il] in ("101", "103")):
                                w.writerow(row + [fn]); n += 1
    print("ok WHO suicide rows", n)
    log("C06_suicide_family_status", os.path.basename(out), "derived from morticd10_part1-6 + add", f"filtered: Cause X60-X84/Y870 or list-101/103 code 1101; {n} rows")

def wpp():
    f = "shared_UN_WPP_2024"
    get("https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Demographic_Indicators_Medium.csv.gz", f, "WPP2024_Demographic_Indicators_Medium.csv.gz", note="unfiltered, all countries")
    for fn in ["WPP2024_Demographic_Indicators_OtherVariants.csv.gz",
               "WPP2024_PopulationByAge5GroupSex_OtherVariants.csv.gz",
               "WPP2024_PopulationByAge5GroupSex_Medium.csv.gz",
               "WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023.csv.gz",
               "WPP2024_Population1JanuaryBySingleAgeSex_Medium_2024-2100.csv.gz",
               "WPP2024_Fertility_by_Age1.csv.gz",
               "WPP2024_Life_Table_Complete_Medium_Female_1950-2023.csv.gz",
               "WPP2024_Life_Table_Complete_Medium_Female_2024-2100.csv.gz",
               "WPP2024_Life_Table_Complete_Medium_Male_1950-2023.csv.gz",
               "WPP2024_Life_Table_Complete_Medium_Male_2024-2100.csv.gz"]:
        try: wpp_filter(fn, f)
        except Exception as e: print("FAILED", fn, e); log(f, fn, "", f"FAILED {e}")

{"small": [small], "dst": [dstgroup], "who": [who], "wpp": [wpp], "all": [small, dstgroup, who, wpp]}[GROUP][0]() if GROUP != "all" else [g() for g in (small, dstgroup, who, wpp)]
