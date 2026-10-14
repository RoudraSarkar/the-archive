import sys,os,csv,gzip,datetime
sys.path.insert(0,os.path.dirname(__file__))
OECD38=set("AUS AUT BEL CAN CHL COL CRI CZE DNK EST FIN FRA DEU GRC HUN ISL IRL ISR ITA JPN KOR LVA LTU LUX MEX NLD NZL NOR POL PRT SVK SVN ESP SWE CHE TUR GBR USA".split())
src,outdir,data=sys.argv[1],sys.argv[2],sys.argv[3]
fn=os.path.basename(src); out=os.path.join(outdir,fn.replace(".csv.gz","_OECD38.csv.gz")); n=0
with gzip.open(src,"rt",encoding="utf-8-sig") as fi, gzip.open(out,"wt",newline="") as fo:
    rd=csv.DictReader(fi); w=csv.DictWriter(fo,fieldnames=rd.fieldnames); w.writeheader()
    for r in rd:
        if r["ISO3_code"] in OECD38: w.writerow(r); n+=1
url="https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/"+fn
with open(os.path.join(data,"_manifest.csv"),"a",newline="") as f:
    csv.writer(f).writerow(["shared_UN_WPP_2024",os.path.basename(out),url,datetime.datetime.now(datetime.UTC).isoformat(timespec="seconds"),f"filtered to OECD-38 ISO3 codes; {n} rows"])
print(out,n)
