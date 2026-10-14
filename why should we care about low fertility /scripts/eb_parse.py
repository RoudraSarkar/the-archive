"""eb_parse.py - extract 'most important issues facing (OUR COUNTRY)' -> Immigration share by country
from Standard Eurobarometer Volume A tables (rounds 57-105; .xls in zips for old rounds, .xlsx for new)."""
import os, re, io, zipfile
import pandas as pd

Q_RE = re.compile(r"important issues? facing \(?our country\)?", re.I)
EXCLUDE = re.compile(r"personally|\(EU\)|European Union|the EU at the moment", re.I)

def _sheets(path):
    """yield (sheet_name, DataFrame of raw cells) for every sheet in an xls/xlsx (zipped or not)."""
    def from_bytes(name, data):
        eng = "xlrd" if name.lower().endswith(".xls") else "openpyxl"
        for sh, df in pd.read_excel(io.BytesIO(data), sheet_name=None, header=None, engine=eng).items():
            yield sh, df
    if path.endswith(".zip"):
        z = zipfile.ZipFile(path)
        for n in z.namelist():
            if n.lower().endswith((".xls", ".xlsx")):
                yield from from_bytes(n, z.read(n))
    else:
        with open(path, "rb") as f:
            yield from from_bytes(path, f.read())

def _cell_text(df, i, j):
    v = df.iat[i, j] if j < df.shape[1] else None
    return str(v) if v is not None and pd.notna(v) else ""

def _last_token(v):
    """'UE27\nEU27' or 'UE27 EU27' -> 'EU27' (French label first, English last)."""
    t = str(v).split()
    return t[-1] if t else ""

def _numeric(vals):
    return [v for v in vals if isinstance(v, (int, float)) and pd.notna(v)]

def _extract_block(df, q_row):
    """From the row where the question text appears, find the country header and the Immigration row."""
    hdr_i = None
    for i in range(q_row, min(q_row + 20, len(df))):
        vals = [_last_token(v) for v in df.iloc[i].tolist()]
        if sum(bool(re.fullmatch(r"[A-Z]{2}(-[A-Z])?", v)) for v in vals) >= 5:
            hdr_i = i; break
    if hdr_i is None:
        return None
    hdr = [_last_token(v) for v in df.iloc[hdr_i].tolist()]
    for i in range(hdr_i + 1, min(hdr_i + 120, len(df))):
        labs = " ".join(str(df.iat[i, j]) for j in range(min(3, df.shape[1]))
                        if isinstance(df.iat[i, j], str))       # text cells only (numbers are not labels)
        if Q_RE.search(labs) and i > hdr_i + 2:          # reached the next question block
            return None
        if re.search(r"(^|\n|\s)Immigration\s*$", labs.strip(), re.I) or re.fullmatch(r"\s*(L'immigration\s*)?Immigration\s*", labs.replace("\n", " "), re.I):
            row = df.iloc[i].tolist()
            nums = _numeric(row)
            # counts on this row and shares on the next (unlabelled) row -> use the next row
            if nums and max(nums) > 1.5 and i + 1 < len(df):
                nxt = df.iloc[i + 1].tolist(); nn = _numeric(nxt)
                if nn and max(nn) <= 1.0:
                    row = nxt
            rows = [(c, float(v)) for c, v in zip(hdr, row)
                    if re.fullmatch(r"[A-Z]{2}(-[A-Z])?|E[UA]\d*|UE\d+", c) and isinstance(v, (int, float)) and pd.notna(v)]
            return rows or None
    return None

def parse_round(path):
    for sh, df in _sheets(path):
        for i in range(len(df)):
            txt = " ".join(_cell_text(df, i, j) for j in range(min(df.shape[1], 12)))
            if Q_RE.search(txt) and not EXCLUDE.search(txt):
                rows = _extract_block(df, i)
                if rows:
                    out = pd.DataFrame(rows, columns=["geo_raw", "value"])
                    scale = 100.0 if out.value.max() <= 1.0 else 1.0
                    out["immigration_pct"] = out.value * scale
                    out["sheet"] = sh
                    return out
    return None
