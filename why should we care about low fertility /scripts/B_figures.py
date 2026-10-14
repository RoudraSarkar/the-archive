"""B_figures.py - VISUALISATION ONLY for 'Why Should We Care About Low Fertility?'.

Reads out/*.csv and nothing else. If a figure needs a number that isn't in out/,
that number belongs in A_analysis.py - do not compute it here.

House style = theme_archives (Python port of names~fertility/theme_archives.R):
Georgia serif, cream ground, terracotta reserved for the salient series, no y-grid,
faint taupe x-grid, no ticks, legend top-left untitled, caption 'Data: ... · Chart: the archives',
9 in wide at 300 dpi.

Font: Georgia if installed (it is on macOS). Otherwise falls back to Gelasio
(fonts/, SIL OFL - drawn on Georgia's metrics) and prints a warning, so the
fallback is never silent.

Usage:  python3 scripts/B_figures.py [fig01 fig02 ...]   (no args = all)
"""
import os, sys, warnings
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib import font_manager
from matplotlib.lines import Line2D
from matplotlib.patches import Patch

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
OUT = os.path.join(ROOT, "out")
FIG = os.path.join(ROOT, "figures")
os.makedirs(FIG, exist_ok=True)

# ---------------------------------------------------------------- theme_archives
PAL = dict(cream="#F1EBDF", charcoal="#2A231C", brown="#6B5D4F", grey="#8A7C6D",
           taupe="#C9BFA9", terracotta="#8C3A2F")
CAT = dict(slate="#3A4A5C", sage="#7A8B6F", sand="#C9A875", warm_grey="#A89C8A")   # terracotta never in rotation


def _font():
    names = {f.name for f in font_manager.fontManager.ttflist}
    if "Georgia" in names:
        return "Georgia"
    fdir = os.path.join(ROOT, "fonts")
    for f in os.listdir(fdir) if os.path.isdir(fdir) else []:
        if f.lower().endswith(".ttf"):
            font_manager.fontManager.addfont(os.path.join(fdir, f))
    warnings.warn("Georgia not found - using Gelasio (Georgia-metric fallback). Final exports should be run on a machine with Georgia.")
    return "Gelasio"


FONT = _font()
BASE = 11
plt.rcParams.update({
    "font.family": FONT, "font.size": BASE, "text.color": PAL["charcoal"],
    "figure.facecolor": PAL["cream"], "axes.facecolor": PAL["cream"], "savefig.facecolor": PAL["cream"],
    "axes.edgecolor": PAL["cream"], "axes.labelcolor": PAL["brown"], "axes.titlecolor": PAL["charcoal"],
    "axes.spines.top": False, "axes.spines.right": False, "axes.spines.left": False, "axes.spines.bottom": False,
    "xtick.color": PAL["brown"], "ytick.color": PAL["brown"], "xtick.major.size": 0, "ytick.major.size": 0,
    "xtick.labelsize": BASE * 0.8, "ytick.labelsize": BASE * 0.8, "axes.labelsize": BASE * 0.82,
    "axes.grid": False, "grid.color": PAL["taupe"], "grid.linewidth": 0.4,
    "legend.frameon": False, "legend.fontsize": BASE * 0.82, "legend.labelcolor": PAL["brown"],
})


def style_ax(ax, grid="x"):
    """grid: 'x' = vertical gridlines only (house default), 'y', 'both', 'none'."""
    if grid in ("x", "both"):
        ax.xaxis.grid(True)
    if grid in ("y", "both"):
        ax.yaxis.grid(True)
    ax.set_axisbelow(True)
    ax.tick_params(length=0, pad=3)


def rich_text(fig, x, y, parts, size, color, **kw):
    """Draw a line of text with <b>-style runs: parts = [(text, bold), ...]. Figure coords, left-aligned.
    Wraps onto new lines at '\n' inside a part."""
    r = fig.canvas.get_renderer()
    line_h = size * 1.45 / 72 / fig.get_figheight()
    x0, cx, cy = x, x, y
    for text, bold in parts:
        for k, chunk in enumerate(text.split("\n")):
            if k > 0:
                cx, cy = x0, cy - line_h
            if not chunk:
                continue
            t = fig.text(cx, cy, chunk, fontsize=size, color=color, fontweight="bold" if bold else "normal",
                         ha="left", va="top", **kw)
            cx += t.get_window_extent(renderer=r).width / fig.dpi / fig.get_figwidth()
    return cy - line_h


def caption(*sources, notes=()):
    return " · ".join(["Data: " + "; ".join(sources)] + list(notes) + ["Chart: the archives"])


def header(fig, title, subtitle_parts, top=0.975, left=0.035):
    fig.text(left, top, title, fontsize=BASE * 1.55, color=PAL["charcoal"], ha="left", va="top")
    n = title.count("\n") + 1
    H = fig.get_figheight()                      # gaps in inches, so tall figures don't stretch them
    return rich_text(fig, left, top - (0.352 + 0.30 * (n - 1)) / H, subtitle_parts, BASE * 1.0, PAL["brown"])


def footer(fig, text, left=0.035, bottom=0.018):
    fig.text(left, bottom, text, fontsize=BASE * 0.72, color=PAL["grey"], ha="left", va="bottom", wrap=True)


def save(fig, name):
    p = os.path.join(FIG, name)
    fig.savefig(p, dpi=300)
    plt.close(fig)
    print(f"  wrote figures/{name}  ({fig.get_figwidth():.0f} x {fig.get_figheight():.1f} in @ 300 dpi, font {FONT})")


def grab(name):
    return pd.read_csv(os.path.join(OUT, name))


# ---------------------------------------------------------------- Figure 1
def fig01():
    """C1 - old-age dependency ratio, 1950-2100, by UN WPP 2024 scenario (small multiples)."""
    d = grab("c01_dependency_ratio_by_variant.csv")
    order = [("KOR", "South Korea"), ("JPN", "Japan"), ("ITA", "Italy"), ("DEU", "Germany"),
             ("GBR", "United Kingdom"), ("USA", "United States"), ("OECD", "OECD-38")]
    y = "ratio_65_per100_2064"
    ymax = d[d.iso3.isin([o[0] for o in order]) & d.variant.isin(["Medium", "Low", "High", "Zero migration",
                                                                   "Instant replacement"])][y].max()
    fig, axes = plt.subplots(2, 4, figsize=(9, 6.4), sharex=True, sharey=True)
    fig.subplots_adjust(left=0.07, right=0.985, top=0.765, bottom=0.12, wspace=0.12, hspace=0.32)
    for ax, (iso, label) in zip(axes.flat, order):
        g = d[d.iso3 == iso]
        v = {k: x.sort_values("year") for k, x in g.groupby("variant")}
        style_ax(ax)
        lo, hi = v["Low"].set_index("year")[y], v["High"].set_index("year")[y]
        ax.fill_between(lo.index, lo.values, hi.reindex(lo.index).values, color=CAT["sand"], alpha=0.35, lw=0)
        ax.plot(v["Zero migration"].year, v["Zero migration"][y], color=CAT["warm_grey"], lw=1.3, ls=(0, (1.2, 1.6)))
        ax.plot(v["Instant replacement"].year, v["Instant replacement"][y], color=CAT["slate"], lw=1.3, ls=(0, (4, 2)))
        med = v["Medium"]
        ax.plot(med[med.year <= 2023].year, med[med.year <= 2023][y], color=PAL["charcoal"], lw=1.6)
        ax.plot(med[med.year >= 2023].year, med[med.year >= 2023][y], color=PAL["terracotta"], lw=1.9)
        ax.axhline(100, color=PAL["taupe"], lw=0.8, ls="-", zorder=0)
        for yr in (2024, 2070):
            val = med.set_index("year")[y][yr]
            ax.plot(yr, val, "o", ms=3.6, color=PAL["charcoal"] if yr == 2024 else PAL["terracotta"],
                    mec=PAL["cream"], mew=0.8, zorder=5)
            ax.annotate(f"{val:.0f}", (yr, val), xytext=(-2 if yr == 2024 else 0, 6 if yr == 2024 else 7),
                        textcoords="offset points", ha="center", fontsize=BASE * 0.72, color=PAL["brown"])
        ax.set_title(label, loc="left", fontsize=BASE * 0.92, color=PAL["charcoal"], pad=4,
                     fontweight="bold" if iso == "KOR" else "normal")
        ax.set_xlim(1950, 2100)
        ax.set_ylim(0, np.ceil(ymax / 20) * 20)
        ax.set_xticks([1950, 2000, 2050, 2100])
        for t, ha in zip(ax.get_xticklabels(), ["left", "center", "center", "right"]):
            t.set_ha(ha)
        ax.set_yticks(range(0, int(np.ceil(ymax / 20) * 20) + 1, 50))
    # 8th cell: the takeaway, not an empty box
    ax = axes.flat[7]
    ax.axis("off")
    o = d[(d.iso3 == "OECD")].pivot_table(index="year", columns="variant", values=y)
    ax.text(0.04, 0.92,
            f"Even if every OECD\ncountry jumped to\nreplacement fertility\nin 2024, the ratio\nwould still rise from\n"
            f"{o.loc[2024, 'Medium']:.0f} to {o.loc[2070, 'Instant replacement']:.0f} by 2070.\n\n"
            "Tomorrow's 65-year-olds\nare already born.",
            transform=ax.transAxes, va="top", ha="left", fontsize=BASE * 0.82, color=PAL["brown"], linespacing=1.35)
    for a in axes[:, 0]:
        a.set_ylabel("65+ per 100 aged 20–64")
    k = d[(d.iso3 == "KOR")].pivot_table(index="year", columns="variant", values=y)
    sub_end = header(fig, "By 2070, South Korea will have more over-65s than working-age adults",
                     [("People aged 65+ per 100 aged 20–64. ", False), ("UN medium projection", True),
                      (", with the low–high fertility range shaded.\n", False),
                      ("Neither ", False), ("instant replacement fertility", True), (" nor ", False),
                      ("zero migration", True), (" changes the direction.", False)])
    handles = [Line2D([], [], color=PAL["charcoal"], lw=1.6, label="Observed"),
               Line2D([], [], color=PAL["terracotta"], lw=1.9, label="Medium projection"),
               Patch(facecolor=CAT["sand"], alpha=0.35, label="Low–high fertility"),
               Line2D([], [], color=CAT["slate"], lw=1.3, ls=(0, (4, 2)), label="Instant replacement (from 2024)"),
               Line2D([], [], color=CAT["warm_grey"], lw=1.3, ls=(0, (1.2, 1.6)), label="Zero migration")]
    fig.legend(handles=handles, loc="upper left", bbox_to_anchor=(0.03, sub_end + 0.005), ncol=5,
               handlelength=2.2, columnspacing=1.2, handletextpad=0.5, borderaxespad=0)
    footer(fig, caption("UN World Population Prospects 2024",
                        notes=["OECD-38 is population-weighted", "grey line = 100 (one over-65 per working-age adult)"]))
    save(fig, "fig01_dependency_ratio.png")


# ---------------------------------------------------------------- Figure 2
EU_NAMES = {"AT": "Austria", "BE": "Belgium", "BG": "Bulgaria", "CY": "Cyprus", "CZ": "Czechia", "DE": "Germany",
            "DK": "Denmark", "EE": "Estonia", "EL": "Greece", "ES": "Spain", "FI": "Finland", "FR": "France",
            "HR": "Croatia", "HU": "Hungary", "IE": "Ireland", "IT": "Italy", "LT": "Lithuania", "LU": "Luxembourg",
            "LV": "Latvia", "MT": "Malta", "NL": "Netherlands", "PL": "Poland", "PT": "Portugal", "RO": "Romania",
            "SE": "Sweden", "SI": "Slovenia", "SK": "Slovakia", "EU": "EU-27"}


def fig02():
    """C2 - extra cost of ageing under 20% lower fertility, EU 2024 Ageing Report (EU line + 2070 country bars)."""
    d = grab("c02_eu_cost_of_ageing_scenarios.csv")
    e = d[d.geo == "EU"].sort_values("year")
    fig = plt.figure(figsize=(9, 6.4))
    axL = fig.add_axes([0.075, 0.12, 0.47, 0.62])
    axR = fig.add_axes([0.70, 0.12, 0.285, 0.62])

    # ---- left: EU-27 gap over time
    style_ax(axL, grid="x")
    gap = e.set_index("year").fertility_gap_pp
    axL.axhline(0, color=PAL["brown"], lw=0.8)
    axL.fill_between(gap.index, gap.values, 0, where=gap.values < 0, color=CAT["sage"], alpha=0.30, lw=0, interpolate=True)
    axL.fill_between(gap.index, gap.values, 0, where=gap.values >= 0, color=PAL["terracotta"], alpha=0.15, lw=0, interpolate=True)
    mig = e.set_index("year").lowmig_gap_pp
    axL.plot(mig.index, mig.values, color=CAT["slate"], lw=1.4, ls=(0, (4, 2)))
    axL.plot(gap.index, gap.values, color=PAL["terracotta"], lw=1.9)
    axL.text(2023, 0.47, "A third less non-EU\nimmigration", ha="left", va="bottom", fontsize=BASE * 0.75, color=PAL["brown"])
    axL.plot(2070, mig[2070], "o", ms=4, color=CAT["slate"], mec=PAL["cream"], mew=0.8, zorder=5)
    axL.annotate(f"{mig[2070]:+.2f}", (2070, mig[2070]), xytext=(6, 0), textcoords="offset points", ha="left",
                 va="center", fontsize=BASE * 0.75, color=PAL["brown"])
    axL.text(2062.5, 0.30, "20% lower\nfertility", ha="left", va="top", fontsize=BASE * 0.75,
             color=PAL["brown"], linespacing=1.25)
    lo_y = gap.idxmin(); cross = gap[(gap.index > lo_y) & (gap > 0)].index.min()
    for yr, dx, dy, ha in ((lo_y, 0, -10, "center"), (2070, 6, 0, "left")):
        axL.plot(yr, gap[yr], "o", ms=4, color=PAL["terracotta"], mec=PAL["cream"], mew=0.8, zorder=5)
        axL.annotate(f"{gap[yr]:+.2f}", (yr, gap[yr]), xytext=(dx, dy), textcoords="offset points", ha=ha,
                     va="top" if yr == lo_y else "center", fontsize=BASE * 0.75, color=PAL["brown"])
    axL.text(2026, -0.47, "Fewer pupils:\neducation spending\nfalls first", fontsize=BASE * 0.75, color=PAL["brown"],
             va="center", linespacing=1.3)
    axL.text(2023, 0.80, f"From {cross}, the\nextra pensioners\noutweigh it", fontsize=BASE * 0.75,
             color=PAL["brown"], va="center", linespacing=1.3)
    axL.set_xlim(2022, 2074); axL.set_xticks([2022, 2030, 2040, 2050, 2060, 2070])
    axL.set_ylim(-0.6, 1.05)
    axL.set_ylabel("Percentage points of GDP")
    axL.set_title("EU-27, extra cost each year", loc="left", fontsize=BASE * 0.92, pad=6)

    # ---- right: 2070 by country
    style_ax(axR, grid="x")
    b = d[(d.year == 2070) & d.geo.isin(EU_NAMES)].set_index("geo").fertility_gap_pp.sort_values()
    hl = {"IT", "DE"}
    cols = [PAL["terracotta"] if g in hl else (CAT["slate"] if g == "EU" else CAT["warm_grey"]) for g in b.index]
    yy = np.arange(len(b))
    axR.barh(yy, b.values, height=0.74, color=cols, lw=0)
    axR.axvline(0, color=PAL["brown"], lw=0.8)
    axR.set_yticks(yy)
    axR.set_yticklabels([EU_NAMES[g] for g in b.index], fontsize=BASE * 0.68)
    for t, g in zip(axR.get_yticklabels(), b.index):
        if g in hl or g == "EU":
            t.set_color(PAL["charcoal"]); t.set_fontweight("bold")
    for i, (g, v) in enumerate(b.items()):
        if g in hl or g == "EU":
            axR.text(v + (0.03 if v >= 0 else -0.03), i, f"{v:+.2f}", va="center", ha="left" if v >= 0 else "right",
                     fontsize=BASE * 0.66, color=PAL["brown"])
    axR.set_ylim(-0.7, len(b) - 0.3)
    axR.set_xlim(min(-0.3, b.min() - 0.1), b.max() + 0.25)
    axR.set_xlabel("pp of GDP, 2070")
    axR.set_title("By country, 2070", loc="left", fontsize=BASE * 0.92, pad=6)

    header(fig, "Fewer babies save money for two decades, then cost more",
           [("Extra public spending on pensions, health care, long-term care and education if\n", False),
            ("fertility runs 20% below", True), (" the baseline", False), (" (dashed: a third less non-EU immigration).\n", False),
            (f"By 2070 the EU-27 pays {gap[2070]:.2f}% of GDP more a year.", True)])
    footer(fig, caption("European Commission, 2024 Ageing Report (statistical annex)",
                        notes=["sensitivity scenarios vs baseline", "% of GDP (a smaller economy raises it too)"]))
    save(fig, "fig02_fiscal_cost_lower_fertility.png")


# ---------------------------------------------------------------- Figure 3
ISO_NAMES = {"KOR": "South Korea", "JPN": "Japan", "ITA": "Italy", "DEU": "Germany", "GBR": "United Kingdom",
             "USA": "United States", "OECD": "OECD-38"}


def fig03():
    """C3 - working-age kin per elderly kin at age 50, by birth cohort (DemoKin-style kinship model)."""
    d = grab("c03_kin_burden_age50.csv")
    fig = plt.figure(figsize=(9, 6.4))
    axL = fig.add_axes([0.075, 0.13, 0.52, 0.60])
    axR = fig.add_axes([0.72, 0.13, 0.265, 0.60])
    cohorts = sorted(d.cohort.unique())

    # ---- left: ratio by cohort, six countries
    style_ax(axL, grid="x")
    w = d.pivot_table(index="cohort", columns="iso3", values="carers_per_elderly")
    col = {"KOR": PAL["terracotta"], "JPN": CAT["slate"]}
    for iso in ["USA", "GBR", "DEU", "ITA", "JPN", "KOR"]:
        c = col.get(iso, CAT["warm_grey"]); lw = 2.0 if iso == "KOR" else (1.5 if iso == "JPN" else 1.1)
        axL.plot(w.index, w[iso], color=c, lw=lw, marker="o", ms=4.5 if iso in col else 3.5,
                 mec=PAL["cream"], mew=0.8, zorder=4 if iso in col else 3)
    # direct labels at the right end, nudged apart
    ends = w.loc[cohorts[-1]].sort_values()
    ypos, last = {}, -1e9
    for iso, v in ends.items():
        y = max(v, last + 0.30); ypos[iso] = y; last = y
    shift = np.mean([ypos[i] - ends[i] for i in ypos])          # centre the label stack on the data
    ypos = {i: y - shift for i, y in ypos.items()}
    for iso, y in ypos.items():
        axL.text(cohorts[-1] + 1.5, y, f"{ISO_NAMES[iso]} {ends[iso]:.1f}", va="center", fontsize=BASE * 0.75,
                 color=PAL["charcoal"] if iso in col else PAL["brown"], fontweight="bold" if iso == "KOR" else "normal")
    axL.annotate(f"{w.loc[cohorts[0], 'KOR']:.1f}", (cohorts[0], w.loc[cohorts[0], "KOR"]), xytext=(-8, 0),
                 textcoords="offset points", ha="right", va="center", fontsize=BASE * 0.75, color=PAL["brown"])
    axL.set_xticks(cohorts)
    axL.set_xticklabels([f"Born {c}\n(age 50 in {c + 50})" for c in cohorts], fontsize=BASE * 0.72)
    axL.set_xlim(cohorts[0] - 6, cohorts[-1] + 18)
    axL.set_ylim(0, 9)
    axL.set_ylabel("Relatives aged 20–64 per relative 65+")
    axL.set_title("Working-age relatives per elderly relative, at age 50", loc="left", fontsize=BASE * 0.92, pad=6)

    # ---- right: Korea's numerator and denominator
    style_ax(axR, grid="x")
    k = d[d.iso3 == "KOR"].set_index("cohort")
    axR.plot(k.index, k.carers_2064, color=CAT["slate"], lw=1.6, marker="o", ms=4, mec=PAL["cream"], mew=0.8)
    axR.plot(k.index, k.elderly_kin_65p, color=PAL["terracotta"], lw=1.6, marker="o", ms=4, mec=PAL["cream"], mew=0.8)
    for c in (cohorts[0], cohorts[-1]):
        for v, dy in ((k.carers_2064[c], 7), (k.elderly_kin_65p[c], -11 if c == cohorts[0] else 7)):
            axR.annotate(f"{v:.1f}", (c, v), xytext=(0, dy), textcoords="offset points", ha="center",
                         va="bottom" if dy > 0 else "top", fontsize=BASE * 0.72, color=PAL["brown"])
    axR.text(1972, k.carers_2064[1970] + 2.3, "You + siblings +\ncousins aged 20–64", fontsize=BASE * 0.72,
             color=PAL["brown"], linespacing=1.25)
    axR.text(1968, k.elderly_kin_65p[1970] - 2.6, "Parents, aunts +\nuncles aged 65+", fontsize=BASE * 0.72,
             color=PAL["brown"], va="top", linespacing=1.25)
    axR.set_xticks(cohorts); axR.set_xticklabels([str(c) for c in cohorts], fontsize=BASE * 0.72)
    axR.set_xlim(cohorts[0] - 8, cohorts[-1] + 8)
    axR.set_ylim(0, 30)
    axR.set_ylabel("Expected number of living relatives")
    axR.set_title("South Korea: the two sides", loc="left", fontsize=BASE * 0.92, pad=6)

    kr = w["KOR"]
    header(fig, f"At 50, a Korean born in 1950 had {kr[1950]:.0f} working-age relatives per\nelderly one. A Korean born in 2010 will have {kr[2010]:.1f}",
           [("Expected living kin at age 50: ", False), ("you, your siblings and cousins aged 20–64", True), (" (the people who\nmight share the care) per ", False),
            ("parent, aunt or uncle aged 65+", True), (". Later cohorts use UN projections.", False)])
    footer(fig, caption("UN World Population Prospects 2024", notes=[
        "matrix kinship model (Caswell 2019), women's rates for both sexes",
        "the person aged 50 counts as a carer"]))
    save(fig, "fig03_kin_care_ratio.png")


# ---------------------------------------------------------------- Figure 4
def fig04():
    """C4 - share of people aged 65+ living alone: Japan, Korea, EU-27 (with member spread); Japan by sex."""
    d = grab("c04_living_alone_65plus.csv")
    band = grab("c04_eu_member_spread.csv")
    fig = plt.figure(figsize=(9, 6.4))
    axL = fig.add_axes([0.075, 0.12, 0.555, 0.64])
    axR = fig.add_axes([0.725, 0.12, 0.26, 0.64])

    style_ax(axL, grid="x")
    axL.fill_between(band.year, band.p10, band.p90, color=CAT["sand"], alpha=0.35, lw=0)
    axL.plot(band.year, band.eu27_aggregate, color=CAT["warm_grey"], lw=1.6)
    jp = d[(d.geo == "JP") & (d.sex == "total")].sort_values("year")
    obs, proj = jp[~jp.projected], jp[jp.projected]
    axL.plot(obs.year, obs.pct_65plus_alone, color=CAT["slate"], lw=1.7, marker="o", ms=3.2, mec=PAL["cream"], mew=0.6)
    axL.plot(pd.concat([obs.tail(1), proj]).year, pd.concat([obs.tail(1), proj]).pct_65plus_alone,
             color=CAT["slate"], lw=1.5, ls=(0, (4, 2)))
    kr = d[(d.geo == "KR") & (d.sex == "total")].sort_values("year")
    brk = kr[kr.series_break].year.min()
    for seg in (kr[kr.year < brk], kr[kr.year >= brk]):
        axL.plot(seg.year, seg.pct_65plus_alone, color=PAL["terracotta"], lw=2.0)
    axL.text((brk + kr[kr.year < brk].year.max()) / 2, 20.0, "census\nbreak", ha="center", va="center",
             fontsize=BASE * 0.62, color=PAL["grey"], linespacing=1.1)
    # direct labels
    lab = lambda x, y, t, **k: axL.text(x, y, t, fontsize=BASE * 0.75, color=PAL["brown"], **k)
    j = jp.set_index("year").pct_65plus_alone; k = kr.set_index("year").pct_65plus_alone
    e = band.set_index("year")
    lab(1979.5, j[1980] + 2.6, f"Japan {j[1980]:.0f}%")
    lab(2020.5, j[2020] - 1.0, f"{j[2020]:.0f}%", ha="left", va="top")
    lab(2050, j[2050] + 1.3, f"{j[2050]:.0f}% (projected)", ha="right")
    lab(1999.5, k[2000] + 1.4, f"South Korea {k[2000]:.0f}%", ha="right")
    lab(2025, k[2025] + 1.0, f"{k[2025]:.0f}%", ha="center", va="bottom")
    lab(2026, e.eu27_aggregate.iloc[-1], f"EU-27 {e.eu27_aggregate.iloc[-1]:.0f}%", va="center")
    lab(2026, e.p90.iloc[-1] + 0.5, "middle 80%\nof members", va="center", linespacing=1.2)
    axL.set_xlim(1978, 2052); axL.set_ylim(0, 50)
    axL.set_xticks(range(1980, 2051, 10))
    axL.set_ylabel("% of people aged 65+ living alone")
    axL.set_title("Share of over-65s living alone", loc="left", fontsize=BASE * 0.92, pad=6)

    style_ax(axR, grid="x")
    for sex, c in (("women", CAT["sage"]), ("men", CAT["slate"])):
        g = d[(d.geo == "JP") & (d.sex == sex)].sort_values("year")
        o, p_ = g[~g.projected], g[g.projected]
        axR.plot(o.year, o.pct_65plus_alone, color=c, lw=1.6)
        axR.plot(pd.concat([o.tail(1), p_]).year, pd.concat([o.tail(1), p_]).pct_65plus_alone, color=c, lw=1.4, ls=(0, (4, 2)))
        v = g.set_index("year").pct_65plus_alone
        axR.text(1981, v[1980] + (-1.3 if sex == "women" else -1.5), f"{v[1980]:.0f}%", va="top",
                 fontsize=BASE * 0.72, color=PAL["brown"])
        axR.text(2049, v[2050] + (1.2 if sex == "women" else -3.6), f"{sex.capitalize()} {v[2050]:.0f}%", ha="right",
                 va="bottom" if sex == "women" else "top", fontsize=BASE * 0.72, color=PAL["brown"])
    axR.set_xlim(1978, 2052); axR.set_ylim(0, 50); axR.set_xticks([1980, 2015, 2050])
    axR.set_title("Japan, by sex", loc="left", fontsize=BASE * 0.92, pad=6)

    header(fig, "Living alone in old age: Europe is flat, East Asia is catching up",
           [("Japan's share has more than doubled since 1980 and keeps rising; ", False), ("Korea's", True),
            (" is climbing fast.\nEurope has been flat at about ", False), ("one in three", True),
            (". Living alone is not the same as being lonely (chart 5).", False)])
    footer(fig, caption("Japan Cabinet Office Ageing White Paper 2025 (census; IPSS projection from 2025), "
                        "Statistics Korea, Eurostat ilc_lvps30",
                        notes=["dashed = projected; Japan's 2020-25 step partly reflects the switch from census to projection base"]))
    save(fig, "fig04_living_alone.png")


# ---------------------------------------------------------------- Figure 5
FAM = [("Partner + children", CAT["slate"]), ("Partner, no children", CAT["sage"]),
       ("No partner, children", CAT["sand"]), ("No partner, no children", PAL["terracotta"])]


def fig05():
    """C5 - % lonely most/all of the time by age and family type (EU Loneliness Survey, EU27, model-adjusted)."""
    d = grab("c05_loneliness_age_by_family.csv")
    d = d[(d["sample"] == "EU27") & (d.outcome == "lonely_direct")]
    bands = ["16-34", "35-49", "50-64", "65+"]
    fig = plt.figure(figsize=(9, 6.4))
    ax = fig.add_axes([0.075, 0.12, 0.90, 0.66])
    style_ax(ax, grid="none")
    off = np.linspace(-0.27, 0.27, len(FAM))
    for (fam, c), dx in zip(FAM, off):
        g = d[d.quantity == fam].set_index("age_band").reindex(bands)
        x = np.arange(len(bands)) + dx
        ax.vlines(x, g.ci_lo, g.ci_hi, color=c, lw=1.4, alpha=0.9)
        filled = fam.startswith("Partner")
        ax.plot(x, g.estimate_pct, "o" if filled else "s", ms=7.5, color=c,
                mfc=c, mec=PAL["cream"], mew=1.0, ls="none", zorder=4, label=fam)
    # old-age gap callout
    gap = d[(d.quantity == "GAP no-partner: childless minus parent") & (d.age_band == "65+")].iloc[0]
    ax.annotate(f"Without a partner, childless over-65s:\n{gap.estimate_pct:+.0f} points vs parents\n"
                f"(95% CI {gap.ci_lo:+.0f} to {gap.ci_hi:+.0f}: suggestive, not solid)",
                xy=(3 + off[3] - 0.05, 16.5), xytext=(2.35, 27.0), fontsize=BASE * 0.72, color=PAL["brown"],
                linespacing=1.25, arrowprops=dict(arrowstyle="-", color=PAL["grey"], lw=0.7,
                                                  connectionstyle="arc3,rad=-0.2"))
    ax.set_xticks(range(len(bands)))
    ax.set_xticklabels([f"Aged {b.replace('-', '–')}" for b in bands], fontsize=BASE * 0.85)
    ax.set_xlim(-0.55, len(bands) - 0.45)
    ax.set_ylim(0, 32)
    ax.set_ylabel("% lonely most or all of the time")
    handles = [Line2D([], [], marker="o" if f.startswith("Partner") else "s", ls="none", ms=7, color=c, mec=PAL["cream"],
                      label=f) for f, c in FAM]
    sub_end = header(fig, "Loneliness tracks partners more than children",
                     [("Share lonely most or all of the time, EU-27, adjusted for sex, country and income. At every age,\n", False),
                      ("people without a partner", True), (" are lonelier; having children makes little difference on average.", False)])
    fig.legend(handles=handles, loc="upper left", bbox_to_anchor=(0.03, sub_end + 0.005), ncol=4, handletextpad=0.3,
               columnspacing=1.4, borderaxespad=0)
    footer(fig, caption("JRC EU Loneliness Survey 2022 (EU-27 online panel, weighted)",
                        notes=["weighted logit with age x family interaction", "bars = 95% bootstrap intervals"]))
    save(fig, "fig05_loneliness_family.png")


# ---------------------------------------------------------------- Figure 6
MAR = [("married", "Married", CAT["slate"], "o"), ("never married", "Never married", PAL["terracotta"], "s"),
       ("widowed", "Widowed", CAT["sage"], "D"), ("divorced", "Divorced", CAT["sand"], "^")]


def fig06():
    """C6 - Japan suicide rate by marital status x age x sex (2019) + Korea/Japan 65+ suicide trend."""
    a = grab("c06a_japan_suicide_by_marital_status.csv")
    w = grab("c06b_suicide_rates_65plus_who.csv")
    ages = ["20-29", "30-39", "40-49", "50-59", "60+"]
    fig = plt.figure(figsize=(9, 6.4))
    axM = fig.add_axes([0.075, 0.155, 0.25, 0.61])
    axW = fig.add_axes([0.355, 0.155, 0.25, 0.61], sharey=axM)
    axK = fig.add_axes([0.70, 0.155, 0.285, 0.61])
    for ax, sex, ttl in ((axM, "men", "Japan, men"), (axW, "women", "Japan, women")):
        style_ax(ax, grid="none")
        for key, lab, c, mk in MAR:
            g = a[(a.sex == sex) & (a.marital_status == key)].set_index("age_band").reindex(ages)
            ax.plot(range(len(ages)), g.suicides_per_100k, color=c, lw=1.4 if key != "never married" else 1.9,
                    marker=mk, ms=4.5, mec=PAL["cream"], mew=0.7, label=lab)
        ax.set_yscale("log")
        ax.set_ylim(3, 260)
        ax.set_yticks([5, 10, 20, 50, 100, 200]); ax.set_yticklabels(["5", "10", "20", "50", "100", "200"])
        ax.minorticks_off()
        ax.set_xticks(range(len(ages))); ax.set_xticklabels([x.replace("-", "–") for x in ages], fontsize=BASE * 0.7)
        ax.set_title(ttl, loc="left", fontsize=BASE * 0.92, pad=6)
        ax.set_xlabel("Age", fontsize=BASE * 0.75)
    axM.set_ylabel("Suicides per 100,000 (log scale)")
    plt.setp(axW.get_yticklabels(), visible=False)
    # Korea vs Japan 65+
    style_ax(axK, grid="x")
    t = w[(w.sex == "total") & w.iso3.isin(["KOR", "JPN"])]
    for iso, c, lw in (("JPN", CAT["slate"], 1.5), ("KOR", PAL["terracotta"], 2.0)):
        g = t[t.iso3 == iso].sort_values("year")
        axK.plot(g.year, g.rate_65plus, color=c, lw=lw)
    axK.axvspan(2011, 2012, color=PAL["taupe"], alpha=0.45, lw=0, zorder=0)
    axK.text(2012.6, 90, "paraquat\nbanned 2011–12", fontsize=BASE * 0.62, color=PAL["grey"], va="top", linespacing=1.2)
    k = t[t.iso3 == "KOR"].set_index("year"); j = t[t.iso3 == "JPN"].set_index("year")
    pk = k.rate_65plus.idxmax()
    for yr, dy, va in ((pk, 6, "bottom"), (k.index.max(), 6, "bottom")):
        axK.plot(yr, k.rate_65plus[yr], "o", ms=4, color=PAL["terracotta"], mec=PAL["cream"], mew=0.8, zorder=5)
        axK.annotate(f"{k.rate_65plus[yr]:.0f}", (yr, k.rate_65plus[yr]), xytext=(0, dy), textcoords="offset points",
                     ha="center", va=va, fontsize=BASE * 0.72, color=PAL["brown"])
    axK.text(1996, k.rate_65plus[2003] + 4, "South Korea", fontsize=BASE * 0.75, color=PAL["charcoal"], fontweight="bold")
    axK.text(2023, j.rate_65plus[2023] - 4, "Japan", fontsize=BASE * 0.75, color=PAL["brown"], ha="right", va="top")
    la = k.kr_pct_65plus_living_alone.dropna()
    axK.text(1995.5, 5, "Korean over-65s living alone:\n" + " → ".join(f"{la[y]:.0f}% ({y})" for y in (2000, 2010, 2023)),
             fontsize=BASE * 0.62, color=PAL["brown"], linespacing=1.3, va="bottom")
    axK.set_xlim(1994, 2024.5); axK.set_ylim(0, 95)
    axK.set_xticks([1995, 2005, 2015, 2023])
    axK.set_ylabel("Suicides per 100,000 aged 65+")
    axK.set_title("Aged 65+, both sexes", loc="left", fontsize=BASE * 0.92, pad=6)

    sub_end = header(fig, "Being unpartnered raises suicide risk, but the risk can fall",
                     [("Japan, 2019: ", False), ("never-married men", True), (" die by suicide at about twice the rate of married men,\n"
                      "divorced men at about six times. Korea's old-age rate ", False), ("halved after 2010", True),
                      (" while living alone kept rising.", False)])
    handles = [Line2D([], [], color=c, marker=mk, ms=5, mec=PAL["cream"], lw=1.4, label=lab) for _, lab, c, mk in MAR]
    fig.legend(handles=handles, loc="upper left", bbox_to_anchor=(0.03, sub_end + 0.005), ncol=4,
               columnspacing=1.4, handletextpad=0.4, borderaxespad=0)
    footer(fig, caption("MHLW Suicide Countermeasures White Paper 2021 (2019 deaths, 2015 census)",
                        "WHO Mortality Database (ICD-10 X60–X84, Y87.0)", "Statistics Korea",
                        notes=["crude rates", "small groups (e.g. widowed under 40) are noisy", "paraquat: Cha et al. 2016, IJE"]))
    save(fig, "fig06_suicide_family_status.png")


# ---------------------------------------------------------------- Figure 7
def fig07():
    """C7 - net migration needed (per 1,000 residents a year, 2025-2070 average) vs observed and UN-assumed."""
    d = grab("c07_summary_per_1000.csv")
    w = d.pivot_table(index="iso3", columns="target", values="required_per_1000_avg")
    base = d.groupby("iso3")[["observed_per_1000_2015_2023", "wpp_assumed_per_1000_avg"]].first()
    order = w["oadr_65_2064"].sort_values().index.tolist()          # bottom -> top
    fig = plt.figure(figsize=(9, 6.0))
    ax = fig.add_axes([0.17, 0.175, 0.80, 0.58])
    style_ax(ax, grid="x")
    y = np.arange(len(order))
    for i, iso in enumerate(order):
        vals = [base.loc[iso, "observed_per_1000_2015_2023"], base.loc[iso, "wpp_assumed_per_1000_avg"],
                w.loc[iso, "working_age_2064"], w.loc[iso, "oadr_65_2064"]]
        ax.hlines(i, min(vals), max(vals), color=PAL["taupe"], lw=1.2, zorder=1)
        ax.plot(vals[0], i, "o", ms=12.5, mfc="none", mec=CAT["warm_grey"], mew=1.6, zorder=6)
        ax.plot(vals[1], i, "D", ms=5, color=CAT["warm_grey"], zorder=3)
        ax.plot(vals[2], i, "o", ms=8.5, color=CAT["slate"], mec=PAL["cream"], mew=0.8, zorder=4)
        ax.plot(vals[3], i, "o", ms=9.5, color=PAL["terracotta"], mec=PAL["cream"], mew=0.8, zorder=5)
        if vals[3] >= max(vals):
            ax.text(vals[3] + 0.5, i, f"{vals[3]:.1f}", va="center", fontsize=BASE * 0.75, color=PAL["brown"])
        else:
            ax.text(vals[3], i + 0.27, f"{vals[3]:.1f}", ha="center", va="bottom", fontsize=BASE * 0.68, color=PAL["brown"])
        ax.text(vals[2], i + 0.27, f"{vals[2]:.1f}", ha="center", va="bottom", fontsize=BASE * 0.68, color=PAL["brown"])
    ax.set_yticks(y)
    ax.set_yticklabels([ISO_NAMES[i] for i in order], fontsize=BASE * 0.88)
    for t, iso in zip(ax.get_yticklabels(), order):
        if iso == "KOR":
            t.set_fontweight("bold"); t.set_color(PAL["charcoal"])
    ax.set_xlim(0, 21.5); ax.set_ylim(-0.6, len(order) - 0.4); ax.set_xticks([0, 5, 10, 15, 20])
    ax.set_xlabel("Net migrants per 1,000 residents per year (average 2025–2070)")
    k = d[(d.iso3 == "KOR") & (d.target == "oadr_65_2064")].iloc[0]
    sub_end = header(fig, "To hold today's age balance, Korea would need nine times the migration it gets",
                     [("Net migration needed every year to 2070 to keep ", False), ("the 65+ to 20–64 ratio", True),
                      (" or ", False), ("the 20–64 population", True), (" at 2024 levels,\ncompared with recent "
                       "migration and the UN's assumption.", False)])
    handles = [Line2D([], [], marker="o", ls="none", ms=9, mfc="none", mec=CAT["warm_grey"], mew=1.6, label="Observed 2015–2023"),
               Line2D([], [], marker="D", ls="none", ms=5, color=CAT["warm_grey"], label="UN assumption"),
               Line2D([], [], marker="o", ls="none", ms=8, color=CAT["slate"], label="Needed: hold 20–64 population"),
               Line2D([], [], marker="o", ls="none", ms=9, color=PAL["terracotta"], label="Needed: hold 65+/20–64 ratio")]
    fig.legend(handles=handles, loc="upper left", bbox_to_anchor=(0.03, sub_end + 0.005), ncol=4, handletextpad=0.3,
               columnspacing=1.2, borderaxespad=0)
    footer(fig, caption("UN World Population Prospects 2024 (Medium and Zero-migration variants)",
                        notes=["own cohort-component projection anchored on the UN Medium variant",
                               "a thought experiment, not a forecast"]))
    save(fig, "fig07_replacement_migration.png")


# ---------------------------------------------------------------- Figure 8
def fig08():
    """C8 - Denmark: age-standardised employment rate 25-64 by origin, 2024 by sex + change 2008->2024."""
    d = grab("c08_employment_by_origin_2008_2024.csv")
    d = d[d.origin != "Total"]
    t = d[d.sex == "Total"].set_index("origin")
    # three labelled sections (Whiz, 11 Oct): one mixed list made the aggregates read as headings for the countries
    # below them (India looked 'Western'). Countries are grouped by Statistics Denmark's Western/non-Western definition.
    agg_order = ["Denmark, total", "Immigrants from western countries", "Descendants from western countries",
                 "Immigrants from non-western countries", "Descendants from non-western countries"]
    sections = [("Overall", agg_order)]
    for g, head in [("western", "Western countries"), ("non-western", "Non-Western countries")]:
        sections.append((head, t[t.dst_group == g].rate_std_2024.sort_values(ascending=False).index.tolist()))
    assert sorted(sum((o for _, o in sections), [])) == sorted(t.index)
    slots = []                                          # top-down: ("head", text) or ("row", origin)
    for head, origins in sections:
        slots.append(("head", head)); slots += [("row", o) for o in origins]
    n = len(slots)
    y = {}; heads = []
    for i, (kind, v) in enumerate(slots):
        (y.__setitem__(v, n - 1 - i) if kind == "row" else heads.append((v, n - 1 - i)))
    order = [v for kind, v in slots if kind == "row"]
    H = 11.6
    fig = plt.figure(figsize=(9, H))
    top, bot = 1 - 1.75 / H, 0.85 / H
    axL = fig.add_axes([0.30, bot, 0.32, top - bot])
    axR = fig.add_axes([0.66, bot, 0.32, top - bot], sharey=axL)
    dk = t.loc["Denmark, total", "rate_std_2024"]
    for ax in (axL, axR):
        style_ax(ax, grid="x")
        ax.axvline(dk, color=PAL["terracotta"], lw=0.9, ls=(0, (3, 2)), zorder=1)
        ax.set_xlim(25, 92)
        ax.set_xticks([30, 40, 50, 60, 70, 80, 90])
        for _, hy in heads[1:]:
            ax.axhline(hy + 0.5, color=PAL["taupe"], lw=0.5, alpha=0.6, zorder=0)
    m = d[d.sex == "Men"].set_index("origin"); w = d[d.sex == "Women"].set_index("origin")
    for o in order:
        yy = y[o]
        if o in m.index and o in w.index:
            axL.hlines(yy, min(m.rate_std_2024[o], w.rate_std_2024[o]), max(m.rate_std_2024[o], w.rate_std_2024[o]),
                       color=PAL["taupe"], lw=1.1, zorder=2)
            axL.plot(m.rate_std_2024[o], yy, "o", ms=5, color=CAT["slate"], mec=PAL["cream"], mew=0.6, zorder=3)
            axL.plot(w.rate_std_2024[o], yy, "o", ms=5, color=CAT["sage"], mec=PAL["cream"], mew=0.6, zorder=3)
        a, b = t.rate_std_2008[o], t.rate_std_2024[o]
        if pd.notna(a):
            axR.annotate("", xy=(b, yy), xytext=(a, yy),
                         arrowprops=dict(arrowstyle="-|>,head_length=0.35,head_width=0.18", color=CAT["slate"], lw=1.1,
                                         shrinkA=0, shrinkB=0), zorder=3)
            hollow = bool(t.composition_changed[o])
            axR.plot(a, yy, "o", ms=4.2, mfc=PAL["cream"] if hollow else CAT["slate"], mec=CAT["slate"], mew=0.9, zorder=4)
        else:
            axR.plot(b, yy, "o", ms=4, color=CAT["slate"], zorder=4)
    relabel = {"Denmark, total": "Danish origin", "USA": "United States", "Yugoslavia": "Former Yugoslavia",
               "Immigrants from western countries": "Immigrants, Western", "Descendants from western countries": "Descendants, Western",
               "Immigrants from non-western countries": "Immigrants, non-Western",
               "Descendants from non-western countries": "Descendants, non-Western"}
    labels = []
    for o in order:
        lab = relabel.get(o, o)
        if t.small_group[o]:
            lab += " †"
        labels.append(lab)
    axL.set_yticks([y[o] for o in order]); axL.set_yticklabels(labels, fontsize=BASE * 0.72)
    for tl, o in zip(axL.get_yticklabels(), order):
        if t.is_aggregate[o]:
            tl.set_fontstyle("italic"); tl.set_color(PAL["charcoal"])
        if o == "Denmark, total":
            tl.set_fontweight("bold"); tl.set_fontstyle("normal")
    from matplotlib.transforms import blended_transform_factory
    htr = blended_transform_factory(fig.transFigure, axL.transData)
    for txt, hy in heads:
        axL.text(0.035, hy - 0.1, txt.upper(), transform=htr, fontsize=BASE * 0.66, color=PAL["brown"],
                 fontweight="bold", va="center", ha="left")
    plt.setp(axR.get_yticklabels(), visible=False)
    axL.set_ylim(-0.8, n - 0.4)
    axL.set_title("2024, by sex", loc="left", fontsize=BASE * 0.92, pad=6)
    axR.set_title("Change 2008 → 2024, men and women", loc="left", fontsize=BASE * 0.92, pad=6)
    for ax in (axL, axR):
        ax.set_xlabel("% employed, aged 25–64 (age-standardised)", fontsize=BASE * 0.72)
    axL.text(dk - 0.8, -0.5, f"Danish origin\n{dk:.0f}%", fontsize=BASE * 0.66, color=PAL["brown"], va="bottom", ha="right",
             linespacing=1.15)
    lb = t.loc["Lebanon"]
    sub_end = header(fig, "In Denmark, employment gaps by origin are large but narrowing",
                     [("Share of 25–64-year-olds in work, adjusted to the Danish-origin age mix. Immigrants and descendants\n"
                       "from ", False), ("Lebanon, Somalia, Syria and Iraq", True), (" are 30–35 points below Danish origin; "
                       "every group\nwith 2008 data has gained since.", False)], top=0.985)
    handles = [Line2D([], [], marker="o", ls="none", ms=6, color=CAT["slate"], label="Men"),
               Line2D([], [], marker="o", ls="none", ms=6, color=CAT["sage"], label="Women"),
               Line2D([], [], marker="o", ls="none", ms=5, mfc=PAL["cream"], mec=CAT["slate"], label="2008 start, group at least doubled since"),
               Line2D([], [], color=PAL["terracotta"], ls=(0, (3, 2)), lw=1, label="Danish origin")]
    fig.legend(handles=handles, loc="upper left", bbox_to_anchor=(0.03, sub_end + 0.003), ncol=4, handletextpad=0.3,
               columnspacing=1.2, borderaxespad=0, fontsize=BASE * 0.75)
    footer(fig, caption("Statistics Denmark RAS208 (end-November)",
                        notes=["country rows = immigrants + descendants; Western/non-Western as defined by Statistics Denmark",
                               "† some 5-year age band under 100 people",
                               "one country, not representative of the EU"]), bottom=0.008)
    save(fig, "fig08_employment_by_origin_dk.png")


# ---------------------------------------------------------------- Figure 9
def fig09():
    """C9 - Denmark: persons found guilty, all offences, raw vs age-sex adjusted index by origin (2022-24 pooled)."""
    d = grab("c09_crime_index_by_origin.csv")
    d = d[(d.measure == "all_offences") & (d.scope == "men and women") & ~d.small_group & ~d.group.isin(["Denmark", "Total"])]
    t = d.set_index("group")
    # three labelled sections (Whiz, 12 Oct; same fix as fig08): one mixed list made the aggregates read as headings
    # for the countries below them. Countries grouped by Statistics Denmark's Western/non-Western definition.
    agg_order = ["Persons of Danish origin", "Immigrants from western countries", "Descendants from western countries",
                 "Immigrants from non-western countries", "Descendants from non-western countries"]
    sections = [("Overall", agg_order)]
    for g, head in [("western", "Western countries"), ("non-western", "Non-Western countries")]:
        sections.append((head, t[t.dst_group == g].adjusted_index.sort_values(ascending=False).index.tolist()))
    assert sorted(sum((o for _, o in sections), [])) == sorted(t.index), set(t.index) ^ set(sum((o for _, o in sections), []))
    slots = []                                          # top-down: ("head", text) or ("row", group)
    for head, groups in sections:
        slots.append(("head", head)); slots += [("row", g) for g in groups]
    n = len(slots)
    y = {}; heads = []
    for i, (kind, v) in enumerate(slots):
        (y.__setitem__(v, n - 1 - i) if kind == "row" else heads.append((v, n - 1 - i)))
    order = [v for kind, v in slots if kind == "row"]
    H = 11.6
    fig = plt.figure(figsize=(9, H))
    top, bot = 1 - 1.75 / H, 1.0 / H
    ax = fig.add_axes([0.33, bot, 0.58, top - bot])
    style_ax(ax, grid="x")
    ax.axvline(100, color=PAL["terracotta"], lw=0.9, ls=(0, (3, 2)), zorder=1)
    for _, hy in heads[1:]:
        ax.axhline(hy + 0.5, color=PAL["taupe"], lw=0.5, alpha=0.6, zorder=0)
    for g in order:
        r, i = t.loc[g], y[g]
        ax.hlines(i, min(r.raw_index, r.adjusted_index), max(r.raw_index, r.adjusted_index), color=PAL["taupe"], lw=1.2, zorder=2)
        ax.hlines(i, r.adj_ci_lo, r.adj_ci_hi, color=CAT["slate"], lw=2.6, alpha=0.35, zorder=3)
        ax.plot(r.raw_index, i, "o", ms=6, mfc="none", mec=CAT["warm_grey"], mew=1.3, zorder=4)
        ax.plot(r.adjusted_index, i, "o", ms=5.5, color=CAT["slate"], mec=PAL["cream"], mew=0.6, zorder=5)
        ax.text(1.005, i, f"{r.adjusted_index:.0f}", transform=ax.get_yaxis_transform(), va="center", ha="left",
                fontsize=BASE * 0.68, color=PAL["brown"])
    relabel = {"Persons of Danish origin": "Danish origin", "USA": "United States", "Yugoslavia": "Former Yugoslavia",
               "Republic of North Macedonia": "North Macedonia",
               "Immigrants from western countries": "Immigrants, Western", "Descendants from western countries": "Descendants, Western",
               "Immigrants from non-western countries": "Immigrants, non-Western",
               "Descendants from non-western countries": "Descendants, non-Western"}
    ax.set_yticks([y[g] for g in order]); ax.set_yticklabels([relabel.get(g, g) for g in order], fontsize=BASE * 0.72)
    for tl, g in zip(ax.get_yticklabels(), order):
        if t.is_aggregate[g]:
            tl.set_fontstyle("italic"); tl.set_color(PAL["charcoal"])
        if g == "Persons of Danish origin":
            tl.set_fontweight("bold"); tl.set_fontstyle("normal")
    from matplotlib.transforms import blended_transform_factory
    htr = blended_transform_factory(fig.transFigure, ax.transData)
    for txt, hy in heads:
        ax.text(0.035, hy - 0.1, txt.upper(), transform=htr, fontsize=BASE * 0.66, color=PAL["brown"],
                fontweight="bold", va="center", ha="left")
    ax.set_ylim(-0.8, n - 0.4)
    ax.set_xlim(0, 420); ax.set_xticks(range(0, 401, 50))
    ax.set_xlabel("Index, population average = 100", fontsize=BASE * 0.75)
    ax.text(103, -0.5, "population\naverage", fontsize=BASE * 0.66, color=PAL["brown"], va="bottom", linespacing=1.15)
    ax.text(1.005, n - 0.5, "adjusted", transform=ax.get_yaxis_transform(), fontsize=BASE * 0.62, color=PAL["grey"], va="center")
    sub_end = header(fig, "In Denmark, conviction rates differ widely by origin, even after adjusting for age",
                     [("People found guilty of an offence, 2022–24, relative to the population average (100). ", False),
                      ("Adjusting for age\nand sex", True), (" narrows most gaps, but descendants of non-Western immigrants and several origin\n"
                       "groups remain at two to three and a half times the average.", False)], top=0.985)
    handles = [Line2D([], [], marker="o", ls="none", ms=6, mfc="none", mec=CAT["warm_grey"], mew=1.3, label="Raw"),
               Line2D([], [], marker="o", ls="none", ms=6, color=CAT["slate"], label="Adjusted for age and sex"),
               Line2D([], [], color=CAT["slate"], lw=3, alpha=0.35, label="95% interval"),
               Line2D([], [], color=PAL["terracotta"], ls=(0, (3, 2)), lw=1, label="Population average")]
    fig.legend(handles=handles, loc="upper left", bbox_to_anchor=(0.03, sub_end + 0.003), ncol=4, handletextpad=0.4,
               columnspacing=1.4, borderaxespad=0, fontsize=BASE * 0.75)
    footer(fig, caption("Statistics Denmark STRAFNA3/STRAFNA9, FOLK1C/FOLK1E",
                        notes=["all offences incl. traffic, as in Statistics Denmark's own index (validated vs its 2022 table)",
                               "country rows = immigrants + descendants; Western/non-Western as defined by Statistics Denmark",
                               "groups under ~5,000 people omitted",
                               "does not adjust for income or education", "one country, not representative of the EU"]),
           bottom=0.008)
    save(fig, "fig09_crime_index_dk.png")


# ---------------------------------------------------------------- Figure 10
def fig10():
    """C10 - immigration salience (Eurobarometer) and far-right vote share, six countries."""
    eb = grab("c10a_immigration_salience_eurobarometer.csv")
    fr = grab("c10b_far_right_vote_share.csv")
    eb["t"] = pd.to_datetime(eb.fieldwork_start); eb["x"] = eb.t.dt.year + (eb.t.dt.dayofyear - 1) / 365.25
    fr["t"] = pd.to_datetime(fr.election_date); fr["x"] = fr.t.dt.year + (fr.t.dt.dayofyear - 1) / 365.25
    eu = eb[eb.is_eu_aggregate].sort_values("x")
    order = [("DEU", "Germany"), ("AUT", "Austria"), ("NLD", "Netherlands"), ("SWE", "Sweden"), ("DNK", "Denmark"), ("ITA", "Italy")]
    fig, axes = plt.subplots(2, 3, figsize=(9, 6.6), sharex=True, sharey=True)
    fig.subplots_adjust(left=0.07, right=0.985, top=0.77, bottom=0.115, wspace=0.10, hspace=0.30)
    for ax, (iso, name) in zip(axes.flat, order):
        style_ax(ax, grid="x")
        ax.plot(eu.x, eu.immigration_pct, color=PAL["taupe"], lw=1.0, zorder=1)
        s_ = eb[eb.iso3 == iso].sort_values("x")
        ax.plot(s_.x, s_.immigration_pct, color=CAT["slate"], lw=1.5, zorder=3)
        v = fr[fr.iso3 == iso].sort_values("x")
        ax.step(np.r_[v.x.values, 2026.9], np.r_[v.far_right_vote_share.values, v.far_right_vote_share.values[-1]],
                where="post", color=PAL["terracotta"], lw=1.0, alpha=0.55, zorder=2)
        ax.plot(v.x, v.far_right_vote_share, "o", ms=4.5, color=PAL["terracotta"], mec=PAL["cream"], mew=0.7, zorder=4)
        last = v.iloc[-1]
        ax.annotate(f"{last.far_right_vote_share:.0f}%", (last.x, last.far_right_vote_share), xytext=(0, 7),
                    textcoords="offset points", ha="center", va="bottom", fontsize=BASE * 0.68, color=PAL["brown"],
                    bbox=dict(boxstyle="square,pad=0.08", fc=PAL["cream"], ec="none"), zorder=6)
        pk = s_.loc[s_.immigration_pct.idxmax()]
        ax.annotate(f"{pk.immigration_pct:.0f}%", (pk.x, pk.immigration_pct), xytext=(4, 0), textcoords="offset points",
                    ha="left", va="center", fontsize=BASE * 0.68, color=PAL["brown"])
        ax.set_title(name, loc="left", fontsize=BASE * 0.92, pad=4)
        ax.set_xlim(2002, 2027.2); ax.set_ylim(0, 82)
        ax.set_xticks([2005, 2015, 2025]); ax.set_yticks([0, 20, 40, 60, 80])
    for a in axes[:, 0]:
        a.set_ylabel("%")
    sub_end = header(fig, "Immigration worry spiked in 2015 and faded; the far-right vote mostly stayed",
                     [("Share naming ", False), ("immigration", True), (" as one of their country's two most important issues, and ", False),
                      ("far-right vote share", True), ("\nat each national election. Salience has fallen back; the vote has mostly not.", False)])
    handles = [Line2D([], [], color=CAT["slate"], lw=1.5, label="Immigration a top-two issue (Eurobarometer)"),
               Line2D([], [], color=PAL["terracotta"], marker="o", ms=4.5, lw=1.0, label="Far-right vote share (elections)"),
               Line2D([], [], color=PAL["taupe"], lw=1.0, label="EU average, salience")]
    fig.legend(handles=handles, loc="upper left", bbox_to_anchor=(0.03, sub_end + 0.005), ncol=3, handletextpad=0.4,
               columnspacing=1.4, borderaxespad=0)
    footer(fig, caption("Standard Eurobarometer 62–105 (QA3a)", "PopuList 4.0 far-right classification",
                        "ParlGov to mid-2023, official results after",
                        notes=["Netherlands includes BBB, as PopuList does", "descriptive, not causal"]))
    save(fig, "fig10_immigration_salience_far_right.png")


STEPS = {"fig01": fig01, "fig02": fig02, "fig03": fig03, "fig04": fig04, "fig05": fig05, "fig06": fig06, "fig07": fig07, "fig08": fig08, "fig09": fig09, "fig10": fig10}

if __name__ == "__main__":
    for k in sys.argv[1:] or list(STEPS):
        print(f"[{k}] {STEPS[k].__doc__.strip().splitlines()[0]}")
        STEPS[k]()
