# Denmark — names given to newborns, 1985–2025

**File:** `dk_newborn_names_1985_2025.csv`
**Columns:** `year, sex (F/M), rank, name, count, per_1000`
**Rows:** 3,700. 41 years x 2 sexes = 82 year-sex cells, ranks contiguous from 1 in every cell.

## Source and how it was collected
Danmarks Statistik, "Navne til nyfødte":
https://www.dst.dk/da/Statistik/emner/borgere/navne/navne-til-nyfoedte

**Danish name statistics are NOT in Statistikbanken.** DST's own documentation says so
outright ("Navnestatistikkerne findes ikke i Statistikbanken"), so there is no API and no
bulk file. The only open route is the topic page's "Top 50 names for newborns" block,
which has a year selector covering 1985–2025.

Collected 2 Sept 2026 by driving that page's year selector (an ASP.NET WebForms postback;
the results table updates in place) and parsing the two rendered tables per year — girls
first, boys second — then transcribing to CSV.

## Verification
The extracted data and the written file were checksummed independently and agree exactly:

| check | value |
|---|---|
| rows | 3,700 |
| sum of `count` | 1,449,891 |
| sum of `per_1000` | 46,489 |
| sum of name lengths | 21,083 |

Ranks are contiguous 1..n in all 82 cells. 2025 spot-checked against the DST page.

## Caveats — read before using

1. **Depth changes in 1993.** 1985–1992 publish a **top 25** per sex; 1993–2025 publish a
   **top 50**. Cell sizes: 16 cells of 25 rows, 66 of 50. A top-10 share is unaffected;
   any tail-sensitive measure (entropy, Hill numbers) is NOT comparable across 1992/93
   without truncating everything to a common depth of 25.

2. **A source break at 1999/2000.** The page's own year codes switch from `YYYY_1`
   (1985–1999) to `YYYY_2` (2000–2025), which implies a change of series or method.
   DST does not explain it on the page. Check before treating 1985–2025 as one series.

3. **Do not trust `per_1000` as a denominator.** It is rounded to whole units, and the
   implied birth totals are internally inconsistent — 2005 in particular implies a far
   smaller denominator than 2004, which the actual Danish birth counts do not support.
   Take exact births by sex from the Human Mortality Database and use `per_1000` only as
   a sanity check.

4. **Coverage starts at 1985** because the underlying source is CPR. Denmark cannot
   contribute to the long (pre-1950) series. Its job in this project is the **2006
   naming-law liberalisation** test.

5. **Beware the English version of this page.** Its machine translation renders Danish
   names as English words — Aksel becomes "Shaft", Lauge "Guild", Ebbe "Ebb", Liv "Life",
   Gry "Gray", Lærke "Lark", Vilde "Wild". The names in this CSV are the Danish originals
   taken from the Danish page.

## First look — approximate top-10 share (%, from the rounded per-1000 column)

Girls fall 26.7 (1985) → 14.0 (2021), boys 34.3 → 14.8, with the fall concentrated in
roughly 1993–2016 and a clear **plateau from about 2016 onward**. Recompute properly with
exact birth denominators before using any of these numbers.
