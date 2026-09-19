# -*- coding: utf-8 -*-
"""
Cover art for "What Baby Names Know About the Birth Rate" — the archives.

House vocabulary, sampled from the existing covers in the archive:
  background   vertical gradient #e5ddcc -> #aa9c84, radial spot upper-left,
               floor wash, soft vignette
  figures      capsule body (rounded rect, rx = w/2) + circle head
  proportions  head r = 0.1385h, body w = 0.2974h, body top = head_cy + 0.62r
  tones        dark #332b23, mid #4a4136, light #5a5145, ghost #8f8471
  accent       terracotta #8a4034, exactly one loaded figure
  shadows      flat ellipse at the baseline, very low opacity
  composition  figures in the lower 45%; upper half left clear for the title

Composition: a dense crowd of children sharing one name on the left, a fading
middle, three widely spaced children with three different names on the right.
Parents stand behind in both groups — smaller and higher on the canvas, so
they read as further back and never as the subject.

Two outputs from one spec:
  .svg  live Georgia text, for editing on a machine that has the font
  .png  1600x900, upload-ready. The background is composited in numpy rather
        than rasterised from the SVG, because cairosvg dithers wide radial
        gradients into visible concentric arcs.
"""
import numpy as np
from PIL import Image
import cairosvg

W, H = 1600, 900

BG_TOP, BG_MID, BG_BOT = "#e5ddcc", "#ded4c0", "#aa9c84"
DARK, MID, LIGHT, GHOST = "#332b23", "#4a4136", "#5a5145", "#8f8471"
PARENT = "#8f8471"   # near the ground tone: present, never the focal point
TERRA = "#8a4034"

# Scaled so the names still read in a Substack feed, where the cover is shown
# around 400px wide. Anything smaller than this and the labels turn to mush.
BACK_BASE,  BACK_H  = 700, 176    # parents: smaller and higher = further back
FRONT_BASE, FRONT_H = 760, 132    # children, front rank
REAR_BASE,  REAR_H  = 738, 122    # children, second rank of the crowd
LABEL_Y = 802
LABEL_SIZE = 27

# ---------------------------------------------------------------------------
# gradient definitions — one source of truth, used for both the SVG defs and
# the numpy render
# ---------------------------------------------------------------------------
SKY   = [(0.0, BG_TOP, 1.0), (0.55, BG_MID, 1.0), (1.0, BG_BOT, 1.0)]
SPOT  = [(0.0, "#f3ede0", 0.42), (0.30, "#f3ede0", 0.30), (0.60, "#f3ede0", 0.14),
         (0.82, "#f3ede0", 0.05), (1.0, "#f3ede0", 0.0)]
FLOOR = [(0.0, "#8a7250", 0.0), (1.0, "#6f5a3c", 0.34)]
VIG   = [(0.52, "#3a2c17", 0.0), (0.74, "#3a2c17", 0.05),
         (0.89, "#3a2c17", 0.12), (1.0, "#3a2c17", 0.19)]
SPOT_C, SPOT_R = (0.36, 0.22), 0.72     # centre (fraction of W,H), radius
VIG_C,  VIG_R  = (0.50, 0.44), 0.76
FLOOR_TOP = 600

def rgb(h):
    h = h.lstrip("#")
    return tuple(int(h[i:i+2], 16) for i in (0, 2, 4))

# ---------------------------------------------------------------------------
# the figures — emitted as SVG, which is also what gets rasterised
# ---------------------------------------------------------------------------
body = []
def add(s): body.append(s)

def figure(x, baseline, h, fill, opacity=1.0, shadow=0.10):
    head_r = h * 0.1385
    head_cy = baseline - h + head_r
    body_w = h * 0.2974
    body_top = head_cy + 0.62 * head_r
    if shadow:
        add(f'<ellipse cx="{x:.1f}" cy="{baseline:.1f}" rx="{body_w*0.92:.1f}" '
            f'ry="{h*0.055:.1f}" fill="#3a2c17" fill-opacity="{shadow}"/>')
    add(f'<rect x="{x-body_w/2:.1f}" y="{body_top:.1f}" width="{body_w:.1f}" '
        f'height="{baseline-body_top:.1f}" rx="{body_w/2:.1f}" '
        f'fill="{fill}" fill-opacity="{opacity}"/>')
    add(f'<circle cx="{x:.1f}" cy="{head_cy:.1f}" r="{head_r:.1f}" '
        f'fill="{fill}" fill-opacity="{opacity}"/>')

def label(x, text, fill=DARK, opacity=0.82):
    add(f'<text x="{x:.1f}" y="{LABEL_Y}" font-family="Georgia, Gelasio, serif" '
        f'font-size="{LABEL_SIZE}" fill="{fill}" fill-opacity="{opacity}" '
        f'text-anchor="middle">{text}</text>')

# parents: two pairs behind the crowd, a pair and a single behind the three
# Each pair overlaps a child horizontally, so occlusion does the work of
# putting them behind. A pair sitting in a clear gap reads as its own subject.
for x in (262, 300, 424, 462, 1062, 1100, 1396):
    figure(x, BACK_BASE, BACK_H, PARENT, opacity=0.38, shadow=0.035)

# the crowd — two ranks so it reads as a mass rather than a row. Uniform tone
# on purpose: the sameness is the subject. Only the front rank is labelled,
# because nine labels at this spacing would collide.
rear  = [191 + 82*i for i in range(4)]
front = [150 + 82*i for i in range(5)]
for x in rear:
    figure(x, REAR_BASE, REAR_H, DARK, opacity=0.82, shadow=0.06)
for x in front:
    figure(x, FRONT_BASE, FRONT_H, MID)
for x in front:
    label(x, "Mary")

# the fade between — the children who are not there
for x, op in ((600, 0.20), (730, 0.13), (850, 0.07)):
    figure(x, FRONT_BASE, FRONT_H, GHOST, opacity=op, shadow=0.0)

# three children, three names. The terracotta one is the loaded figure.
for x, tone, name, lab in ((1040, LIGHT, "Arlo", DARK),
                           (1210, TERRA, "Wren", TERRA),
                           (1380, MID,   "Zephyr", DARK)):
    figure(x, FRONT_BASE, FRONT_H, tone)
    label(x, name, fill=lab, opacity=0.86)

FIGURES = "\n".join(body)

# ---------------------------------------------------------------------------
# the .svg — self-contained, gradients and live text
# ---------------------------------------------------------------------------
def svg_stops(stops):
    return "\n".join(
        f'    <stop offset="{o}" stop-color="{c}" stop-opacity="{a}"/>'
        for o, c, a in stops)

DEFS = f"""<defs>
  <linearGradient id="sky" x1="0" y1="0" x2="0" y2="1">
{svg_stops(SKY)}
  </linearGradient>
  <radialGradient id="spot" cx="{SPOT_C[0]*100:.0f}%" cy="{SPOT_C[1]*100:.0f}%" r="{SPOT_R*100:.0f}%">
{svg_stops(SPOT)}
  </radialGradient>
  <linearGradient id="floor" x1="0" y1="0" x2="0" y2="1">
{svg_stops(FLOOR)}
  </linearGradient>
  <radialGradient id="vig" cx="{VIG_C[0]*100:.0f}%" cy="{VIG_C[1]*100:.0f}%" r="{VIG_R*100:.0f}%">
{svg_stops(VIG)}
  </radialGradient>
</defs>"""

svg = (f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" '
       f'viewBox="0 0 {W} {H}">\n{DEFS}\n'
       f'<rect width="{W}" height="{H}" fill="url(#sky)"/>\n'
       f'<rect width="{W}" height="{H}" fill="url(#spot)"/>\n'
       f'<rect x="0" y="{FLOOR_TOP}" width="{W}" height="{H-FLOOR_TOP}" fill="url(#floor)"/>\n'
       f'{FIGURES}\n'
       f'<rect width="{W}" height="{H}" fill="url(#vig)"/>\n</svg>\n')
open("names_fertility_cover.svg", "w", encoding="utf-8").write(svg)
print("svg written", len(svg), "bytes")

# ---------------------------------------------------------------------------
# the .png — smooth background in numpy, figures rasterised over it
# ---------------------------------------------------------------------------
def ramp(t, stops):
    """Interpolate (colour, alpha) stops over t in [0,1]. t is an array."""
    offs = np.array([s[0] for s in stops])
    cols = np.array([rgb(s[1]) for s in stops], float)
    alph = np.array([s[2] for s in stops], float)
    c = np.empty(t.shape + (3,), float)
    for k in range(3):
        c[..., k] = np.interp(t, offs, cols[:, k])
    return c, np.interp(t, offs, alph)

def over(base, col, alpha):
    return base * (1 - alpha[..., None]) + col * alpha[..., None]

ys, xs = np.mgrid[0:H, 0:W].astype(float)

# vertical sky
canvas, _ = ramp(ys / (H - 1), SKY)

# radial spot — objectBoundingBox units, so the circle stretches with the box
d = np.hypot((xs - SPOT_C[0]*W) / (SPOT_R*W), (ys - SPOT_C[1]*H) / (SPOT_R*H))
c, a = ramp(np.clip(d, 0, 1), SPOT)
canvas = over(canvas, c, a)

# floor wash
m = ys >= FLOOR_TOP
t = np.zeros_like(ys)
t[m] = (ys[m] - FLOOR_TOP) / (H - 1 - FLOOR_TOP)
c, a = ramp(t, FLOOR)
canvas = over(canvas, c, a * m)

bg = Image.fromarray(np.clip(canvas, 0, 255).astype(np.uint8), "RGB")

# figures only, transparent, rendered at 2x then downsampled for clean edges
fig_svg = (f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" '
           f'viewBox="0 0 {W} {H}">\n{FIGURES}\n</svg>\n')
open("_figures.svg", "w", encoding="utf-8").write(fig_svg)
cairosvg.svg2png(url="_figures.svg", write_to="_figures.png",
                 output_width=W*2, output_height=H*2)
fg = Image.open("_figures.png").convert("RGBA").resize((W, H), Image.LANCZOS)
bg = Image.alpha_composite(bg.convert("RGBA"), fg)

# vignette last, in float, so it never bands
d = np.hypot((xs - VIG_C[0]*W) / (VIG_R*W), (ys - VIG_C[1]*H) / (VIG_R*H))
c, a = ramp(np.clip(d, 0, 1), VIG)
final = over(np.asarray(bg.convert("RGB"), float), c, a)

Image.fromarray(np.clip(final, 0, 255).astype(np.uint8), "RGB").save(
    "names_fertility_cover.png")
print("png written 1600x900")
