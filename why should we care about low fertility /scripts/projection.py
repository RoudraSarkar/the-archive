"""projection.py — single-year cohort-component projection (1 Jan to 1 Jan) with optional net migration,
used for chart 7 (replacement migration). Inputs are UN WPP 2024 medium-variant rates.

Conventions (validated against WPP in scripts/validation/validate_projection.py):
  survivors   P[x+1](t+1) = P[x](t) * Sx[x](t)       for x < 99
              P[100](t+1) = P[99](t) * Sx[99] + P[100](t) * Sx[100]   (open interval)
  births      B(t) = sum_x ASFR_x(t)/1000 * mean(F_x(t), F_x(t+1)), split by SRB, survive to age 0 by L0/l0
  migrants    M(t) * profile[sex, age] added at the end of the year (after survival), so a migrant's
              first-year mortality and births are ignored (small, standard simplification).
"""
import numpy as np

A = 101  # ages 0..100+


def step(P, S, asfr, srb, l0, migrants=None):
    """P: (2, A) [male, female] at 1 Jan t. S: (2, A) survival ratios. asfr: (A,) per 1000. srb: males per 100 females.
    l0: (2,) L0/l0 by sex. migrants: (2, A) or None. Returns P at 1 Jan t+1 and births."""
    N = np.zeros_like(P)
    N[:, 1:100] = P[:, 0:99] * S[:, 0:99]
    N[:, 100] = P[:, 99] * S[:, 99] + P[:, 100] * S[:, 100]
    births = np.sum(asfr / 1000.0 * 0.5 * (P[1] + N[1]))
    pm = srb / (100.0 + srb)
    N[0, 0] = births * pm * l0[0]
    N[1, 0] = births * (1 - pm) * l0[1]
    if migrants is not None:
        N = N + migrants
    return N, births


def oadr(P):
    """65+ per 100 aged 20-64."""
    return 100 * P[:, 65:].sum() / P[:, 20:65].sum()


def wa(P):
    return P[:, 20:65].sum()
