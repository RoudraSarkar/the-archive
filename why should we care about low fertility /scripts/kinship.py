"""kinship.py — one-sex time-variant matrix kinship model (Caswell 2019; Caswell & Song 2021),
a line-by-line port of DemoKin 1.0.3 kin_time_invariant() / kin_time_variant() (living kin only).

Inputs (numpy arrays, ages 0..omega in rows, years in columns):
  p   survival probabilities p_x(t)
  f   age-specific fertility rates, births of BOTH sexes per woman (ASFR as a rate, not per 1000)
  n   female population (for the distribution of mothers' ages at birth, pi); optional
  birth_female  share of births that are girls (scalar or per year)
Output: dict year -> dict kin_code -> array (age_kin, age_focal) of expected living kin.
Kin codes follow DemoKin: d gd ggd m gm ggm os ys nos nys oa ya coa cya.
"""
import numpy as np

KIN = ["d", "gd", "ggd", "m", "gm", "ggm", "os", "ys", "nos", "nys", "oa", "ya", "coa", "cya"]


def _U(p):
    a = len(p)
    U = np.zeros((a, a))
    U[np.arange(1, a), np.arange(a - 1)] = p[:-1]
    U[a - 1, a - 1] = p[-1]          # open interval keeps survivors (as DemoKin)
    return U


def kin_time_invariant(p, f, birth_female=1 / 2.04, pi=None):
    a = len(p)
    U = _U(p)
    F = np.zeros((a, a)); F[0, :] = f * birth_female
    if pi is None:
        A = U + F
        vals, vecs = np.linalg.eig(A)
        w = np.real(vecs[:, np.argmax(np.real(vals))]); w = w / w.sum()
        pi = w * A[0, :] / np.sum(w * A[0, :])
    K = {k: np.zeros((a, a)) for k in KIN}
    I = np.eye(a)
    K["m"][:, 0] = pi
    for i in range(a - 1):
        K["d"][:, i + 1] = U @ K["d"][:, i] + F @ I[:, i]
        K["gd"][:, i + 1] = U @ K["gd"][:, i] + F @ K["d"][:, i]
        K["ggd"][:, i + 1] = U @ K["ggd"][:, i] + F @ K["gd"][:, i]
        K["m"][:, i + 1] = U @ K["m"][:, i]
        K["ys"][:, i + 1] = U @ K["ys"][:, i] + F @ K["m"][:, i]
        K["nys"][:, i + 1] = U @ K["nys"][:, i] + F @ K["ys"][:, i]
    K["gm"][:, 0] = K["m"] @ pi
    for i in range(a - 1):
        K["gm"][:, i + 1] = U @ K["gm"][:, i]
    K["ggm"][:, 0] = K["gm"] @ pi
    for i in range(a - 1):
        K["ggm"][:, i + 1] = U @ K["ggm"][:, i]
    K["os"][:, 0] = K["d"] @ pi
    K["nos"][:, 0] = K["gd"] @ pi
    for i in range(a - 1):
        K["os"][:, i + 1] = U @ K["os"][:, i]
        K["nos"][:, i + 1] = U @ K["nos"][:, i] + F @ K["os"][:, i]
    K["oa"][:, 0] = K["os"] @ pi
    K["ya"][:, 0] = K["ys"] @ pi
    K["coa"][:, 0] = K["nos"] @ pi
    K["cya"][:, 0] = K["nys"] @ pi
    for i in range(a - 1):
        K["oa"][:, i + 1] = U @ K["oa"][:, i]
        K["ya"][:, i + 1] = U @ K["ya"][:, i] + F @ K["gm"][:, i]
        K["coa"][:, i + 1] = U @ K["coa"][:, i] + F @ K["oa"][:, i]
        K["cya"][:, i + 1] = U @ K["cya"][:, i] + F @ K["ya"][:, i]
    return K


# source term for each kin type: kin(t+1)[:, x+1] = U kin(t)[:, x] + F src(t)[:, x]
_SRC = {"d": "I", "gd": "d", "ggd": "gd", "ys": "m", "nos": "os", "nys": "ys",
        "ya": "gm", "coa": "oa", "cya": "ya"}
# initial condition at focal age 0: kin(t+1)[:,0] = prev(t)[parent_type] @ pi_t
_INIT = {"gm": "m", "ggm": "gm", "os": "d", "nos": "gd", "oa": "os", "ya": "ys", "coa": "nos", "cya": "nys"}


def kin_time_variant(p, f, years, n=None, birth_female=1 / 2.04, keep_years=None, stable_birth_female=None):
    """stable_birth_female: share female used ONLY for the stable start (eigenvector); defaults to birth_female.
    For the androgynous (both-sex) approximation run with birth_female=1 (every child counted) but keep
    stable_birth_female ~ 1/2.05 so the pre-1950 stable age structure is not inflated by a doubled growth rate.
    Returns {year: {kin: (age_kin x age_focal)}} for years[0] .. years[-1]+1 (kin at 1 Jan of each year)."""
    a, T = p.shape
    bf = np.broadcast_to(np.asarray(birth_female, float), (T,)) if np.ndim(birth_female) else np.full(T, birth_female)
    if n is not None:
        nf = n * f
        pis = nf / nf.sum(axis=0, keepdims=True)
    out = {}
    sbf = bf[0] if stable_birth_female is None else stable_birth_female
    U0 = _U(p[:, 0]); F0 = np.zeros((a, a)); F0[0, :] = f[:, 0] * sbf
    A = U0 + F0
    vals, vecs = np.linalg.eig(A)
    w = np.real(vecs[:, np.argmax(np.real(vals))]); w = w / w.sum()
    pi1 = w * A[0, :] / np.sum(w * A[0, :])
    prev = kin_time_invariant(p[:, 0], f[:, 0], birth_female=bf[0], pi=pi1)
    if keep_years is None or years[0] in keep_years:
        out[years[0]] = prev
    I = np.eye(a)
    for t in range(T):
        U = _U(p[:, t]); F = np.zeros((a, a)); F[0, :] = f[:, t] * bf[t]
        if n is not None:
            pit = pis[:, t]
        else:
            if stable_birth_female is not None:
                raise ValueError("pass n when using stable_birth_female (projecting w with total births would inflate growth)")
            A = U + F; w = A @ w; pit = w * A[0, :] / np.sum(w * A[0, :])
        new = {}
        for k in KIN:
            K = np.zeros((a, a))
            K[:, 1:] = U @ prev[k][:, :-1]
            if k in _SRC:
                S = I if _SRC[k] == "I" else prev[_SRC[k]]
                K[:, 1:] += F @ S[:, :-1]
            if k == "m":
                K[:, 0] = pit
            elif k in _INIT:
                K[:, 0] = prev[_INIT[k]] @ pit
            new[k] = K
        prev = new
        y = years[t] + 1
        if keep_years is None or y in keep_years:
            out[y] = new
    return out
