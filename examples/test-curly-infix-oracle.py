#!/usr/bin/env python3
"""
Independent Python oracle for the Scheme+ curly-infix test expressions.

It computes each expression again, in plain Python, then runs the Scheme+
oracle (test-curly-infix-oracle+.rkt) and diffs the two sets of results.

Semantic mapping used:
  {a / b}      -> exact rational  (fractions.Fraction), like Scheme
  {a ** b} {a ² }  -> a ** b
  {a % b}      -> a % b
  {a << b} {a >> b}
  ·            -> *
  √ x          -> exact integer sqrt of a perfect square (Scheme returns exact)
  {n !}        -> math.factorial(n)
  <> , ≠       -> !=
  ≤ , ≥        -> <= , >=
  {a = b}      -> a == b
  and / or     -> Python and / or
"""

import math
import subprocess
import sys
from fractions import Fraction as F

import os
HERE = os.path.dirname(os.path.abspath(__file__))


def fib(n):
    return n if n < 2 else fib(n - 1) + fib(n - 2)


def fact(n):
    return 1 if n == 0 else n * fact(n - 1)


def norm(x, y):
    return x ** 2 + y ** 2


def linelen(x0, y0, x1, y1):
    return math.isqrt((x1 - x0) ** 2 + (y1 - y0) ** 2)


def discriminant(a, b, c):
    return b ** 2 - 4 * a * c


# id -> Python value, mirroring each expression of test-curly-infix-oracle+.rkt
PY = {
    "e01": 2 + 3 * 4,
    "e02": 3 * 5 + 2,
    "e03": 5 + 2,
    "e04": 3 * 7 + 2 - F(12, 3) + (-2),
    "e05": 10 - 2 - 3,
    "e06": F(100, 10) / 5,
    "e07": 1 + 2 + 3 + 4 + 5,
    "e08": 2 * 3 * 4,
    "e09": -5 + 3,
    "e10": 3 * -2,
    "e11": 2 ** 3,
    "e12": 2 * 2 ** 3,
    "e13": 3 ** 2,
    "e14": 4 ** 2 + 3 ** 2,
    "e15": 3 * 5 + 2 ** 3,
    "e16": 3 ** 2 + 2 * 3 * 5 + 5 ** 2,
    "e17": F(1, 2),
    "e18": F(1, 2) + F(1, 4),
    "e19": 17 % 5,
    "e20": 1 << 5,
    "e21": 40 >> 2,
    "e22": 3 * (2 + 4) - 1,
    "e23": (3 + 1) * (2 + 4),
    "e24": ((1 + 2) * 3) - F(4, (1 + 1)),
    "e25": F(10 - 2, 1 + 1) + (2 * (3 + 1)),
    "e26": 2 ** (1 + 2),
    "e27": (2 + 3) ** 2 - (1 + 1) ** 3,
    "e28": 3 * (9 - 4) + 2,
    "e29": (7 - (3 * (2 + 4) - 1)) + 3,
    "e30": (3 + 1) * (2 * (2 + 1) - math.sin(0.3)) + ((2 * 5) - 5),
    "e31": 3 * (2 + 4) - 1,
    "e32": (3 + 3) - (2 + 2),
    "e33": max(3, 7) + min(10, 2),
    "e34": (2 ** 3) + 1,
    "e35": sum([1, 2, 3, 4]) * 2,
    "e36": [10, 20, 30][1] + 5,
    "e37": (10 - 4) * 2,
    "e38": math.gcd(12, 18) + math.lcm(3, 4),
    "e39": (2 * 3) + (4 * 5),
    "e40": math.isqrt(16),
    "e41": math.isqrt(9 + 16),
    "e42": (1 + 2) * (3 + 4),
    "e43": F(10, 2) + 2 ** 3,
    "e44": ((1 + 1) + (2 + 2)) * (3 + 4),
    "e45": fib(7),
    "e46": fib(10),
    "e47": fact(5),
    "e48": norm(3, 4),
    "e49": linelen(0, 0, 3, 4),
    "e50": discriminant(1, -5, 6),
    "e51": discriminant(2, 4, 2),
    "e52": 2 + 3,
    "e53": 2 + 3 + 4 + 5 + 6,
    "e54": 1 + 2 + 3 + 4,
    "e55": math.factorial(5),
    "e56": 4 ** 2,
    "e57": 2 ** 3,
    "e58": math.factorial(2 + 3),
    "e59": (1 + 1) + 3 + 4,
    "e60": (2 + 3) ** 2,
    "e61": 3 < 5,
    "e62": (2 + 2) == 4,
    "e63": 3 != 4,
    "e64": 3 != 3,
    "e65": (2 < 3) and (4 < 5),
    "e66": (2 < 3) and (5 < 4),
    "e67": [n * n for n in [1, 2, 3, 4]],
}


def parse_scheme(tok):
    """Parse one displayed Scheme value into a normalized Python object."""
    tok = tok.strip()
    if tok == "#t":
        return True
    if tok == "#f":
        return False
    if tok.startswith("(") and tok.endswith(")"):
        inner = tok[1:-1].split()
        return tuple(parse_scheme(x) for x in inner)
    if "/" in tok and all(p.lstrip("-").isdigit() for p in tok.split("/")):
        n, d = tok.split("/")
        return F(int(n), int(d))
    try:
        return F(int(tok))
    except ValueError:
        return float(tok)


def normalize_py(v):
    if isinstance(v, bool):
        return v
    if isinstance(v, int):
        return F(v)
    if isinstance(v, F):
        return v
    if isinstance(v, float):
        return v
    if isinstance(v, (list, tuple)):
        return tuple(normalize_py(x) for x in v)
    raise TypeError(repr(v))


def eq(a, b):
    if isinstance(a, bool) or isinstance(b, bool):
        return a == b
    if isinstance(a, tuple) or isinstance(b, tuple):
        return (isinstance(a, tuple) and isinstance(b, tuple)
                and len(a) == len(b)
                and all(eq(x, y) for x, y in zip(a, b)))
    if isinstance(a, float) or isinstance(b, float):
        fa, fb = float(a), float(b)
        return abs(fa - fb) <= 1e-9 * max(1.0, abs(fa), abs(fb))
    return a == b


def fmt(v):
    if isinstance(v, F):
        return str(v.numerator) if v.denominator == 1 else f"{v.numerator}/{v.denominator}"
    if isinstance(v, tuple):
        return "(" + " ".join(fmt(x) for x in v) + ")"
    return str(v)


def main():
    out = subprocess.run(
        ["racket", f"{HERE}/test-curly-infix-oracle+.rkt"],
        capture_output=True, text=True, check=True,
    ).stdout
    scheme = {}
    for line in out.splitlines():
        if "\t" in line and line[:1] == "e" and line[1:3].isdigit():
            k, v = line.split("\t", 1)
            scheme[k] = parse_scheme(v)

    ids = sorted(PY)
    ok = 0
    bad = 0
    missing = 0
    for i in ids:
        py = normalize_py(PY[i])
        if i not in scheme:
            print(f"{i}  MISSING from Scheme oracle output")
            missing += 1
            continue
        sc = scheme[i]
        if eq(py, sc):
            ok += 1
            print(f"{i}  OK    scheme={fmt(sc):<22} python={fmt(py)}")
        else:
            bad += 1
            print(f"{i}  DIFF  scheme={fmt(sc):<22} python={fmt(py)}")

    print()
    print(f"{ok} OK, {bad} DIFF, {missing} MISSING  (of {len(ids)})")
    sys.exit(1 if (bad or missing) else 0)


if __name__ == "__main__":
    main()
