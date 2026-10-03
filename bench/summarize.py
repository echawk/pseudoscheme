#!/usr/bin/env python3
"""Summarize r7rs-benchmarks results as a Markdown table.

    python3 bench/summarize.py [results-dir] [--baseline NAME]

Reads every results.* file in results-dir (default
bench/r7rs-benchmarks), collects the "+!CSVLINE!+impl,benchmark,seconds"
lines that the benchmark harness prints, and prints one row per
benchmark: each implementation's time and, in parentheses, its ratio
to the baseline (default: the implementation whose name starts with
"chez").  Failures (CRASHED, ULIMITKILLED, COMPILEERROR) show as such.
The last rows give each implementation's geometric mean ratio over the
benchmarks that it and the baseline both completed, and how many it
completed.
"""

import glob
import math
import os
import sys


def short(impl):
    # "chez-10.4.1+akku-r7rs" -> "chez", "pseudoscheme-0.2" -> "pseudoscheme"
    return impl.split("-")[0]


def main():
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    baseline = "chez"
    if "--baseline" in sys.argv:
        baseline = sys.argv[sys.argv.index("--baseline") + 1]
        args = [a for a in args if a != baseline]
    here = os.path.dirname(os.path.abspath(__file__))
    rdir = args[0] if args else os.path.join(here, "r7rs-benchmarks")

    results = {}          # (impl, bench) -> seconds or failure string
    impls, benches = [], []
    for path in sorted(glob.glob(os.path.join(rdir, "results.*"))):
        for line in open(path, errors="replace"):
            if not line.startswith("+!CSVLINE!+"):
                continue
            impl, bench, value = line[len("+!CSVLINE!+"):].strip().split(",")[:3]
            impl, bench = short(impl), bench.split(":")[0]
            try:
                value = float(value)
            except ValueError:
                pass
            results[(impl, bench)] = value      # the last run wins
            if impl not in impls:
                impls.append(impl)
            if bench not in benches:
                benches.append(bench)

    impls.sort(key=lambda i: (i != baseline, i != "pseudoscheme", i))
    print("| benchmark | " + " | ".join(impls) + " |")
    print("|---|" + "---|" * len(impls))
    logs = {i: [] for i in impls}
    for b in benches:
        base = results.get((baseline, b))
        cells = []
        for i in impls:
            v = results.get((i, b))
            if v is None:
                cells.append("")
            elif isinstance(v, str):
                cells.append(v.lower())
            elif isinstance(base, float) and base > 0:
                cells.append("%.2f (%.1fx)" % (v, v / base))
                logs[i].append(math.log(v / base))
            else:
                cells.append("%.2f" % v)
        print("| %s | %s |" % (b, " | ".join(cells)))
    print("| **geometric mean vs %s** | %s |" % (baseline, " | ".join(
        ("%.1fx" % math.exp(sum(l) / len(l))) if l else "" for l in logs.values())))
    print("| **completed** | %s |" % " | ".join(
        str(sum(1 for b in benches if isinstance(results.get((i, b)), float)))
        + "/" + str(len(benches)) for i in impls))


if __name__ == "__main__":
    main()
