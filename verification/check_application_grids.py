#!/usr/bin/env python3
# Author: JoonHo Lee (jlee296@ua.edu)
"""Check the scientific units and the retained-draw populations of the public grids."""
import csv
import json
import math
from collections import Counter, defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SIZES = {"star": 79, "bd": 48, "rasch": 500}

def verify(root=ROOT):
    source = root / "data-frozen/application-summaries"

    def read(name):
        with (source / (name + ".csv")).open() as stream:
            return list(csv.DictReader(stream))

    def by(rows, *keys):
        groups = defaultdict(list)
        for row in rows:
            groups[tuple(row[k] for k in keys)].append(row)
        return groups

    def require(condition, message):
        if not condition:
            raise ValueError(message)

    cdfs = by(read("finite-cdf"), "app", "prior")
    require(Counter(app for app, prior in cdfs) == Counter(dict.fromkeys(SIZES, 5)), "Five CDFs per application required")
    rasch_grid = defaultdict(list)
    at_three = {}
    for (app, prior), group in cdfs.items():
        group.sort(key=lambda r: float(r["x"]))
        grid = [float(r["x"]) for r in group]
        expected = (-4, 9, 521) if app == "rasch" else (-2, 2, 161)
        require((min(grid), max(grid), len(set(grid))) == expected and len(grid) == expected[2], "Finite CDF grid differs")
        require({int(row["n"]) for row in group} == {SIZES[app]}, "Finite population size differs")
        for field in ("mean", "lower", "upper"):
            curve = [float(r[field]) for r in group]
            require(all(math.isfinite(v) and 0 <= v <= 1 for v in curve) and all(x <= y+1e-13 for x, y in zip(curve, curve[1:])), f"Invalid {app} {prior} {field} CDF")
        for row in group:
            require(float(row["lower"]) <= float(row["upper"]), "Reversed pointwise interval")
            if app == "rasch":
                x = float(row["x"])
                rasch_grid[x].append(float(row["mean"]))
                if x == 3:
                    at_three[prior] = {k: float(row[k]) for k in ("mean", "lower", "upper")}
    spread = max(max(ordinates)-min(ordinates) for ordinates in rasch_grid.values())
    require(round(100*spread, 2) == .26, "Rasch maximum between-prior CDF range differs")

    # The within-draw finite-site spread exists even when all observed units
    # share one normal component. Recover its mean from the two draw strata.
    for row in read("spread-by-count"):
        n, one = int(row["draws"]), int(row["draws_one_component"])
        require(n == 16000 and 0 < one < n, "Invalid cluster-count strata")
        a, b = float(row["sd_mean_one_component"]), float(row["sd_mean_two_or_more"])
        mixture = (one * a + (n-one) * b) / n
        require(a > 0 and abs(mixture-float(row["sd_mean_all"])) < 1e-12,
                "Stratified finite-site SD does not reproduce the overall posterior mean")

    densities = by(read("population-density-normal"), "app", "prior")
    runs = {(r["app"], r["prior_label"]): r for r in read("density-runs")}
    require(set(densities) == set(runs) and len(runs) == 6, "Six dedicated school/study density fits required")
    for key, group in densities.items():
        run = runs[key]
        group.sort(key=lambda r: float(r["x"]))
        require(len(group) == int(run["grid_nodes"]) and run["numerical_status"] == "NUMERICALLY_VERIFIED", "Unverified density grid")
        y = [float(r["mean"]) for r in group]
        x = [float(r["x"]) for r in group]
        integral = sum((a+b)*(q-p)/2 for a,b,p,q in zip(y,y[1:],x,x[1:]))
        require(all(math.isfinite(v) and v >= 0 for v in y) and .999 < integral <= 1.001, "Population density mass differs")

    for (app, prior, block), group in by(read("chain-mean-by-unit-or-score"), "app", "prior", "block").items():
        frequency = sum(int(r["frequency"]) for r in group)
        expected_rows = 14 if app == "rasch" else SIZES[app]
        require(frequency == SIZES[app] and len({r["unit_or_score"] for r in group}) == expected_rows, "Unit frequencies differ")
    for (app, pa, ba, pb, bb), group in by(read("rank-bin-sums"), "app", "prior_a", "block_a", "prior_b", "block_b").items():
        require({int(r["bin"]) for r in group} == set(range(1,101)), "Missing percentile bin")
        draws = 16000 if ba == bb == "full" else 8000
        require(sum(int(r["n_rank"]) for r in group) == SIZES[app]*draws, "Percentile comparison uses incorrect draw count")
    return {"finite_cdfs": len(cdfs), "density_runs": len(densities),
            "rasch_maximum_cdf_range": spread, "rasch_at_3_logits": at_three}

if __name__ == "__main__":
    result = verify()
    output = ROOT / "outputs/verification/application-grids.json"
    output.parent.mkdir(exist_ok=True, parents=True)
    output.write_text(json.dumps(result, indent=2) + "\n")
    print(f"Verified {result['finite_cdfs']} finite CDFs and {result['density_runs']} dedicated density fits.")
