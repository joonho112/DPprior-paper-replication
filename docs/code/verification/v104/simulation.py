#!/usr/bin/env python3
# Author: JoonHo Lee (jlee296@ua.edu)
"""Verify the 104 simulation tables, panels, and exact converged-fit membership."""
import argparse
from collections import defaultdict
import csv
import hashlib
from html.parser import HTMLParser
import re
import json
import math
from pathlib import Path
import sys


class Mismatch(ValueError):
    pass


def csv_rows(path):
    with path.open(newline="") as stream:
        return list(csv.DictReader(stream))


def index(rows, keys, label):
    result = {}
    for row in rows:
        key = []
        for column in keys:
            value = row[column]
            if value in ("", "NA"):
                value = ""
            elif column in ("K_true", "I_target", "I", "percentile", "x", "replicate"):
                value = float(value)
            key.append(value)
        key = tuple(key)
        if key in result:
            raise Mismatch(f"{label}: duplicate key {key}")
        result[key] = row
    return result


def close(expected, actual, context, tolerance=1e-10):
    try:
        a, b = float(actual), float(expected)
    except (ValueError, TypeError):
        if actual == expected:
            return
    else:
        if math.isfinite(a) and math.isfinite(b) and abs(a - b) <= tolerance:
            return
    raise Mismatch(f"{context}: expected {expected}, observed {actual}")


def compare(label, expected, actual, keys, fields=None):
    ref = index(expected, keys, label + " reference")
    obs = index(actual, keys, label)
    if ref.keys() != obs.keys():
        missing = list(ref.keys() - obs.keys())[:2]
        extra = list(obs.keys() - ref.keys())[:2]
        raise Mismatch(f"{label}: {len(ref)} expected keys, {len(obs)} observed; missing {missing}, extra {extra}")
    if fields is None:
        fields = [f for f in expected[0] if f not in keys]
    for key, row in ref.items():
        for column in fields:
            close(row[column], obs[key][column], f"{label} {key} {column}")
    return len(ref) * len(fields)


def membership(fits):
    if len(fits) != 8607 or len({r["fit_key"] for r in fits}) != 8607:
        raise Mismatch("Expected 8,607 distinct main fit keys")
    groups = defaultdict(dict)
    for r in fits:
        key = r["K_true"], r["I_target"], int(r["replicate"])
        if r["prior"] in groups[key]:
            raise Mismatch(f"Duplicated prior within simulation dataset {key}")
        groups[key][r["prior"]] = r
    rows = []
    for scope, roles in [("all-five", ("Vague", "DORO-Unif", "TSMM", "Dual-Anchor", "SSI-Weight")),
                         ("Gamma-Dual", ("Vague", "Dual-Anchor"))]:
        designs = defaultdict(list)
        for (k, i, rep), available in groups.items():
            hashes = {r["dgp_sha256"] for r in available.values()}
            if len(hashes) != 1:
                raise Mismatch(f"Different synthetic datasets were paired at {(k, i, rep)}")
            if all(role in available for role in roles):
                designs[k, i].extend((rep, next(iter(hashes)), role, available[role]["fit_key"]) for role in roles)
        for (k, i), members in designs.items():
            payload = "".join(f"{rep},{dgp},{role},{fit}\n" for rep, dgp, role, fit in sorted(members))
            rows.append(dict(K_true=k, I_target=i, scope=scope, n_datasets=len(members)//len(roles),
                             sha256=hashlib.sha256(payload.encode()).hexdigest()))
    return rows


def panel_reference(five, ranges, quantiles):
    rows = []
    for r in five:
        rows.append(dict(panel="A", K_true=r["K_true"], I_target=r["I_target"], prior=r["prior"],
                         percentile="", n=r["n_all_five"], metric="posterior_mean_sd", value=r["posterior_mean_sd"]))
    for r in ranges:
        for field in ("CB_ISEL_range_pct", "site_PM_RMSE_range_pct"):
            rows.append(dict(panel="B", K_true=r["K_true"], I_target=r["I_target"], prior="",
                             percentile="", n=r["n_all_five"], metric=field, value=r[field]))
    for r in quantiles:
        if int(r["percentile"]) in (50, 95, 99):
            rows.append(dict(panel="C", K_true=r["K_true"], I_target=r["I_target"], prior="",
                             percentile=r["percentile"], n=r["n"], metric="mean_abs_shift", value=r["mean_abs_shift"]))
    return rows


class DisplayRows(HTMLParser):
    def __init__(self):
        super().__init__()
        self.rows = []
        self.row = []
        self.cell = None

    def handle_starttag(self, tag, attrs):
        if tag == "tr":
            self.row = []
        if tag == "td":
            self.cell = []

    def handle_endtag(self, tag):
        if tag == "td" and self.cell is not None:
            self.row.append("".join(self.cell))
            self.cell = None
        if tag == "tr" and self.row:
            self.rows.append(self.row)

    def handle_data(self, data):
        if self.cell is not None:
            self.cell.append(data)


def rendered_values(name, path):
    if path.suffix == ".html":
        parser = DisplayRows()
        parser.feed(path.read_text())
        rows = parser.rows
    else:
        content = path.read_text().split(r"\midrule\endhead", 1)[1].split(r"\bottomrule", 1)[0]
        rows = [[x.strip() for x in line.split(r"\\", 1)[0].split("&")]
                for line in content.splitlines() if "&" in line]
    results = []
    for row in rows:
        obs = {"K_true": row[0]}
        if name in ("main-table-3", "osm-table-F.6"):
            mean, sd = re.fullmatch(r"([\d.]+) \(([\d.]+)\)", row[2]).groups()
            n, attempts = row[6].split("/")
            obs.update(prior=row[1], mean_E_K=mean, sd_between_E_K=sd, n_scientific_eligible=n,
                       n_manifest=attempts, mean_p_K1=row[3], coverage95_K=row[4], mean_interval_width_K=row[5])
            if name.endswith("F.6"):
                obs.update(mean_K_bias=row[7], rmse_E_K=row[8])
        elif name in ("osm-table-F.12", "osm-table-F.13", "osm-table-F.14"):
            obs["prior"] = "Vague" if row[1] == "Gamma(1,1)" else row[1]
            fields = "n E_K posterior_mean_sd sd_bias site_PM_RMSE PM_ISEL CB_ISEL PM_KS CB_KS".split()
            obs.update(zip(fields, row[2:]))
        elif name == "osm-table-F.15":
            obs["prior"] = "Vague" if row[1] == "Gamma(1,1)" else row[1]
            obs.update(zip(("posterior_CDF_ISEL", "posterior_CDF_KS", "GR_ISEL", "GR_KS"), row[4:]))
        else:
            fields = "n percentile mean_truth bias_gamma mae_gamma rmse_gamma bias_dual mae_dual rmse_dual mean_shift mean_abs_shift".split()
            obs.update(zip(fields, row[1:]))
        results.append(obs)
    return results


def verify_displays(entries, out):
    by_name = defaultdict(list)
    for entry in entries:
        by_name[entry["exhibit"]].append(entry)
    differences = []
    for name, table in by_name.items():
        keys = ("K_true", "percentile") if table[0]["percentile"] else ("K_true", "prior")
        reference_keys = {tuple(float(e[k]) if k != "prior" else e[k] for k in keys) for e in table}
        for fmt in ("html", "tex"):
            obs = index(rendered_values(name, out / (name + "." + fmt)), keys, name)
            if obs.keys() != reference_keys:
                raise Mismatch(f"{name}.{fmt}: published and rendered row identities differ")
            for e in table:
                key = tuple(float(e[k]) if k != "prior" else e[k] for k in keys)
                value = obs[key][e["metric"]]
                if value != e["printed"]:
                    differences.append(f"{name}.{fmt} {key} {e['metric']}: printed {e['printed']}, observed {value}")
    if differences:
        raise Mismatch("\n".join(differences))
    return sum(map(len, by_name.values())) * 2


def verify(root):
    out = root / "outputs/v104"
    derived = root / "data-derived/v104"
    source = root / "verification/expected/v104/simulation"
    for name, expected in json.loads((source / "manifest.json").read_text()).items():
        if hashlib.sha256((source / name).read_bytes()).hexdigest() != expected:
            raise Mismatch(f"Independent simulation reference changed: {name}")
    def ref(name):
        return csv_rows(source / name)
    def frame(name):
        return csv_rows(out / (name + ".csv"))
    def data(name):
        return csv_rows(derived / ("simulation-" + name + ".csv"))
    cell = ("K_true", "I_target", "prior")
    design = cell[:2]
    quantile = (*design, "percentile")
    count = ref("low-count.csv")
    means = ref("cells.csv")
    five = ref("all-five.csv")
    ranges = ref("ranges.csv")
    quant = ref("percentiles.csv")
    replay = ref("replay.csv")
    audits = {}
    audits["main-table-3"] = compare("main-table-3", count, frame("main-table-3"), cell,
                                     list(count[0])[3:-2])
    audits["osm-table-F.6"] = compare("osm-table-F.6", count, frame("osm-table-F.6"), cell)
    audits["per-prior means and MCSEs"] = compare("individual-prior cells", means, data("cell-means"), cell)
    audits["all-five cell means"] = compare("all-five cell means", five, data("all-five-means"), cell)
    audits["all-five error ranges"] = compare("all-five ranges", ranges, data("all-five-ranges"), design)
    audits["same-data memberships"] = compare("same-data memberships", ref("paired-membership.csv"),
                                             membership(data("fit-metrics")), (*design, "scope"))
    paired_counts = [r for r in ref("paired-membership.csv") if r["scope"] == "Gamma-Dual"]
    audits["two-prior denominators"] = compare("two-prior denominators",
        [dict(K_true=r["K_true"], I_target=r["I_target"], n_paired=r["n_datasets"]) for r in paired_counts],
        data("pair-convergence"), design)
    audits["main-figure-4"] = compare("main-figure-4", panel_reference(five, ranges, quant), frame("main-figure-4"),
                                      ("panel", *design, "prior", "percentile", "metric"))
    recovery = "n E_K posterior_mean_sd sd_bias site_PM_RMSE PM_ISEL CB_ISEL PM_KS CB_KS".split()
    printed_lookup = {}
    for j, info in enumerate(("0.2", "0.5", "0.8")):
        name = f"osm-table-F.{12+j}"
        obs = frame(name)
        audits[name] = compare(name, [r for r in means if r["I_target"] == info], obs, cell, recovery)
        name = f"osm-table-F.{16+j}"
        obs = frame(name)
        audits[name] = compare(name, [r for r in quant if r["I_target"] == info], obs, quantile)
    replay_cols = ["fit_key", "dgp_sha256", "posterior_draws", "posterior_CDF_ISEL", "posterior_CDF_KS", "GR_ISEL", "GR_KS"]
    audits["osm-table-F.15"] = compare("osm-table-F.15", replay, frame("osm-table-F.15"),
                                      ("K_true", "prior", "I", "replicate"), replay_cols)
    audits["osm-figure-F.5"] = compare("osm-figure-F.5", ref("curves.csv"), frame("osm-figure-F.5"),
                                      ("K_true", "prior", "I", "replicate", "x"))
    for name in ("osm-figure-F.6", "osm-figure-F.7"):
        audits[name] = compare(name, quant, frame(name), quantile)
    for entry in ref("printed-cells.csv"):
        name = entry["exhibit"]
        if name not in printed_lookup:
            rows = frame(name)
            keys = ("K_true", "percentile") if "percentile" in rows[0] else ("K_true", "prior")
            printed_lookup[name] = (keys, index(rows, keys, name))
        keys, lookup = printed_lookup[name]
        key = next(iter(index([entry], keys, name)))
        token = entry["printed"]
        decimals = len(token.partition(".")[2])
        tolerance = .5 * 10**-decimals + 1e-12 if decimals else 0
        close(token, lookup[key][entry["metric"]], f"{name} printed {key} {entry['metric']}", tolerance)
    audits["104 TeX printed values"] = len(ref("printed-cells.csv"))
    audits["literal HTML and LaTeX display values"] = verify_displays(ref("printed-cells.csv"), out)
    return audits


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[2])
    args = parser.parse_args()
    try:
        result = verify(args.root.resolve())
        print(json.dumps(result, indent=2))
    except (Mismatch, KeyError, OSError, ValueError) as exc:
        print(f"Simulation verification failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
