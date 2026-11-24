#!/usr/bin/env python3
# Author: JoonHo Lee (jlee296@ua.edu)
"""Compare November method exhibits to the specified model and the original printed values."""
import argparse
import csv
import hashlib
import json
import math
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
def rows(path):
    with path.open(newline="") as stream:
        return list(csv.DictReader(stream))

def verify(root):
    out = root / "outputs/exhibits"
    oracle = rows(root/"verification/expected/exhibits/methods-printed.csv")
    tables = {}
    for aid, keys in [("main-table-2", ["setting"]), ("osm-table-E.1", ["application","prior_role"]),
                      ("osm-table-G.1", ["prior"])]:
        data = rows(out/(aid + ".csv"))
        tables[aid] = {"|".join(row[k] for k in keys):row for row in data}
        if len(tables[aid]) != len(data):
            raise ValueError(f"Duplicate {aid} row")
    for cell in oracle:
        number = float(tables[cell["exhibit"]][cell["row_key"]][cell["field"]])
        printed = cell["printed"]
        decimals = len(printed.partition(".")[2])
        if not math.isfinite(number) or abs(number-float(printed)) > 0.5*10**(-decimals)+1e-12:
            raise ValueError(f"Incorrect printed method result: {cell}")
    example = json.loads((out/"G2-example.json").read_text())
    actual_hash = hashlib.sha256((root/"data-frozen/calibration-examples/G2-example.R").read_bytes()).hexdigest()
    if not (example["listing_sha256"] == actual_hash and example["package"]=="2.0.0" and
            (example["fit_nodes"],example["verify_nodes"])==(160,320) and example["hard_gate"]["ok"]):
        raise ValueError("Worked example or integration order differs.")
    grid = rows(out/"osm-figure-G.1.csv")
    eligible = [float(r["lambda"]) for r in grid if r["verified"]=="TRUE" and float(r["achieved_weight"]) <= .40]
    if not (len(grid)==29 and max(eligible)==example["selected_lambda"]==.30 and
            example["soft_target"]==.25 and example["trigger"]==.40 and round(example["tsmm_tail"],3)==.497):
        raise ValueError("G.2 eligibility or weight policy differs.")
    a = [r for r in rows(out/"main-figure-1.csv") if r["panel"]=="A"]
    if len(a)!=50 or abs(sum(float(r["induced"]) for r in a)-1)>1e-8 or abs(sum(float(r["target"]) for r in a)-1)>1e-8:
        raise ValueError("Figure 1 count distributions have incorrect support or normalization.")
    if len(rows(out/"main-table-1.csv"))!=5 or len(rows(out/"main-figure-2.csv"))!=66:
        raise ValueError("Method functional or design-size catalogue is incomplete.")
    if [int(r["step"]) for r in rows(out/"main-figure-3.csv")] != list(range(1,7)):
        raise ValueError("Incorrect reporting schematic.")
    return len(oracle)
if __name__ == "__main__":
    parser=argparse.ArgumentParser()
    parser.add_argument("--root",type=Path,default=ROOT)
    print(f"Verified {verify(parser.parse_args().root)} printed entries and the policy and figure checks.")
