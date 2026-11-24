# Author: JoonHo Lee (jlee296@ua.edu)
"""Verify the frozen simulation CSVs against their declared schema and SHA-256."""

import csv
import hashlib
import json
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]


def check_inputs():
    directory = ROOT / "data-frozen" / "simulation104"
    specifications = json.loads((HERE / "input-schema.json").read_text())
    files = {s["file"] for s in specifications}
    if files != {p.name for p in directory.iterdir() if p.is_file()}:
        raise ValueError("Unexpected or missing frozen simulation input")
    for spec in specifications:
        path = directory / spec["file"]
        if (path.stat().st_size != spec["bytes"]
                or hashlib.sha256(path.read_bytes()).hexdigest() != spec["sha256"]):
            raise ValueError(f"Frozen input bytes changed: {path.name}")
        with path.open(newline="") as stream:
            rows = csv.reader(stream)
            header = next(rows)
            if header != spec["columns"]:
                raise ValueError(f"Column mismatch: {path.name}")
            indices = [header.index(k) for k in spec["key"]]
            seen = set()
            for row in rows:
                if len(row) != len(header):
                    raise ValueError(f"Wrong row width: {path.name}")
                key = tuple(row[i] for i in indices)
                if not all(key) or key in seen:
                    raise ValueError(f"Non-unique or missing key: {path.name} {key}")
                seen.add(key)
            if len(seen) != spec["rows"]:
                raise ValueError(f"Row count changed: {path.name}")
    print(f"Verified {len(files)} synthetic inputs, schemas, hashes and row keys.")


if __name__ == "__main__":
    check_inputs()
