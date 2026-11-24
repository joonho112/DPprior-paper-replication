#!/usr/bin/env python3
# Author: JoonHo Lee (jlee296@ua.edu)
"""Synchronize the reader's guide to the verified 104 artifact registry."""
import argparse
import csv
import hashlib
import json
import re
import shutil
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DOCS = ROOT / "docs"
MANIFEST = ROOT / "provenance/book-assets.csv"
RECORDS = {
    "reproduction-map.csv": "provenance/reproduction-map.csv",
    "artifact-contract.csv": "provenance/artifact-contract.csv",
    "software-lock.csv": "provenance/software-lock.csv",
    "data-rights.csv": "provenance/data-rights.csv",
    "provenance/refit-status.csv": "provenance/refit-status.csv",
    "provenance/selected-replay-verification.csv": "provenance/selected-replay-verification.csv",
    "provenance/104-inputs.md": "provenance/104-inputs.md",
    "refit/README.md": "refit/README.md",
    "rasch-fit-registry.csv": "data-frozen/rasch/prior_and_fit_registry.csv",
    "calibration-policy.yml": "calibration/policy.yml",
    "DATA_ACCESS.md": "DATA_ACCESS.md",
    "LICENSE-SCOPE.md": "LICENSE-SCOPE.md",
    "verification/refit-smoke.md": "verification/refit-smoke.md",
    "toolchain.md": "common/TOOLCHAIN.md",
    "G2-example.R": "data-frozen/methods104/G2-example.R",
    "workflow.tex": "data-frozen/methods104/workflow.tex",
    "application-inputs.csv": "data-frozen/applications104/input-contract.csv",
}
CODE_DIRS = {"applications", "calibration", "common", "exhibits", "rasch", "refit", "simulation", "verification"}
def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

def csv_rows(path):
    with path.open(newline="") as stream:
        return list(csv.DictReader(stream))

def source_path(name):
    path = ROOT / name
    if not path.is_file() or not path.resolve().is_relative_to(ROOT):
        raise ValueError(f"Missing public source: {name}")
    return path

def current_artifacts():
    registry = ROOT / "outputs/exhibit-registry.csv"
    proof = json.loads((ROOT / "outputs/verification/reproduction.json").read_text())
    if proof.get("schema") != "v104-verification/1" or not proof.get("passed") or proof.get("registry_sha256") != digest(registry):
        raise ValueError("Verify the current build with Rscript verify_reproduction.R before synchronizing the guide.")
    contract = csv_rows(ROOT / "provenance/artifact-contract.csv")
    mapping = {r["artifact_id"]:r for r in csv_rows(ROOT / "provenance/reproduction-map.csv")}
    ids = {r["artifact_id"] for r in contract}
    if len(contract) != 62 or len(ids) != 62 or ids != set(mapping) or ids != {r["artifact_id"] for r in csv_rows(registry)}:
        raise ValueError("Current contract, reproduction map and exhibit registry must agree on all 62 artifacts.")
    copies = {}
    lines = ["# Tables and figures","",
       "The index covers the 43 tables and 19 figures in manuscript version 104. Download a table as HTML or LaTeX, a figure as PNG or PDF, and the unrounded data as CSV. The calculation column distinguishes source specifications and computations from supplied data. The reproduction map records additional input files for figures with multiple panels. [Reproduction map](records/reproduction-map.csv)","",
       "| Manuscript result | Preview | Print format | Data | Builder | Calculation |",
       "|---|---|---|---|---|---|"]
    for r in contract:
        id = r["artifact_id"]
        mapping_row = mapping[id]
        receipt = json.loads((ROOT / "outputs/exhibit-receipts" / (id+".json")).read_text())
        for item in receipt["outputs"] + receipt["inputs"]:
            if digest(source_path(item["path"])) != item["sha256"]:
                raise ValueError(f"Receipt mismatch for {id}: {item['path']}")
        preview, vector = ("png","pdf") if r["kind"] == "figure" else ("html","tex")
        rendered = {"outputs/v104/"+id+"."+suffix for suffix in (preview,vector)}
        if rendered != {p["path"] for p in receipt["outputs"]}:
            raise ValueError(f"Unexpected display files for {id}")
        for suffix in (preview,vector,"csv"):
            name = f"{id}.{suffix}"
            copies["artifacts/"+name] = "outputs/v104/"+name
        builder = mapping_row["builder"]
        document = "Main" if r["document"] == "main" else "OSM"
        label = f"{document} {r['kind'].title()} {r['printed_number']}"
        tier = receipt["tier"].replace("-"," ")
        lines.append(f"| {label} | [View](artifacts/{id}.{preview}) | [{vector.upper()}](artifacts/{id}.{vector}) | [CSV](artifacts/{id}.csv) | [Code](code/{builder}) | {tier} |")
    return copies, "\n".join(lines)+"\n"

def main():
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument("--execute",action="store_true")
    if not cli.parse_args().execute:
        print("After verification, run with --execute to refresh the guide snapshots and index.")
        return
    copies, index = current_artifacts()
    texts = [p.read_text() for p in DOCS.glob("*.qmd") if p.name!="08-artifact-index.qmd"]+[index]
    for relative in sorted(set(re.findall(r"\]\((code/[^)\s]+)\)", "\n".join(texts)))):
        src = Path(relative).relative_to("code")
        if src.parts[0] not in CODE_DIRS and str(src) not in {"00_setup.R","verify_reproduction.R"}:
            raise ValueError(f"Unexpected code source: {src}")
        if src.suffix not in {".R",".py",".yml",".json",".sh"} or ".." in src.parts:
            raise ValueError(f"Unexpected code link: {src}")
        copies[relative] = str(src)
    for name,src in RECORDS.items():
        copies["records/"+name] = src
    old = {x["destination"] for x in csv_rows(MANIFEST)} if MANIFEST.is_file() else set()
    current = {"docs/"+s for s in copies}
    with tempfile.TemporaryDirectory(prefix="guide-snapshot-") as temp:
        stage = Path(temp)
        new = []
        for dest,src in sorted(copies.items()):
            target = stage/dest;target.parent.mkdir(parents=True,exist_ok=True)
            shutil.copyfile(source_path(src),target)
            new.append({"source":src,"destination":"docs/"+dest,"sha256":digest(target)})
        for dest in copies:
            target = DOCS/dest
            target.parent.mkdir(parents=True,exist_ok=True)
            shutil.copyfile(stage/dest,target)
        for stale in old-current:
            relative = Path(stale).relative_to("docs")
            if relative.parts[0] not in {"artifacts","code","records"} or ".." in relative.parts:
                raise ValueError(f"Unexpected path in prior snapshot registry: {stale}")
            # The renderer may retain previously published assets when reusing its output directory.
            for parent in (DOCS,DOCS/"_book"):
                if (parent/relative).is_file(): (parent/relative).unlink()
        with MANIFEST.open("w",newline="") as stream:
            writer=csv.DictWriter(stream,fieldnames=["source","destination","sha256"])
            writer.writeheader();writer.writerows(new)
        (DOCS/"08-artifact-index.qmd").write_text(index)
    print(f"Refreshed {len(new)} snapshots for all 62 manuscript artifacts.")

if __name__=="__main__":
    main()
