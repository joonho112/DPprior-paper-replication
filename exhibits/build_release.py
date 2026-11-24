#!/usr/bin/env python3
# Author: JoonHo Lee (jlee296@ua.edu)
"""Rebuild from reviewed inputs, then install a complete set of scientific outputs."""
import contextlib
import csv
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]
STAGING = ROOT / ".staging"
ACTIVE = STAGING / "november-build"
COMPLETE = "outputs/exhibit-registry.csv"
SOURCE_DIRS = ("applications", "calibration", "common", "data-frozen", "exhibits", "provenance",
               "rasch", "refit", "simulation", "verification")
def digest(p):
    return hashlib.sha256(p.read_bytes()).hexdigest()

def read_csv(path):
    with path.open(newline="", encoding="utf-8-sig") as stream:
        return list(csv.DictReader(stream))

@contextlib.contextmanager
def build_lock():
    STAGING.mkdir(exist_ok=True)
    with (STAGING / "november-build.lock").open("a+b") as lock:
        if os.name == "nt":
            import msvcrt
            if lock.tell() == 0:
                lock.write(b"0"); lock.flush()
            lock.seek(0)
            msvcrt.locking(lock.fileno(), msvcrt.LK_NBLCK, 1)
        else:
            import fcntl
            fcntl.flock(lock.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
        yield

def remove(p):
    if p.is_dir() and not p.is_symlink():
        shutil.rmtree(p)
    elif p.exists() or p.is_symlink():
        p.unlink()

def recover():
    journal = ACTIVE / "publication.json"
    if journal.exists() and not (ACTIVE / "COMMITTED").exists():
        for rel, existed in reversed(json.loads(journal.read_text())):
            backup = ACTIVE / "previous" / rel
            target = ROOT / rel
            if backup.exists():
                remove(target)
                backup.replace(target)
            elif not existed:
                remove(target)
    remove(ACTIVE)

def link_input(src, dest):
    try:
        os.link(src, dest)
    except OSError:
        shutil.copy2(src, dest)
    return dest

def source_tree(new):
    new.mkdir(parents=True)
    for name in (".replication-root", "config.yml", "00_setup.R", "DESCRIPTION"):
        link_input(ROOT/name, new/name)
    for name in SOURCE_DIRS:
        shutil.copytree(ROOT/name, new/name, copy_function=link_input,
                        ignore=shutil.ignore_patterns("__pycache__", ".DS_Store"))

def register(new):
    contract = read_csv(new / "provenance/artifact-contract.csv")
    registry = []
    for row in contract:
        aid = row["artifact_id"]
        folder = new / "outputs/v104"
        frame = folder / (aid + ".csv")
        receipt = new / "outputs/exhibit-receipts" / (aid + ".json")
        record = json.loads(receipt.read_text())
        if [x["path"] for x in record["inputs"]] != row["direct_inputs"].split(";") or record["tier"] != row["reproduction_tier"]:
            raise ValueError("Direct inputs or calculation type differ from the contract: " + aid)
        paths = record["outputs"] + record["inputs"]
        if any(digest(new / item["path"]) != item["sha256"] for item in paths):
            raise ValueError("Invalid exhibit file checksum: " + aid)
        record["numeric_sha256"] = digest(frame)
        receipt.write_text(json.dumps(record, indent=2, ensure_ascii=False) + "\n")
        registry.append(dict(artifact_id=aid, tier=record["tier"], rows=record["rows"],
                             numeric_sha256=record["numeric_sha256"], receipt_sha256=digest(receipt),
                             status="built", note=record["note"]))
    if len(registry) != 62 or len({row["artifact_id"] for row in registry}) != len(registry):
        raise ValueError("The November inventory requires 62 distinct exhibits.")
    with (new / COMPLETE).open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(registry[0]))
        writer.writeheader()
        writer.writerows(registry)

def install(new):
    # These two directories represent a single manuscript edition. Other numerical
    # directories may also contain user-run comparisons, so update their generated files individually.
    groups = ["outputs/v104", "outputs/exhibit-receipts"]
    extras = []
    for area in ("data-derived", "outputs"):
        for path in (new / area).rglob("*"):
            if path.is_file():
                name = path.relative_to(new).as_posix()
                if name != COMPLETE and not any(name.startswith(g + "/") for g in groups):
                    extras.append(name)
    old_proof = "outputs/verification/reproduction.json"
    names = [COMPLETE, old_proof] + groups + sorted(extras)
    journal = ACTIVE / "publication.json"
    tmp = journal.with_suffix(".tmp")
    tmp.write_text(json.dumps([[key, (ROOT / key).exists()] for key in names]))
    tmp.replace(journal)
    for rel in names:
        target = ROOT / rel
        if target.exists():
            backup = ACTIVE / "previous" / rel
            backup.parent.mkdir(parents=True, exist_ok=True)
            target.replace(backup)
        if rel not in (COMPLETE, old_proof):
            target.parent.mkdir(exist_ok=True, parents=True)
            (new / rel).replace(target)
    (new / COMPLETE).replace(ROOT / COMPLETE)
    (ACTIVE / "COMMITTED").touch()

def main():
    with build_lock():
        recover()
        new = ACTIVE / "source"
        try:
            source_tree(new)
            env = os.environ.copy()
            libs = [str(ROOT / ".library"), env.get("R_LIBS", "")]
            env["R_LIBS"] = os.pathsep.join(p for p in libs if p)
            local = ROOT / ".venv" / ("Scripts/python.exe" if os.name == "nt" else "bin/python3")
            if local.is_file():
                env.setdefault("DPP_PYTHON", str(local))
            subprocess.run([sys.executable, "verification/check_inputs.py"], cwd=new, env=env, check=True)
            subprocess.run(["Rscript", "exhibits/build_tracks.R"], cwd=new, env=env, check=True)
            register(new)
            install(new)
        finally:
            recover()
    print("Built 62 November exhibits. See outputs/exhibit-registry.csv.")
if __name__ == "__main__":
    main()
