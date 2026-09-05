#!/usr/bin/env python3
# Author: JoonHo Lee (jlee296@ua.edu)
# Test the public files in an isolated workspace
#
# Copy only the declared public files into an empty location and rebuild
# there. The installed dependency cache is isolated too; this checks
# independence from private research files, not a fresh package installation.
#
# Run: python3 verification/cold_start.py --execute
# Writes a temporary workspace and outputs/verification/cold_start.json.
# On macOS, the original home directory and network are denied during the run.

"""Build an exact allowlisted copy with empty outputs and isolated R libraries.

Installed pinned source-built dependencies are copied as a runtime cache. This is
an isolated execution check, not evidence of a fresh network/source installation.
On macOS, sandbox-exec also denies network and the author's original home tree.
"""
from pathlib import Path
import argparse
import csv
import hashlib
import json
import os
import shutil
import subprocess
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--execute', action='store_true')
    args = parser.parse_args()
    if not args.execute:
        print(
            'Plan: copy exact allowlist, isolate dependencies/HOME, rebuild, verify, render static Book. Add --execute.'
        )
        return

    # Create a new public workspace. The spaces in its path also exercise path
    # quoting.
    temp = Path(tempfile.mkdtemp(prefix='dpprior cold start ')).resolve()
    root = temp / 'public copy'
    root.mkdir()
    home = temp / 'isolated-home'
    home.mkdir()

    # Copy exact allowlisted bytes, excluding private working directories.
    ledger = ROOT / 'provenance/release-allowlist.csv'
    rows = list(csv.DictReader(ledger.open()))
    for row in rows:
        src = ROOT / row['path']
        dst = root / row['path']
        if (
            src.is_symlink()
            or not src.is_file()
            or hashlib.sha256(src.read_bytes()).hexdigest() != row['sha256']
        ):
            raise RuntimeError('Changed/unreviewed input: ' + row['path'])
        dst.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(src, dst)
    shutil.copyfile(ledger, root / 'provenance/release-allowlist.csv')
    # Build a dependency-only cache with no personal R profile or response data.
    library = root / '.library'
    library.mkdir()
    closure = list(csv.DictReader((ROOT / 'provenance/dependency-closure.csv').open()))
    packages = [
        x
        for x in closure
        if 'rebuild' in x['profiles'].split(';') and x['priority'] != 'base'
    ]
    names = [x['package'] for x in packages]
    expression = (
        '.script_dir<-getwd();source("common/R/bootstrap.R");pkgs<-c('
        + ','.join(json.dumps(x) for x in names)
        + ');for(p in pkgs)cat(p,"\\t",find.package(p),"\\n",sep="")'
    )
    lookup = subprocess.run(
        ['Rscript', '--vanilla', '-e', expression],
        cwd=ROOT,
        text=True,
        capture_output=True,
        check=True,
    )
    found = dict(
        line.split('\t', 1) for line in lookup.stdout.splitlines() if '\t' in line
    )
    for x in packages:
        shutil.copytree(found[x['package']], library / x['package'])

    # Disable personal startup files and point R at the isolated package
    # cache.
    env = os.environ.copy()
    env.update(
        HOME=str(home),
        R_LIBS_USER=str(library),
        R_LIBS_SITE=str(temp / 'empty-site'),
        R_PROFILE_USER='/dev/null',
        R_ENVIRON_USER='/dev/null',
    )
    env.pop('V3_ROOT', None)
    env.pop('R_PROFILE', None)
    env.pop('R_ENVIRON', None)
    # Restrict runtime access to the original user's home after copying dependencies.
    prefix = []
    sandboxed = False
    if shutil.which('sandbox-exec'):
        original_home = Path.home().resolve()
        profile = temp / 'cold.sb'
        escaped = str(original_home).replace('\\', '\\\\').replace('"', '\\"')
        profile.write_text(
            '(version 1)\n(allow default)\n(deny network*)\n(deny file-read* file-write* (subpath "'
            + escaped
            + '"))\n'
        )
        prefix = ['sandbox-exec', '-f', str(profile)]
        sandboxed = True

    # Run the same reader-facing sequence from empty generated-output
    # directories.
    commands = [
        ['Rscript', '--vanilla', '00_setup.R', '--profile', 'rebuild'],
        ['Rscript', '--vanilla', 'exhibits/00_build_all.R'],
        ['Rscript', '--vanilla', 'verify_reproduction.R'],
        ['quarto', 'render', 'docs'],
        ['python3', 'verification/check_book.py'],
    ]
    steps = []
    started = time.time()
    for i, cmd in enumerate(commands, 1):
        log = temp / f'{i:02d}-run.log'
        start = time.time()
        with log.open('w') as f:
            run = subprocess.run(
                prefix + cmd, cwd=root, env=env, stdout=f, stderr=subprocess.STDOUT
            )
        steps.append(
            dict(
                command=' '.join(cmd),
                returncode=run.returncode,
                elapsed_seconds=round(time.time() - start, 3),
                log=log.name,
            )
        )
        print(json.dumps(steps[-1]), flush=True)
        if run.returncode:
            break

    # Record which restrictions were actually available, together with each
    # command result.
    result = dict(
        schema='public-cold-start/1',
        passed=len(steps) == len(commands) and all(x['returncode'] == 0 for x in steps),
        copied_candidate_files=len(rows) + 1,
        ledger_sha256=hashlib.sha256(ledger.read_bytes()).hexdigest(),
        runtime_cache_packages=len(packages),
        personal_profiles_disabled=True,
        original_home_denied_by_os_sandbox=sandboxed,
        network_denied_by_os_sandbox=sandboxed,
        source_install_tested=False,
        platform=os.uname().sysname,
        steps=steps,
        elapsed_seconds=round(time.time() - started, 3),
        workspace=str(temp),
    )
    out = ROOT / 'outputs/verification/cold_start.json'
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result, indent=2))
    raise SystemExit(0 if result['passed'] else 1)


if __name__ == '__main__':
    main()
