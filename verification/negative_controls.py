#!/usr/bin/env python3
# Author: JoonHo Lee (jlee296@ua.edu)
# Check that incorrect inputs are detected
#
# Alter one disposable copy at a time, run the relevant checker and restore
# the original bytes. A parse failure or missing dependency does not count as
# successfully detecting a numerical error.
#
# Run: python3 verification/negative_controls.py
# Uses an exact temporary public copy; writes
# outputs/verification/negative_controls.json.
# The clean-copy control must pass before deliberate disclosure errors are
# tested.

"""Deliberately corrupt disposable public copies; a failed computation is required."""
from pathlib import Path
import csv
import io
import json
import os
import shutil
import subprocess
import tempfile
import zipfile
from disclosure_scan import scan

ROOT = Path(__file__).resolve().parents[1]


# Create a disposable copy from the reviewed file list, not from a recursive
# directory copy.
def copy_candidate(destination):
    for r in csv.DictReader((ROOT / 'provenance/release-allowlist.csv').open()):
        p = destination / r['path']
        p.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(ROOT / r['path'], p)
    shutil.copyfile(
        ROOT / 'provenance/release-allowlist.csv',
        destination / 'provenance/release-allowlist.csv',
    )


# Apply one deliberate table alteration while preserving its field order.
def mutate_csv(path, fn):
    rows = list(csv.DictReader(path.open()))
    fields = list(rows[0])
    fn(rows)
    with path.open('w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=fields)
        w.writeheader()
        w.writerows(rows)


def main():
    results = []
    with tempfile.TemporaryDirectory(prefix='dpprior-negative-') as temp:
        root = Path(temp)
        copy_candidate(root)

        # Numerical failures: each altered input should be rejected for a data
        # or hash reason.
        cases = [
            (
                'selected lambda',
                'data-frozen/calibration/dual_anchor_grid.csv',
                [
                    'Rscript',
                    '-e',
                    '.script_dir<-getwd();source("common/R/bootstrap.R");source("exhibits/helpers.R")',
                ],
                lambda p: mutate_csv(p, lambda rows: rows[0].update(selected='TRUE')),
            ),
            (
                'score sample size',
                'data-frozen/rasch/score_counts.csv',
                ['Rscript', 'rasch/01_wle.R'],
                lambda p: mutate_csv(p, lambda rows: rows[0].update(n='4')),
            ),
            (
                'fixed item order',
                'data-frozen/rasch/wle_item_difficulties.csv',
                ['Rscript', 'rasch/01_wle.R'],
                lambda p: mutate_csv(p, lambda rows: rows.reverse()),
            ),
            (
                'fit denominator',
                'data-frozen/simulation/fit_status.csv',
                ['Rscript', 'simulation/01_reaggregate.R'],
                lambda p: mutate_csv(
                    p, lambda rows: rows[0].update(scientific_eligible='FALSE')
                ),
            ),
            (
                'missing required metrics',
                'data-frozen/simulation/fit_metrics.csv',
                ['Rscript', 'simulation/01_reaggregate.R'],
                lambda p: p.unlink(),
            ),
        ]
        for label, rel, command, change in cases:
            p = root / rel
            old = p.read_bytes()
            change(p)
            run = subprocess.run(
                command, cwd=root, text=True, capture_output=True, timeout=180
            )
            # Do not count a parse error or an absent dependency as a detected data error.
            message = run.stdout + run.stderr
            detected = run.returncode != 0 and any(
                s in message.lower()
                for s in [
                    'changed',
                    'hash',
                    'does not exist',
                    'not identical',
                    'file.exists',
                    'fingerprint',
                    'source mismatch',
                ]
            )
            results.append(
                dict(
                    test=label,
                    kind='numerical-input',
                    detected=detected,
                    returncode=run.returncode,
                )
            )
            p.write_bytes(old)
        # Remove only files created by these disposable checks before candidate scans.
        for name in ['outputs', 'data-derived']:
            if (root / name).exists():
                shutil.rmtree(root / name)
        base = scan(root)
        results.append(
            dict(
                test='clean candidate',
                kind='disclosure',
                detected=base['passed'],
                returncode=0,
            )
        )

        # Restore each disclosure trial after checking the specific reason for
        # rejection.
        def trial(label, path, data, reason):
            p = root / path
            exists = p.exists()
            old = p.read_bytes() if exists else None
            p.parent.mkdir(parents=True, exist_ok=True)
            p.write_bytes(data)
            res = scan(root)
            detected = not res['passed'] and any(reason in x for x in res['failures'])
            results.append(
                dict(
                    test=label,
                    kind='disclosure',
                    detected=detected,
                    returncode=1 if not res['passed'] else 0,
                )
            )
            if exists:
                p.write_bytes(old)
            else:
                p.unlink()

        # Disclosure failures: exercise unexpected files, identifying fields
        # and hidden contents.
        trial('unlisted payload', 'extra.txt', b'unlisted benign content', 'Unlisted')
        trial(
            'private path',
            'README.md',
            (
                'reference '
                + '/'
                + 'Users/'
                + 'fictional_reviewer/Documents/private.txt'
            ).encode(),
            'workstation path',
        )
        trial(
            'forbidden person field',
            'data-frozen/rasch/score_counts.csv',
            b'raw_score,n,person_id\n0,3,example\n',
            'Forbidden person column',
        )
        nested = io.BytesIO()
        with zipfile.ZipFile(nested, 'w') as z:
            z.writestr(
                'hidden.txt',
                ('/' + 'Users/' + 'fictional_reviewer/Documents/hidden').encode(),
            )
        outer = io.BytesIO()
        with zipfile.ZipFile(outer, 'w') as z:
            z.writestr('inner.zip', nested.getvalue())
        trial(
            'nested archive',
            'opaque.dat',
            outer.getvalue(),
            'workstation path: opaque.dat:inner.zip:hidden.txt',
        )
        link = root / 'escape-link'
        link.symlink_to(root.parent)
        res = scan(root)
        results.append(
            dict(
                test='escaping symlink',
                kind='disclosure',
                detected=any('Symlink' in x for x in res['failures']),
                returncode=1,
            )
        )
        link.unlink()

    # Report every case so a failed negative control is visible individually.
    out = ROOT / 'outputs/verification/negative_controls.json'
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(
        json.dumps(
            dict(tests=results, passed=all(x['detected'] for x in results)), indent=2
        )
        + '\n'
    )
    print(out.read_text())
    return 0 if all(x['detected'] for x in results) else 1


if __name__ == '__main__':
    raise SystemExit(main())
