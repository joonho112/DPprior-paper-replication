#!/usr/bin/env python3
# Author: JoonHo Lee (jlee296@ua.edu)
# Refresh the static guide after verification
#
# Copy verified exhibit outputs and current public code into the guide. The
# Book can then render without executing R. This changes distributed examples
# and hashes, so use it only after the numerical build passes.
#
# Run: python3 docs/sync_assets.py --execute
# Reads output receipts; writes docs/artifacts/, docs/code/, docs/records/ and
# the asset map.

"""Explicitly refresh static guide examples after a verified exhibit build.

This maintainer command changes the candidate and its hashes. It does not run
analysis, render the Book, or publish anything. Review and re-scan after use.
"""
from pathlib import Path
import argparse
import csv
import hashlib
import json
import shutil

ROOT = Path(__file__).resolve().parents[1]


# Write records with their existing column order.
def write_csv(path, rows):
    with path.open('w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=rows[0])
        w.writeheader()
        w.writerows(rows)


# Require an explicit refresh and a passed numerical build.
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--execute', action='store_true')
    args = parser.parse_args()
    if not args.execute:
        print(
            'Plan: refresh reviewed static artifacts, code/record copies, reproduction map and index. Add --execute after verify_reproduction.R passes.'
        )
        return
    proof = json.loads((ROOT / 'outputs/verification/reproduction.json').read_text())
    if not proof['passed']:
        raise RuntimeError('Independent verification has not passed')
    mapping = list(csv.DictReader((ROOT / 'provenance/reproduction-map.csv').open()))
    for row in mapping:
        x = json.loads(
            (
                ROOT / 'outputs/exhibit-receipts' / f"{row['artifact_id']}.json"
            ).read_text()
        )
        for output in x['outputs']:
            if (
                hashlib.sha256((ROOT / output['path']).read_bytes()).hexdigest()
                != output['sha256']
            ):
                raise RuntimeError('Exhibit changed after its receipt')
        row.update(
            inputs=';'.join(z['path'] for z in x['inputs']),
            outputs=';'.join(z['path'] for z in x['outputs']),
            tier=x['tier'],
            note=x['note'],
        )
    write_csv(ROOT / 'provenance/reproduction-map.csv', mapping)

    # Copy the reviewed examples and their source scripts.
    copies = []

    def copy(src, dst):
        dst.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(src, dst)
        copies.append(
            dict(
                source=src.relative_to(ROOT).as_posix(),
                destination=dst.relative_to(ROOT).as_posix(),
                sha256=hashlib.sha256(dst.read_bytes()).hexdigest(),
            )
        )

    d = ROOT / 'docs'
    # Existing snapshots are retained unless they are one of the contracted outputs.
    for area in ['main', 'osm']:
        for p in (ROOT / 'outputs' / area).iterdir():
            if p.suffix in ['.png', '.pdf', '.csv', '.html', '.tex']:
                copy(p, d / 'artifacts' / p.name)
    for area in [
        'common',
        'calibration',
        'simulation',
        'applications',
        'rasch',
        'refit',
        'verification',
        'exhibits',
    ]:
        for p in (ROOT / area).rglob('*.R'):
            copy(p, d / 'code' / p.relative_to(ROOT))
    copy(ROOT / 'verify_reproduction.R', d / 'code/verify_reproduction.R')
    for src, name in [
        ('provenance/reproduction-map.csv', 'reproduction-map.csv'),
        ('provenance/software-lock.csv', 'software-lock.csv'),
        ('provenance/data-rights.csv', 'data-rights.csv'),
        ('provenance/manuscript-discrepancies.csv', 'manuscript-discrepancies.csv'),
        ('provenance/refit-status.csv', 'refit-status.csv'),
        ('data-frozen/rasch/prior_and_fit_registry.csv', 'rasch-fit-registry.csv'),
        ('calibration/policy.yml', 'calibration-policy.yml'),
        ('DATA_ACCESS.md', 'DATA_ACCESS.md'),
        ('LICENSE-SCOPE.md', 'LICENSE-SCOPE.md'),
        ('verification/refit-smoke.md', 'refit-smoke.md'),
    ]:
        copy(ROOT / src, d / 'records' / name)
    write_csv(ROOT / 'provenance/book-assets.csv', copies)

    # Build the artifact index from the same mapping used for the copies.
    lines = [
        '# Tables and figures',
        '',
        'The index links each table and figure to its numerical data and R script. The previews are included with the package; the CSV files retain unrounded values.',
        '',
        '| Result | Preview | Data | R script | Calculation |',
        '|---|---|---|---|---|',
    ]
    tier_labels = {
        'rebuilt-from-public-summary': 'From supplied summaries',
        'conceptual-specification': 'Conceptual summary',
        'design-specification': 'Specified design',
        'frozen-design-input': 'Specified design',
        'independently-recomputed-WLE': 'WLE calculation',
        'rebuilt-from-verified-calibration': 'From calibrated priors',
        'recomputed-closed-forms': 'Closed-form calculation',
    }
    for x in mapping:
        doc_label = 'Main' if x['document'] == 'main' else 'OSM'
        id = x['artifact_id']
        extension = 'png' if x['kind'] == 'figure' else 'html'
        lines.append(
            f"| {doc_label} {x['kind'].title()} {x['printed_number']} | [View](artifacts/{id}.{extension}) | [CSV](artifacts/{id}.csv) | [Source](code/{x['builder']}) | {tier_labels[x['tier']]} |"
        )
    lines += [
        '',
        '## Formats and verification',
        '',
        'Each figure also has a PDF, and each table has a LaTeX fragment. The LaTeX files use `booktabs` and `longtable`. Formatting is adapted for reading in the guide.',
        '',
        'Run `Rscript exhibits/00_build_all.R` to rebuild the results, followed by `Rscript verify_reproduction.R` to check them. The [reproduction map](records/reproduction-map.csv) gives the input and output paths; selected numerical results are written to `outputs/key_numbers.csv`.',
        '',
        'The comparisons use the numerical data and figure geometry. PDF creation metadata can change between runs. The [verification chapter](07-verification-refit.qmd) describes the checks and the Table 2 rounding difference.',
    ]
    (d / '08-artifact-index.qmd').write_text('\n'.join(lines) + '\n')
    print(
        f'Refreshed {len(copies)} static copies and {len(mapping)} artifact entries; publication approval is unchanged.'
    )


if __name__ == '__main__':
    main()
