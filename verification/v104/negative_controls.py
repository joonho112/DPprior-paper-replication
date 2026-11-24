#!/usr/bin/env python3
# Author: JoonHo Lee (jlee296@ua.edu)
"""Change disposable copies of numerical data and require a specific verification failure."""
import argparse
import csv
import hashlib
import json
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def change_csv(path, transform):
    with path.open(newline='') as stream:
        reader = csv.DictReader(stream)
        columns = reader.fieldnames
        rows = list(reader)
    transform(rows)
    with path.open('w', newline='') as stream:
        writer = csv.DictWriter(stream, columns)
        writer.writeheader()
        writer.writerows(rows)


def swap_fit_keys(rows):
    # These Gamma fits belong to different generated data sets in the same design.
    # Swapping identifiers preserves fit counts and every numerical field.
    selected = [r for r in rows if r['K_true'] == '5' and r['I_target'] == '0.2'
                and r['prior'] == 'Vague' and int(r['replicate']) in (1, 2)]
    if len(selected) != 2 or selected[0]['dgp_sha256'] == selected[1]['dgp_sha256']:
        raise ValueError('Expected two distinct datasets for the identity control.')
    a, b = selected
    a['fit_key'], b['fit_key'] = b['fit_key'], a['fit_key']


def shift_first(field, difference):
    def mutate(rows):
        rows[0][field] = str(float(rows[0][field]) + difference)
    return mutate


def reverse_sign(rows):
    row = next(r for r in rows if r['group'] == 'upper' and float(r['delta_displacement']) != 0)
    row['delta_displacement'] = str(-float(row['delta_displacement']))


CASES = [
    ('fit-key swap within a five-prior design', 'simulation',
     'data-derived/v104/simulation-fit-metrics.csv', swap_fit_keys,
     r'^Simulation verification failed: same-data memberships .* sha256: expected'),
    ('wrong two-prior convergence denominator', 'simulation',
     'data-derived/v104/simulation-pair-convergence.csv', shift_first('n_paired', -1),
     r'^Simulation verification failed: two-prior denominators .* n_paired: expected'),
    ('wrong Table 3 attempted denominator', 'simulation',
     'outputs/v104/main-table-3.csv', shift_first('n_manifest', 1),
     r'^Simulation verification failed: main-table-3 .* n_manifest: expected'),
    ('reversed upper-unit sign in Figure 6', 'applications',
     'outputs/v104/main-figure-6.csv', reverse_sign,
     r'^Application verification failed: Figure 6 points .* delta_displacement: expected'),
    ('one Rasch score group missing', 'applications',
     'data-frozen/rasch/score_counts.csv', lambda rows: rows.pop(0),
     r'^Application verification failed: Rasch score frequency is missing, duplicated or changed$'),
    ('modified frozen posterior spread', 'input integrity',
     'data-frozen/simulation104/posterior-spread.csv', shift_first('posterior_mean_sd', .0001),
     r'^ValueError: Frozen input bytes changed: posterior-spread\.csv$'),
]


def run_suite(root):
    with tempfile.TemporaryDirectory(prefix='replication 104 negative ') as tmp:
        copy = Path(tmp)
        # Ordinary file copies keep every write in this temporary directory.
        for name in ('data-derived/v104', 'outputs/v104', 'verification/expected/v104',
                     'data-frozen/simulation104'):
            shutil.copytree(root/name, copy/name)
        for name in ('data-frozen/rasch/score_counts.csv',
                     'data-frozen/applications104/chain-mean-by-unit-or-score.csv',
                     'simulation/v104/check_inputs.py', 'simulation/v104/input-schema.json'):
            (copy/name).parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(root/name, copy/name)
        commands = {label: [sys.executable, '-B', str(root/'verification/v104'/f'{label}.py'),
                            '--root', str(copy)] for label in ('simulation', 'applications')}
        commands['input integrity'] = [sys.executable, '-B', str(copy/'simulation/v104/check_inputs.py')]

        def run(label):
            return subprocess.run(commands[label], cwd=copy, capture_output=True, text=True)

        def clean(label):
            result = run(label)
            if result.returncode:
                raise ValueError(f'Clean {label} verifier failed: {result.stderr.strip()}')

        for label in commands:
            clean(label)
        results = []
        for case, label, relative, mutate, expected in CASES:
            file = copy/relative
            original = (root/relative)
            if file.samefile(original) or sha(file) != sha(original):
                raise ValueError(f'Fixture isolation failed: {relative}')
            try:
                change_csv(file, mutate)
                if sha(file) == sha(original):
                    raise ValueError(f'Mutation had no effect: {case}')
                result = run(label)
                reason = next((line for line in result.stderr.splitlines()
                               if re.search(expected, line)), None)
                if result.returncode != 1 or reason is None:
                    raise ValueError(f'{case}: expected the designated verification failure, '
                                     f'got exit {result.returncode}: {result.stderr.strip()}')
                results.append(dict(case=case, verifier=label, mutated_file=relative,
                                    exit_code=result.returncode, detected_reason=reason))
            finally:
                shutil.copyfile(original, file)
            if sha(file) != sha(original):
                raise ValueError(f'Fixture restoration failed: {case}')
            clean(label)
        return dict(clean_before=True, mutations=results, restored_clean_controls=len(results),
                    expected_failures=len(CASES), passed=True)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=Path(__file__).resolve().parents[2])
    root = parser.parse_args().root.resolve()
    report = run_suite(root)
    destination = root/'outputs/verification/v104-negative.json'
    destination.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile('w', dir=destination.parent, delete=False) as temp:
        json.dump(report, temp, indent=2)
        temp.write('\n')
    Path(temp.name).replace(destination)
    print(f"{len(report['mutations'])} targeted mutations detected; all restored controls passed.")
