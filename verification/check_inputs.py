#!/usr/bin/env python3
# Author: JoonHo Lee (jlee296@ua.edu)
"""Verify the contents of the frozen input tree before any calculation."""
from pathlib import Path
import csv
import hashlib

ROOT = Path(__file__).resolve().parents[1]

def verify(root=ROOT):
    inventory = root / 'provenance/frozen-inputs.csv'
    with inventory.open(encoding='utf-8', newline='') as stream:
        entries = list(csv.DictReader(stream))
    indexed = {row['path'] for row in entries}
    actual = {
        p.relative_to(root).as_posix() for p in (root/'data-frozen').rglob('*')
        if p.is_file() and not p.name.startswith('.')
    }
    if len(entries) != len(indexed) or indexed != actual:
        raise ValueError(f'Frozen inventory differs: missing {sorted(indexed-actual)}; unexpected {sorted(actual-indexed)}')
    for row in entries:
        path = root / row['path']
        if path.is_symlink() or path.stat().st_size != int(row['bytes']):
            raise ValueError(f'Invalid file or length: {path.name}')
        with path.open('rb') as stream:
            checksum = hashlib.sha256()
            for chunk in iter(lambda: stream.read(1024 * 1024), b''):
                checksum.update(chunk)
            digest = checksum.hexdigest()
        if digest != row['sha256']:
            raise ValueError(f'Frozen input checksum differs: {row["path"]}')
    return len(entries)

if __name__ == '__main__':
    print(f'Verified {verify()} frozen inputs.')
