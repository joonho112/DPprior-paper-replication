#!/usr/bin/env python3
# Author: JoonHo Lee (jlee296@ua.edu)
# Inspect the exact public candidate
#
# Compare the candidate with its file allowlist, then inspect text, CSV
# fields, PDF contents and image metadata. Reject unexpected files and inspect
# nested archive contents even though archives themselves are not allowed.
#
# Run: python3 verification/disclosure_scan.py CANDIDATE_DIRECTORY
# For the rendered guide, supply docs/_book --site-only.
# The scanner reports problems; it never edits or removes the candidate files.

"""Fail-closed inspection of an exact public candidate; never scan private roots."""
from pathlib import Path
import argparse
import csv
import hashlib
import io
import json
import re
import struct
import subprocess
import tarfile
import zipfile
import zlib

# Text patterns cover personal workstation locations and recognizable
# credential forms.
PATTERNS = {
    'workstation path': re.compile(r'/(?:Users|home)/[A-Za-z0-9][A-Za-z0-9._-]{2,}/'),
    'AWS access key': re.compile(r'(?:AKIA|ASIA)[A-Z0-9]{16}'),
    'GitHub token': re.compile(r'gh[pousr]_[A-Za-z0-9]{30,}'),
    'private key': re.compile(r'-----BEGIN (?:RSA |EC |OPENSSH )?PRIVATE KEY-----'),
    'private network address': re.compile(
        r'(?<![\w.])(?:10\.(?:\d{1,3}\.){2}\d{1,3}|192\.168\.\d{1,3}\.\d{1,3})(?![\w.])'
    ),
}
FORBIDDEN_COLUMNS = {
    'respondent_id',
    'person_id',
    'student_id',
    'pupil_id',
    'original_id',
    'email',
    'ip_address',
    'age',
    'date_of_birth',
}
MAX_BYTES = 80 * 1024 * 1024
TEXT_EXT = {
    '.R',
    '.r',
    '.py',
    '.sh',
    '.md',
    '.qmd',
    '.yml',
    '.yaml',
    '.json',
    '.csv',
    '.tsv',
    '.tex',
    '.cff',
    '.txt',
    '.scss',
    '.css',
    '.js',
    '.html',
    '.svg',
    '.bib',
    '.xml',
    '.map',
    '.bst',
}


def digest(p):
    return hashlib.sha256(p.read_bytes()).hexdigest()


# Inspect known binary containers before treating a payload as ordinary text.
def inspect_bytes(data, name, failures, depth=0):
    if len(data) > MAX_BYTES:
        failures.append(f'Oversized inspection input: {name}')
        return
    if depth > 3:
        failures.append(f'Archive nesting limit: {name}')
        return

    # Inspect ZIP members recursively; do not trust an innocuous outer
    # filename.
    if data.startswith(b'PK\x03\x04'):
        failures.append(f'Archive is not permitted: {name}')
        try:
            with zipfile.ZipFile(io.BytesIO(data)) as z:
                members = z.infolist()
                if len(members) > 2000 or sum(m.file_size for m in members) > MAX_BYTES:
                    raise ValueError('archive size limit')
                for m in members:
                    if m.filename.startswith('/') or '..' in Path(m.filename).parts:
                        failures.append(f'Unsafe archive path: {name}:{m.filename}')
                    if not m.is_dir():
                        inspect_bytes(
                            z.read(m), name + ':' + m.filename, failures, depth + 1
                        )
        except Exception as e:
            failures.append(f'Archive inspection failed: {name}: {type(e).__name__}')
        return

    # Apply the same content and traversal checks to compressed tar archives.
    if data.startswith(b'\x1f\x8b') or (len(data) > 262 and data[257:262] == b'ustar'):
        failures.append(f'Archive is not permitted: {name}')
        try:
            with tarfile.open(fileobj=io.BytesIO(data), mode='r:*') as t:
                members = t.getmembers()
                if len(members) > 2000 or sum(m.size for m in members) > MAX_BYTES:
                    raise ValueError('archive size limit')
                for m in members:
                    if (
                        m.name.startswith('/')
                        or '..' in Path(m.name).parts
                        or m.issym()
                        or m.islnk()
                    ):
                        failures.append(f'Unsafe archive entry: {name}:{m.name}')
                    if m.isfile():
                        inspect_bytes(
                            t.extractfile(m).read(),
                            name + ':' + m.name,
                            failures,
                            depth + 1,
                        )
        except Exception as e:
            failures.append(f'Archive inspection failed: {name}: {type(e).__name__}')
        return
    if data.startswith(b'\x89PNG\r\n\x1a\n'):
        # Inspect textual PNG metadata, including compressed comments.
        pos = 8
        while pos + 12 <= len(data):
            size = struct.unpack('>I', data[pos : pos + 4])[0]
            kind = data[pos + 4 : pos + 8]
            chunk = data[pos + 8 : pos + 8 + size]
            if kind in (b'tEXt', b'iTXt', b'zTXt'):
                try:
                    if kind == b'zTXt':
                        chunk = zlib.decompress(chunk.split(b'\x00', 1)[1][1:])
                    if kind == b'iTXt':
                        fields = chunk.split(b'\x00', 5)
                        if len(fields) == 6 and fields[1] == b'\x01':
                            chunk = zlib.decompress(fields[5])
                    inspect_text(
                        chunk.decode('utf-8', errors='replace'),
                        name + ' metadata',
                        failures,
                    )
                except Exception:
                    failures.append(f'PNG metadata inspection failed: {name}')
            pos += 12 + size
            if kind == b'IEND':
                if pos != len(data):
                    failures.append(f'Trailing PNG payload: {name}')
                break
        return
    inspect_text(data.decode('utf-8', errors='replace'), name, failures)


# Keep pattern detection separate from container parsing so nested content
# uses the same rules.
def inspect_text(text, name, failures):
    for label, pattern in PATTERNS.items():
        if pattern.search(text):
            failures.append(f'{label}: {name}')
    if any(ord(c) < 32 and c not in '\n\r\t' for c in text):
        failures.append(f'Control character in text: {name}')


# Require an exact file set for the repository candidate; rendered-site checks
# use a separate mode.
def scan(root, site_only=False):
    root = Path(root).resolve()
    failures = []
    count = 0
    pdf_count = 0
    csv_count = 0
    if site_only:
        paths = [p for p in root.rglob('*') if p.is_file() or p.is_symlink()]
        expected = None
    else:
        ledger = root / 'provenance/release-allowlist.csv'
        rows = list(csv.DictReader(ledger.open()))
        expected = {x['path']: x for x in rows}
        if len(expected) != len(rows):
            failures.append('Duplicate release ledger path')
        paths = [p for p in root.rglob('*') if p.is_file() or p.is_symlink()]
        actual = {p.relative_to(root).as_posix() for p in paths}
        unlisted = actual - set(expected) - {'provenance/release-allowlist.csv'}
        missing = set(expected) - actual
        failures += ['Unlisted candidate file: ' + x for x in sorted(unlisted)]
        failures += ['Missing candidate file: ' + x for x in sorted(missing)]

    # Read the reviewed column and row-count expectations for public CSV
    # files.
    schemas = {}
    if not site_only:
        schema_path = root / 'provenance/csv-schema-contract.json'
        if schema_path.is_file():
            schemas = json.loads(schema_path.read_text())['files']
        else:
            failures.append('Missing CSV schema contract')
    for p in paths:
        rel = p.relative_to(root).as_posix()
        count += 1
        if p.is_symlink():
            failures.append('Symlink is not permitted: ' + rel)
            continue
        if (
            expected is not None
            and rel in expected
            and digest(p) != expected[rel]['sha256']
        ):
            failures.append('Candidate hash mismatch: ' + rel)
        if any(
            x in p.relative_to(root).parts
            for x in [
                '.git',
                '.library',
                '.private',
                '.staging',
                'log',
                'data-derived',
                'outputs',
                '__pycache__',
            ]
        ):
            failures.append('Private/generated directory in candidate: ' + rel)
        data = p.read_bytes()

        # Inspect PDF metadata and extracted text together.
        if data.startswith(b'%PDF-'):
            pdf_count += 1
            try:
                info = subprocess.run(
                    ['pdfinfo', str(p)], capture_output=True, text=True, check=True
                ).stdout
                content = subprocess.run(
                    ['pdftotext', str(p), '-'],
                    capture_output=True,
                    text=True,
                    check=True,
                ).stdout
                inspect_text(
                    (info + content).replace('\f', '\n'),
                    rel + ' PDF metadata/text',
                    failures,
                )
            except Exception:
                failures.append('PDF inspection unavailable or failed: ' + rel)
        elif (
            (site_only or rel == 'docs/theme/bootstrap-icons.woff')
            and p.suffix.lower() in {'.woff', '.woff2', '.ttf', '.otf'}
            and data[:4] in {b'wOFF', b'wOF2', b'OTTO', b'\x00\x01\x00\x00'}
        ):
            # The source font is one reviewed, hash-checked guide asset.
            # Other source binary fonts remain outside the public allowlist.
            inspect_text(
                ' '.join(x.decode('ascii') for x in re.findall(rb'[ -~]{8,}', data)),
                rel + ' font metadata',
                failures,
            )
        elif (
            p.suffix in TEXT_EXT
            or data.startswith((b'PK\x03\x04', b'\x1f\x8b', b'\x89PNG'))
            or p.name
            in [
                'LICENSE',
                'LICENSE-DATA',
                'DESCRIPTION',
                '.gitignore',
                '.replication-root',
            ]
        ):
            inspect_bytes(data, rel, failures)
        else:
            failures.append('Unreviewed binary/file type: ' + rel)

        # Check the data unit as well as textual content; a known filename
        # does not permit new identifying fields.
        if p.suffix == '.csv':
            csv_count += 1
            try:
                reader = csv.reader(io.StringIO(data.decode('utf-8-sig')))
                columns = next(reader)
                n = sum(1 for _ in reader)
                forbidden = {x.strip().lower() for x in columns} & FORBIDDEN_COLUMNS
                if forbidden:
                    failures.append('Forbidden person column: ' + rel)
                if rel in schemas:
                    if columns != schemas[rel]['columns'] or n != schemas[rel]['rows']:
                        failures.append('CSV schema/row-unit mismatch: ' + rel)
                elif not site_only and rel != 'provenance/release-allowlist.csv':
                    failures.append('Unregistered CSV: ' + rel)
            except Exception:
                failures.append('CSV inspection failed: ' + rel)
    result = dict(
        schema='public-disclosure-scan/1',
        files=count,
        pdfs=pdf_count,
        csvs=csv_count,
        site_only=site_only,
        failures=failures,
        passed=not failures,
    )
    return result


if __name__ == '__main__':
    a = argparse.ArgumentParser()
    a.add_argument('root')
    a.add_argument('--site-only', action='store_true')
    a.add_argument('--receipt')
    args = a.parse_args()
    result = scan(args.root, args.site_only)
    if args.receipt:
        Path(args.receipt).write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result, indent=2))
    raise SystemExit(0 if result['passed'] else 1)
