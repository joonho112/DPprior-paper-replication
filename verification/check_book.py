#!/usr/bin/env python3
# Author: JoonHo Lee (jlee296@ua.edu)
# Check the rendered guide
#
# Check the static examples, copied source code and local links separately. A
# newly rendered PDF may have different timestamp bytes, but the distributed
# example and every copied source file must match their recorded hashes.
#
# Run after quarto render docs: python3 verification/check_book.py
# Writes: outputs/verification/book_check.json. This is not browser visual
# inspection.

"""Check static Book assets, local links and control characters; no browser claim."""
from pathlib import Path
from html.parser import HTMLParser
from urllib.parse import urlsplit, unquote
import csv
import hashlib
import json

ROOT = Path(__file__).resolve().parents[1]


# Collect only link and image targets; this is structural HTML inspection.
class Links(HTMLParser):
    def __init__(self):
        super().__init__()
        self.links = []

    def handle_starttag(self, tag, attrs):
        for key, value in attrs:
            if key in ('href', 'src') and value:
                self.links.append(value)


# Check the distributed assets first, then resolve local links in the rendered
# Book.
def check(root=ROOT):
    failures = []
    rows = list(csv.DictReader((root / 'provenance/book-assets.csv').open()))
    for x in rows:
        # Generated exhibits are fixed Book snapshots. Rebuilt PDF bytes include
        # timestamps, so only authored code/record copies require source byte identity.
        keys = (
            ('destination',)
            if x['source'].startswith('outputs/')
            else ('source', 'destination')
        )
        for key in keys:
            p = root / x[key]
            if (
                not p.is_file()
                or hashlib.sha256(p.read_bytes()).hexdigest() != x['sha256']
            ):
                failures.append(f'Asset drift: {key} {x[key]}')

    # The Book must stand on its own: no link may escape its rendered
    # directory.
    book = root / 'docs/_book'
    pages = list(book.glob('*.html'))
    if len(pages) != 9:
        failures.append(f'Expected nine rendered chapters, got {len(pages)}')
    links = 0
    for p in pages:
        text = p.read_text()
        parser = Links()
        parser.feed(text)
        if any(ord(c) < 32 and c not in '\t\n\r' for c in text):
            failures.append(f'Control character: {p.name}')
        for href in parser.links:
            u = urlsplit(href)
            if u.scheme or u.netloc or not u.path:
                continue
            path = (p.parent / unquote(u.path)).resolve()
            if not path.is_relative_to(book.resolve()):
                failures.append(f'Link escapes Book: {p.name}: {href}')
            elif not path.exists():
                failures.append(f'Missing local link: {p.name}: {href}')
            links += 1
    for p in (root / 'docs').glob('*.qmd'):
        if any(ord(c) < 32 and c not in '\t\n\r' for c in p.read_text()):
            failures.append(f'Control character: {p.name}')

    # Write a machine-readable result without claiming browser visual review.
    result = dict(
        schema='static-book-check/1',
        asset_copies=len(rows),
        chapter_pages=len(pages),
        local_links_checked=links,
        failures=failures,
        passed=not failures,
        browser_visual_inspection=False,
    )
    out = root / 'outputs/verification/book_check.json'
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(json.dumps(result, indent=2) + '\n')
    if failures:
        raise RuntimeError('\n'.join(failures))
    print(
        f'Book: {len(pages)} chapters, {len(rows)} pinned assets and source-matched code/records, {links} local links checked.'
    )
    return result


if __name__ == '__main__':
    check()
