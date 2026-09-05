#!/usr/bin/env python3
# Author: JoonHo Lee (jlee296@ua.edu)
# Keep the guide's icon stylesheet limited to icons used by its pages.
#
# Quarto runs this after rendering. Scan the rendered HTML and supporting
# scripts for explicit icon classes, then remove unused glyph selectors.
# Preserve the upstream license notice, font declaration and shared styles.
# This only edits docs/_book/; numerical outputs and source figures are untouched.

from pathlib import Path
import hashlib
import json
import re
import shutil

BOOK = Path(__file__).resolve().parent / '_book'
ICON_RULE = re.compile(r'\.bi-([\w-]+)::before\s*\{[^}]*\}')
ICON_REFERENCE = re.compile(r'\bbi-([\w-]+)')


def main():
    # Read every rendered page and supporting text asset before pruning.
    stylesheets = list(BOOK.rglob('bootstrap-icons.css'))
    if len(stylesheets) != 1:
        raise RuntimeError('Expected one Bootstrap icon stylesheet.')

    stylesheet = stylesheets[0]
    required = set()
    for path in BOOK.rglob('*'):
        if path == stylesheet or path.suffix not in {'.html', '.js', '.json', '.css'}:
            continue
        if path.is_file():
            required.update(ICON_REFERENCE.findall(path.read_text()))

    original = stylesheet.read_text()
    available = set(ICON_RULE.findall(original))
    missing = required - available
    if missing:
        raise RuntimeError('Referenced icons are missing: ' + ', '.join(sorted(missing)))

    # Retain the complete rule for each required glyph, including its code point.
    cleaned = ICON_RULE.sub(
        lambda match: match.group(0) if match.group(1) in required else '',
        original,
    )
    cleaned = re.sub(r'\n{3,}', '\n\n', cleaned)
    if set(ICON_RULE.findall(cleaned)) != required:
        raise RuntimeError('Icon selection differs from the rendered references.')

    # The reviewed font contains the same outlines for the required icons.
    # Fail if a theme update adds an icon that is absent from this subset.
    theme = Path(__file__).resolve().parent / 'theme'
    subset = json.loads((theme / 'bootstrap-icons.json').read_text())
    if required != set(subset['icons']):
        raise RuntimeError('Guide icon usage changed; review the font subset.')
    font = theme / 'bootstrap-icons.woff'
    if hashlib.sha256(font.read_bytes()).hexdigest() != subset['sha256']:
        raise RuntimeError('The reviewed guide font has changed.')
    for name, codepoint in subset['icons'].items():
        rule = re.search(r'\.bi-' + re.escape(name) + r'::before\s*\{[^}]*\}', cleaned)
        if not rule or ('\\' + codepoint.lower()) not in rule.group(0).lower():
            raise RuntimeError('Icon code point changed: ' + name)

    stylesheet.write_text(cleaned)
    shutil.copyfile(font, stylesheet.with_suffix('.woff'))
    shutil.copyfile(
        theme / 'bootstrap-icons-LICENSE.txt',
        stylesheet.parent / 'bootstrap-icons-LICENSE.txt',
    )
    print(f'Guide icons: retained {len(required)} referenced selectors.')


if __name__ == '__main__':
    main()
