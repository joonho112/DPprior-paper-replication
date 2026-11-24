#!/usr/bin/env python3
# Author: JoonHo Lee (jlee296@ua.edu)
"""Check source, snapshot and published bytes, current locators, and local HTML links."""
import argparse
import csv
import hashlib
import json
import re
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import urlsplit,unquote
ROOT=Path(__file__).resolve().parents[1]
class Links(HTMLParser):
    def __init__(self):
        super().__init__();self.targets=[];self.ids=set()
    def handle_starttag(self,tag,attrs):
        self.targets.extend(v for k,v in attrs if k in ("href","src") and v)
        self.ids.update(v for k,v in attrs if k=="id" and v)
def check(root=ROOT, current=False):
    docs=root/"docs";book=docs/"_book";errors=[]
    with (root/"provenance/book-assets.csv").open(newline="") as f: records=list(csv.DictReader(f))
    with (root/"provenance/artifact-contract.csv").open(newline="") as f: contract=list(csv.DictReader(f))
    expected=set()
    for row in contract:
        for ext in (("csv","pdf","png") if row["kind"]=="figure" else ("csv","html","tex")):
            expected.add(row["artifact_id"]+"."+ext)
    if len(contract)!=62 or len(expected)!=186:errors.append("Expected 62 manuscript results and three files per result.")
    snapshots={Path(r["destination"]).relative_to("docs") for r in records}
    for folder in (docs,book):
        present={p.relative_to(folder) for area in ("artifacts","records","code") for p in (folder/area).rglob("*") if p.is_file()}
        if present!=snapshots:errors.append(f"Snapshot inventory differs in {folder.name}: {sorted(map(str,present^snapshots))[:6]}")
        if {p.name for p in (folder/"artifacts").iterdir() if p.is_file()}!=expected:errors.append("Unexpected artifact inventory in "+folder.name)
    for r in records:
        src=root/r["source"];copy=root/r["destination"];live=book/copy.relative_to(docs)
        # Distributed snapshots render before any numerical build. Select --current
        # after sync to require byte identity with all newly generated artifacts.
        generated = r["source"].startswith("outputs/")
        compare_source = current or not generated or (src.suffix==".csv" and src.is_file())
        for p in ([src,copy,live] if compare_source else [copy,live]):
            if not p.is_file() or hashlib.sha256(p.read_bytes()).hexdigest()!=r["sha256"]:
                errors.append("Missing or stale asset "+str(p.relative_to(root)))
    pages=sorted(book.glob("*.html")); links=0
    if len(pages)!=9:errors.append("Expected nine rendered chapters.")
    parsed={}
    for p in pages:
        text=p.read_text();parser=Links();parser.feed(text);parsed[p.resolve()]=parser
        if any(ord(c)<32 and c not in "\t\r\n" for c in text):errors.append("Control character in "+p.name)
    fragments=0
    for p,parser in parsed.items():
        for target in parser.targets:
            u=urlsplit(target)
            if u.scheme or u.netloc:continue
            dst=(p.parent/unquote(u.path)).resolve() if u.path else p
            if not dst.is_relative_to(book.resolve()) or not dst.exists():errors.append(f"Broken local link {p.name}: {target}")
            if u.fragment and dst in parsed:
                fragments+=1
                if unquote(u.fragment) not in parsed[dst].ids:errors.append(f"Missing fragment {p.name}: {target}")
            if u.path:links+=1
    record_links=0
    for page in (book/"records").rglob("*.md"):
        for target in re.findall(r"\[[^\]]+\]\(([^)\s]+)\)",page.read_text()):
            u=urlsplit(target)
            if u.scheme or u.netloc or not u.path:continue
            dst=(page.parent/unquote(u.path)).resolve()
            if not dst.is_relative_to(book.resolve()) or not dst.exists():errors.append(f"Broken record link {page.name}: {target}")
            record_links+=1
    result=dict(schema="v104-guide-check/1",artifact_count=len(contract),asset_copies=len(records),chapter_pages=len(pages),
                local_links_checked=links,local_fragments_checked=fragments,record_links_checked=record_links,current_build_required=current,source_snapshot_rendered_byte_parity=(not errors if current else None),failures=errors,passed=not errors)
    out=root/"outputs/verification/book_check.json";out.parent.mkdir(parents=True,exist_ok=True)
    out.write_text(json.dumps(result,indent=2)+"\n")
    if errors:raise RuntimeError("\n".join(errors[:40]))
    print(f"Guide: {len(pages)} chapters, {len(contract)} exhibits, {len(records)} pinned assets, {links} local links.")
    return result
if __name__=="__main__":
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--current",action="store_true",help="Require all source bytes from the synchronized build as well.")
    check(current=parser.parse_args().current)
