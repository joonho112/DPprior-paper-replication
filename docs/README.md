# Replication guide

Author: JoonHo Lee (jlee296@ua.edu)

Open [Tables and figures](results.html) in a browser to read the tables, view figures, or download their numerical data. To read the full guide, run `quarto render docs` and open `docs/_book/index.html`. The source chapters, code examples, numerical frames and exhibits under `docs/` are sufficient for rendering.

After the numerical build and `Rscript verify_reproduction.R` pass, run `python3 docs/sync_assets.py --execute`, render again, and use `python3 verification/check_book.py --current` to verify every byte against the new outputs. `provenance/book-assets.csv` records the source file, snapshot and hash.

To refresh the standalone results index after changing the exhibit list, run this command from the repository root.

~~~sh
quarto pandoc docs/08-artifact-index.qmd --from markdown --to html5 --standalone --metadata "pagetitle=Tables and figures | DPprior replication" --metadata "author=JoonHo Lee (jlee296@ua.edu)" --metadata lang=en --variable maxwidth=68em --output docs/results.html
~~~
