# Static replication Book

Author: JoonHo Lee (jlee296@ua.edu)

Run `quarto render docs` with Quarto 1.9.37. The Book has `execute.eval: false` and uses only files inside docs/. It never reads private logs or runs analysis while rendering.

Static artifact and code copies are pinned in `provenance/book-assets.csv`; verification checks that these copies match their public sources. They are readable examples, not an alternative computational input. Regenerate the paper outputs with `Rscript exhibits/00_build_all.R`. Deployment is a separate approval-gated action.
