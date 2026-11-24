# Frozen input codebook

Author: JoonHo Lee (jlee296@ua.edu)

Frozen files are immutable computational inputs. Build scripts write to `data-derived/` and `outputs/`; they do not update this directory or `verification/expected/`.

Each track adds a codebook with: exact filename; observation unit and keys; column names, meanings and units; allowed missingness and failure codes; source fingerprint and derivation; public-release unit and rights; and which current artifact uses it. The export receipt records any removed fields. Original private paths are not data provenance columns.

Synthetic fit metrics retain condition, replication, seed and diagnostic status so failed or unattempted fits remain in denominators. Rasch rows are fit/item/score groups, not persons. OCRS uses public-use data: necessary school/model derivatives can be exported, and the source-to-input calculation is checked separately from frozen posterior rebuilding and optional refitting.

The November additions and their units are described in the [input guide](../provenance/104-inputs.md). The file-by-file inventory is [frozen-inputs.csv](../provenance/frozen-inputs.csv).
