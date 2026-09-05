# Release disclosure checks

Author: JoonHo Lee (jlee296@ua.edu)

The release checker must inspect a concrete candidate, not assume that `.gitignore` guarantees privacy.

## Inputs and fail-closed rules

- `provenance/release-allowlist.csv` lists each exact package-relative file, artifact class, rights basis, review status, and content hash. There are no directory wildcards. Generated Book resources use a separate exact build manifest.
- A candidate fails if an unlisted file is staged, tracked for the new release, archived, or deployed to Pages. Draft ledger rows are not release approvals. A file changed after review must be reviewed and rehashed.
- Reject absolute workstation paths, credentials/private keys, private network/storage/account configuration, private logs, response/person data, and unapproved binary containers. Preserve legitimate author citations and software attribution.
- Reject symlinks leaving the candidate, path traversal entries, hidden payloads, and archive entries outside the root. Never extract untrusted archives into the working tree.
- Archives are denied by default; any approved archive needs recursive file-type detection and entry inspection with depth/size limits. Inspection failure is failure, not a skipped PASS.
- For CSV/TSV, check actual columns and rows against dataset-specific allowlists. Scan text fields and deny person-level records even if IDs are absent. Mixed application files must use approved data units for all applications, including OCRS. Accept approved public-use school/model derivatives; reject unnecessary student/person rows and identifiers.
- Inspect rendered HTML, JSON search indexes, YAML front matter, downloadable source files, and PDF/image metadata. Look for private paths in URLs, attributes, comments, embedded resources, and code snippets.

## Necessary negative controls

Use isolated temporary copies to insert an unlisted file, a forbidden column, an absolute path, a nested archive containing a disallowed file, and an escaping symlink. Each must fail. Also show that legitimate author names, citations and approved score-group summaries pass. Remove temporary fixtures after the test; do not commit them or weaken the oracle to obtain a PASS.

## Evidence

Record the inspected commit/tree, ledger hash, checker version, input file count, actual failures, and archive/Book hashes. A local scan, a clean-clone rebuild, and a live Pages check are distinct results. The checker writes its results for the inspected files. See `provenance/validation-summary.json` for the recorded checks.
