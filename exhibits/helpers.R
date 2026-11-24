# Author: JoonHo Lee (jlee296@ua.edu)
# Write figures, tables and their numeric sources
#
# Keep unrounded numerical frames separate from their formatted display. Each
# exhibit receives an input/output hash receipt so readers can trace a
# displayed table or figure to the summaries used to build it.
#
# Sourced by the exhibit modules after common bootstrap.
# Writes: outputs/exhibits/ and outputs/exhibit-receipts/.
# LaTeX tables are portable longtable fragments, not standalone documents.

source(rp_path('exhibits', 'style.R', must_exist = TRUE))
suppressPackageStartupMessages(library(patchwork))

# Read frames and verify the frozen exhibit inputs
ex_read <- function(
  track,
  file,
  area = 'data-derived'
) rp_read_csv(rp_path(area, track, file, must_exist = TRUE))
ex_input_check <- function() {
  reg <- jsonlite::read_json(rp_path('provenance', 'exhibit-inputs.json'),
    simplifyVector = TRUE
  )$exports
  for (i in seq_len(nrow(reg))) if (rp_sha256(file.path(.replication_root, reg$path[i])) != reg$sha256[i]) stop('Exhibit input changed: ', reg$path[i])
}
ex_input_check()

# Save the unrounded frame and traceable file hashes
ex_meta <- function(id, files, frame, sources, tier, note = '') {
  rel <- function(p) substring(p, nchar(.replication_root) + 2L)
  rp_write_csv(
    frame,
    rp_output_path(
      'outputs',
      'exhibits',
      paste0(id, '.csv')
    )
  )
  rp_write_json(
    list(
      artifact_id = id, tier = tier, rows = nrow(frame), columns = names(frame),
      inputs = lapply(
        sources,
        function(p) list(path = p, sha256 = rp_sha256(file.path(.replication_root, p)))
      ),
      outputs = lapply(files, function(p) list(path = rel(p), sha256 = rp_sha256(p))), note = note
    ),
    rp_output_path('outputs', 'exhibit-receipts', paste0(id, '.json'))
  )
}

# Render paired PDF/PNG outputs with one numerical source
ex_figure <- function(
  id,
  p,
  frame,
  sources,
  width = 7.5,
  height = 4.5,
  tier = 'rebuilt-from-public-summary',
  note = ''
) {
  out <- save_figure_v3(p,
    id,
    width_in = width,
    height_in = height,
    dir = rp_path('outputs', 'exhibits')
  )
  ex_meta(id, unname(out), frame, sources, tier, note)
}

# Escape table text for HTML and LaTeX
ex_html <- function(x) {
  x <- gsub('&', '&amp;', x, fixed = TRUE)
  x <- gsub('<', '&lt;', x, fixed = TRUE)
  gsub('>', '&gt;', x, fixed = TRUE)
}
ex_tex <- function(x) {
  x <- gsub('\\', 'BACKSLASHTOKEN', x, fixed = TRUE)
  for (k in c('&', '%', '$', '#', '_', '{', '}')) x <- gsub(k, paste0('\\', k), x, fixed = TRUE)
  gsub('BACKSLASHTOKEN', '\\textbackslash{}', x, fixed = TRUE)
}

# Write a readable table and a portable LaTeX fragment
ex_table <- function(
  id,
  frame,
  display = frame,
  sources,
  tier = 'rebuilt-from-public-summary',
  note = ''
) {
  contract <- rp_read_csv(rp_path('provenance', 'artifact-contract.csv'))
  title <- paste(
    if (startsWith(id, 'main')) 'Main' else 'OSM',
    'Table',
    contract$printed_number[match(id, contract$artifact_id)]
  )
  display[] <- lapply(
    display,
    function(x) if (is.numeric(x)) format(x, digits = 6, trim = TRUE) else as.character(x)
  )
  display[] <- lapply(display, function(x) {
    x[is.na(x) | x == 'NA'] <- '--'
    x
  })
  area <- 'exhibits'
  html <- rp_output_path('outputs', area, paste0(id, '.html'))
  tex <- rp_output_path('outputs', area, paste0(id, '.tex'))
  dir.create(dirname(html), recursive = TRUE, showWarnings = FALSE)
  head <- paste0('<th>', ex_html(names(display)), '</th>', collapse = '')
  body <- apply(
    display,
    1,
    function(x) paste0('<tr>', paste0('<td>', ex_html(x), '</td>', collapse = ''), '</tr>')
  )
  writeLines(
    c(
      '<!doctype html><html lang="en"><meta charset="utf-8"><meta name="author" content="JoonHo Lee (jlee296@ua.edu)"><meta name="viewport" content="width=device-width, initial-scale=1">',
      paste0('<title>', title, '</title>'),
      '<style>body{font:16px/1.5 system-ui,sans-serif;color:#243040;margin:2rem auto;max-width:1100px;padding:0 1rem}table{border-collapse:collapse;width:100%;font-size:14px}th,td{padding:10px 12px;border-bottom:1px solid #dde3e9;text-align:left}th{background:#eef3f8}tr:nth-child(even){background:#f8fafc}.scroll{overflow-x:auto}p{max-width:85ch}</style>',
      paste0(
        '<h1>',
        title,
        '</h1><p>',
        ex_html(note),
        '</p><div class="scroll"><table><thead><tr>',
        head,
        '</tr></thead><tbody>'
      ),
      body,
      '</tbody></table></div></html>'
    ),
    html
  )
  # A portable longtable fragment, for users who want to typeset the rebuilt table.
  writeLines(c(
    '% Author: JoonHo Lee (jlee296@ua.edu)',
    '% Requires booktabs and longtable. Numeric source is the adjacent CSV.',
    paste0('\\begin{longtable}{', paste(rep('l', ncol(display)), collapse = ''), '}'),
    '\\toprule', paste0(paste(ex_tex(names(display)), collapse = ' & '), ' \\\\'),
    '\\midrule\\endhead',
    apply(
      display,
      1,
      function(x) paste0(paste(ex_tex(x), collapse = ' & '), ' \\\\ \\addlinespace[3pt]')
    ),
    '\\bottomrule\\end{longtable}'
  ), tex)
  ex_meta(id, c(html, tex), frame, sources, tier, note)
}

# Integrate count moments over the Gamma hyperprior
#
# Conditional mean and variance follow the finite-J CRP count law. The
# marginal variance uses E[var(K | alpha)] + var[E(K | alpha)].
ex_moments <- function(J, a, b) {
  lo <- qgamma(1e-13, a, rate = b)
  hi <- qgamma(1 - 1e-13, a, rate = b)
  m <- function(x) x * (digamma(x + J) - digamma(x))
  v <- function(x) m(x) + x * x * (trigamma(x + J) - trigamma(x))
  integ <- function(f) integrate(function(x) f(x) * dgamma(x, a, rate = b),
    lo,
    hi,
    rel.tol = 1e-9,
    subdivisions = 2000
  )$value
  mean <- integ(m)
  c(mean = mean, var = integ(function(x) v(x) + m(x)^2) - mean^2)
}

# Closed-form size-biased tail and display formatting
#
# Gamma parameters are shape a and rate b. Formatting helpers are used only
# for labels; numerical frames keep full precision.
ex_tail <- function(a, b, t) (b / (b - log1p(-t)))^a
ex_f <- function(x, d = 3) ifelse(is.na(x), '--', sprintf(paste0('%.', d, 'f'), x))
ex_ci <- function(m, l, u, d = 2) paste0(ex_f(m, d), ' [', ex_f(l, d), ', ', ex_f(u, d), ']')

# Keep prior labels and order consistent across applications
ex_roles <- c(
  'Vague',
  'DORO-Unif',
  'Literature default',
  'TSMM',
  'Dual-Anchor',
  'Dual-Anchor (hard)',
  'SSI-Weight',
  'Count 25'
)
ex_role <- function(x) factor(x, levels = ex_roles)
ex_rasch_labels <- c(
  vague = 'Vague',
  litdef = 'Literature default',
  tsmm = 'TSMM',
  dasoft = 'Dual-Anchor',
  dahard = 'Dual-Anchor (hard)'
)
ex_rasch_role <- function(x) ifelse(x$arm == 'sens', 'Count 25', unname(ex_rasch_labels[x$role]))
