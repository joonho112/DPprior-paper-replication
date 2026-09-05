#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Compare the placement of a dominant cluster
#
# The dominant cluster contains exactly .60 of the generated units. Compare
# its central and balanced-random locations while preserving that allocation;
# this is a data-generating share, not a prior tail probability.
#
# Run after 01_reaggregate.R: Rscript simulation/05_unequal_weights.R
# Writes: data-derived/simulation/unequal_weights.csv.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
source(file.path(.script_dir, 'aggregation.R'))

# Read the condition summaries
cells <- rp_read_csv(rp_path('data-derived', 'simulation', 'cell_summary.csv', must_exist = TRUE))

# Check the exact dominant allocation before comparison
result <- simv2_synthesis_dominant_location(cells)
stopifnot(
  all(result$weight_scheme == 'dominant_exact'), all(result$dominant_share == .6),
  all(result$J * result$dominant_share == round(result$J * result$dominant_share))
)
result$specified_dominant_count <- as.integer(result$J * result$dominant_share)

# Save both location schemes with their specified counts
rp_write_csv(result, rp_output_path('data-derived', 'simulation', 'unequal_weights.csv'))
cat(nrow(result), 'dominant-location cells; exact count specification recorded.\n')
