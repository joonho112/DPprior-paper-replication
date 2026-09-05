#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Prepare STAR and writing-to-learn source data
#
# Check the STAR kindergarten sample and rebuild the 48-study writing-to-learn
# input. STAR school contrasts are estimated in the next script.
#
# Run: Rscript applications/01_prepare_public_data.R
# Reads: mlmRev::star and metadat::dat.bangertdrowns2004.
# Writes: the study-level input and a sample/version receipt; no pupil rows.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
source(file.path(.script_dir, 'data_loaders.R'))

# Check source samples and construct the study input
star <- app_load_star()
bd <- app_load_bd()
app_input_parity(bd, 'bangert_drowns_primary')

# Save the aggregate input and source-version record
rp_write_csv(
  bd,
  rp_output_path('data-derived', 'applications', 'bangert_drowns_primary.csv')
)
rp_write_json(
  list(
    STAR_source_kindergarten = 6325, STAR_complete_cases = 5786,
    Bangert_Drowns_studies = nrow(bd), participant_rows_saved = FALSE,
    mlmRev = as.character(packageVersion('mlmRev')),
    metadat = as.character(packageVersion('metadat'))
  ),
  rp_output_path('outputs', 'applications', 'public_data_receipt.json')
)
cat('STAR 6325 kindergarten / 5786 complete; Bangert-Drowns 48 study rows.\n')
