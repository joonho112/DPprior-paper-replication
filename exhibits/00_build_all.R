#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Verify frozen inputs and rebuild the November exhibits from an empty output tree.
script <- sub('^--file=', '', grep('^--file=', commandArgs(FALSE), value = TRUE)[1])
here <- dirname(normalizePath(gsub('~+~', ' ', script, fixed = TRUE)))
python <- Sys.which('python3')
if (!nzchar(python)) stop('The exhibit build requires python3 on PATH.', call. = FALSE)
status <- system2(python, shQuote(file.path(here, 'build_release.py')))
if (status != 0L) quit(status = status)
