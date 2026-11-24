#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Install the recorded dependencies
#
# Restore one task profile into the project library. Dependencies are ordered
# before the packages that need them, and unavailable pinned sources stop the
# installation rather than being replaced with newer versions.
#
# Run: Rscript common/install_dependencies.R --profile rebuild
# Add --execute to download and install; otherwise only the plan is printed.
# Reads: provenance/software-lock.csv and dependency-closure.csv.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
if (!length(.file_arg)) stop('Run this script with Rscript.', call. = FALSE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1L]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, 'R', 'bootstrap.R'))
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  execute <- '--execute' %in% args
  prefer_binary <- '--prefer-binaries' %in% args && startsWith(.Platform$pkgType, 'mac.binary')
  args <- args[!args %in% c('--execute', '--prefer-binaries')]
  if (length(args) != 2L || args[1] != '--profile' ||
    !args[2] %in% c('core', 'rebuild', 'calibration', 'public-data', 'refit', 'book'))
    stop('Usage: Rscript common/install_dependencies.R --profile PROFILE [--execute] [--prefer-binaries]',
      call. = FALSE
    )
  profile <- args[2]
  if (profile %in% c('core', 'book')) {
    cat('No R packages to install for this profile. Book rendering requires a separate Quarto installation.\n')
    quit(status = 0L)
  }
  lock <- rp_read_csv(rp_path('provenance', 'software-lock.csv', must_exist = TRUE))
  closure <- rp_read_csv(rp_path('provenance', 'dependency-closure.csv', must_exist = TRUE))
  selected <- vapply(
    strsplit(closure$profiles, ';', fixed = TRUE),
    function(x) profile %in% x,
    logical(1)
  )
  plan <- closure[selected & (is.na(closure$priority) | closure$priority != 'base'), ]
  cat('Profile:', profile, '; source package pins:', nrow(plan), '\n')
  print(plan[c('package', 'version')], row.names = FALSE)
  if (!execute) {
    cat('Plan only. Add --execute to download and install into .library.\n')
    quit(status = 0L)
  }
  lib <- rp_path('.library')
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  # Dependencies must precede dependents; no implicit install.packages dependency
  # resolution is used because it would silently select newer package versions.
  parse_deps <- function(s) {
    s <- s[!is.na(s) & nzchar(s)]
    if (!length(s)) return(character())
    trimws(gsub('\\s*\\([^)]*\\)', '', unlist(strsplit(s, ','))))
  }
  remaining <- plan$package
  ordered <- character()
  while (length(remaining)) {
    ready <- remaining[vapply(remaining, function(p) {
      z <- plan[plan$package == p, ]
      !length(intersect(parse_deps(c(z$depends, z$imports, z$linking_to)), remaining))
    }, logical(1))]
    if (!length(ready)) stop('Dependency cycle or malformed lock; no installation attempted for remaining packages.',
      call. = FALSE
    )
    ordered <- c(ordered, ready)
    remaining <- setdiff(remaining, ready)
  }
  work <- tempfile('dpprior-install-')
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  for (p in ordered) {
    expected <- plan$version[plan$package == p]
    present <- tryCatch(utils::packageVersion(p, lib.loc = lib), error = function(e) NULL)
    if (!is.null(present) && present == package_version(expected)) {
      cat('Present in project library:', p, expected, '\n')
      next
    }
    row <- lock[lock$package == p, ]
    archive <- file.path(work, paste0(p, '_', expected, '.tar.gz'))
    urls <- if (nrow(row) && row$source == 'GitHub') {
      paste0(
        sub('https://github.com/', 'https://api.github.com/repos/', row$repository),
        '/tarball/',
        row$commit
      )
    } else c(
      paste0('https://cloud.r-project.org/src/contrib/', p, '_', expected, '.tar.gz'),
      paste0(
        'https://cloud.r-project.org/src/contrib/Archive/',
        p,
        '/',
        p,
        '_',
        expected,
        '.tar.gz'
      )
    )
    package_type <- 'source'
    if (prefer_binary && (!nrow(row) || row$source != 'GitHub')) {
      binary_root <- utils::contrib.url('https://cloud.r-project.org', type = .Platform$pkgType)
      urls <- c(paste0(binary_root, '/', p, '_', expected, '.tgz'), urls)
    }
    downloaded <- FALSE
    for (url in urls) {
      # The macOS package installer recognizes the .tgz suffix.
      is_binary <- endsWith(url, '.tgz')
      archive <- file.path(work, paste0(p, '_', expected, if (is_binary) '.tgz' else '.tar.gz'))
      status <- tryCatch(suppressWarnings(utils::download.file(url, archive, mode = 'wb', quiet = TRUE)),
        error = function(e) 1L
      )
      if (identical(status, 0L)) {
        downloaded <- TRUE
        package_type <- if (is_binary) .Platform$pkgType else 'source'
        cat('Installing', p, expected, 'from', package_type, url, '\n')
        break
      }
    }
    if (!downloaded) stop('Pinned source unavailable: ',
      p,
      ' ',
      expected,
      '. Do not substitute a newer version.',
      call. = FALSE
    )
    # Inspect only the archive listing and DESCRIPTION before installation.
    members <- utils::untar(archive, list = TRUE)
    if (any(grepl('(^/|(^|/)\\.\\.(/|$))', members))) stop('Unsafe source archive paths.', call. = FALSE)
    desc <- members[grepl('^[^/]+/DESCRIPTION$', members)]
    if (length(desc) != 1L) stop('Source archive lacks a unique root DESCRIPTION.', call. = FALSE)
    checkdir <- file.path(work, paste0(p, '-description'))
    dir.create(checkdir)
    utils::untar(archive, files = desc, exdir = checkdir)
    metadata <- read.dcf(file.path(checkdir, desc))
    if (metadata[1, 'Package'] != p ||
      package_version(metadata[1, 'Version']) != package_version(expected)) stop('Downloaded package identity/version mismatch.', call. = FALSE)
    utils::install.packages(archive, repos = NULL, type = package_type, lib = lib, dependencies = FALSE)
    if (utils::packageVersion(p, lib.loc = lib) != package_version(expected)) stop('Installed package version mismatch.', call. = FALSE)
  }
  cat(
    'Installation finished. Run 00_setup.R --profile',
    profile,
    'to check versions and pinned R functions.\n'
  )
}
main()
