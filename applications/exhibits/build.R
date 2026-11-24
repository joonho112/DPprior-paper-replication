#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Reproduce the application exhibits from public summary inputs.
arg <- grep("^--file=", commandArgs(FALSE), value=TRUE)
.script_dir <- dirname(normalizePath(gsub("~+~"," ",sub("^--file=","",arg[1]),fixed=TRUE)))
source(file.path(.script_dir,"..","..","common","R","bootstrap.R"))
source(rp_path("exhibits","helpers.R"))
inputs <- rp_path("data-frozen","application-summaries")
frames <- rp_output_path("data-derived","exhibits","applications")
out <- rp_output_path("outputs","exhibits")
dir.create(frames,recursive=TRUE,showWarnings=FALSE)
dir.create(out,recursive=TRUE,showWarnings=FALSE)
contract <- read.csv(file.path(inputs,"input-contract.csv"))
for (i in seq_len(nrow(contract))) {
  spec <- contract[i,]; path <- file.path(inputs,spec$file)
  stopifnot(rp_sha256(path)==spec$sha256, file.size(path)==spec$bytes)
  f <- read.csv(path,check.names=FALSE)
  stopifnot(nrow(f)==spec$rows,identical(names(f),strsplit(spec$columns," ",fixed=TRUE)[[1]]),
            !anyDuplicated(f[strsplit(spec$key," ",fixed=TRUE)[[1]]]))
}
frozen <- function(name) read.csv(file.path(inputs,paste0(name,".csv")),stringsAsFactors=FALSE)
save_frame <- function(x,name) rp_write_csv(x,file.path(frames,paste0(name,".csv")))
frame_path <- function(name) file.path("data-derived","exhibits","applications",paste0(name,".csv"))
input_path <- function(name) file.path("data-frozen","application-summaries",paste0(name,".csv"))
apps <- c(star=79L,bd=48L,rasch=500L)
app_names <- c(star="Project STAR",bd="Writing-to-learn",rasch="Vocabulary checklist")
priors <- c(gamma="Gamma(1,1)",tsmm="TSMM",dual="Dual-Anchor")
labels <- c(priors,external="DORO-Unif",litdef="Gamma(1,3) reference",hard="Dual-Anchor (hard)")
roles <- c(Vague="gamma",TSMM="tsmm","Dual-Soft-Selected"="dual","Dual-Hard-Sensitivity"="hard",
           "DORO-Unif"="external",vague="gamma",tsmm="tsmm",dasoft="dual",dahard="hard",litdef="litdef")
name_role <- function(x) unname(labels[roles[x]])
run <- function(executable,args) {
  status <- system2(executable, shQuote(args))
  if (status != 0L) stop("Application renderer failed: ",basename(args[1]))
}
source(file.path(.script_dir,"sensitivity.R"), local=TRUE)
source(file.path(.script_dir,"tables.R"), local=TRUE)
source(file.path(.script_dir,"distribution.R"), local=TRUE)
source(file.path(.script_dir,"pooling.R"), local=TRUE)
# Draw the accepted layouts from the generated numerical frames.
figure_python <- system2(Sys.which("python3"),shQuote(c(rp_path("common","figure_runtime.py"),"--python")),stdout=TRUE)
stopifnot(length(figure_python)==1L,file.exists(figure_python))
run(figure_python,c(file.path(.script_dir,"figure-5.py"),frames,out))
run(file.path(R.home("bin"),"Rscript"),c(file.path(.script_dir,"figure-pooling.R"),frames,out))
source(file.path(.script_dir,"figure-H2.R"),local=new.env(parent=environment()))
source(file.path(.script_dir,"figure-H3.R"),local=new.env(parent=environment()))
source(file.path(.script_dir,"figure-H4.R"),local=new.env(parent=environment()))
record <- function(id,frame,files,note) {
  pdf <- file.path(out,paste0(id,".pdf")); png <- file.path(out,paste0(id,".png"))
  # Rasterize the finished PDF so the two formats retain the same panel placement.
  run(Sys.which("pdftocairo"),c("-png","-singlefile","-r","175",pdf,sub("[.]png$","",png)))
  stopifnot(file.size(pdf)>1000,file.size(png)>1000)
  ex_meta(id,c(pdf,png),frame,files,"rebuilt-from-public-summary",note)
}
record("main-figure-5",panel_b,c(frame_path("figure-5-panel-a"),frame_path("figure-5-panel-b"),
     "outputs/exhibits/osm-table-H.18.csv"),"The frame contains all 199 percentiles for each application and chain block. The first companion CSV contains the same-unit and same-percentile RMS values in Panel A.")
record("main-figure-6",fig6,frame_path("figure-6"),"Dual minus Gamma. Dashed lines locate middle-group mean changes; upper-unit bars show one Monte Carlo SE.")
record("osm-figure-H.2",curves,c(frame_path("finite-cdf-curves"),frame_path("pointwise-prior-ranges"),frame_path("grid-maxima")),"CDF and pointwise intervals for the observed units, with within-node ranges across priors.")
record("osm-figure-H.3",exceptions,c(frame_path("interval-exceptions"),frame_path("rasch-score-changes")),"Intervals and lower-endpoint MCSE for selected school/study effects. Panel C uses score-group EAP changes and errors.")
record("osm-figure-H.4",density,frame_path("population-density"),"Posterior population densities for a new exchangeable unit on the original common grids.")
record("osm-figure-H.5",fixed,frame_path("fixed-count"),"Middle-minus-upper co-assignment within draws with the same occupied count.")
cat("Rebuilt seven application tables and six figures.\n")
