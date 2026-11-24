# Author: JoonHo Lee (jlee296@ua.edu)
# Assign groups once from observed effects and join the two fits by analytic unit.
share <- frozen("pooling-unit")
errors <- frozen("pooling-contrast-mcse")
upper_info <- h19_rows <- points <- list()
for (app in c("star","bd")) {
  gamma <- u[u$app==app & u$prior_label=="Vague",]
  dual <- u[u$app==app & u$prior_label=="Dual-Soft-Selected",]
  dual <- dual[match(gamma$unit,dual$unit),]
  stopifnot(identical(gamma$unit,seq_len(apps[app])),identical(gamma$tau_hat,dual$tau_hat))
  observed <- gamma$tau_hat
  q <- quantile(observed,c(.25,.75,.9),type=7,names=FALSE)
  mid <- observed>=q[1] & observed<=q[2];high <- observed>=q[3]
  s <- share[share$app==app,]; a <- s[s$prior=="gamma",]; b <- s[s$prior=="dual",]
  a <- a[match(gamma$unit,a$unit),]; b <- b[match(gamma$unit,b$unit),]
  stopifnot(identical(a$unit,gamma$unit),identical(b$unit,gamma$unit))
  unit <- data.frame(app,unit=gamma$unit,group=ifelse(high,"upper",ifelse(mid,"central","other")),unit_id="",
   coassign_gamma=a$sharing,coassign_dual=b$sharing,displacement_gamma=abs(observed-gamma$posterior_mean),
   displacement_dual=abs(observed-dual$posterior_mean),mcse_delta_mean=sqrt(a$mcse_mean^2+b$mcse_mean^2))
  # The two annotations already printed in the manuscript are the only source designations used.
  if (app=="star") unit$unit_id[unit$unit==22] <- as.character(gamma$school[gamma$unit==22])
  if (app=="bd") unit$unit_id[unit$unit==46] <- "Willey (1988)"
  unit$delta_coassign <- unit$coassign_dual-unit$coassign_gamma
  unit$delta_displacement <- unit$displacement_dual-unit$displacement_gamma
  points[[app]] <- unit[unit$group!="other",]
  for(group in c("upper","central")) {
    g <- unit[unit$group==group,]
    h19_rows[[paste(app,group)]] <- data.frame(app,group,n=nrow(g),coassign_gamma=mean(g$coassign_gamma),
     coassign_dual=mean(g$coassign_dual),coassign_reduction_pp=-100*mean(g$delta_coassign),
     distance_gamma=mean(g$displacement_gamma),distance_dual=mean(g$displacement_dual),distance_reduction=-mean(g$delta_displacement))
  }
  upper <- h19_rows[[paste(app,"upper")]];central <- h19_rows[[paste(app,"central")]]
  contrast <- upper;contrast$group <- "excess"
  contrast[c("n","coassign_gamma","coassign_dual","distance_gamma","distance_dual")] <- NA
  contrast$coassign_reduction_pp <- upper$coassign_reduction_pp-central$coassign_reduction_pp
  contrast$distance_reduction <- upper$distance_reduction-central$distance_reduction
  h19_rows[[paste(app,"excess")]] <- contrast
  err <- errors[errors$app==app,];stopifnot(setequal(err$prior,c("gamma","dual")))
  contrast$group <- "mcse";contrast$coassign_reduction_pp <- 100*sqrt(sum(err$coassign^2))
  contrast$distance_reduction <- sqrt(sum(err$distance^2))
  h19_rows[[paste(app,"mcse")]] <- contrast
  high_rows <- unit[high,]
  upper_info[[app]] <- data.frame(app,unit=high_rows$unit,share_of_total_upper_reduction=
     high_rows$delta_displacement/sum(high_rows$delta_displacement),
     below_one_mcse=abs(high_rows$delta_displacement)<high_rows$mcse_delta_mean)
}
fig6 <- do.call(rbind,points);rownames(fig6)<-NULL
h19 <- do.call(rbind,h19_rows);rownames(h19)<-NULL
save_frame(fig6,"figure-6");save_frame(do.call(rbind,upper_info),"upper-unit-contributions")
h19_display <- h19;h19_display$app <- app_names[h19$app]
h19_display$group <- c(upper="Upper",central="Middle",excess="Excess reduction",mcse="Monte Carlo SE")[h19$group]
for (col in c("coassign_gamma","coassign_dual")) h19_display[[col]] <- ex_f(100*h19[[col]],2)
h19_display$coassign_reduction_pp <- ex_f(h19$coassign_reduction_pp,2)
for (col in c("distance_gamma","distance_dual","distance_reduction")) h19_display[[col]] <- ex_f(h19[[col]],4)
ex_table("osm-table-H.19",h19,h19_display,sources=c(frame_path("figure-6"),input_path("pooling-contrast-mcse")),
 note="Reduction is Gamma minus Dual. Co-clustering reductions and their Monte Carlo SEs are percentage points. Excess is upper minus middle. Independent fit variances are added after taking the within-fit contrast.")
fixed <- frozen("pooling-fixed-count")
fixed$upper_minus_central_pp <- 100*(fixed$upper_to_central-fixed$central_to_other_central)
stopifnot(nrow(fixed)==16L,all(fixed$min_draws_per_chain>=174),all(fixed$upper_minus_central_pp[fixed$K==1]==0))
save_frame(fixed,"fixed-count")
cutoff <- frozen("pooling-cutoffs")
cutoff$upper_minus_central_coassign <- cutoff$upper_coassignment-cutoff$central_coassignment
cutoff$upper_minus_central_distance <- cutoff$upper_gap-cutoff$central_gap
contrasts <- function(d,keys,metrics) {
  gamma <- d[d$prior=="gamma",c(keys,metrics)]; dual <- d[d$prior=="dual",c(keys,metrics)]
  both <- merge(gamma,dual,by=keys,suffixes=c("_gamma","_dual"))
  for (k in metrics) both[[paste0(k,"_reduction")]] <- both[[paste0(k,"_gamma")]]-both[[paste0(k,"_dual")]]
  both
}
cutoff <- contrasts(cutoff,c("app","upper_fraction","n_upper","n_central"),c("upper_minus_central_coassign","upper_minus_central_distance"))
save_frame(cutoff,"pooling-cutoff-contrasts")
halves <- frozen("pooling-chain-halves")
save_frame(contrasts(halves,c("app","group","half"),c("coassign","displacement")),"pooling-split-contrasts")
