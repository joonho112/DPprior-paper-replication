# Author: JoonHo Lee (jlee296@ua.edu)
# The posterior mean finite CDF for observed units and a new-unit population density have different grids.
curves <- frozen("finite-cdf")
curves$prior <- name_role(curves$prior)
curves$diagnostics <- curves$status
stopifnot(!anyNA(curves$prior),all(curves$n==apps[curves$app]),all(curves$mean>=0 & curves$mean<=1),
          all(curves$lower>=0 & curves$lower<=curves$upper & curves$upper<=1))
ranges <- peaks <- list()
for (app in names(apps)) {
  f <- curves[curves$app==app,]
  grid <- seq(if (app=="rasch") -4 else -2, if (app=="rasch") 9 else 2,.025)
  groups <- split(f,f$prior)
  stopifnot(length(groups)==5L)
  for (z in groups) {
    stopifnot(length(z$x)==length(grid),max(abs(z$x-grid))<1e-10,
              all(diff(z$mean)>=-1e-12),all(diff(z$lower)>=-1e-12),all(diff(z$upper)>=-1e-12))
  }
  for (set in c("Primary three","All five")) {
    subset_priors <- if (set=="Primary three") unname(priors) else names(groups)
    means <- vapply(groups[subset_priors],function(x)x$mean,numeric(length(grid)))
    span <- 100*(apply(means,1,max)-apply(means,1,min))
    lower <- colnames(means)[max.col(-means,ties.method="first")]
    upper <- colnames(means)[max.col(means,ties.method="last")]
    peak <- which.max(span)
    a <- groups[[lower[peak]]][peak,]; b <- groups[[upper[peak]]][peak,]
    stopifnot(a$status=="VERIFIED",b$status=="VERIFIED")
    ranges[[length(ranges)+1L]] <- data.frame(app,set,x=grid,range_pp=span,lower_prior=lower,upper_prior=upper)
    peaks[[length(peaks)+1L]] <- data.frame(app,set,location=grid[peak],max_gap_pp=span[peak],lower_prior=lower[peak],upper_prior=upper[peak],
      pointwise_mcse_difference_pp=100*sqrt(a$mcse_mean^2+b$mcse_mean^2))
  }
}
save_frame(curves,"finite-cdf-curves")
save_frame(do.call(rbind,ranges),"pointwise-prior-ranges")
save_frame(do.call(rbind,peaks),"grid-maxima")
point3 <- curves[curves$app=="rasch" & abs(curves$x-3)<1e-12,]
eap <- ex_read("rasch","score_groups.csv","data-frozen")
for (i in seq_len(nrow(point3))) {
  z <- eap[eap$arm=="main" & name_role(eap$role)==point3$prior[i],]
  point3$score_EAP_count_le3[i] <- sum(z$n_r[z$EAP_r<=3])
}
save_frame(point3,"vocabulary-at-three-logits")

u <- ex_read("applications","unit_summary.csv","data-frozen")
u <- u[u$app %in% c("star","bd") & u$analysis_spec=="primary_scale_aligned",]
endpoint <- frozen("interval-endpoint-mcse")
exceptions <- merge(u[u$prior_label %in% c("Vague","TSMM"),c("app","unit","prior_label","posterior_mean","q025","q975")],
                    endpoint,by=c("app","unit"))
exceptions$endpoint_mcse <- ifelse(exceptions$prior_label=="TSMM",exceptions$mcse_q025_tsmm,exceptions$mcse_q025_vague)
exceptions$interval_excludes_zero <- exceptions$q025>0 | exceptions$q975<0
stopifnot(nrow(exceptions)==6, all(abs(exceptions$q025[exceptions$prior_label=="TSMM"]) < exceptions$endpoint_mcse[exceptions$prior_label=="TSMM"]))
ref <- scores[scores$role=="vague",]
score_list <- lapply(unique(scores$role),function(role) {
  z <- scores[scores$role==role,];ref <- ref[match(z$r,ref$r),]
  data.frame(prior=name_role(z$role),score=z$r,respondents=z$n_r,posterior_mean=z$EAP_r,change_logits=z$EAP_r-ref$EAP_r,
             MCSE_change_logits=if(role=="vague") 0 else sqrt(z$mcse_EAP_r^2+ref$mcse_EAP_r^2))
})
score_all <- do.call(rbind,score_list)
stopifnot(all(table(score_all$prior)==14L),all(tapply(score_all$respondents,score_all$prior,sum)==500L))
save_frame(exceptions,"interval-exceptions");save_frame(score_all,"rasch-score-changes")

density <- frozen("population-density-normal")
density$prior <- name_role(density$prior)
names(density)[names(density)=="lower"] <- "lo";names(density)[names(density)=="upper"] <- "hi"
density <- density[c("app","prior","x","mean","lo","hi")]
rasch_grid <- ex_read("rasch","model_density.csv","data-frozen")
rasch_grid <- rasch_grid[rasch_grid$arm=="main",]
rasch_density <- data.frame(app="rasch",prior=name_role(rasch_grid$role),x=rasch_grid$x,mean=rasch_grid$mean,lo=rasch_grid$lower,hi=rasch_grid$upper)
g <- ex_read("rasch","gaussian_density.csv","data-frozen")
gaussian <- data.frame(app="rasch",prior="Gaussian benchmark",x=g$x,mean=g$mean,lo=g$lower,hi=g$upper)
density <- rbind(density,rasch_density,gaussian)
runs <- frozen("density-runs")
stopifnot(nrow(runs)==6,all(runs$whole_grid_status=="FULLY_VERIFIED"),all(runs$nodes_verified==321),
          nrow(density)==6*321+6*961,all(density$lo>=0 & density$lo<=density$hi),all(density$mean>=0))
distances <- list()
for (app in names(apps)) {
  z <- density[density$app==app,];ref <- z[z$prior==priors["gamma"],]
  for (prior in unique(z$prior)) {
    d <- z[z$prior==prior,];stopifnot(identical(d$x,ref$x))
    delta <- abs(d$mean-ref$mean)
    distances[[length(distances)+1L]] <- data.frame(app,prior,n_grid=nrow(d),xmin=min(d$x),xmax=max(d$x),
         L1=sum(diff(d$x)*(head(delta,-1)+tail(delta,-1))/2))
  }
}
save_frame(density,"population-density");save_frame(do.call(rbind,distances),"population-density-distances")
