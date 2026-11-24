# Author: JoonHo Lee (jlee296@ua.edu)
# Align prior parameters, posterior counts/weights and finite within-draw spread.
a <- ex_read("applications","primary.csv")
a <- a[a$app %in% c("star","bd"),]
rp <- ex_read("rasch","priors.csv","data-frozen")
r <- ex_read("rasch","posterior_reportable.csv")
r <- r[r$arm=="main",]
rp <- rp[rp$arm=="main",]
rp <- rp[match(r$role,rp$role),]
stopifnot(identical(r$role,rp$role),all(rp$arm=="main"),all(rp$verified),all(r$gate_pass),
          all(r$n_draws==16000),all(a$final_diagnostic_pass),all(a$retained_partition_draws==16000))
normal_pr <- data.frame(app=a$app,prior=name_role(a$prior_label),n=a$J,a=a$manifest_a,b=a$manifest_b,
 prior_mean_count=a$manifest_prior_EK,q05=a$manifest_prior_K05,q95=a$manifest_prior_K95)
rasch_pr <- data.frame(app="rasch",prior=name_role(rp$role),n=500L,a=rp$a,b=rp$b,
 prior_mean_count=rp$exact_E_K,q05=rp$q05,q95=rp$q95)
h3 <- rbind(normal_pr,rasch_pr)
# Stable evaluations also cover the near-degenerate hard Gamma(shape, rate) prior.
h3$prior_majority <- exp(-h3$a*log1p(-log1p(-.5)/h3$b))
h3$prior_tail_09 <- exp(-h3$a*log1p(-log1p(-.9)/h3$b))
stopifnot(max(abs(tail(h3$prior_majority,nrow(rp))-rp$wsb_gt_05)) < 1e-10)
order_rows <- function(d) d[order(match(d$app,names(apps)),match(d$prior,unname(labels))),]
h3 <- order_rows(h3)
prior_view <- data.frame(Application=app_names[h3$app],Prior=h3$prior,
  'Gamma (shape, rate)'=ifelse(h3$a>1e5,"alpha approximately 2",sprintf("(%.2f, %.2f)",h3$a,h3$b)),
  'Expected count [90% interval]'=sprintf("%.2f [%d, %d]",h3$prior_mean_count,as.integer(h3$q05),as.integer(h3$q95)),
  'Pr(weight > .5)'=sprintf("%.3f",h3$prior_majority),'Pr(weight > .9)'=sprintf("%.3f",h3$prior_tail_09),check.names=FALSE)
ex_table("osm-table-H.3",h3,prior_view,sources=c("data-derived/applications/primary.csv","data-frozen/rasch/priors.csv"),
 note="Prior implications for the five fitted concentration priors per application. The weight is the size-biased population component weight.")
normal_post <- data.frame(app=a$app,prior=name_role(a$prior_label),n=a$J,posterior_mean_count=a$E_K,q025=a$q025_K,q975=a$q975_K,
 posterior_one=a$p_K1,largest_observed_share=a$observed_largest_share_mean,largest_population_weight=a$population_largest_atom_weight_mean,
 unit_rms=a$rmse_unit_shift_from_vague)
scores <- subset(ex_read("rasch","score_groups.csv","data-frozen"),arm=="main")
rasch_rms <- function(role) {
  z <- scores[scores$role==role,]; ref <- scores[scores$role=="vague",]
  z <- z[match(frequencies$raw_score,z$r),];ref <- ref[match(frequencies$raw_score,ref$r),]
  stopifnot(identical(z$r,0:13),identical(z$n_r,frequencies$n),identical(z$n_r,ref$n_r))
  sqrt(weighted.mean((z$EAP_r-ref$EAP_r)^2,z$n_r))
}
rasch_post <- data.frame(app="rasch",prior=name_role(r$role),n=500L,posterior_mean_count=r$E_K,q025=r$q025_K,q975=r$q975_K,
 posterior_one=r$p_K1,largest_observed_share=r$observed_largest_share_mean,largest_population_weight=r$population_largest_atom_weight_mean,
 unit_rms=vapply(r$role,rasch_rms,numeric(1)))
stopifnot(max(abs(rasch_post$unit_rms-r$rmsd_vs_reference))<1e-10)
h4 <- order_rows(rbind(normal_post,rasch_post))
post_view <- data.frame(Application=app_names[h4$app],Prior=h4$prior,
 'Posterior count [95% interval]'=sprintf("%.2f [%d, %d]",h4$posterior_mean_count,as.integer(h4$q025),as.integer(h4$q975)),
 'One cluster'=sprintf("%.3f",h4$posterior_one),'Largest observed share'=sprintf("%.3f",h4$largest_observed_share),
 'Largest population weight'=sprintf("%.3f",h4$largest_population_weight),'Unit-mean RMS change'=sprintf("%.4f",h4$unit_rms),check.names=FALSE)
ex_table("osm-table-H.4",h4,post_view,sources=c("data-derived/applications/primary.csv","data-derived/rasch/posterior_reportable.csv",
 "data-frozen/rasch/score_groups.csv","data-frozen/rasch/score_counts.csv"),
 note="All 15 fits passed the original diagnostic criteria. Vocabulary EAP changes use the 14 score frequencies.")
spread <- frozen("spread-functionals")
spread$prior <- ifelse(spread$prior=="external" & spread$app=="rasch",labels["litdef"], unname(c(vague=labels[["gamma"]],
 tsmm=labels[["tsmm"]],soft=labels[["dual"]],external=labels[["external"]],hard=labels[["hard"]])[spread$prior]))
stopifnot(nrow(spread)==30L,all(spread$n==apps[spread$app]),all(spread$rhat<=1.01),min(spread$ess_bulk,spread$ess_tail)>=400)
summarize <- function(stat) {
  d <- spread[spread$functional==stat,c("app","prior","n","mean","lower","upper")]
  names(d)[4:6] <- paste0(tolower(stat),"_",names(d)[4:6]);d
}
h16 <- order_rows(merge(summarize("SD"),summarize("IQR"),by=c("app","prior","n")))
effect_interval <- function(mean,lo,hi) sprintf("%s [%.2f, %.2f]",ifelse(h16$app=="rasch",sprintf("%.2f",mean),sprintf("%.3f",mean)),lo,hi)
display16 <- data.frame(Application=app_names[h16$app],Prior=h16$prior,
 'SD [95% interval]'=effect_interval(h16$sd_mean,h16$sd_lower,h16$sd_upper),
 'IQR [95% interval]'=effect_interval(h16$iqr_mean,h16$iqr_lower,h16$iqr_upper),check.names=FALSE)
ex_table("osm-table-H.16",h16,display16,sources=input_path("spread-functionals"),
 note="SD has denominator J. Interquartile ranges use inverse empirical quantiles within each draw. Posterior intervals use type 7. The STAR TSMM lower-endpoint MCSEs are .0054 and .0081. Vocabulary upper SD and lower IQR endpoints have MCSEs of .012–.019 and .012–.024 logits.")
t4 <- order_rows(Reduce(function(x,y) merge(x,y,by=c("app","n","prior")),list(
 h3[c("app","n","prior","prior_mean_count","prior_majority")],
 h4[c("app","n","prior","posterior_mean_count","posterior_one","largest_population_weight")],
 h16[c("app","n","prior","sd_mean")])))
t4 <- t4[t4$prior %in% unname(priors),];names(t4)[names(t4)=="sd_mean"] <- "finite_sd"
stopifnot(nrow(t4)==9L,!anyDuplicated(t4[c("app","prior")]))
display4 <- data.frame(Application=app_names[t4$app],Units=t4$n,'Stated 80% count range'=c(star="2–8",bd="1–8",rasch="2–10")[t4$app],
 Prior=t4$prior,'Prior mean count'=sprintf("%.2f",t4$prior_mean_count),'Prior majority (%)'=sprintf("%.1f",100*t4$prior_majority),
 'Posterior mean count'=sprintf("%.2f",t4$posterior_mean_count),'One cluster (%)'=sprintf("%.1f",100*t4$posterior_one),
 'Largest weight (%)'=sprintf("%.1f",100*t4$largest_population_weight),'Effect SD'=ifelse(t4$app=="rasch",sprintf("%.2f",t4$finite_sd),sprintf("%.3f",t4$finite_sd)),check.names=FALSE)
ex_table("main-table-4",t4,display4,sources=c("outputs/exhibits/osm-table-H.3.csv","outputs/exhibits/osm-table-H.4.csv","outputs/exhibits/osm-table-H.16.csv"),
 note="The largest weight refers to the population mixing distribution. Effect SD averages the within-draw spread of the observed 79 schools, 48 studies or 500 respondents.")
