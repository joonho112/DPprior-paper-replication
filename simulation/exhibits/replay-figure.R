# Author: JoonHo Lee (jlee296@ua.edu)
# Plot full posterior CDF replay summaries for the two prespecified examples.
draw_replay <- function(curves, out) {
  suppressPackageStartupMessages({library(ggplot2);library(patchwork)})
  sets <- lapply(c(5, 30), function(k) curves[curves$K_true == k, ])
  stack_curves <- function(wide, group_name) {
    fields <- names(wide)[-1L]
    long <- data.frame(x = rep(wide$x, each = length(fields)))
    long[[group_name]] <- rep(fields, times = nrow(wide))
    long$value <- as.vector(t(as.matrix(wide[fields])))
    long
  }
  panels <- lapply(seq_along(sets),function(i) {
    data <- sets[[i]]
    priors <- if(i==1) c("TSMM","Dual-Anchor") else c("Vague","TSMM")
    names <- if(i==1) priors else c("Gamma(1,1)","TSMM")
    a <- data[data$prior==priors[1],]
    b <- data[data$prior==priors[2],]
    stopifnot(identical(a$x,b$x),identical(a$truth_CDF,b$truth_CDF),a$K_true[1]==c(5,30)[i])
    series <- data.frame(x=a$x,Truth=a$truth_CDF,first=a$direct_CDF,second=b$direct_CDF)
    dis <- data.frame(x=a$x, direct=100*(b$direct_CDF-a$direct_CDF),CB=100*(b$CB_CDF-a$CB_CDF))
    cdf <- stack_curves(series, "curve")
    cdf$curve <- factor(cdf$curve,c("Truth","first","second"))
    deltas <- stack_curves(dis, "estimator")
    prior_colours <- c(Vague="#272727",TSMM="#0072A4","Dual-Anchor"="#CE511F")
    truecc <- c("#666666",unname(prior_colours[priors]))
    base <- theme_minimal(base_size=11) + theme(panel.grid.minor=element_blank(),
      panel.grid.major.x=element_blank(),legend.position="bottom",legend.title=element_blank(),
      legend.key.width=grid::unit(.36,"in"),plot.title=element_text(size=12,face="bold"),
      plot.subtitle=element_text(size=10),plot.margin=margin(6,10,3,8))
    top <- ggplot(cdf,aes(x,value,color=curve,linetype=curve))+
      geom_step(data=subset(cdf,curve=="Truth"),linewidth=.55)+
      geom_line(data=subset(cdf,curve!="Truth"),linewidth=.75)+
      scale_colour_manual(values=truecc,labels=c("True sites",names),drop=FALSE)+
      scale_linetype_manual(values=c("dotted","longdash","solid"),labels=c("True sites",names),drop=FALSE)+
      coord_cartesian(xlim=c(-.75,.75),ylim=c(0,1))+
      labs(title=paste("Generating count =",a$K_true[1]),
           subtitle="Posterior mean finite-site CDF and true site CDF",x=NULL,y="Cumulative proportion")+base
    lower <- ggplot(deltas,aes(x,value,colour=estimator))+
      geom_hline(yintercept=0,linewidth=.4,colour="#555555")+
      geom_step(data=subset(deltas,estimator=="CB"),linewidth=.45)+
      geom_line(data=subset(deltas,estimator=="direct"),linewidth=.85)+
      scale_colour_manual(values=c(CB="#969da2",direct="#126476"),breaks=c("direct","CB"),
                          labels=c("Posterior mean CDF","CB EDF"))+
      coord_cartesian(xlim=c(-.75,.75),ylim=c(-6,6))+
      labs(title=paste(names[2],"minus",names[1]),x="Site effect",y="CDF difference\n(percentage points)")+
      base
    list(top,lower)
  })
  g <- (panels[[1]][[1]] + panels[[2]][[1]]) / (panels[[1]][[2]] + panels[[2]][[2]]) +
   plot_layout(heights=c(1.23,1))+
   plot_annotation(title="Two data sets replayed with the original posterior sampler",
      subtitle="One low-information data set per design; four chains for each prior. Each panel uses the same 100 sites across priors.",
      caption="Display grid spacing is 0.0025. Reported KS distances and ISEL use all empirical jumps over the full real line.",
      theme=theme(plot.title=element_text(size=15,face="bold"),plot.caption=element_text(hjust=0,size=9)))
  ggsave(file.path(out,"osm-figure-F.5.pdf"),g,width=10,height=7,device=grDevices::pdf,useDingbats=FALSE)
  ggsave(file.path(out,"osm-figure-F.5.png"),g,width=10,height=7,dpi=185,device=ragg::agg_png,bg="white")
}
