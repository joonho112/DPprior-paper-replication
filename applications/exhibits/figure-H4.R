# Author: JoonHo Lee (jlee296@ua.edu)
# Plot the population densities for new units.
curves <- read.csv(file.path(frames,"population-density.csv"))
colors <- c("Gamma(1,1)"="#26343B","TSMM"="#0072A4","Dual-Anchor"="#BF502A",
            "Gamma(1,3) reference"="#946325","Dual-Anchor (hard)"="#007D65",
            "Gaussian benchmark"="#72517F")
styles <- c("solid","dashed","solid","dotdash","dotted","longdash")
names(styles) <- names(colors)
curves$prior <- factor(curves$prior,levels=names(colors))
suppressPackageStartupMessages({library(ggplot2);library(patchwork)})
panel <- function(app,title,xlab,limits,br) {
 d <- curves[curves$app==app & curves$x>=limits[1] & curves$x<=limits[2],]
 standard <- d[d$prior=="Gamma(1,1)",]
 # Keep the archived uncertainty summaries: all three priors for normal applications,
 # TSMM, selected Dual-Anchor and Gaussian for the Rasch comparison (plus default ribbon).
 limits_set <- if(app=="rasch") c("TSMM","Dual-Anchor","Gaussian benchmark") else names(colors)[1:3]
 bounds <- d[d$prior %in% limits_set,]
 ggplot(d,aes(x,mean,group=prior))+
   geom_ribbon(data=standard,aes(ymin=lo,ymax=hi),fill="#CFD6D9",alpha=.70)+
   geom_line(data=bounds,aes(y=lo,colour=prior),alpha=.56,linewidth=.30,show.legend=FALSE)+
   geom_line(data=bounds,aes(y=hi,colour=prior),alpha=.56,linewidth=.30,show.legend=FALSE)+
   geom_line(aes(colour=prior,linetype=prior),linewidth=.61)+
   scale_colour_manual(name=NULL,values=colors,breaks=names(colors),drop=FALSE)+
   scale_linetype_manual(name=NULL,values=styles,breaks=names(colors),drop=FALSE)+
   scale_x_continuous(breaks=br,expand=expansion(mult=.005))+
   scale_y_continuous(expand=expansion(mult=c(0,.04)))+
   labs(title=title,x=xlab,y="Density")+
   theme_minimal(base_size=9.5)+theme(panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),
     panel.grid.major.y=element_line(colour="#E2E6E8",linewidth=.25),
     plot.title=element_text(face="bold",size=10.5,hjust=0),
     legend.position="none",plot.margin=margin(2,5,3,3),axis.title=element_text(size=8.5))
}
top <- panel("star","Project STAR","Small-class effect (achievement SDs)",c(-2,2),-2:2) +
  panel("bd","Writing-to-learn","Effect (standardized mean difference)",c(-2,2),-2:2)
bottom <- panel("rasch","Vocabulary checklist","Latent ability (item-anchored logits)",c(-4,9),c(-4,0,3,6,9))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE),linetype=guide_legend(nrow=2,byrow=TRUE))+
  theme(legend.position="bottom",legend.text=element_text(size=8),legend.key.width=grid::unit(16,"pt"),
        legend.margin=margin(0,0,0,0))
fig <- top / bottom + plot_layout(heights=c(1,1.28))
ggsave(file.path(out,"osm-figure-H.4.pdf"),fig,width=6.5,height=5.2,device=grDevices::pdf,useDingbats=FALSE)

