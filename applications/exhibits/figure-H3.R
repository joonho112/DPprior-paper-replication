# Author: JoonHo Lee (jlee296@ua.edu)
# Plot school and study intervals and Rasch score summaries.
library(grid)
selected <- read.csv(file.path(frames,"interval-exceptions.csv"))
score_all <- read.csv(file.path(frames,"rasch-score-changes.csv"))
W <- 6.95; H <- 5.45
ink <- "#172630"; muted <- "#425363"; pale <- "#DFE5E7"
pal <- c("Vague"="#343B40","TSMM"="#007298","Dual-Anchor"="#B84C21")
pch <- c("Vague"=21,"TSMM"=17,"Dual-Anchor"=18)
X <- function(x) unit(x,"inches")
Y <- function(y) unit(H-y,"inches")
line <- function(x0,y0,x1,y1,col=pale,w=.7,lt=1)
  grid.segments(X(x0),Y(y0),X(x1),Y(y1),gp=gpar(col=col,lwd=w,lty=lt,lineend="butt"))
lab <- function(x,y,s,sz=8.4,col=ink,just="left",face="plain")
  grid.text(s,X(x),Y(y),just=c(just,"centre"),gp=gpar(col=col,fontsize=sz,fontface=face,fontfamily="sans"))
dot <- function(x,y,prior,sz=5.6) grid.points(X(x),Y(y),pch=unname(pch[prior]),size=unit(sz,"pt"),
  gp=gpar(col=unname(pal[prior]),fill=if(prior=="Vague")"white" else unname(pal[prior]),lwd=.8))
map <- function(x,lo,hi,from,to) from+(x-lo)/(hi-lo)*(to-from)
plot_data <- function(){
  grid.newpage()
  ax <- function(x) map(x,-.06,1.18,1.02,3.46)
  bx <- function(x) map(x,-.012,.017,4.23,6.79)
  lab(.04,.16,"A   School and study effects",11,face="bold")
  lab(3.84,.16,"B   Lower interval endpoints",11,face="bold")
  lab(.04,.40,"Posterior mean and 95% interval",8.3,muted)
  lab(3.84,.40,"Posterior 2.5th percentile and ±1 MCSE",8.3,muted)
  yu <- c(.80,1.22,1.64)
  at <- c(0,.4,.8,1.1); bt <- c(-.01,0,.01)
  line(ax(at),.59,ax(at),1.91);line(bx(bt),.59,bx(bt),1.91)
  line(ax(0),.59,ax(0),1.9,"#697780",1,"42");line(bx(0),.59,bx(0),1.9,"#697780",1,"42")
  lab(.04,yu,c("School 5","Study 18","Study 19"),8.5)
  for (j in seq_len(3)){
    for (pr in c("Vague","TSMM")){
      z <- selected[(selected$app==c("star","bd","bd")[j]) & (selected$unit==c(5,18,19)[j]) &
                    (selected$prior_label==pr),]
      y <- yu[j]+if (pr=="Vague") -.068 else .068
      line(ax(z$q025),y,ax(z$q975),y,pal[pr],1.25)
      dot(ax(z$posterior_mean),y,pr)
      left <- bx(z$q025-z$endpoint_mcse); right <- bx(z$q025+z$endpoint_mcse)
      line(left,y,right,y,pal[pr],1.15); line(c(left,right),y-.028,c(left,right),y+.028,pal[pr],.85)
      dot(bx(z$q025),y,pr)
    }
  }
  line(ax(-.06),1.91,ax(1.18),1.91,"#9FAAAD");line(bx(-.012),1.91,bx(.017),1.91,"#9FAAAD")
  lab(ax(at),2.04,c("0","0.4","0.8","1.1"),8,muted,"centre")
  lab(bx(bt),2.04,c("-0.01","0","0.01"),8,muted,"centre")
  lab(1,2.26,"Standardized effect",8,muted)
  lab(4.23,2.26,"Standardized effect",8,muted)
  dot(1.03,2.50,"Vague",6.7);lab(1.16,2.50,"Gamma(1,1)",8.2)
  dot(2.19,2.50,"TSMM",6.7);lab(2.32,2.50,"TSMM",8.2)
  lab(.04,2.93,"C   Vocabulary ability by raw score",11,face="bold")
  lab(.04,3.16,"Change in posterior mean from Gamma(1,1), in logits (±1 MCSE)",8.4,muted)
  cx <- function(x) map(x,0,13,.89,6.70)
  cy <- function(z) map(z,-.06,.06,4.83,3.52)
  ticks <- c(-.06,-.03,0,.03,.06)
  line(cx(0)-.13,cy(ticks),cx(13)+.13,cy(ticks),pale,.65)
  line(cx(0)-.13,cy(0),cx(13)+.13,cy(0),"#78858D",.9,"42")
  lab(cx(0)-.20,cy(ticks),c("-0.06","-0.03","0","0.03","0.06"),8,muted,"right")
  for (pr in c("TSMM","Dual-Anchor")){
    z <- score_all[score_all$prior==pr,]
    x <- cx(z$score)+(if(pr=="TSMM")-.047 else .047)
    hi <- cy(z$change_logits+z$MCSE_change_logits);lo <- cy(z$change_logits-z$MCSE_change_logits)
    line(x,lo,x,hi,pal[pr],1);line(x-.023,lo,x+.023,lo,pal[pr],.8);line(x-.023,hi,x+.023,hi,pal[pr],.8)
    dot(x,cy(z$change_logits),pr,5.4)
  }
  z <- score_all[score_all$prior=="TSMM",]
  lab(.02,5.02,"Raw score",8.0,muted);lab(cx(z$score),5.02,as.character(z$score),8,ink,"centre")
  lab(.02,5.21,"Persons",8.0,muted);lab(cx(z$score),5.21,as.character(z$respondents),7.55,muted,"centre")
  dot(4.57,2.91,"TSMM",6.0);lab(4.68,2.91,"TSMM",8.1)
  dot(5.50,2.91,"Dual-Anchor",6.7);lab(5.63,2.91,"Dual-Anchor",8.1)
}
# WinAnsi handles the ± sign; explicit device settings preserve type and line lengths.
grDevices::pdf(file.path(out,"osm-figure-H.3.pdf"),W,H,useDingbats=FALSE)
plot_data();invisible(dev.off())
