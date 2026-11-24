# Author: JoonHo Lee (jlee296@ua.edu)
# Accepted 104 finite-CDF layout, evaluated on the original grids.
library(grid)
curves <- read.csv(file.path(frames,"finite-cdf-curves.csv"))
ranges <- read.csv(file.path(frames,"pointwise-prior-ranges.csv"))
maxima <- read.csv(file.path(frames,"grid-maxima.csv"))
apps <- c("star","bd","rasch")
app_label <- c("Project STAR","Writing-to-learn meta-analysis","Vocabulary checklist")
unit_label <- c("79 schools","48 studies","500 respondents")
effect <- c("Small-class effect (standard deviations)","Achievement effect (standardized mean difference)","Vocabulary ability (logits)")
cols <- c("#343B40","#0072A4","#BC4D25","#92702C","#00795D")
patterns <- c("solid","52","solid","32","13")
ink <- "#19272E"
soft <- "#44535C"
gap_col <- "#654781"
area <- "#DEE3E6"
all_three <- "Primary three"
all_five <- "All five"
width <- 6.0; height <- 6.38
u <- function(x) unit(x,"in")
v <- function(y) unit(height-y,"in")
txt <- function(x,y,s,fontsize=8,colour=ink,just="left",font="plain"){
  grid.text(s,u(x),v(y),just=c(just,"centre"),
            gp=gpar(fontsize=fontsize,fontfamily="sans",fontface=font,col=colour))
}
seg <- function(x,y,x1,y1,col="#DEE3E4",lw=.55,lt="solid") {
  grid.segments(u(x),v(y),u(x1),v(y1),gp=gpar(col=col,lwd=lw,lty=lt,lineend="butt"))
}
path <- function(x,y,col,lw,lt="solid"){
  grid.lines(u(x),v(y),gp=gpar(col=col,lwd=lw,lty=lt,linejoin="round",lineend="round"))
}
draw <- function() {
  grid.newpage()
  txt(.075,.11,"A   Distribution among observed units",10.2,font="bold")
  txt(3.31,.11,"B   How much the CDFs differ",10.2,font="bold")
  txt(.075,.32,"Posterior mean CDF and pointwise 95% intervals",7.45,soft)
  txt(3.31,.32,"Range of posterior means (percentage points)",7.45,soft)
  for (i in seq_along(apps)) {
    id <- apps[i]; C <- curves[curves$app==id,]; R <- ranges[ranges$app==id,]
    m <- maxima[maxima$app==id & maxima$set==all_five, ]
    title_y <- .58+(i-1)*1.55
    bot <- title_y+1.04; top <- title_y+.18
    xends <- range(C$x)
    xf1 <- function(x) .455 + (x-xends[1])/diff(xends)*2.42
    xf2 <- function(x) 3.46 + (x-xends[1])/diff(xends)*2.39
    yf1 <- function(p) bot-p*(bot-top)
    yf2 <- function(p) bot-p*(bot-top)/1.25
    txt(.075,title_y,paste0(app_label[i]," (",unit_label[i],")"),9.1,font="bold")
    txt(5.92,title_y,sprintf("Maximum across five priors: %.2f pp",m$max_gap_pp),7.85,soft,"right")
    for(q in c(0,.5,1)){
      seg(xf1(xends[1]),yf1(q),xf1(xends[2]),yf1(q))
      txt(.405,yf1(q),sprintf("%d%%",round(100*q)),7.25,soft,"right")
    }
    for(q in c(0,.5,1)){
      seg(xf2(xends[1]),yf2(q),xf2(xends[2]),yf2(q))
      txt(3.405,yf2(q),format(q,nsmall=1),7.25,soft,"right")
    }
    priors <- c("Gamma(1,1)","TSMM","Dual-Anchor",if(id=="rasch") "Gamma(1,3) reference" else "DORO-Unif","Dual-Anchor (hard)")
    ref <- subset(C,prior==priors[1]); stopifnot(!is.unsorted(ref$x))
    grid.polygon(u(xf1(c(ref$x,rev(ref$x)))),v(yf1(c(ref$lower,rev(ref$upper)))),
                 gp=gpar(col=NA,fill=area))
    for(j in seq_along(priors)){
      d <- C[C$prior==priors[j], ]; stopifnot(!is.unsorted(d$x))
      for(which in c("lower","upper"))
        path(xf1(d$x),yf1(d[[which]]),adjustcolor(cols[j],.47),.33)
    }
    for(j in seq_along(priors)){
      d <- C[C$prior==priors[j], ]
      path(xf1(d$x),yf1(d$mean),cols[j],.83,patterns[j])
    }
    # A light fill marks the region between the two descriptive ranges.
    g5 <- subset(R,set==all_five); g3 <- subset(R,set==all_three)
    stopifnot(identical(g5$x,g3$x),all(g3$range_pp <= g5$range_pp + 1e-12))
    grid.polygon(u(xf2(c(g5$x,rev(g3$x)))),v(yf2(c(g5$range_pp,rev(g3$range_pp)))),
                 gp=gpar(col=NA,fill="#F0ECF2"))
    path(xf2(g5$x),yf2(g5$range_pp),gap_col,1.28)
    path(xf2(g3$x),yf2(g3$range_pp),"#48535D",1.05,"52")
    ticks <- if(id=="rasch") c(-4,0,3,6,9) else c(-2,-1,0,1,2)
    for(xf in list(xf1,xf2)){
      seg(xf(xends[1]),bot,xf(xends[2]),bot,"#A7B1B6",.60)
      seg(xf(ticks),bot,xf(ticks),bot+.036,"#859198",.60)
      txt(xf(ticks),bot+.132,as.character(ticks),7.45,soft,"centre")
    }
    txt(3,bot+.323,effect[i],8.05,ink,"centre")
    if (i<3) seg(.075,bot+.425,5.92,bot+.425,"#CBD4D8",.60)
  }
  # The main and sensitivity fits have a common role across applications.
  # External references have different parameter values and are named in the caption.
  key <- function(x,y,label,col,lt="solid",lwd=1.35){
    seg(x,y,x+.24,y,col,lwd,lt); txt(x+.30,y,label,7.65)
  }
  txt(.075,5.32,"CDF curves",8.30,font="bold")
  key(.075,5.54,"Gamma(1,1)",cols[1],patterns[1])
  key(1.57,5.54,"TSMM",cols[2],patterns[2])
  key(.075,5.76,"Dual-Anchor",cols[3],patterns[3])
  key(1.57,5.76,"External reference",cols[4],patterns[4])
  key(.075,5.98,"Dual-Anchor (hard)",cols[5],patterns[5])
  grid.rect(u(.19),v(6.20),width=u(.23),height=u(.09),gp=gpar(fill=area,col=NA))
  txt(.375,6.20,"Gamma(1,1) pointwise interval",7.65)
  txt(3.31,5.32,"Prior comparisons",8.30,font="bold")
  key(3.31,5.54,"All five priors",gap_col)
  key(3.31,5.76,"Three main priors", "#48535D","52")
  txt(3.31,5.99,"Gamma(1,1), TSMM and Dual-Anchor",7.3,soft)
}
grDevices::pdf(file.path(out,"osm-figure-H.2.pdf"),width=width,height=height,
               family="Helvetica",useDingbats=FALSE,title="Finite-sample distributions in three applications")
draw();invisible(dev.off())
