# fit  LM-OSL  with 3 exponentials using Luminescence package
rm(list=ls())
suppressMessages(library("Luminescence",warn.conflicts = FALSE))
## fit LM data with background subtraction
CaF2LMx <- read.table("CaF2LMx.txt")
CaF2LMy <- read.table("CaF2LMy.txt")
d<-data.frame(CaF2LMx,CaF2LMy)
bgdx<-seq(from=15,to=1000,by=15)
bgdy<-seq(from=3,to=200,by=3)
bgd<-data.frame(bgdx,bgdy)
invisible(capture.output(a<-fit_LMCurve(d,bgd,
                                        n.components = 3, main=" ",
                                        start_values = data.frame(Im = c(1500,800,500),
                                                                  xm = c(30,130,200)))))
cat("\nImax values: ",a$data$Im1,a$data$Im2,a$data$Im3)
cat("\ntmax values: ",a$data$xm1,a$data$xm2,a$data$xm3)
cat("\nn0 values: ",a$data$n01,a$data$n02,a$data$n03)
cat("\nCross-section values (cm^2)")
cat("\n",a$data$cs1,a$data$cs2,a$data$cs3)
cat("\nR^2=",a$data$`pseudo-R^2`)