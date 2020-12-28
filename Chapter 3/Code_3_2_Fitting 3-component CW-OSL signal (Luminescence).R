# Fitting CW-OSL  with package Luminescence (3 components)
rm(list=ls())
suppressMessages(library("Luminescence",warn.conflicts= FALSE))
##load and fit the data
data <- read.table("KST4ph300IR.txt")
data<-data.frame(data[1:800,1],data[1:800,2]) #use t=1-800 s
invisible(capture.output(fit <- fit_CWCurve(
  values = data,
  main = " ",
  n.components.max = 3)))
cat("\nBest fit parameters"," ")
cat("\nI01=",fit$data$I01,"  I02=",fit$data$I02,
    "  I03=",fit$data$I03)
cat("\nlambda1=",fit$data$lambda1,"  lambda2=",fit$data$lambda2)
cat("\nlambda3=",fit$data$lambda3)