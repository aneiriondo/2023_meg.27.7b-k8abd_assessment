## Run analysis, write model results


install.packages("devtools")
install.packages("FLCore", repo = "http://flr-project.org/R")
install_github("ices-tools-prod/msy")
install.packages("ggplotFL", repos="http://flr-project.org/R")
#install.packages("msy", repos="http://flr-project.org/R")
install.packages(c("copula","triangle", "coda"))
install.packages("FLEDA", repos="http://flr-project.org/R")
install.packages("FLFishery", repos="http://flr-project.org/R")
install.packages(c("FLa4a", "FLasher", "ggplotFL"), repos="https://flr-project.org/R")
install.packages(c("ggplot2", "snpar", "foreach", "data.table"))
devtools::install_github("flr/a4adiags")

library(icesTAF)

#==============================================================================
# REPORT SECTION
#==============================================================================

mkdir("report")

# load fit, index and stock

#load("model/MegFit_FINAL.Rdata")

# load retro run
#load("model/MegRetroFit_FINAL.Rdata")

stk1 <- stock + fit1
plot(stk1)
name(stk1) <- 'Run1'

AIC(fit1)
BIC(fit1)


# hacked function from a4a Fitresiduals-class
plotr <- function(x, y=missing, auxline="smooth",...){
  args <- list()
  args$data <- as.data.frame(x)
  args$x <- as.formula("data~year|factor(age)*qname")
  args$type=c("p", auxline)
  args$groups <- quote(qname)
  args$cex=0.3
  args$lwd=0
  args$ylab="standardized residuals"
  args$xlab=""
  args$panel=function(x,y,...){
    panel.abline(h=0, col.line="gray80")
    panel.xyplot(x,y,...)
  }
  args$par.settings=list(
    superpose.symbol=list(col=1, pch=19, cex=0.2),
    superpose.line=list(col="gray75", lty=1, lwd=0, col=NA),
    strip.background=list(col="gray90"),
    strip.border=list(col="black"),
    box.rectangle=list(col="gray90"))
  args$main="log residuals of catch and abundance indices by age"
  if(is(latticeExtra::useOuterStrips, "function")) latticeExtra::useOuterStrips(do.call("xyplot", args)) else do.call("xyplot", args)
}

res <- residuals(fit1, stock, tun.sel2)
print(plotr(res, auxline = "none"))
SavePlot('Residuals1',10,6)

res$catch.n[is.na(res$catch.n)] <- 0 #hack
#res$FR_EVHOE[is.na(res$FR_EVHOE)] <- 0 #hack
res$SP_PORC[is.na(res$SP_PORC)] <- 0 #hack
#res$CPUE.Vigo84[is.na(res$CPUE.Vigo84)] <- 0 #hack
#res$CPUE.Vigo99[is.na(res$CPUE.Vigo99)] <- 0 #hack
#res$LPUE.ITBB[is.na(res$LPUE.ITBB)] <- 0 #hack
#res$CPUE.IAM[is.na(res$CPUE.IAM)] <- 0 #hack
res$CPUE.IRLFRsurvey[is.na(res$CPUE.IRLFRsurvey)] <- 0 #hack

bubbles(res)
SavePlot('Residuals2',10,6)

res <- residuals(fit1, stock, tun.sel2)
qqmath(res)
SavePlot('Residuals3')

wireframe(fit1@harvest)
#a <- xyplot(data~age,groups=year,stk1@harvest,type='b',ylim=c(0,0.9),ylab='F',main='Fishing mortality')
a <- xyplot(data~age,groups=year,stk1@harvest,type='b',ylim=c(0,1),ylab='F',main='Fishing mortality')
a
SavePlot('F')

#


fitted <- predict(pars(fit1))

#a1 <- xyplot(data~age,groups=year,data=fitted$qmodel[1],type='b',ylab='Catchabilty',main='Q FR_EVHOE',ylim=c(0,4e-5))
#a1
a2 <- xyplot(data~age,groups=year,data=fitted$qmodel[1],type='b',ylab='Catchabilty',main=names(tun.sel2)[1])
a2
#a3 <- xyplot(data~age,groups=year,data=fitted$qmodel[3],type='b',ylab='Catchabilty',main='Q CPUE.Vigo84',ylim=c(0,0.005))
#a3
#a4 <- xyplot(data~age,groups=year,data=fitted$qmodel[4],type='b',ylab='Catchabilty',main='Q CPUE.Vigo99',ylim=c(0,0.0015))
#a4
#a5 <- xyplot(data~age,groups=year,data=fitted$qmodel[5],type='b',ylab='Catchabilty',main='Q LPUE.ITBB',ylim=c(0,0.02))
#a5
#a6 <- xyplot(data~age,groups=year,data=fitted$qmodel[6],type='b',ylab='Catchabilty',main='CPUE.IAM',ylim=c(0,0.03))
#a6
a7 <- xyplot(data~age,groups=year,data=fitted$qmodel[2],type='b',ylab='Catchabilty',main=names(tun.sel2)[2])
a7

grid.arrange(a,a2,a7,ncol=2)
SavePlot('F and Q',8,6)




# simulate

fits <- simulate(fit1, 1000)
flqs <- FLQuants(sim=iterMedians(stock.n(fits)), det=stock.n(fit1))
keylst <- list(points=FALSE, lines=TRUE, space="right")
xyplot(data~year|factor(age), groups=qname, data=flqs, type="l", main="Median simulations VS fit", scales=list(y=list(relation="free")), auto.key=keylst)
#fits@stock.n[,1:2] <- fit1@stock.n[,1:2]
#fits@catch.n[,1:2] <- fit1@catch.n[,1:2]
stks <- stock + fits
plot(stks) + facet_wrap(~qname,scales='free_y')
SavePlot('summary0')


#==============================================================================
# PROJECTIONS: GEOMETRIC MEAN AND F SCALED OR UNSCALED
#==============================================================================

#summary table for the report 

years <- stk1@range[4]:stk1@range[5]
nyears <- length(years)
GM <- round(exp(mean(log(c(stk1@stock.n[1,1:(nyears-2)])))),0)/1000 #Geometric mean of recruitment (1984-2 last years); thousands.
GM


fsq <- fsq <- mean(fbar(stk1)[,nyears-2:0]) #F NOT SCALED (Average F(2020-2022))
#fsq <- fsq <- fbar(stk1)[,nyears]  #F SCALED TO THE LAST YEAR

# AI: 08/06/2022 in the ADG the 2sd is requested and it is included in all xxse variables for example: tsbse <- 2*apply(tsb(stks),2,sd).

#Summary table using coeficient interval of 1sd

tsb <- tsb(stk1)
tsbse <- apply(tsb(stks),2,sd)
ssb <- ssb(stk1)
ssbse <- apply(ssb(stks),2,sd)
ssbInt <- NA #get this from stf
catchobs <- stock@catch
discardsobs <- stock@discards
catch <- stk1@catch
recr <- stk1@stock.n[1,]
recrse <- apply(stks@stock.n[1,],2,sd)
fbar <- fbar(stk1)
fbarse <- apply(fbar(stks),2,sd)

#Summary table using coeficient interval of 2sd

tsb <- tsb(stk1)
tsb2sd <- 2*apply(tsb(stks),2,sd)
ssb <- ssb(stk1)
ssb2sd <- 2*apply(ssb(stks),2,sd)
ssbInt <- NA #get this from stf
catchobs <- stock@catch
discardsobs <- stock@discards
catch <- stk1@catch
recr <- stk1@stock.n[1,]
recr2sd <- 2*apply(stks@stock.n[1,],2,sd)
fbar <- fbar(stk1)
fbar2sd <- 2*apply(fbar(stks),2,sd)


sum1 <-
  data.frame(
    Year = c(years, paste0(max(years) + 1, "*")),
    Lan = c(catchobs - discardsobs, NA),
    Dis = c(discardsobs, NA),
    Cat = c(catchobs, NA),
    CatEst = c(catch, NA),
    Tsb = c(tsb, NA),
    Ssb = c(ssb, ssbInt),
    SsbCv = c(ssbse / ssb, NA),
    Recr = c(recr, GM),
    RecrCv = c(recrse / recr, NA),
    Fbar = c(fbar, fsq),
    FbarCv = c(fbarse / fbar, NA)
  )

knitr::kable(sum1,row.names=F,digits=c(rep(3,12)))
write.csv(sum1,'report/Summary_FINAL.csv',row.names=F)

plot(years,tsb,type='l',ylim=c(0,120))
lines(years,ssb,type='l',col=2)
plot(years,ssb/tsb,type='l',ylim=c(0,1))


#summary for standard graphs template with coeficient interval of 1sd
sum2 <- data.frame(year=c(years,max(years+1))
                   ,recrlo=c(recr-recrse,NA)
                   ,recr=c(recr,GM)
                   ,recrhi=c(recr+recrse,NA)
                   ,tsblo=c(tsb-tsbse,NA)
                   ,tsb=c(tsb,NA)
                   ,tsbhi=c(tsb+tsbse,NA)
                   ,ssblo=c(ssb-ssbse,NA)
                   ,ssb=c(ssb,ssbInt)
                   ,ssbhi=c(ssb+ssbse,NA)
                   ,catch=c(catchobs,NA)
                   ,lan=c(catchobs-discardsobs,NA)
                   ,dis=c(discardsobs,NA)
                   ,ibc=NA
                   ,ur=NA
                   ,yssb=NA
                   ,flo=c(fbar-fbarse,NA)
                   ,f=c(fbar,fsq)
                   ,fhi=c(fbar+fbarse,NA)
)
knitr::kable(sum2,row.names=F,digits=c(rep(0,16),rep(3,3)))
write.csv(sum2,'report/Summary_2_1sd.csv', row.names=F)


#summary for standard graphs template with coeficient interval of 2sd
sum3 <- data.frame(year=c(years,max(years+1))
                   ,recrlo=c(recr-recr2sd,NA)
                   ,recr=c(recr,GM)
                   ,recrhi=c(recr+recr2sd,NA)
                   ,tsblo=c(tsb-tsb2sd,NA)
                   ,tsb=c(tsb,NA)
                   ,tsbhi=c(tsb+tsb2sd,NA)
                   ,ssblo=c(ssb-ssb2sd,NA)
                   ,ssb=c(ssb,ssbInt)
                   ,ssbhi=c(ssb+ssb2sd,NA)
                   ,catch=c(catchobs,NA)
                   ,lan=c(catchobs-discardsobs,NA)
                   ,dis=c(discardsobs,NA)
                   ,ibc=NA
                   ,ur=NA
                   ,yssb=NA
                   ,flo=c(fbar-fbar2sd,NA)
                   ,f=c(fbar,fsq)
                   ,fhi=c(fbar+fbar2sd,NA)
)
knitr::kable(sum3,row.names=F,digits=c(rep(0,16),rep(3,3)))
write.csv(sum3,'report/Summary_3_2sd.csv', row.names=F)


# sensitivity for template - not needed anymore?
ages <- stk1@range[1]:stk1@range[2]
stk3y <- window(stk1, start = max(years) - 2)
p <- apply(landings.n(stk3y) / catch.n(stk3y), 1, mean, na.rm = T)
p <- c(ifelse(is.na(p), 0, p))
sen <- data.frame(
  Age = ages,
  M = c(apply(m(stk3y), 1, mean)),
  Mat = c(apply(mat(stk3y), 1, mean)),
  PF = c(apply(harvest.spwn(stk3y), 1, mean)),
  PM = c(apply(m.spwn(stk3y), 1, mean)),
  Sel = c(apply(harvest(stk3y), 1, mean)) * p,
  WeCa = c(apply(landings.wt(stk3y), 1, mean)),
  Fd = c(apply(harvest(stk3y), 1, mean)) * (1 - p),
  WeCad = c(apply(discards.wt(stk3y), 1, mean)),
  Fi = 0,
  WeCai = 0
)
knitr::kable(sen, row.names = F, digits = c(0, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3))
write.csv(sen, "report/Sen.csv", row.names = F)


plot(fit1, stock)
SavePlot("Fit1")
plot(fit1, tun.sel[1])
SavePlot("Fit2")
plot(fit1, tun.sel[2])
SavePlot("Fit3")
plot(fit1, tun.sel[3])
SavePlot("Fit4")
plot(fit1, tun.sel[4])
SavePlot("Fit5")
plot(fit1, tun.sel[5])
SavePlot("Fit6")
plot(fit1, tun.sel[6])
SavePlot("Fit7")
plot(fit1, tun.sel[7])
SavePlot("Fit8")

fitSumm(fit1)
#    iters
# 1
# nopar       9.900000e+01
# nlogl       8.020026e+02
# maxgrad     1.068531e-05
# nobs        1.043000e+03
# gcv         3.957918e-01
# convergence 0.000000e+00
# accrate               NA
# nlogl_comp1 1.065390e+02
# nlogl_comp2 1.534890e+02
# nlogl_comp3 1.262590e+02
# nlogl_comp4 9.153670e+01
# nlogl_comp5 1.720600e+02
# nlogl_comp6 1.521190e+02




# Summary plot with reference points

error <- function(years, x, se) {
  #  polygon(c(years,rev(years)),c(x-se,rev(c(x+se))),col='#0000FF20',border=NA)
  polygon(c(years, rev(years)), c(x - 1.96 * se, rev(c(x + 1.96 * se))), col = "#0000FF20", border = NA)
}

pal <- c("#7570B3", "#1B9E77", "#E6AB02", "#D95F02", "#E7298A")


#### KOBE PLOT (AI: updated RP for megrim 78 ICES. 2022a. Benchmark Workshop for selected Megrim Stocks (WKMEGRIM)

kobe <- data.frame(Btrig=40444,Fmsy=0.233,B=rev(c(ssb(stk1))),F=rev(c(fbar(stk1))))
with(kobe[1,],plot(B/Btrig,F/Fmsy,xlim=c(0,3.5),ylim=c(0,3),main='meg78'))
rect(-1,-1,5,5,col='yellow',border=NA)
rect(-1,1,1,5,col='red',border=NA)
rect(1,-1,5,1,col='green',border=NA)
with(kobe[1,],text(B/Btrig,F/Fmsy,max(years),pos=1))
with(kobe[1,],points(B/Btrig,F/Fmsy,pch=16))
with(kobe,lines(B/Btrig,F/Fmsy))
SavePlot('Kobe',6,6)

