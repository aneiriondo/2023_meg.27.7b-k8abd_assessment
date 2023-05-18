## Run analysis, write model results


# install.packages("devtools")
# install.packages("FLCore", repo = "http://flr-project.org/R")
# install_github("ices-tools-prod/msy")
# install.packages("ggplotFL", repos="http://flr-project.org/R")
# #install.packages("msy", repos="http://flr-project.org/R")
# install.packages(c("copula","triangle", "coda"))
# install.packages("FLEDA", repos="http://flr-project.org/R")
# install.packages("FLFishery", repos="http://flr-project.org/R")
# 
# install.packages(c("FLa4a", "FLasher", "ggplotFL"), repos="https://flr-project.org/R")
# install.packages(c("ggplot2", "snpar", "foreach", "data.table"))
# devtools::install_github("flr/a4adiags")



library(devtools)
library(FLCore)
library(icesTAF)
library(FLa4a)
library(FLEDA)
library(ggplotFL)
library(gridExtra)
library(icesAdvice)
library(FLFishery)
library(FLasher)
library(msy)
library(a4adiags)
library(devtools)
library(ggplot2)


#2023_meg.27.7b-k8abd_assessment
#a4a model assessment model

#  #TO WORK from TAF
# # 
taf.bootstrap()
# # # load index and stock

load(taf.data.path("stock", "meg78_stock.RData"))
load(taf.data.path("indices", "meg78_indices.RData"))
# 
mkdir("model")

# specify submodels defined as follows:
#==============================================================================
#FIT: ONLY SURVEYS

tun.sel2<- tun.sel[c("SP_PORC","CPUE.IRLFRsurvey")]

#strange outlier, set to NA in IRLFRsurvey 2011

tun.sel2[[2]]@index['1','2011'] <- NA

#we do not really believe that the increase in 1-year-olds in the catch is real so we shouldn't formulate a model that treats it as real. I think the better approach is to 1984:

plot(stock@catch.n['1',])
stock@catch.n['1',as.character(1984:2000)] <- NA

# Also deal with  pretty flat after age 7 so reduce the age where they are 'bound'

qmod <- list(~ I(1 / (1 + exp(-age))),~ I(1 / (1 + exp(-age))))
srmod <- ~ factor(year) 

fmod <- ~ factor(replace(age,age>7,7)) + factor(year)

fit1 <- sca(stock, tun.sel2, fmodel = fmod, qmodel = qmod, srmodel = srmod)

stk1 <- stock + fit1

tun.sel2

#==============================================================================

#FIT 1: FIT FINAL

stk1 <- stock + fit1

plot(stk1)

# utilities
SavePlot <- function(plotname, width = 10, height = 7) {
  file <- file.path("report", paste0("meg78_data_", plotname, ".png"))
  dev.print(png, file, width = width, height = height, units = "in", res = 600)
}

save(stk1,stock, tun.sel2, fit1, file = "model/MegFit_FINAL.Rdata")

submodels(fit1)

# fmodel: ~factor(replace(age, age > 7, 7)) + factor(year)
# srmodel: ~factor(year)
# n1model: ~s(age, k = 3)
# qmodel:
#   SP_PORC:          ~I(1/(1 + exp(-age)))
# CPUE.IRLFRsurvey: ~I(1/(1 + exp(-age)))
# vmodel:
#   catch:            ~s(age, k = 3)
# SP_PORC:          ~1
# CPUE.IRLFRsurvey: ~1

name(stk1) <- 'Run1'

AIC(fit1)

BIC(fit1)

idx<-tun.sel2

#==============================================================================
# RETRO
#==============================================================================

#retro without smoothers-> 
back <- 5
retro <- split(1:back, 1:back)
retro <- lapply(retro, function(x){
  yr <- range(stock)["maxyear"] - x
  stk <- window(stock, end=yr)
  idx <- window(idx, end=yr)
  stk + sca(stk, idx,fmodel=fmod, qmodel=qmod, srmodel=srmod)
})

#FLStocks(retro)

mkdir("report")

retro$"0" <- stock+fit1

plot(FLStocks(retro),col=1,lwd=1)+ facet_wrap(~qname,scales='free_y')

save(retro, file = "model/MegRetroFit_FINAL.Rdata")

SavePlot('Retro_Fit_FINAL')

#==============================================================================
# Retro value calculation
#==============================================================================

Retro_F <- data.frame(Y0=c(fbar(retro$`0`))
                      ,Y1=c(fbar(retro$`1`),NA)
                      ,Y2=c(fbar(retro$`2`),NA,NA)
                      ,Y3=c(fbar(retro$`3`),NA,NA,NA)
                      ,Y4=c(fbar(retro$`4`),NA,NA,NA,NA)                
                      ,Y5=c(fbar(retro$`5`),NA,NA,NA,NA,NA)
)

mohn(Retro_F,details=T) 
mohn(Retro_F,plot=T) 

error <- function(years,x,se){
  #  polygon(c(years,rev(years)),c(x-se,rev(c(x+se))),col='#0000FF20',border=NA)
  polygon(c(years,rev(years)),c(x-1.96*se,rev(c(x+1.96*se))),col='#0000FF20',border=NA)
}

pal <- c("#7570B3","#1B9E77","#E6AB02","#D95F02","#E7298A")
years <- stk1@range[4]:stk1@range[5]
nyears <-length(years)
fits <- simulate(fit1, 1000)
stks <- stock + fits
fbar <- fbar(stk1)
fbarse <- apply(fbar(stks),2,sd) # AI: considering 1sd
fbar2sd <- 2*apply(fbar(stks),2,sd) # AI: considering 2sd
ylim <- c(0,max(fbar+1.96*fbarse))
ylim <- c(0,max(fbar+1.96*fbar2sd))
xlim <-range(years)
plot(NA,xlim=xlim,ylim=ylim,xlab='Year',ylab='F',main=paste0('Fbar ',stk1@range[6],'-',stk1@range[7]))
error(years,fbar,fbarse)
error(years,fbar,fbar2sd)
for(i in 1:5) lines(years[1:(nyears-i)],fbar(retro[[i]]),lwd=1,col=pal[i])
lines(years,fbar,lwd=2,col=4)


Retro_SSB <- data.frame(Y0=c(ssb(retro$`0`))
                        ,Y1=c(ssb(retro$`1`),NA)
                        ,Y2=c(ssb(retro$`2`),NA,NA)
                        ,Y3=c(ssb(retro$`3`),NA,NA,NA)
                        ,Y4=c(ssb(retro$`4`),NA,NA,NA,NA)                
                        ,Y5=c(ssb(retro$`5`),NA,NA,NA,NA,NA)
)

ssb <- ssb(stk1)/1000
ssbse <- apply(ssb(stks),2,sd)/1000 # AI: considering 1sd
ssb2sd <- 2*apply(ssb(stks),2,sd)/1000 # AI: considering 2sd
ylim <- c(0,max(ssb+1.96*ssbse))# AI: considering 1sd
ylim <- c(0,max(ssb+ssb2sd))# AI: considering 2sd
xlim <-range(years)
plot(NA,xlim=xlim,ylim=ylim,xlab='Year',ylab='F',main=paste0('SSB '))
error(years,ssb,ssbse) #1sd
error(years,ssb,ssb2sd) #2sd
for(i in 1:5) lines(years[1:(nyears-i)],ssb(retro[[i]])/1000,lwd=1,col=pal[i])
lines(years,ssb,lwd=2,col=4)


mohn(Retro_SSB) 
mohn(Retro_SSB,details=T) 
mohn(Retro_SSB,plot=T) 


recr <- function(x) x@stock.n[1,]
Retro_R <- data.frame(Y0=c(recr(retro$`0`))
                      ,Y1=c(recr(retro$`1`),NA)
                      ,Y2=c(recr(retro$`2`),NA,NA)
                      ,Y3=c(recr(retro$`3`),NA,NA,NA)
                      ,Y4=c(recr(retro$`4`),NA,NA,NA,NA)                
                      ,Y5=c(recr(retro$`5`),NA,NA,NA,NA,NA)
)
mohn(Retro_R) 
mohn(Retro_R,details=T) 
mohn(Retro_R,plot=T) 


AIC(fit1)
BIC(fit1)

