# model.R - Run forecast
# 2020_sol.27.4_forecast/model.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


install.packages("devtools")
install.packages("FLCore", repo = "http://flr-project.org/R")
install_github("ices-tools-prod/msy")
install.packages(c("FLa4a", "FLasher", "ggplotFL"), repos="https://flr-project.org/R")
install.packages(c("ggplot2", "snpar", "foreach", "data.table"))
devtools::install_github("flr/a4adiags")


library(FLCore)
library(icesTAF)
library(ggplot2)
library(ggplotFL)
library(FLFishery)
library(FLasher)
library(FLa4a)
library (a4adiags)

getwd()

setwd("~/GRUPOS DE TRABAJO/WGBIE_2023/2.Assessment_mgw78_WGBIE23")



mkdir("model")

# Load assessment

load('Refpoints_WKMEGRIM_megrim78/Megrim_2020_EqSim_Workspace.RData')
#AI: importante cargar primero el archivo de Refpoints y luego el Fit, porque en este ultimo tenemos todos los años del assessment.

load('model/MegFit_FINAL.RData')

run <- stk1 # replace XXX with the final FLStock object


#years <- range(stks)["minyear"]:range(stks)["maxyear"]

# --- SETUP

# refpts: Bpa, Fpa, Blim, Flim, BmsyTrig, Fmsy, FmsyLow, FmsyUpp

# MODEL, ADVICE and FORECAST year

dy <- dims(run)$maxyear #last year of assessment input data
ay <- dy + 1
fy <- ay + 1

# TAC & advice current year

#advice <- FLQuant(19184, dimnames=list(age='all', year=ay), units="tonnes")  aqui se pondría el consejo para el año del grupo de trabajo es decir, en WGBIE 2022 hay que poner el consejo para el 2022. 

advice <- FLQuant(23596, dimnames=list(age='all', year=ay), units="tonnes") #en WGBIE 2023, advice para el año 2023 son 23596 tn.
tac <- advice


# GEOMEAN but last year
#rec1gm <- exp(mean(log(window(stock.n(run)["1",], end=-1))))

# GEOMEAN for all years minus last 2
rec1gm <- exp(mean(log(window(stock.n(run)["1",], end=-2))))

# GEOMEAN for years 1984:2018. AI: used in WGGBIE 2021 (GEOMEAN for all years minus last 2) # AI: distintas formas de obtener "rec1gm" en el primero por defecto, en este hay que modificar el año manualmente.
rec1gm <- exp(mean(log(window(stock.n(run)["1",], start=1984, end=2020))))

#################
# GEOMEAN: para cambiar el reclutamiento del último año del assessment por geomean si consideramos que tenemos mucha incertidumbre.

#plot(run@stock.n['1',])
#run@stock.n['1','2022'] <- rec1gm #AI: oara la evalución con datos hasta 2022.



# --- SETUP future

# 3 years, 5 years wts/selex, 3 years discards
fut <- stf(run, nyears=3, wts.nyears = 5, fbar.nyears=5, disc.nyears=3)  

# GET F status quo (Fsq)
Fsq <- expand(yearMeans(fbar(fut)[, ac(seq(dy - 2, dy))]), year=2023) #F NOT SCALED (Average F last 3 years)
#Fsq <- expand(fbar(fut)[, ac(dy)], year=ay) # #F SCALED TO THE LAST YEAR

#######################################################################################
# #AI script used in WGBIE 2021 for Fsq
# #PROJECTIONS: F SCALED OR UNSCALED??
# 
# fsq <- fsq <- mean(fbar(stk1)[,nyears-2:0]) #F NOT SCALED (Average F(2017-2019))
# #fsq <- fsq <- fbar(stk1)[,nyears]  #F SCALED TO THE LAST YEAR

########################################################################################

# SET geomean SRR
gmsrr <- predictModel(model=rec~a, params=FLPar(c(rec1gm), units="thousands",
  dimnames=list(params="a", year=seq(ay, length=3), iter=1)))

# > gmsrr   # AI: reemplaza el reclutamiento con rec1gm del año intermedio (2023) y los dos siguientes.
# An object of class "FLQuants": EMPTY
# model:  
#   rec ~ a
# 
# params:  
#   An object of class "FLPar"
# year
# params    2022        2023      2024  
# a        221059      221059    221059
# units:  thousands 

# GENERATE targets from refpts
refpts <- FLPar(refPts[1,])
targets <- expand(as(refpts, 'FLQuant'), year=fy)

##############################################################################
#AI: added to include a new variable in the output
# GENERATE targets from refpts   
#need to run the forecast once to see what the SSB at the start of the advice year is: as.numeric(ssb(runs[[1]])[,as.character(fy)])

as.numeric(ssb(runs[[1]])[,as.character(fy)]) 
#95693.08


# Then enter it manually
refPts <- cbind(refPts,  95693) 
dimnames(refPts)[[2]][ncol(refPts)] <- "constSSB"
refpts <- FLPar(refPts[1,])
targets <- expand(as(refpts, 'FLQuant'), year=fy)


#################

#AI: to complete in the advice the assumptions for interim year and forecast.


fbar(runs$Fmsy)[, '2023']
fbar(runs$Fmsy)[, '2024']
ssb(runs$Fmsy)[, '2024']
rec(runs$Fmsy)[, '2024']
catch(runs$Fmsy)[, '2023']
landings(runs$Fmsy)[, '2023']
discards(runs$Fmsy)[, '2023']

#Otro forma de obtener los valores de los años intermedios:

# as.numeric(catch(runs[[1]])[,as.character(ay)])
# #[1] 19993.89
# ay
# #[1] 2022
# as.numeric(landings(runs[[1]])[,as.character(ay)])
# #[1] 17123.85
# as.numeric(discards(runs[[1]])[,as.character(ay)])
# #[1] 2870.042


ssb(runs$Fmsy)[, '2023'] # La SSB del año intermedio (2023) hay que incluirla en la tabla del advice donde se da la summary table pero este ultimo año no hay valor (NA).
#95559

###############################################################################


# --- PROJECT catch options

# Targets 2021
C0 <- FLQuant(0, dimnames=list(age='all', year=fy))
Fiy <- FLQuants(fbar=append(Fsq, C0))
Ciy <- FLQuants(catch=append(tac, C0))

# RUN for Fsq
Fsqrun <- fwd(fut, sr=gmsrr, control=as(Fiy, "fwdControl"))

# TEST if Cay < TACay
if(catch(Fsqrun)[, ac(ay)] <= tac) {
  itarget <- Fiy
} else {
  itarget <- Ciy
  Fsqrun <- fwd(fut, sr=gmsrr, control=as(Ciy, "fwdControl"))
}

# DEFINE catch options
catch_options <- list(

  # FMSY
  Fmsy=FLQuants(fbar=targets["Fmsy",]),

  # lowFMSY
  lFmsy=FLQuants(fbar=targets["Fmsylower",]),

  # uppFMSY
  uFmsy=FLQuants(fbar=targets["Fmsyupper",]),

  # F0
  F0=FLQuants(fbar=FLQuant(0, dimnames=list(age='all', year=fy))),

  # Fpa
  Fpa=FLQuants(fbar=targets["Fpa",]),

  # Flim
  Flim=FLQuants(fbar=targets["Flim",]),

  # Bpa
  Bpa=FLQuants(ssb_flash=targets["Bpa",]),

  # Blim
  Blim=FLQuants(ssb_flash=targets["Blim",]),

  # MSYBtrigger
  MSYBtrigger=FLQuants(ssb_flash=targets["MSYBtrigger",]),

  # F sq
  Fsq=FLQuants(fbar=expand(fbar(Fsqrun)[, ac(ay)], year=fy)),
  
  # SB sq
  SBsq=FLQuants(ssb_flash=expand(ssb(Fsqrun)[, ac(ay+1)], year=fy)),
  
  # TAC 2021
  rotac=FLQuants(catch=expand(tac, year=fy))
)

# C0, 2023
F0 <- FLQuants(fbar=FLQuant(0, dimnames=list(age='all', year=fy + 1)))

# CONVERT to fwdControl

fctls <- lapply(catch_options, function(x) {
  as(FLQuants(c(window(itarget, end=ay), x, F0)), "fwdControl")
}
)

# RUN!

runs <- FLStocks(lapply(fctls, function(x) fwd(fut, sr=gmsrr, control=x)))

# SSB 2024




# COMPARE

Map(compare, runs, fctls)


# --- PROJECT F levels

flevels <- seq(0, 0.50, 0.01)

control <- as(as(c(lapply(window(itarget, end=ay), propagate, length(flevels)),
  FLQuants(fbar=FLQuant(flevels,  dimnames=list(year=fy, iter=seq(length(flevels))))),
  lapply(F0, propagate, length(flevels))), "FLQuants"), "fwdControl")

f_runs <- divide(fwd(fut, sr=gmsrr, control=control), names=flevels)

# SAVE

save(runs, f_runs, rec1gm, tac, advice, file="model/runs.RData")





