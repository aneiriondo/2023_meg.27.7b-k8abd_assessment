## ----include=FALSE---------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- warning=F, message=F--------------------------------------------------------------------------------------------------------
library(FLCore)
library(FLEDA)
library(gridExtra)
library(dplyr)
library(tidyr)
library(ggridges)

mkdir("data")

# load index and stock
load(taf.data.path("stock", "meg78_stock.RData"))
load(taf.data.path("indices", "meg78_indices.RData"))

# utilities
SavePlot <- function(plotname, width = 10, height = 7) {
  file <- file.path("data", paste0("meg78_data_", plotname, ".png"))
  dev.print(png, file, width = width, height = height, units = "in", res = 300)
}

## ---------------------------------------------------------------------------------------------------------------------------------
ages <- stock@range["min"]:stock@range["max"]

## ---------------------------------------------------------------------------------------------------------------------------------
stock <- setPlusGroup(stock, 10)
sum(stock@catch.n == 0, na.rm = T)
stock@catch.n[stock@catch.n == 0] <- NA
tun.sel
with(subset(as.data.frame(stock@catch.n), !is.na(data) & data > 0), plot(year, age - 0.3, pch = "-", col = 4, cex = 4, ylim = c(10.5, 0), yaxs = "i", lab = c(8, 8, 7), ylab = "age", main = "Data used in the assessment"))
for (i in 0.5:6.5) abline(h = i, col = "grey")
with(subset(as.data.frame(tun.sel[[1]]@index), !is.na(data)), points(year, age - 0.1, pch = "-", col = 5, cex = 4))
with(subset(as.data.frame(tun.sel[[2]]@index), !is.na(data)), points(year, age + 0.1, pch = "-", col = 3, cex = 4))
with(subset(as.data.frame(tun.sel[[3]]@index), !is.na(data)), points(year, age + 0.3, pch = "-", col = 6, cex = 4))
with(subset(as.data.frame(tun.sel[[4]]@index), !is.na(data)), points(year, age + 0.3, pch = "-", col = 7, cex = 4))
with(subset(as.data.frame(tun.sel[[5]]@index), !is.na(data)), points(year, age + 0.3, pch = "-", col = 8, cex = 4))
legend("topleft", c("Catch nos", "FR_EVHOE", "SP_PORC", "CPUE.Vigo84", "CPUE.Vigo99", "LPUE.ITBB"), pch = "-", pt.cex = 4, col = c(4, 5, 3, 6, 7, 8), ncol = 1)
SavePlot("DataAvailability", 10, 5)


## ---------------------------------------------------------------------------------------------------------------------------------
par(mfrow = c(2, 2))
with(as.data.frame(stock@catch.n), plot(age, log(data), main = "catch.n"))
for (i in 1:5) with(as.data.frame(tun.sel[[i]]@index), plot(age, log(data), main = name(tun.sel[[i]])))
SavePlot("LogNumbersAtAgeDistribution", 10, 10)


## ---------------------------------------------------------------------------------------------------------------------------------
fun <- function(x, digits = 0) with(as.data.frame(x), tapply(round(data, digits), list(year, age), sum))
filename <- "data/Meg78_Input.csv"
write.table("meg78abd input data", filename, sep = ",", quote = F, row.names = F, col.names = F)
write.table("\ncatch.n", filename, append = T, quote = F, sep = ",", row.names = F, col.names = F, eol = ",")
write.table(fun(catch.n(stock)), filename, append = T, quote = F, sep = ",", na = "", row.names = T, col.names = T)


write.table("\nprop.dis", filename, append = T, quote = F, sep = ",", row.names = F, col.names = F, eol = ",")
write.table(fun(discards.n(stock) / catch.n(stock), 5), filename, append = T, quote = F, sep = ",", na = "", row.names = T, col.names = T)

write.table("\ncatch.wt", filename, append = T, quote = F, sep = ",", row.names = F, col.names = F, eol = ",")
write.table(fun(catch.wt(stock), 3), filename, append = T, quote = F, sep = ",", na = "", row.names = T, col.names = T)

write.table("\nstock.wt", filename, append = T, quote = F, sep = ",", row.names = F, col.names = F, eol = ",")
write.table(fun(stock.wt(stock), 3), filename, append = T, quote = F, sep = ",", na = "", row.names = T, col.names = T)

for (i in 1:5) {
  write.table(paste0("\n", name(tun.sel[[i]])), filename, append = T, quote = F, sep = ",", row.names = F, col.names = F, eol = ",")
  write.table(fun(index(tun.sel[[i]]), 3), filename, append = T, quote = F, sep = ",", na = "", row.names = T, col.names = T)
}

## ---------------------------------------------------------------------------------------------------------------------------------
LA <- as.data.frame(stock@landings.n)
DI <- as.data.frame(stock@discards.n)
LA$unit <- "1. landings"
DI$unit <- "2. discards"

barchart(data / 1000 ~ as.factor(age) | as.factor(year), groups = unit, data = na.omit(rbind(LA, DI)), stack = T, as.table = T, ylab = "Catch nos (millions)", col = c("grey", "white"), xlab = "age", par.strip.text = list(cex = 0.8))
SavePlot("cnna1", 10, 5)

LA <- as.data.frame(stock@landings.n * stock@landings.wt)
DI <- as.data.frame(stock@discards.n * stock@discards.wt)
LA$unit <- "1. landings"
DI$unit <- "2. discards"
barchart(data / 1000 ~ as.factor(age) | as.factor(year), groups = unit, data = na.omit(rbind(LA, DI)), stack = T, as.table = T, ylab = "Catch wts (kT)", col = c("grey", "white"), xlab = "age", par.strip.text = list(cex = 0.8))
SavePlot("cnaa2", 10, 5)

## ---------------------------------------------------------------------------------------------------------------------------------
pfun <- function(x, y, groups, subscripts, ...) {
  panel.superpose(x, y, subscripts, type = "l", groups = groups, lwd = 1, ...)
  lpoints(x, y, pch = 16, col = "white", cex = 1.25)
  ltext(x, y, groups, cex = 0.6)
}
a <- xyplot(data ~ year, groups = age, stock@discards.n / stock@catch.n, panel = pfun, ylab = "Proportion discarded", main = "Discards")
b <- xyplot(data ~ age, groups = year, stock@discards.n / stock@catch.n, panel = pfun, ylab = "Proportion discarded", main = "Discards")
grid.arrange(a, b, ncol = 2)
SavePlot("pdis")


## ---------------------------------------------------------------------------------------------------------------------------------
a <- xyplot(data ~ year, groups = age, stock@stock.wt, panel = pfun, ylim = c(0, 1.2), ylab = "Mean weight at age (kg)", main = "Stock weights")
b <- xyplot(data ~ year, groups = age, stock@catch.wt, panel = pfun, ylim = c(0, 1.2), ylab = "Mean weight at age (kg)", main = "Catch weights")
grid.arrange(a, b, ncol = 2)
# b
SavePlot("cwaa")


## ---------------------------------------------------------------------------------------------------------------------------------
a <- catch.n(stock)
a[1:2, as.character(1986:2002)] <- apply(window(a, start = 2003), 1, mean)[1:2, ]
b <- spay(a)
b[is.na(b)] <- 0
b[1:2, as.character(1986:2002)] <- 0
bubbles(age ~ year, b, bub.scale = 5, bub.col = c("#00000050", "#FFFFFF50"), main = "Catch")

SavePlot("bubbles3")

## ---------------------------------------------------------------------------------------------------------------------------------
a <- ccplot(data ~ age, logcc(catch.n(stock)), type = "b", col = "#00000050", ylab = "log ratio", pch = 16, cex = 0.5)
x1 <- logcc(catch.n(stock))
x2 <- FLCohort(catch.n(stock))
x3 <- apply(x1 * x2[rownames(x1), colnames(x1), ], 1, mean, na.rm = T)
x4 <- apply(x2[rownames(x1), colnames(x1), ], 1, mean, na.rm = T)
# b <- xyplot(x3/x4~ages[1:10],ylim=c(-1,1.75),type='b',ylab='weighted mean logratio')
b <- ccplot(data ~ year, logcc(catch.n(stock)), type = "l", col = 1, ylab = "log ratio")
grid.arrange(a, b, ncol = 2)
SavePlot("logratio")

## ---------------------------------------------------------------------------------------------------------------------------------
xyplot(log(data) ~ year, groups = year - age, data = stock@catch.n, type = "l", ylab = "log indices", col = 1)
SavePlot("cc")

## ----warning=F--------------------------------------------------------------------------------------------------------------------
a <- xyplot(data ~ year, z(stock@catch.n, agerng = 3:6)@zy, type = "l", ylab = "mean Z age 3-6", main = "Catch")
x <- stock@catch.n
x[x == 0] <- 1
b <- xyplot(data ~ age, z(x, agerng = 3:6)@za, type = "l", ylab = "mean Z", main = "Catch")
grid.arrange(a, b, ncol = 2)

## ---------------------------------------------------------------------------------------------------------------------------------
x <- trim(index(tun.sel[[1]]), age = 1:5)
x[is.na(x)] <- 0
sp <- spay(x)
sp[is.na(sp)] <- 0
bubbles(age ~ year, sp, bub.scale = 10, bub.col = c("#00000050", "#FFFFFF50"), main = tun.sel[[1]]@name)
SavePlot("bubbles_tun_EVHOE")

## ---------------------------------------------------------------------------------------------------------------------------------
x <- index(tun.sel[[2]])
x[is.na(x)] <- 0
sp <- spay(x)
sp[is.na(sp)] <- 0
bubbles(age ~ year, sp, bub.scale = 6, bub.col = c("#00000050", "#FFFFFF50"), main = tun.sel[[2]]@name)
SavePlot("bubbles_tun_PORCU")

## ---------------------------------------------------------------------------------------------------------------------------------
x <- index(tun.sel[[3]])
x[is.na(x)] <- 0
bubbles(age ~ year, spay(x), bub.scale = 10, bub.col = c("#00000050", "#FFFFFF50"), main = tun.sel[[3]]@name)
SavePlot("bubbles_tun_vigo84")

## ---------------------------------------------------------------------------------------------------------------------------------
x <- index(tun.sel[[4]])
x[is.na(x)] <- 0
bubbles(age ~ year, spay(x), bub.scale = 10, bub.col = c("#00000050", "#FFFFFF50"), main = tun.sel[[4]]@name)
SavePlot("bubbles_tun_vigo99")

## ---------------------------------------------------------------------------------------------------------------------------------
x <- index(tun.sel[[5]])
x[is.na(x)] <- 0
bubbles(age ~ year, spay(x), bub.scale = 10, bub.col = c("#00000050", "#FFFFFF50"), main = tun.sel[[5]]@name)
SavePlot("bubbles_tun_IRTBB")

## ---------------------------------------------------------------------------------------------------------------------------------
# temp function to standardise indices
fun <- function(x) {
  arr <- apply(x@.Data, c(1, 3, 4, 5, 6), scale)
  arr <- aperm(arr, c(2, 1, 3, 4, 5, 6))
  dimnames(arr) <- dimnames(x)
  x <- FLQuant(arr)
}

# # first the full dataset
# lst <- lapply(FLIndices(tun[[1]],tun[[2]],tun[[3]]),index)
# inds <- mcf(lst)
# inds1 <- lapply(inds,fun)

# then only the selected ages
lst <- lapply(FLIndices(tun.sel[[1]], tun.sel[[2]], tun.sel[[3]], tun.sel[[4]], tun.sel[[5]]), index)
inds <- mcf(lst)
inds2 <- lapply(inds, fun)


## ----warning=F--------------------------------------------------------------------------------------------------------------------
akey <- list(points = F, lines = T, columns = 1, cex = 0.8, space = "right")
xyplot(data ~ (year - as.numeric(as.character(age))) | qname, groups = age, data = inds2, type = "b", auto.key = akey, as.table = T, xlab = "Cohort", ylab = "standardised CPUE")
SavePlot("tun_consist2", 10.5)

## ----warning=F--------------------------------------------------------------------------------------------------------------------
xyplot(data ~ year | factor(age), groups = qname, data = inds2, type = "b", auto.key = akey, as.table = T, ylab = "standardised CPUE")
SavePlot("tun_consist3", 10, 5)

## ----warning=F,fig.keep='last'----------------------------------------------------------------------------------------------------
# a <- plot(tun[[1]])
b <- plot(tun.sel[[1]])
# grid.arrange(a,b,ncol=2)


## ----warning=F,fig.keep='last'----------------------------------------------------------------------------------------------------
# a <- plot(tun[[2]])
b <- plot(tun.sel[[2]])
# grid.arrange(a,b,ncol=2)

## ----warning=F,fig.keep='last'----------------------------------------------------------------------------------------------------
# a <- plot(tun[[3]])
b <- plot(tun.sel[[3]])
# grid.arrange(a,b,ncol=2)


## ----warning=F,fig.keep='last'----------------------------------------------------------------------------------------------------
# a <- plot(tun[[4]])
b <- plot(tun.sel[[4]])
# grid.arrange(a,b,ncol=2)

## ----warning=F,fig.keep='last'----------------------------------------------------------------------------------------------------
# a <- plot(tun[[5]])
b <- plot(tun.sel[[5]])
# grid.arrange(a,b,ncol=2)

## ---------------------------------------------------------------------------------------------------------------------------------
cc <- as.data.frame(lapply(FLIndices(tun.sel[[1]], tun.sel[[2]], tun.sel[[3]], tun.sel[[4]], tun.sel[[5]]), function(x) logcc(index(x))))
xyplot(data ~ age | cname, groups = cohort, data = cc, type = "b", col = 1, ylab = "log ratio")
xyplot(data ~ age | cname, groups = cohort, data = cc, type = "b", col = 1, ylab = "log ratio", ylim = c(-2, 3))
SavePlot("tun_logratio")


## ----warning=F--------------------------------------------------------------------------------------------------------------------
tun.ind <- as.data.frame(lapply(tun.sel, index))
tun.ind$yearclass <- as.numeric(as.character(tun.ind$year)) - as.numeric(as.character(tun.ind$age))
xyplot(log(data) ~ year | qname, groups = yearclass, data = tun.ind, scales = list(y = "free"), type = "l", ylab = "log indices", col = 1)
SavePlot("tun_cc")


## ---------------------------------------------------------------------------------------------------------------------------------
par(mfrow = c(2, 3))
for (i in 1:5) {
  range <- tun.sel[[i]]@range
  time <- range[4]:range[5] + mean(range[6:7])
  abund <- apply(tun.sel[[i]]@index, 2, sum, na.rm = T)
  abund <- ifelse(abund == 0, NA, abund)
  plot(time, abund, type = "b", xlim = c(1984, 2018), ylim = c(0, max(abund, na.rm = T)), main = tun.sel[[i]]@name, xlab = "Year", ylab = "Abundance index (all ages)")
}

SavePlot("indices", 8, 3.5)
