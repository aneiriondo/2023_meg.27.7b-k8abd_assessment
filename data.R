## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)
library(knitr)

mkdir("data")


# run data exploration and save output in html
stitch_rhtml("data_exploration.R")
cp("data_exploration.html", "data", move = TRUE)

mkdir("data/figure")
cp("figure", "data", move = TRUE)

# write out standard TAF tables

