source("helper_functions.R")

drink_info <- read.csv("drink_info.csv", stringsAsFactors=FALSE)

# Using units meter, kg, seconds and litre unless noted.
# Also using proportions (e.g. 0.25) rather than percentages (e.g. 25%)

absorption_halflife <- 0.20 * 60 * 60  # converted from hours to seconds
height <- 1.79 # m
weight <- 75 # kg
beta = 0.018 / 100 / (60 * 60) #  bac / sec, converted to proportion from percentage and from hour to second
sex = "male"

drinks <- read.csv(textConnection(
'name,vol,alc_prop,time
beer,0.33,0.05,3600
beer,0.33,0.05,7200
wine,0.15,0.12,14200
'), stringsAsFactors = FALSE)


drinks <- drinks[FALSE,]
if(nrow(drinks) > 0) {
  start_time = min(drinks$time)
  end_time = max(drinks$time) + 60 * 60 * 24
} else {
  start_time <- as.integer(time_now) - 60L * 60L
  end_time <- start_time + 60L * 60L * 24L
}


bac_ts <- calc_bac_ts(drinks, height, weight, sex, absorption_halflife, beta, start_time, end_time)
time_now <- as.POSIXct(4500, origin="1970-01-01", tz = "UTC")

plot_bac_ts(bac_ts, drinks, time_now, drink_info)

plot(bac_ts$time , bac_ts$kg_absorbed, type="l")
plot(bac_ts$time, bac_ts$eliminated, type="l")
plot(bac_ts$time, bac_ts$bac_perc, type="l")