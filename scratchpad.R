# Using units meter, kg, seconds and litre unless noted

absorption_halflife <- 0.15 * 60 * 60  # converted from hours to seconds
height <- 1.69 # m
weight <- 72 # kg
beta = 0.015 / (60 * 60) # bac / second, transformed from bac / hour 
sex = "male"

drinks <- read.csv(textConnection(
  'name,vol,alc_prop,time
beer,0.66,0.05,3600
beer,0.66,0.05,7200
wine,0.15,0.12,14200
'), stringsAsFactors = FALSE)


# Calculates the “Widmark factor” r according to Seidl et al 2000
# The default values are the mean height and weight for the USA according
# to Wikipedia (avaraged over men and women).
calc_widmark_factor <- function(height = 1.7, weight = 82, sex = "unknown") {
  r_female <- 0.31223 - 0.006446 * weight + 0.4466 * height
  r_male   <- 0.31608 - 0.004821 * weight + 0.4632 * height
  if(sex == "female") {
    r_female
  } else if(sex == "male") {
    r_male
  } else { # take the mean if sex is unspecified
    (r_male + r_female) / 2
  }
}

cumalative_absorption <- function(drinks) {
  absorption_time_start <- min(drinks$time)
  absorption_time_end <- max(drinks$time) + 60 * 60 * 24 
  absorption_minutes <- round( (absorption_time_end - absorption_time_start) /  60)
  # The points in time (in s) to calculate the absorbtion for.
  t_sec <- seq(from = absorption_time_start, length.out = absorption_minutes, by = 60)
  # A matrix to hold the amount of alcohol absorbed from each drink at different 
  # points in time
  absorption_mat <- matrix(0, nrow = nrow(drinks), ncol = absorption_minutes)
  for(i in 1:nrow(drinks)) {
    #The absorption equation from p. 35 in Posey and Mozayani 2007
    absorption_mat[i,] <- drinks$alc_kg[i] * (1 - exp(-(t_sec - drinks$time[i]) * log(2) / absorption_halflife))
  }
  absorption_mat[absorption_mat < 0] <- 0 # We don't absorb a negative amount of alcohol...
  # Summing the columns to calculate the total amount of absorbed alcohol at each time point
  kg_absorbed <- colSums(absorption_mat)
  data.frame(kg_absorbed = kg_absorbed, time = t_sec)
}


calc_bac_ts <- function(drinks, height, weight, sex) {
  drinks$alc_vol <- drinks$vol * drinks$alc_prop # in litres 
  drinks$alc_kg <- drinks$alc_vol * 0.789 # 0.789 is the weight of one liter of alcohol
  r <- calc_widmark_factor(height, weight, sex)
  
  # "Starting" a data.frame time series to hold information about different aspects of
  # the Blood Alcohol Concentration (bac)
  bac_ts <- cumalative_absorption(drinks)
  bac_ts$time <- as.POSIXct(bac_ts$time, origin="1970-01-01", tz = "UTC")
  
  bac_ts$bac_excluding_elimination <- bac_ts$kg_absorbed / (r * weight)
  bac_ts$eliminated <- rep(0, nrow(bac_ts))
  for(i in 2:nrow(bac_ts)) {
      current_bac <- bac_ts$bac_excluding_elimination[i] - bac_ts$eliminated[i - 1]
      bac_ts$eliminated[i] <- bac_ts$eliminated[i - 1] + 
        min(current_bac, beta) # We can't eliminate more bac than we got...
  }
  
  bac_ts$bac <- bac_ts$bac_excluding_elimination - bac_ts$eliminated
  bac_ts$bac_perc <- bac_ts$bac * 100
  # Removing the end of the time series
  ts_end_i <- max(which.min(bac_ts$bac[-1]) + 10, 5 * 60)
  bac_ts <- bac_ts[seq_len(ts_end_i),]
  bac_ts
}

bac_ts <- calc_bac_ts(drinks, height, weight, sex)

plot(bac_ts$time , bac_ts$kg_absorbed, type="l")
plot(bac_ts$time, bac_ts$eliminated, type="l")
plot(bac_ts$time, bac_ts$bac_perc, type="l")
