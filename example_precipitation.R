rm(list = ls())

source('./src/bc_gcc_dhi.R')

# getting path of time series files example
ts <- dir("./data", full.names = TRUE)[1:3]
names(ts) <- sapply(strsplit(ts, "_"), function(x) strsplit(x[[3]], "[.]")[[1]][1])

# importing time series as data.frame (Date, value)
ts <- lapply(ts, function(x){
  
  x <- read.table(x)
  x_date <- as.Date(as.character(x[1,1]), format = "%Y%m%d")
  
  data.frame(Date = seq(x_date, length.out = length(x$V1[-1]), by = "day"),
             value = x$V1[-1])
  })

# bias-correcting (monthly variation) time series
# (correcting historical model data)
ts$his_c <- bc_gcc_dhi(his_mod = ts$his,
                       his_obs = ts$obs,
                       fut_mod = ts$his,
                       wet_day = TRUE)  # wet_day = FALSE for temperature data 

# how "good" has been the bias-correction (monthly variation)
ts$his$month <- substr(ts$his$Date, 6, 7)
ts$his$type <- "his"

ts$obs$month <- substr(ts$obs$Date, 6, 7)
ts$obs$type <- "obs"

ts$his_c$month <- substr(ts$his_c$Date, 6, 7)
ts$his_c$type <- "his_c"

lattice::bwplot(value ~ type | month, 
                data = rbind(ts$obs, ts$his, ts$his_c),
                scales = list(y = list(relation = "free")),
                cex = .75, pch = 19, 
                par.settings = list(plot.symbol = list(cex = .5),
                                  box.umbrella = list(lty = 5)))

# monthly window seasonal varying
plot(eqm_qmap_sv(his_mod = ts$his,
                 his_obs = ts$obs,
                 fut_mod = ts$his,
                 wet_day = TRUE), type = "l")

# monthly varying
plot(eqm_qmap_m(his_mod = ts$his,
                his_obs = ts$obs,
                fut_mod = ts$his,
                wet_day = TRUE), type = "l")

# seasonal varying
plot(eqm_qmap_s(his_mod = ts$his,
                his_obs = ts$obs,
                fut_mod = ts$his,
                wet_day = TRUE), type = "l")