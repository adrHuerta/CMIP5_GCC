library(raster)
library(hyfo)

GCM_his <- brick("/home/adrian/Downloads/GCM_prueba.nc")
piscop <- brick("/home/adrian/Documents/wa_budyko_datasets/netcdf/P/PISCOpd.nc")
piscop <- piscop[[1:nlayers(GCM_his)]]

Date = seq(as.Date("1981-01-01"), length.out = nlayers(GCM_his), by = "day")

ncell(GCM_his) == ncell(piscop)
extent(GCM_his) == extent(piscop)

#GCM_his_corrected <- GCM_his

for( xj in 1:ncell(piscop)){
  
  GCM_data <- as.numeric(GCM_his[xj])
  
  if(all(is.na(GCM_data))){
    
    next
    
  } else {
    
    hist_obs <- data.frame(Date = Date,
                           value = as.numeric(piscop[xj])) # la extracción demora un egg
    
    hist_mod <- data.frame(Date = Date,
                           value = GCM_data)
    
    # corrección por cada mes
    do.call(rbind,
            lapply(c("01", "02", "03",
                     "04", "05", "06",
                     "07", "08", "09",
                     "10", "11", "12"), function(x){
                       
                       hist_obs_m <- hist_obs[format(hist_obs$Date, "%m") %in% x, ]
                       hist_mod_m <- hist_mod[format(hist_mod$Date, "%m") %in% x, ]
                       
                       biasCorrect(frc = hist_mod_m,
                                   hindcast = hist_mod_m,
                                   obs = hist_obs_m,
                                   method = 'eqm',
                                   preci = TRUE)
                       
                     })) -> pixel_corrected
    
    # ordenando fechas
    pixel_corrected <- pixel_corrected[order(pixel_corrected$Date), ]
    
    # celda
    print(xj) 
    # reemplazando los valores corregidos al brick nuevo # no funciona
    # GCM_his_corrected[xj] <- pixel_corrected$value # funciona si es un solo valor por layer
    # Se puede guardar el pixel corregido como txt y luego armarlo nuevamente
    
  }
}





GCM_his_res <- resample(GCM_his, piscop[[1]])

library(hyfo)

obs <- data.frame(Date = seq(as.Date("1981-01-01"), length.out = 1000, by = "day"), 
                  value = rnorm(1000, 10, 2))

obs[format(obs$Date, "%m") %in% "02", ]

hindcast <- data.frame(Date = seq(as.Date("1981-01-01"), length.out = 100, by = "day"), 
                       value = rgamma(100, 50, 1))
frc <- hindcast

frc_corrected <- biasCorrect(frc, hindcast, obs, method = 'eqm')

plot(ecdf(obs$value), xlim = c(0, 65))
plot(ecdf(hindcast$value), add=TRUE, col="gray")
plot(ecdf(frc_corrected$value), add=TRUE, col="red", cex = .1)

