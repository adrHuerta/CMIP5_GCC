bc_gcc_dhi <- function(his_mod,
                       his_obs,
                       fut_mod,
                       wet_day)
  {
  
  if(isTRUE(wet_day)){
    
    if(isTRUE(is_possible_to_bc(his_mod))){
      
      eqm_qmap_m(his_mod = his_mod,
                 his_obs = his_obs,
                 fut_mod = fut_mod,
                 wet_day = wet_day)
    } else {
      
      response <- fut_mod
      response[, 2] = NA
      response
      
      }
    
    } else {
    
    eqm_qmap_m(his_mod = his_mod,
               his_obs = his_obs,
               fut_mod = fut_mod,
               wet_day = wet_day)
  }

  }


is_possible_to_bc <- function(his_mod)
  {
  
  sapply(c("01", "02", "03",
           "04", "05", "06",
           "07", "08", "09",
           "10", "11", "12"), 
                 function(x){
                   his_mod_m <- his_mod[format(his_mod$Date, "%m") %in% x, ]
                   length(levels(factor(his_mod_m$value)))
                 }
         ) -> number_of_non_NA
  
  !any(number_of_non_NA == 1)
  
  }

eqm_qmap_m <- function(his_mod,
                       his_obs,
                       fut_mod,
                       wet_day)
{
  
  do.call(rbind,
          lapply(c("01", "02", "03",
                   "04", "05", "06",
                   "07", "08", "09",
                   "10", "11", "12"), 
                 function(x){
                   
                   his_obs_m <- his_obs[format(his_obs$Date, "%m") %in% x, ]
                   his_mod_m <- his_mod[format(his_mod$Date, "%m") %in% x, ]
                   fut_mod_m <- fut_mod[format(fut_mod$Date, "%m") %in% x, ]
                   
                   eqm_model <- qmap::fitQmapQUANT(his_obs_m$value,
                                                   his_mod_m$value,
                                                   wet.day = wet_day,
                                                   qstep = 0.01)
                   
                   fut_mod_m_c <- qmap::doQmapQUANT(fut_mod_m$value, 
                                                    eqm_model, 
                                                    type = "linear")
                   
                   response <- fut_mod_m
                   response[, colnames(fut_mod_m)[1]] = fut_mod_m[,1]
                   response[, colnames(fut_mod_m)[2]] = round(fut_mod_m_c, 1)
                   response
                 }
          )
  ) -> fut_mod_c
  
  fut_mod_c[order(fut_mod_c$Date),]
  
}


