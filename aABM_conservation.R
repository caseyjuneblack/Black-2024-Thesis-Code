################################################################################
################## aABM: archaeological Agent Based Model ######################
############################ Conservation model ################################
################################################################################
# c.black 4.1.2024

rm(list=ls())

## EXISTING DEPENDIENCES 
packages <-c('ggplot2','tidyr','dplyr','sf','terra','tidyterra','magrittr','zoo','spdep','nngeo')
for(p in packages) if(p %in% rownames(installed.packages()) == F) { install.packages(p) }
for(p in packages) suppressPackageStartupMessages(library(p,quietly=T,character.only=T))

theme_set(theme_bw())

### CODE FOR SEAN  ### 
#setwd("C:/Users/sfield3/OneDrive - University of Wyoming/TEACHING & ADVISING/R_Casey")
territory_ex <- sf::read_sf("./Data/territory.shp")
simulated_range <- sf::read_sf("./Data/Range Scenario/simulated_range_v2.0.shp")
resources <- read.csv("./Data/Thesis_resources.csv",header=T,fileEncoding = 'UTF-8-BOM')
dbm <- read.csv("./Data/Thesis DBM values_black.csv",header=T,fileEncoding = 'UTF-8-BOM')
# demo_ex <- read.csv("./Data/demographic_reconstruction.csv")

### CODE FOR CASEY ####
setwd("~/Documents/ANTH_5155")
# dem <- terra::rast("./Data/AV/north_america_DEM.tif") 
territory_ex <- sf::read_sf("./Data/AV/territory.shp")
territory_ex <- sf::read_sf("./Data/AV/territory_limited.shp")
demo <- read.csv("./Data/AV/demographic_reconstruction.csv")
simulated_range <- sf::read_sf("./Data/Thesis/simulated_range_v2.0.shp")
simulated_range <- sf::read_sf("./Output/sim_1_range.shp")
simulated_range <- sf::read_sf("./Data/AV/simulated_range_200km_500m.shp")
resources <- read.csv("./Data/AV/Thesis_resources.csv",header=T,fileEncoding = 'UTF-8-BOM')
dbm <- read.csv("./Data/AV/Thesis DBM values_black.csv",header=T,fileEncoding = 'UTF-8-BOM')

simulated_range <- subset(simulated_range, )
#################### PREPARING THE MODEL #######################################

## create an output, which  will measure resource change across time
out_df <- as.data.frame(matrix(NA,2000,19))%>%
  setNames(c("it_num","moose_potential","moose_used","moose_remaining",
             "caribou_potential","caribou_used","caribou_remaining",
             "sheep_potential","sheep_used","sheep_remaining",
             "fish_potential","fish_used","fish_remaining",
             "rodents_potential","rodents_used","rodents_remaining",
             "hare_potential","hare_used","hare_remaining"))

### adding dbm function
dbm_pc <- function(dbm) {
  output_dbm <- as.data.frame(matrix(0,6,7))%>%
    setNames(c("1","2","3","4","5","6","ofrr"))
  rownames(output_dbm) = (c("1","1+2","1+2+3","1+2+3+4","1+2+3+4+5","1+2+3+4+5+6"))
  
  for (i in 1:nrow(dbm)) {
    er   <- dbm[i,6]
    kcal <- dbm[i,3]
    ht   <- dbm[i,5]
    sum  <- sum(dbm[1:i,6]) 
    
    if (i == 1) {
      output_dbm[i,1] <- (er/er)
      
      output_dbm[i,7] <- ((output_dbm[i,1]*kcal)/((output_dbm[i,1]*ht)+ 1/er)) 
    }
    
    if (i == 2) {
      output_dbm[i,1] <- (dbm[1,6]/sum)
      output_dbm[i,2] <- (er/sum) 
      
      output_dbm[i,7] <- ((output_dbm[i,1]*dbm[1,3])+(output_dbm[i,2]*kcal))/
        ((output_dbm[i,1]*dbm[1,5])+(output_dbm[i,2]*ht)+(1/sum))
    }
    
    if (i == 3) {
      output_dbm[i,1] <- (dbm[1,6]/sum)
      output_dbm[i,2] <- (dbm[2,6]/sum)
      output_dbm[i,3] <- (er/sum)
      
      output_dbm[i,7] <- ((output_dbm[i,1]*dbm[1,3])+(output_dbm[i,2]*dbm[2,3])+(output_dbm[i,3]*kcal))/
        ((output_dbm[i,1]*dbm[1,5])+(output_dbm[i,2]*dbm[2,5])+(output_dbm[i,3]*ht)+(1/sum))  
    }
    
    if (i == 4) {
      output_dbm[i,1] <- (dbm[1,6]/sum)
      output_dbm[i,2] <- (dbm[2,6]/sum)
      output_dbm[i,3] <- (dbm[3,6]/sum)
      output_dbm[i,4] <- (er/sum)
      
      output_dbm[i,7] <- ((output_dbm[i,1]*dbm[1,3])+(output_dbm[i,2]*dbm[2,3])+(output_dbm[i,3]*dbm[3,3])+(output_dbm[i,4]*kcal))/
        ((output_dbm[i,1]*dbm[1,5])+(output_dbm[i,2]*dbm[2,5])+(output_dbm[i,3]*dbm[3,5])+(output_dbm[i,4]*ht)+(1/sum))    
    }
    
    if (i == 5) {
      output_dbm[i,1] <- (dbm[1,6]/sum)
      output_dbm[i,2] <- (dbm[2,6]/sum)
      output_dbm[i,3] <- (dbm[3,6]/sum)
      output_dbm[i,4] <- (dbm[4,6]/sum)
      output_dbm[i,5] <- (er/sum)
      
      output_dbm[i,7] <- ((output_dbm[i,1]*dbm[1,3])+(output_dbm[i,2]*dbm[2,3])+(output_dbm[i,3]*dbm[3,3])+(output_dbm[i,4]*dbm[4,3])+(output_dbm[i,5]*kcal))/
        ((output_dbm[i,1]*dbm[1,5])+(output_dbm[i,2]*dbm[2,5])+(output_dbm[i,3]*dbm[3,5])+(output_dbm[i,4]*dbm[4,5])+(output_dbm[i,5]*ht)+(1/sum))    
    }
    
    if (i == 6) {
      output_dbm[i,1] <- (dbm[1,6]/sum)
      output_dbm[i,2] <- (dbm[2,6]/sum)
      output_dbm[i,3] <- (dbm[3,6]/sum)
      output_dbm[i,4] <- (dbm[4,6]/sum)
      output_dbm[i,5] <- (dbm[5,6]/sum)
      output_dbm[i,6] <- (er/sum)
      
      output_dbm[i,7] <- ((output_dbm[i,1]*dbm[1,3])+(output_dbm[i,2]*dbm[2,3])+(output_dbm[i,3]*dbm[3,3])+(output_dbm[i,4]*dbm[4,3])+(output_dbm[i,5]*dbm[5,3])+(output_dbm[i,6]*kcal))/
        ((output_dbm[i,1]*dbm[1,5])+(output_dbm[i,2]*dbm[2,5])+(output_dbm[i,3]*dbm[3,5])+(output_dbm[i,4]*dbm[4,5])+(output_dbm[i,5]*dbm[5,5])+(output_dbm[i,6]*ht)+(1/sum))  
    }
  }
  return(output_dbm) }


#################### RUNNING THE MODEL #########################################
## Run the simulation by time step ("it_num")



for (i in 1:10){
  out_df[i,c("it_num")] <- i
  interval_range <- subset(simulated_range,it_num == i) 
  speed <- 250000/8760
  m2 <- st_area(territory_ex)
  
  if(i==1){
    for(j in 1:nrow(resources)) {
      resources_sub <- resources[j,]
      
      if(resources_sub$Species == "Moose") {
        resource_num <- resources_sub$Beginning_Pop
        resource_avail <- st_sample(territory_ex,resource_num,type="random")%>%
          st_as_sf()%>%
          mutate(id = 1:nrow(.))
        
        
        resource_in_range <- st_intersection(resource_avail,interval_range)%>%
          st_as_sf()
        resource_potential <- length(unique(resource_in_range$id))
        dbm[1,6] <- speed*(resource_potential/m2)
        out_df[i,c("moose_potential")] <- resource_potential
        
      } 
      
      if(resources_sub$Species == "Caribou") {
        resource_num <- resources_sub$Beginning_Pop
        caribou_avail <- st_sample(territory_ex,resource_num,type="random")%>%
          st_as_sf()%>%
          mutate(id = 1:nrow(.))
        
        min_leaders <- round(length(unique(caribou_avail$id))/1000)
        
        max_leaders <- round(length(unique(caribou_avail$id))/500)
        
        total_leaders <- round(sample(min_leaders:max_leaders,1))
        
        resource_in_range <- st_intersection(caribou_avail,interval_range)%>%
          st_as_sf()
        resource_potential <- length(unique(resource_in_range$id))
        dbm[2,6] <- speed*(resource_potential/m2)
        out_df[i,c("caribou_potential")] <- resource_potential
        
      } 
      
      if(resources_sub$Species == "Dall sheep") {
        resource_num <- resources_sub$Beginning_Pop
        resource_avail <- st_sample(territory_ex,resource_num,type="random")%>%
          st_as_sf()%>%
          mutate(id = 1:nrow(.))
        
        resource_in_range <- st_intersection(resource_avail,interval_range)%>%
          st_as_sf()
        resource_potential <- length(unique(resource_in_range$id))
        dbm[3,6] <- speed*(resource_potential/m2)
        out_df[i,c("sheep_potential")] <- resource_potential
      } 
      
      if(resources_sub$Species == "Fish") {
        resource_num <- resources_sub$Beginning_Pop
        resource_avail <- st_sample(territory_ex,resource_num,type="random")%>%
          st_as_sf()%>%
          mutate(id = 1:nrow(.))
        
        resource_in_range <- st_intersection(resource_avail,interval_range)%>%
          st_as_sf()
        resource_potential <- length(unique(resource_in_range$id))
        dbm[4,6] <- speed*(resource_potential/m2)
        out_df[i,c("fish_potential")] <- resource_potential
      }
      
      if(resources_sub$Species == "Rodents") {
        resource_num <- resources_sub$Beginning_Pop
        resource_avail <- st_sample(territory_ex,resource_num,type="random")%>%
          st_as_sf()%>%
          mutate(id = 1:nrow(.))
        
        resource_in_range <- st_intersection(resource_avail,interval_range)%>%
          st_as_sf()
        resource_potential <- length(unique(resource_in_range$id))
        dbm[5,6] <- speed*(resource_potential/m2)
        out_df[i,c("rodents_potential")] <- resource_potential
      } 
      
      if(resources_sub$Species == "Hare") {
        resource_num <- resources_sub$Beginning_Pop
        resource_avail <- st_sample(territory_ex,resource_num,type="random")%>%
          st_as_sf()%>%
          mutate(id = 1:nrow(.))
        
        resource_in_range <- st_intersection(resource_avail,interval_range)%>%
          st_as_sf()
        resource_potential <- length(unique(resource_in_range$id))
        dbm[6,6] <- speed*(resource_potential/m2)
        out_df[i,c("hare_potential")] <- resource_potential
      }
    }
    

    
    for (a in 1:nrow(resources)) {
 
      total_cal_need <- length(unique(interval_range$agent)) * 730000 * 6
      output_dbm <- dbm_pc(dbm)
      diet <- ifelse(output_dbm[1,7] > 0,1,0)
      diet <- ifelse(output_dbm[1,7] <= dbm[2,4],2,diet)
      diet <- ifelse(output_dbm[2,7] <= dbm[3,4],3,diet)
      diet <- ifelse(output_dbm[3,7] <= dbm[4,4],4,diet) 
      diet <- ifelse(output_dbm[4,7] <= dbm[5,4],5,diet)
      diet <- ifelse(output_dbm[5,7] <= dbm[6,4],6,diet)
     
      if (a == 1) {
        species_pc_cal <- total_cal_need * output_dbm[diet,1]
        total_taken <- round(species_pc_cal/dbm[1,c("Ind_calories")])
        out_df[i,c("moose_used")] <- total_taken
        
      } else if (a == 2) {
        if (output_dbm[1,7] <= dbm[2,4]) {
          species_pc_cal <- total_cal_need * output_dbm[diet,2]
          total_taken <- round(species_pc_cal/dbm[2,c("Ind_calories")])
          leaders_subtracted <- round((total_leaders*total_taken)/length(unique(caribou_avail$id)))
          out_df[i,c("caribou_used")] <- total_taken - leaders_subtracted
        } else {
          out_df[i,c("caribou_used")] <- 0
        } 
        
      } else if (a == 3) {
        if (output_dbm[2,7] <= dbm[3,4]) {
          species_pc_cal <- total_cal_need * output_dbm[diet,3]
          total_taken <- round(species_pc_cal/dbm[3,c("Ind_calories")])
          out_df[i,c("sheep_used")] <- total_taken
        } else {
          out_df[i,c("sheep_used")] <- 0
        } 
        
      } else if (a == 4) {
        if (output_dbm[3,7] <= dbm[4,4]) {
          species_pc_cal <- total_cal_need * output_dbm[diet,4]
          total_taken <- round(species_pc_cal/dbm[4,c("Ind_calories")])
          out_df[i,c("fish_used")] <- total_taken
        } else {
          out_df[i,c("fish_used")] <- 0
        } 
        
      } else if (a == 5) {
        if (output_dbm[4,7] <= dbm[5,4]) {
          species_pc_cal <- total_cal_need * output_dbm[diet,5]
          total_taken <- round(species_pc_cal/dbm[5,c("Ind_calories")])
          out_df[i,c("rodents_used")] <- total_taken
        } else {
          out_df[i,c("rodents_used")] <- 0
        }
        
      } else {
        if (output_dbm[5,7] <= dbm[6,4]) {
          species_pc_cal <- total_cal_need * output_dbm[diet,6]
          total_taken <- round(species_pc_cal/dbm[6,c("Ind_calories")])
          out_df[i,c("hare_used")] <- total_taken
        } else {
          out_df[i,c("hare_used")] <- 0
        } } 
    }
    
    for (r in 1:nrow(resources)) {
      resources_sub <- resources[r,]
      
      if (resources_sub$Species == "Moose") {
        n <- resources_sub$Sine
        pop <- resources_sub$Beginning_Pop - out_df[i,c("moose_used")]
        if ((i - 1) %% (n + n) <= n) {
          out_df[i,c("moose_remaining")] <- round(pop * (rnorm(1, resources_sub$Max, 0.01)))
        } else {
          out_df[i,c("moose_remaining")] <- round(pop * (rnorm(1, resources_sub$Min, 0.01)))
        } }
      
      if (resources_sub$Species == "Caribou") {
        n <- resources_sub$Sine
        pop <- resources_sub$Beginning_Pop - out_df[i,c("caribou_used")]
        if ((i - 1) %% (n + n) <= n) {
          out_df[i,c("caribou_remaining")] <- round(pop * (rnorm(1, resources_sub$Max, 0.01)))
        } else {
          out_df[i,c("caribou_remaining")] <- round(pop * (rnorm(1, resources_sub$Min, 0.01)))
        } } 
      
      if (resources_sub$Species == "Dall sheep") {
        n <- resources_sub$Sine
        pop <- resources_sub$Beginning_Pop - out_df[i,c("sheep_used")]
        if ((i - 1) %% (n + n) <= n) {
          out_df[i,c("sheep_remaining")] <- round(pop * (rnorm(1, resources_sub$Max, 0.01)))
        } else {
          out_df[i,c("sheep_remaining")] <- round(pop * (rnorm(1, resources_sub$Min, 0.01)))
        } }
      
      if (resources_sub$Species == "Fish") {
        n <- resources_sub$Sine
        pop <- resources_sub$Beginning_Pop - out_df[i,c("fish_used")]
        if ((i - 1) %% (n + n) <= n) {
          out_df[i,c("fish_remaining")] <- round(pop * (rnorm(1, resources_sub$Max, 0.01)))
        } else {
          out_df[i,c("fish_remaining")] <- round(pop * (rnorm(1, resources_sub$Min, 0.01)))
        }  
      }
      
      if (resources_sub$Species == "Rodents") {
        n <- resources_sub$Sine
        pop <- resources_sub$Beginning_Pop - out_df[i,c("rodents_used")]
        if ((i - 1) %% (n + n) <= n) {
          out_df[i,c("rodents_remaining")] <- round(pop * (rnorm(1, resources_sub$Max, 0.01)))
        } else {
          out_df[i,c("rodents_remaining")] <- round(pop * (rnorm(1, resources_sub$Min, 0.01)))
        } }
      
      if (resources_sub$Species == "Hare") {
        n <- resources_sub$Sine
        pop <- resources_sub$Beginning_Pop - out_df[i,c("hare_used")]
        if ((i - 1) %% (n + n) <= n) {
          out_df[i,c("hare_remaining")] <- round(pop * (rnorm(1, resources_sub$Max, 0.01)))
        } else {
          out_df[i,c("hare_remaining")] <- round(pop * (rnorm(1, resources_sub$Min, 0.01)))
        } 
      }
      
    }
    
  } else {
    # run the same chunk of code if i >= 2, with the only difference being the
    # resource_num being derived from prior iteration
    for(j in 1:nrow(resources)) {
      resources_sub <- resources[j,]
      
      if(resources_sub$Species == "Moose") {
        if (out_df[i-1,c("moose_remaining")] == 0) {
          dbm[1,6] <- 0
          out_df[i,c("moose_potential")] <- 0
          out_df[i,c("moose_used")] <- 0
        } else {
          resource_num <- out_df[i-1,c("moose_remaining")]
          resource_avail <- st_sample(territory_ex,resource_num,type="random")%>%
            st_as_sf()%>%
            mutate(id = 1:nrow(.))
          
          resource_in_range <- st_intersection(resource_avail,interval_range)%>%
            st_as_sf()
          resource_potential <- length(unique(resource_in_range$id))
          dbm[1,6] <- speed*(resource_potential/m2)
          out_df[i,c("moose_potential")] <- resource_potential
        } } 
      
      if(resources_sub$Species == "Caribou") {
        if (out_df[i-1,c("caribou_remaining")] == 0) {
          dbm[2,6] <- 0
          out_df[i,c("caribou_potential")] <- 0
          out_df[i,c("caribou_used")] <- 0
        } else {
          resource_num <- out_df[i-1,c("caribou_remaining")]
          caribou_avail <- st_sample(territory_ex,resource_num,type="random")%>%
            st_as_sf()%>%
            mutate(id = 1:nrow(.))
          
          min_leaders <- round(length(unique(caribou_avail$id))/1000)
          
          max_leaders <- round(length(unique(caribou_avail$id))/500)
          
          total_leaders <- round(sample(min_leaders:max_leaders,1))
          
          resource_in_range <- st_intersection(caribou_avail,interval_range)%>%
            st_as_sf()
          resource_potential <- length(unique(resource_in_range$id))
          dbm[2,6] <- speed*(resource_potential/m2)
          out_df[i,c("caribou_potential")] <- resource_potential
        } }
      
      if(resources_sub$Species == "Dall sheep") {
        if (out_df[i-1,c("sheep_remaining")] == 0) {
          dbm[3,6] <- 0
          out_df[i,c("sheep_potential")] <- 0
          out_df[i,c("sheep_used")] <- 0
        } else {
          resource_num <- out_df[i-1,c("sheep_remaining")]
          resource_avail <- st_sample(territory_ex,resource_num,type="random")%>%
            st_as_sf()%>%
            mutate(id = 1:nrow(.))
          
          resource_in_range <- st_intersection(resource_avail,interval_range)%>%
            st_as_sf()
          resource_potential <- length(unique(resource_in_range$id))
          dbm[3,6] <- speed*(resource_potential/m2)
          out_df[i,c("sheep_potential")] <- resource_potential
        } }
      
      if(resources_sub$Species == "Fish") {
        if (out_df[i-1,c("fish_remaining")] == 0) {
          dbm[4,6] <- 0
          out_df[i,c("fish_potential")] <- 0
          out_df[i,c("fish_used")] <- 0
        } else {
          resource_num <- out_df[i-1,c("fish_remaining")]
          resource_avail <- st_sample(territory_ex,resource_num,type="random")%>%
            st_as_sf()%>%
            mutate(id = 1:nrow(.))
          
          resource_in_range <- st_intersection(resource_avail,interval_range)%>%
            st_as_sf()
          resource_potential <- length(unique(resource_in_range$id))
          dbm[4,6] <- speed*(resource_potential/m2)
          out_df[i,c("fish_potential")] <- resource_potential
        } }
      
      if(resources_sub$Species == "Rodents") {
        if (out_df[i-1,c("rodents_remaining")] == 0) {
          dbm[5,6] <- 0
          out_df[i,c("rodents_potential")] <- 0
          out_df[i,c("rodents_used")] <- 0
        } else {
          resource_num <- out_df[i-1,c("rodents_remaining")]
          resource_avail <- st_sample(territory_ex,resource_num,type="random")%>%
            st_as_sf()%>%
            mutate(id = 1:nrow(.))
          
          resource_in_range <- st_intersection(resource_avail,interval_range)%>%
            st_as_sf()
          resource_potential <- length(unique(resource_in_range$id))
          dbm[5,6] <- speed*(resource_potential/m2)
          out_df[i,c("rodents_potential")] <- resource_potential
        } }
      
      if(resources_sub$Species == "Hare") {
        resource_num <- out_df[i-1,c("hare_remaining")]
        resource_avail <- st_sample(territory_ex,resource_num,type="random")%>%
          st_as_sf()%>%
          mutate(id = 1:nrow(.))
        
        resource_in_range <- st_intersection(resource_avail,interval_range)%>%
          st_as_sf()
        resource_potential <- length(unique(resource_in_range$id))
        dbm[6,6] <- speed*(resource_potential/m2)
        out_df[i,c("hare_potential")] <- resource_potential
      }
      
    }
    
    for (a in 1:nrow(resources)) {
      total_cal_need <- length(unique(interval_range$agent)) * 730000 * 6
      output_dbm <- dbm_pc(dbm)
      output_dbm[is.na(output_dbm)] <- 0
      diet <- ifelse(output_dbm[1,7] > 0,1,0)
      diet <- ifelse(output_dbm[1,7] <= dbm[2,4],2,diet)
      diet <- ifelse(output_dbm[2,7] <= dbm[3,4],3,diet)
      diet <- ifelse(output_dbm[3,7] <= dbm[4,4],4,diet) 
      diet <- ifelse(output_dbm[4,7] <= dbm[5,4],5,diet)
      diet <- ifelse(output_dbm[5,7] <= dbm[6,4],6,diet)
      
      if (a == 1) {
        species_pc_cal <- total_cal_need * output_dbm[diet,1]
        total_taken <- round(species_pc_cal/dbm[1,c("Ind_calories")])
        out_df[i,c("moose_used")] <- total_taken
        
      } else if (a == 2) {
        if (output_dbm[1,7] <= dbm[2,4]) {
          species_pc_cal <- total_cal_need * output_dbm[diet,2]
          total_taken <- round(species_pc_cal/dbm[2,c("Ind_calories")])
          leaders_subtracted <- round((total_leaders*total_taken)/length(unique(caribou_avail$id)))
          out_df[i,c("caribou_used")] <- total_taken - leaders_subtracted
        } else {
          out_df[i,c("caribou_used")] <- 0
        } 
        
      } else if (a == 3) {
        if (output_dbm[2,7] <= dbm[3,4]) {
          species_pc_cal <- total_cal_need * output_dbm[diet,3]
          total_taken <- round(species_pc_cal/dbm[3,c("Ind_calories")])
          out_df[i,c("sheep_used")] <- total_taken
        } else {
          out_df[i,c("sheep_used")] <- 0
        } 
        
      } else if (a == 4) {
        if (output_dbm[3,7] <= dbm[4,4]) {
          species_pc_cal <- total_cal_need * output_dbm[diet,4]
          total_taken <- round(species_pc_cal/dbm[4,c("Ind_calories")])
          out_df[i,c("fish_used")] <- total_taken
        } else {
          out_df[i,c("fish_used")] <- 0
        } 
        
      } else if (a == 5) {
        if (output_dbm[4,7] <= dbm[5,4]) {
          species_pc_cal <- total_cal_need * output_dbm[diet,5]
          total_taken <- round(species_pc_cal/dbm[5,c("Ind_calories")])
          out_df[i,c("rodents_used")] <- total_taken
        } else {
          out_df[i,c("rodents_used")] <- 0
        }
        
      } else {
        if (output_dbm[5,7] <= dbm[6,4]) {
          species_pc_cal <- total_cal_need * output_dbm[diet,6]
          total_taken <- round(species_pc_cal/dbm[6,c("Ind_calories")])
          out_df[i,c("hare_used")] <- total_taken
        } else {
          out_df[i,c("hare_used")] <- 0
        } } 
    }
    
    for (r in 1:nrow(resources)) {
      resources_sub <- resources[r,]
      
      if (resources_sub$Species == "Moose") {
        n <- resources_sub$Sine
        pop <- out_df[i-1,c("moose_remaining")] - out_df[i,c("moose_used")]
        if ((i - 1) %% (n + n) <= n) {
          out_df[i,c("moose_remaining")] <- round(pop * (rnorm(1, resources_sub$Max, 0.01)))
        } else {
          out_df[i,c("moose_remaining")] <- round(pop * (rnorm(1, resources_sub$Min, 0.01)))
        } }   
      
      if (resources_sub$Species == "Caribou") {
        n <- resources_sub$Sine
        pop <- out_df[i-1,c("caribou_remaining")] - out_df[i,c("caribou_used")]
        if ((i - 1) %% (n + n) <= n) {
          out_df[i,c("caribou_remaining")] <- round(pop * (rnorm(1, resources_sub$Max, 0.01)))
        } else {
          out_df[i,c("caribou_remaining")] <- round(pop * (rnorm(1, resources_sub$Min, 0.01)))
        } } 
      
      if (resources_sub$Species == "Dall sheep") {
        n <- resources_sub$Sine
        pop <- out_df[i-1,c("sheep_remaining")] - out_df[i,c("sheep_used")]
        if ((i - 1) %% (n + n) <= n) {
          out_df[i,c("sheep_remaining")] <- round(pop * (rnorm(1, resources_sub$Max, 0.01)))
        } else {
          out_df[i,c("sheep_remaining")] <- round(pop * (rnorm(1, resources_sub$Min, 0.01)))
        } }   
      
      if (resources_sub$Species == "Fish") {
        n <- resources_sub$Sine
        pop <- out_df[i-1,c("fish_remaining")] - out_df[i,c("fish_used")]
        if ((i - 1) %% (n + n) <= n) {
          out_df[i,c("fish_remaining")] <- round(pop * (rnorm(1, resources_sub$Max, 0.01)))
        } else {
          out_df[i,c("fish_remaining")] <- round(pop * (rnorm(1, resources_sub$Min, 0.01)))
        }
      }
      
      if (resources_sub$Species == "Rodents") {
        n <- resources_sub$Sine
        pop <- out_df[i-1,c("rodents_remaining")] - out_df[i,c("rodents_used")]
        if ((i - 1) %% (n + n) <= n) {
          out_df[i,c("rodents_remaining")] <- round(pop * (rnorm(1, resources_sub$Max, 0.01)))
        } else {
          out_df[i,c("rodents_remaining")] <- round(pop * (rnorm(1, resources_sub$Min, 0.01)))
        } }  
      
      if (resources_sub$Species == "Hare") {
        n <- resources_sub$Sine
        pop <- out_df[i-1,c("hare_remaining")] - out_df[i,c("hare_used")]
        if ((i - 1) %% (n + n) <= n) {
          out_df[i,c("hare_remaining")] <- round(pop * (rnorm(1, resources_sub$Max, 0.01)))
        } else {
          out_df[i,c("hare_remaining")] <- round(pop * (rnorm(1, resources_sub$Min, 0.01)))
        } 
      }
      
    }
  }
}



### MOOSE POP 
ggplot()+
  geom_line(data=out_df,aes(it_num,moose_remaining),col="#C6EBBE")

### MOOSE USED
ggplot()+
  geom_line(data=out_df,aes(it_num,moose_used),col="salmon2")

### CARIBOU POP ###
ggplot()+
  geom_line(data=out_df,aes(it_num,caribou_remaining),col="#C6EBBE")

### CARIBOU USED ###
ggplot()+
  geom_line(data=out_df,aes(it_num,caribou_used),col="salmon2")

### DALL SHEEP POP 
ggplot()+
  geom_line(data=out_df,aes(it_num,sheep_remaining),col="#C6EBBE")

### DALL SHEEP USED
ggplot()+
  geom_line(data=out_df,aes(it_num,sheep_used),col="salmon2")

### FISH POP 
ggplot()+
  geom_line(data=out_df,aes(it_num,fish_remaining),col="#95B2B0")

### FISH USED 
ggplot()+
  geom_line(data=out_df,aes(it_num,fish_used),col="salmon2")

### SMALL GAME POP 
ggplot()+
  geom_line(data=out_df,aes(it_num,rodents_remaining),col="#C6EBBE")

### SMALL GAME USED
ggplot()+
  geom_line(data=out_df,aes(it_num,rodents_used),col="salmon2")

### hare POP 
ggplot()+
  geom_line(data=out_df,aes(it_num,hare_remaining),col="#C6EBBE")

### hare USED
ggplot()+
  geom_line(data=out_df,aes(it_num,hare_used),col="salmon2")

### ALL RESOUrCES POP ###
ggplot()+
  geom_line(data=out_df,aes(it_num,moose_remaining),col="red")+
  geom_line(data=out_df,aes(it_num,caribou_remaining),col="orange")+
  geom_line(data=out_df,aes(it_num,sheep_remaining),col="yellow")+
  geom_line(data=out_df,aes(it_num,fish_remaining),col="green")+
  geom_line(data=out_df,aes(it_num,rodents_remaining),col="blue")+
  geom_line(data=out_df,aes(it_num,hare_remaining),col="purple")

### ALL USED RESOURCES ###
ggplot()+
  geom_line(data=out_df,aes(it_num,moose_used),col="red")+
  geom_line(data=out_df,aes(it_num,caribou_used),col="orange")+
  geom_line(data=out_df,aes(it_num,sheep_used),col="yellow")+
  geom_line(data=out_df,aes(it_num,fish_used),col="green")+
  geom_line(data=out_df,aes(it_num,rodents_used),col="blue")+
  geom_line(data=out_df,aes(it_num,hare_used),col="purple")

#could make it 1 instead of 0 when NA values
