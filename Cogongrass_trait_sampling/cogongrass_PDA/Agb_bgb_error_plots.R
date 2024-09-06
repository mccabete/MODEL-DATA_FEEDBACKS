

### Read in ABG and BGB of runs and the assosiated error witht he emulator. 

## Geo path /projectnb/dietzelab/pecan.data/output//tmccabe/PDA_150_knot/
# Possbiel test-pecan path: /fs/data2/output/PEcAn_PDA_150_knot

outfile <- "/fs/data2/output//PEcAn_1000019795/out/"

id <- "1000019795"
name <- "PDA_50kn_MODEL_REDO_after_fix_of_obs"#"50_knots_local"

knots <- list.files(path = outfile, pattern = "knot")

biomass <- tibble(abg = rep(NA, length(knots)), bgb = rep(NA, length(knots)))

#bad_runs <- c(26)
bad_runs <- c(47)

for(i in seq_along(knots)){
  path <- paste0("/fs/data2/output//PEcAn_", id, "/out/", name, ".knot.", i, "/")
  
  if(i %in% bad_runs){
    next
  }
  
  nc <- try(ncdf4::nc_open(paste0(path, "2019.nc")))
  if(class(nc) != "try-error"){
    agb <- ncdf4::ncvar_get(nc, "AbvGrndWood")
    bgb <- ncdf4::ncvar_get(nc, "TotSoilCarb")
    
    biomass$abg[i] <- agb[4] ## Take most recent value
    biomass$bgb[i] <- bgb[4]
    ncdf4::nc_close(nc)
  }else{
    next
  }
  rm(nc)
}


geo_path  <- paste0("/projectnb/dietzelab/pecan.data/output//tmccabe/", id, "/")
ss_filepath <- paste0(geo_path, "ss.pda", name, ".Rdata" )

load(ss_filepath)

## Plot Bivariate error

plot(SS[[1]][,3], SS[[2]][,3], xlab = "Error From Aboveground Biomass", ylab = "Error from Belowground Biomass")

## Plot how the parameter values map onto error

plot(SS[[1]][,1], log(SS[[1]][,2]), xlab = "growth resp", ylab = "water conductance", cex = SS[[1]][,3]*25, main = "Error from aboveground Biomass")
plot(SS[[2]][,1], log(SS[[2]][,2]), xlab = "growth resp", ylab = "water conductance", cex = SS[[2]][,3]*5, main = "Error from belowground Biomass")
plot(biomass$abg, na.omit(biomass$bgb), cex = SS[[2]][,3]*5, xlab = "modeled aboveground biomass", ylab = "modeled belowground biomass", main = "Error in belowground biomass" )
plot(biomass$abg, na.omit(biomass$bgb), cex = SS[[1]][,3]*25, xlab = "modeled aboveground biomass", ylab = "modeled belowground biomass", main = "Error in aboveground biomass" )

par(mfrow = c(2,2))

plot(x = na.omit(biomass$abg), y =  SS[[1]][,3],  xlab = "modeled aboveground biomass", ylab = "error -- abovegorund biomass", main = "Error in aboveground biomass" )
plot(x = na.omit(biomass$bgb), y = SS[[2]][,3],  xlab = "modeled belowground biomass", ylab = "error -- belowground biomass", main = "Error in belowground biomass" )



plot(na.omit(biomass$abg), SS[[2]][,3],  xlab = "modeled aboveground biomass", ylab = "error -- belowground biomass", main = "Error in belowground biomass" )
plot(na.omit(biomass$bgb), SS[[1]][,3],  xlab = "modeled belowground biomass", ylab = "error -- aboveground biomass", main = "Error in aboveground biomass" )

plot(SS[[1]][,1], na.omit(biomass$abg), xlab = "growth resp", ylab = "modeled aboveground biomass")
plot(log(SS[[1]][,2]), na.omit(biomass$abg), xlab = "logged water conductence", ylab = "modeled aboveground biomass")

plot(SS[[1]][,2], na.omit(biomass$abg), xlab = "water conductence", ylab = "aboveground biomass")

plot(SS[[2]][,1], na.omit(biomass$bgb), xlab = "growth resp", ylab = "modeled belowground biomass")
plot(log(SS[[2]][,2]), na.omit(biomass$bgb), xlab = "logged water conductence", ylab = "belowground biomass")

plot(SS[[2]][,2], na.omit(biomass$abg), xlab = "water conductence", ylab = "belowground biomass")
plot(SS[[1]][,3], SS[[2]][,3], xlab = "error in aboveground biomass", ylab = "error in belowground biomass")


## Error by parameter
plot(x = SS[[2]][,1], SS[[2]][,3], xlab = "growth resp", ylab = "error belowground biomass", cex = SS[[2]][,3]*5, main = "Error from belowground Biomass")
plot(x = log(SS[[2]][,2]), SS[[2]][,3], xlab = "logged water cnductance", ylab = "error belowground biomass", cex = SS[[2]][,3]*5, main = "Error from belowground Biomass")

plot(x = SS[[1]][,1], SS[[1]][,3], xlab = "growth resp", ylab = "error aboveground biomass", cex = SS[[2]][,3]*5, main = "Error from Aboveground Biomass")
plot(x = log(SS[[1]][,2]), SS[[1]][,3], xlab = "logged water conductance", ylab = "error aboveground biomass", cex = SS[[2]][,3]*5, main = "Error from Aboveground Biomass")

plot(x = SS[[1]][,1], SS[[1]][,3], xlab = "growth resp", ylab = "error aboveground biomass", cex = SS[[2]][,3]*5, main = "Error from Aboveground Biomass")
plot(x = SS[[1]][,2], SS[[1]][,3], xlab = "water conductance", ylab = "error aboveground biomass", cex = SS[[2]][,3]*5, main = "Error from Aboveground Biomass")




obs_agb <- ambient$tillars_corrected + ambient$leaves_corrected
obs_bgb <- ambient$fine_roots_corrected_area + ambient$rhizome_corrected_area

plot(y = na.omit(biomass$abg), x = na.omit(biomass$bgb), ylab = "aboveground biomass", xlab = "belowground biomass", main= "Aboveground error in design matrix. Obs in red.",  cex = SS[[1]][,3]*25, ylim = c(min(na.omit(biomass$abg), na.omit(obs_agb)), max(na.omit(biomass$abg), na.omit(obs_agb))), xlim = c(min(na.omit(biomass$bgb), na.omit(obs_bgb)), max(na.omit(biomass$bgb), na.omit(obs_bgb))))
points(y = obs_agb, x = obs_bgb,  col = "red")

plot(y = na.omit(biomass$abg), x = na.omit(biomass$bgb), ylab = "aboveground biomass", xlab = "belowground biomass", main= "Belowground error in design matrix. Obs in red.",  cex = SS[[2]][,3]*5, ylim = c(min(na.omit(biomass$abg), na.omit(obs_agb)), max(na.omit(biomass$abg), na.omit(obs_agb))), xlim = c(min(na.omit(biomass$bgb), na.omit(obs_bgb)), max(na.omit(biomass$bgb), na.omit(obs_bgb))))
points(y = obs_agb, x = obs_bgb,  col = "red")


plot(y = na.omit(biomass$abg), x = na.omit(biomass$bgb), ylab = "aboveground biomass", xlab = "belowground biomass", main = "logged water conductance. Obs in red", cex = abs(log(SS[[1]][,2]))*0.2, ylim = c(min(na.omit(biomass$abg), na.omit(obs_agb)), max(na.omit(biomass$abg), na.omit(obs_agb))), xlim = c(min(na.omit(biomass$bgb), na.omit(obs_bgb)), max(na.omit(biomass$bgb), na.omit(obs_bgb))))
points(y = obs_agb, x = obs_bgb,  col = "red")

plot(y = na.omit(biomass$abg), x = na.omit(biomass$bgb), ylab = "aboveground biomass", xlab = "belowground biomass", main = "growth respiration. Obs in red", cex = SS[[1]][,1]*5, ylim = c(min(na.omit(biomass$abg), na.omit(obs_agb)), max(na.omit(biomass$abg), na.omit(obs_agb))), xlim = c(min(na.omit(biomass$bgb), na.omit(obs_bgb)), max(na.omit(biomass$bgb), na.omit(obs_bgb))))
points(y = obs_agb, x = obs_bgb,  col = "red")


plot(biomass$abg, ylim = c(min(biomass$agb, na.omit(obs_agb)), max(biomass$agb, na.omit(obs_agb))))
points(obs_agb, col = "red")

plot(na.omit(biomass$bgb), ylim = c(min(na.omit(biomass$bgb), na.omit(obs_bgb)), max(na.omit(biomass$bgb), na.omit(obs_bgb))))
points(obs_bgb, col = "blue")

### Set up color scale

library(paletteer)
nColor <- 10
colors <- paletteer_c("grDevices::Temps", n = nColor)

# Transform the numeric variable in bins
rank <- as.factor( as.numeric( cut(mcmc.param.list[[2]][[1]][,2], nColor))) ## Tau estimate belowground biomass



## Plotting the mcmc object SHOW MIKE SOMETHING IS UP! SEEMS LIKE THE MCMC ISN"T SAMPLING THE LOG_SPACE WELL???


plot(SS[[2]][,1], log(SS[[2]][,2]), xlab = "growth resp", ylab = "logged water conductance", cex = SS[[2]][,3]*5, main = "Error from belowground Biomass")
points(x = mcmc.param.list[[1]][[3]][,1], y = log(mcmc.param.list[[1]][[3]][,2]), pch = ".", col = alpha("blue", (1 - (as.numeric(rank) / 10))))

plot(SS[[2]][,1], SS[[2]][,2], xlab = "growth resp", ylab = "water conductance", cex = SS[[2]][,3]*5, main = "Error from belowground Biomass")
points(x = mcmc.param.list[[1]][[2]][,1], y = mcmc.param.list[[1]][[2]][,2], pch = ".", col = alpha("blue", (1 - (as.numeric(rank) / 10))))

plot(SS[[1]][,1], log(SS[[1]][,2]), xlab = "growth resp", ylab = "logged water conductance", cex = SS[[1]][,3]*25, main = "Error from aboveground Biomass")
points(x = mcmc.param.list[[1]][[3]][,1], y = log(mcmc.param.list[[1]][[3]][,2]), pch = ".", col = alpha("blue", (1 - (as.numeric(rank) / 10))))

plot(x = gp[[1]]$X[,1], y = gp[[1]]$X[,2], cex = gp[[1]]$Z*20, xlab = "growth respiration", ylab = "water_conductance (not logged)", main = "GP design matrix")

image(gp[[1]]$invVarMatrix)

plot(x = gp[[2]]$X[,1], y = gp[[2]]$X[,2], cex = gp[[2]]$Z*20, xlab = "growth respiration", ylab = "water_conductance (not logged)", main = "GP design matrix - BGB")

image(gp[[2]]$invVarMatrix)

plot(knots.params$cogongrass2[,2], log(knots.params$cogongrass2[,10]), xlab = "Growth Respiration Factor", ylab = "Water conductance")

