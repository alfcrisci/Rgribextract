######################################################################################################
library(raster)
library(rts)
library(xts)
library(rgdal)
library(maptools)
library(gdalUtils)
source("Rgribextract_aux.r")
######################################################################################################
# read vectors


za.new.poly.cfr=readRDS("za.new.poly.cfr.rds")
stazioni_auto_pluvio.cfr=readRDS("stazioni_auto_pluvio.cfr.rds")


ecm012_run00_precip=brick("ecm012_run00_precip.grb" ,lvar=1)





station_pre_diff=raster::extract(ecm012_run00_precip,stazioni_auto_pluvio.cfr)*1000


###########################################################################################################################################################
# Extract

station_par=cbind(station_pre_diff[,1],as.data.frame(t(apply(station_pre_diff,1,diff))))
zone_par_pre_diff=raster::extract(ecm012_run00_precip,za.new.poly.cfr)

zone_par=lapply(zone_par_pre_diff,function(x) rbind(x[,1],apply(x,1,diff)))


###########################################################################################################################################################
# Max & min calculation

zone_par_max=do.call("rbind",lapply(zone_par,function(x) apply(x,2,max)))*1000
zone_par_mean=do.call("rbind",lapply(zone_par,function(x) apply(x,2,mean)))*1000



###########################################################################################################################################################
# Time index retrieval

time_sec=as.numeric(gsub(" sec UTC","",gsub("    GRIB_VALID_TIME=  ","",robust.system("gdalinfo ecm012_run00_precip.grb |grep GRIB_VALID_TIME=")$stdout)))
time_ini=as.numeric(gsub(" sec UTC","",gsub("    GRIB_REF_TIME=","",robust.system("gdalinfo ecm012_run00_precip.grb |grep GRIB_REF_TIME=")$stdout)))
for_sec=abs(time_ini-time_sec)
for_h=abs(time_ini-time_sec)/3600
datetime_for=ISOdate(1970,1,1)+time_sec
datetime_ini=ISOdate(1970,1,1)+time_ini

###########################################################################################################################################################
# Naming
names_hours=paste0("pcp_",for_h,"h")

names(station_par)=names_hours
names(zone_par_max)=names_hours
names(zone_par_mean)=names_hours

###########################################################################################################################################################
# Building frames

stazione_par_fin=cbind(stazioni_auto_pluvio.cfr@data[c("IDStazione","Nome","Comune","Provincia","Fiume","nome_area","cod_area")],coordinates(stazioni_auto_pluvio.cfr),station_par)
zone_par_max_fin=cbind(za.new.poly.cfr@data,zone_par_max)
zone_par_mean_fin=cbind(za.new.poly.cfr@data,zone_par_mean)

###########################################################################################################################################################
# Loading Geo objects

za.zone_par_max=za.new.poly.cfr
za.zone_par_mean=za.new.poly.cfr
za.staz_par_mean=stazioni_auto_pluvio.cfr

za.zone_par_max@data=zone_par_max_fin
za.zone_par_mean@data=zone_par_mean_fin
za.staz_par_mean@data=stazione_par_fin

###########################################################################################################################################################
# Write shapefile and geojson

writeOGR(za.zone_par_max, "za.zone_par_max", "za.pcp.max", driver="ESRI Shapefile",overwrite_layer=T)
writeOGR(za.zone_par_mean, "za.zone_par_mean", "za.pcp.mean", driver="ESRI Shapefile",overwrite_layer=T)
writeOGR(za.zone_par_mean, "za.zone_par_mean", "za.pcp.mean", driver="ESRI Shapefile",overwrite_layer=T)

shplist=Sys.glob("*_par*.shp")

sapply(shplist,FUN=function(x) shp2geojson(x))



###########################################################################################################################################################
# Export in csv like

write.csv(stazione_par_fin,"stazione_par_fin.csv",row.names = FALSE)
write.csv(zone_par_max_fin,"zone_par_max_fin.csv",row.names = FALSE)
write.csv(zone_par_mean_fin,"zone_par_mean_fin.csv",row.names = FALSE)

###########################################################################################################################################################
