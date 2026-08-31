#########################################
### Data management for BOEM CIPA project
#########################################

####
##
# Clear environment, load packages, set working directory
rm(list=ls())
z<-c("fst","rnaturalearth","sf","lwgeom","raster","ggplot2","rnaturalearthhires",
     "auk","dplyr")
lapply(z, library, character.only=TRUE)



####
##
#  Tracking data by species ----------------------------------------------------
####
##
#

####
files <- list.files("Raw tracking data") # vector of file names
list_df <- setNames(vector("list", length(files)), # empty list of file names
                   c(files))
for (i in 1:length(files)){ # populate list of file names
  list_df[[paste0("",files[i],"")]] <- read.csv(paste("Raw tracking data/",paste0("",files[i],""),sep=""))}

####
sp <- c() # empty vector of species names
for (i in 1:length(files)){
  list_df[[paste0("",files[i],"")]][["file.name"]] <- files[i] # incl col:file name w/in each data set
  sp <- c(sp, # vector of species names
          unique(list_df[[paste0("",files[i],"")]][["individual.taxon.canonical.name"]]))}
list_tracks <- setNames(vector("list", length(unique(sp))), c(unique(sp))) # empty list of species names
df.comp.og <- data.frame(event.id = NA,
                         timestamp = NA,
                         location.long = NA,
                         location.lat = NA,
                         argos.lc = NA,
                         sensor.type = NA,
                         individual.taxon.canonical.name = NA,
                         tag.local.identifier = NA,
                         individual.local.identifier = NA,
                         study.name = NA,
                         file.name = NA)
df.comp <- df.comp.og
for (i in 1:length(list_tracks)){ # populate list of species names
  for (j in 1:length(list_df)){ # search for species across files and stitch the data sets together
    if (names(list_tracks)[i] %in% unique(list_df[[j]][["individual.taxon.canonical.name"]])){
      df <- list_df[[j]]
      df2 <- data.frame(if (is.null(df$event.id)){event.id = NA} else {event.id = df$event.id},
                        timestamp = df$timestamp,
                        location.long = df$location.long,
                        location.lat = df$location.lat,
                        if (is.null(df$argos.lc)){argos.lc = NA} else {argos.lc = df$argos.lc},
                        if (is.null(df$sensor.type)){sensor.type = NA} else {sensor.type = df$sensor.type},
                        individual.taxon.canonical.name = df$individual.taxon.canonical.name,
                        if (is.null(df$tag.local.identifier)){tag.local.identifier = NA} else {tag.local.identifier = df$tag.local.identifier},
                        if (is.null(df$individual.local.identifier)){individual.local.identifier = NA} else {individual.local.identifier = df$individual.local.identifier},
                        study.name = df$study.name,
                        file.name = df$file.name)
      names(df2)[c(1,5,6,8,9)] <- c("event.id","argos.lc","sensor.type","tag.local.identifier","individual.local.identifier") 
      df3 <- df2[df2$individual.taxon.canonical.name==names(list_tracks)[i] & # this accounts for some files including multiple taxa
                   is.na(df2$location.long)==F,]
      df.comp <- rbind(df.comp,df3)}
  }
  list_tracks[[names(list_tracks)[i]]] <- df.comp[-1,] # move df.comp into list
  df.comp <- df.comp.og # reset df.comp
}

####
for (i in 1:length(list_tracks)) {
  df <- list_tracks[[i]]
  df <- df[df$argos.lc== "" |
             df$argos.lc== "1" |
             df$argos.lc== "2" |
             df$argos.lc== "3" |
             df$argos.lc== "G",] %>%
    drop_na(timestamp)
  for (j in 1:dim(df)[1]){
    if (df$argos.lc[j]==""){
      df$argos.lc[j] <- df$sensor.type[j]}
  }
  df <- df[df$argos.lc== "gps" |
             df$argos.lc== "1" |
             df$argos.lc== "2" |
             df$argos.lc== "3" |
             df$argos.lc== "G",] %>%
    drop_na(timestamp)
  list_tracks[[i]] <- df
}

####
names(list_tracks) <- c("Black Scoter", "Brant",
                    "Common Merganser","Common Murre","Cook Inlet Gull",
                    "Glaucous-winged Gull","Glaucous Gull","American Herring Gull",
                    "King Eider","Kittlitz's Murrelet","Long-tailed Duck",
                    "Marbled Godwit","Marbled Murrelet","Northern Fulmar",
                    "Red-throated Loon","Iceland Gull",
                    "Tufted Puffin","Tundra Swan","Hudsonian Whimbrel",
                    "Yellow-billed Loon")
for (i in 1:length(names(list_tracks))){
  list_tracks[[i]]$event.id <- as.character(list_tracks[[i]]$event.id)
  list_tracks[[i]]$timestamp <- as.character(list_tracks[[i]]$timestamp)
  list_tracks[[i]]$location.long <- as.numeric(list_tracks[[i]]$location.long)
  list_tracks[[i]]$location.lat <- as.numeric(list_tracks[[i]]$location.lat)
  list_tracks[[i]]$argos.lc <- as.character(list_tracks[[i]]$argos.lc)
  list_tracks[[i]]$sensor.type <- as.character(list_tracks[[i]]$sensor.type)
  list_tracks[[i]]$individual.taxon.canonical.name <- as.character(list_tracks[[i]]$individual.taxon.canonical.name)
  list_tracks[[i]]$tag.local.identifier <- as.character(list_tracks[[i]]$tag.local.identifier)
  list_tracks[[i]]$individual.local.identifier <- as.character(list_tracks[[i]]$individual.local.identifier)
  list_tracks[[i]]$study.name <- as.character(list_tracks[[i]]$study.name)
  list_tracks[[i]]$file.name <- as.character(list_tracks[[i]]$file.name)
  #
  list_tracks[[i]]$Species <- names(list_tracks)[i]
  list_tracks[[i]]$Year <- format(as.Date(list_tracks[[i]]$timestamp), "%Y")
  list_tracks[[i]]$Month <- format(as.Date(list_tracks[[i]]$timestamp), "%B")
  list_tracks[[i]]$Time_continuous <- format(as.Date(list_tracks[[i]]$timestamp), "%j")
  list_tracks[[i]]$Time_continuous <- as.numeric(list_tracks[[i]]$Time_continuous)
  list_tracks[[i]]$Timestamp2 <- format(as.Date(list_tracks[[i]]$timestamp), "%d %b %Y")
  list_tracks[[i]]$location.long.dateline <- NA
  list_tracks[[i]]$location.lat.dateline <- list_tracks[[i]]$location.lat
  list_tracks[[i]]$location.label <- NA
  list_tracks[[i]]$Tag.pal <- NA
  for (j in 1:dim(list_tracks[[i]])[1]){
    if (list_tracks[[i]]$location.long[j] < 0){
      list_tracks[[i]]$location.long.dateline[j] <- list_tracks[[i]]$location.long[j]+360}
    else {list_tracks[[i]]$location.long.dateline[j] <- list_tracks[[i]]$location.long[j]}
    list_tracks[[i]]$individual.id[j] <- paste(names(list_tracks[i]),
                                           list_tracks[[i]]$tag.local.identifier[j],
                                           list_tracks[[i]]$individual.local.identifier[j],sep=".")
    list_tracks[[i]]$location.label[j] <- paste(list_tracks[[i]]$Species[j],
                                            paste0(" (Tag: ","",list_tracks[[i]]$tag.local.identifier[j],"); "),
                                            list_tracks[[i]]$Timestamp2[j], sep="")
    }
}



####
list_tracks <- list_tracks[c("Brant","Tundra Swan","King Eider", # reorder
                     "Black Scoter","Long-tailed Duck",
                     "Common Merganser","Hudsonian Whimbrel",
                     "Marbled Godwit","Tufted Puffin",
                     "Kittlitz's Murrelet","Marbled Murrelet",
                     "Common Murre","American Herring Gull", "Glaucous Gull",
                     "Glaucous-winged Gull","Cook Inlet Gull",
                     "Iceland Gull","Red-throated Loon",
                     "Yellow-billed Loon","Northern Fulmar")]

list_tracks["Glaucous Gull"] <- NULL # remove Glaucous Gull
#save(list_tracks, file="Processed data objects/list_tracks.rda")

# de-list and save as .fst files
names <- c("a_Brant","b_Tundra Swan","c_King Eider", # reorder
           "d_Black Scoter","e_Long-tailed Duck",
           "f_Common Merganser","g_Hudsonian Whimbrel",
           "h_Marbled Godwit","i_Tufted Puffin",
           "j_Kittlitz's Murrelet","k_Marbled Murrelet",
           "l_Common Murre","m_American Herring Gull",
           "n_Glaucous-winged Gull","o_Cook Inlet Gull",
           "p_Iceland Gull","q_Red-throated Loon",
           "r_Yellow-billed Loon","s_Northern Fulmar")
# for (i in 1:length(list_tracks)){
#   df <- list_tracks[[i]]
#   write.fst(df, paste("Processed data objects/fast.tracks/",paste0("",names[i],""),".fst",sep=""))
# }


# END --------------------------------------------------------------------------



####
##
# Spatial domains --------------------------------------------------------------
####
##
#

####
# Greater Cook Inlet Planning Area
# project points
df.POIN <- data.frame(Region = c("east","east",
                                 "west","west","west"),
                      Longitude = c(-148.9,-148.9,
                                    -148.9,-155.5,-157.5),
                      Latitude = c(64,54,
                                   63,59,57))
points <- st_as_sf(df.POIN, coords = c("Longitude", "Latitude"), crs = 4326)
points <- st_transform(points, crs = "+proj=laea +lat_0=59.6 +lon_0=-152.6 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
cut.line <- st_union(points) |> st_cast("LINESTRING")
for (i in 1:dim(df.POIN)[1]){
  df.POIN$Longitude[i]<-points$geometry[[i]][1]
  df.POIN$Latitude[i]<-points$geometry[[i]][2]}
# project regions
df.MEDO <- data.frame(Region = "midpoint",
                      Longitude = c(-152.6),
                      Latitude = c(59.6))
medoids <- st_as_sf(df.MEDO, coords = c("Longitude", "Latitude"), crs = 4326)
medoids <- st_transform(medoids, crs = "+proj=laea +lat_0=59.6 +lon_0=-152.6 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
df.MEDO.poly <- st_buffer(medoids, dist = 380000)
for (i in 1:dim(df.MEDO)[1]){
  df.MEDO$Longitude[i]<-medoids$geometry[[i]][1]
  df.MEDO$Latitude[i]<-medoids$geometry[[i]][2]}
# Cut poly and extract
cut <- st_split(df.MEDO.poly,cut.line)
areas <- st_collection_extract(cut, "POLYGON")
area.GCI <- st_as_sfc(areas["1.1", 2], crs = "+proj=laea +lat_0=59.6 +lon_0=-152.6 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
area.GCI.map <- st_transform(area.GCI, crs = 4326)

####
# Greater Cook Inlet Planning Area (offshore)
#
base.lnd<-ne_countries(scale="large", returnclass="sf")
base.lnd<-st_transform(base.lnd, crs = st_crs("+proj=laea +lat_0=59.6 +lon_0=-152.6 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
GCI.intr <- st_intersection(base.lnd, area.GCI)
GCI.off <- st_difference(area.GCI, st_union(GCI.intr))
area.GCI.off <- st_buffer(GCI.off, dist = -1, allow_holes=TRUE) # buffer by 10km
area.GCI.off.map <- st_transform(area.GCI.off, crs = 4326)

####
# Cook Inlet Planning Area
#
CIPA <- st_read("BOEM_shapefiles/CookInletPlanningArea")
CIPA <- st_as_sfc(CIPA, crs = "+proj=laea +lat_0=59.6 +lon_0=-152.6 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
area.CIP.map <- st_transform(CIPA, crs = 4326)

####
# Lease Area
#
BBC1 <- st_read("BOEM_shapefiles/BBC1")
BBC1 <- st_as_sfc(BBC1, crs = "+proj=laea +lat_0=59.6 +lon_0=-152.6 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
area.LSA.map <- st_transform(BBC1, crs = 4326)
area.LSA.map[[1]][[1]] <- area.LSA.map[[1]][[1]][,-3] 

####
# LISTING
#
list.shp <- setNames(vector("list", 6), # empty list of file names
                    c("area.GCI","area.GCI.off","area.GCI.map",
                      "area.GCI.off.map","area.CIP.map","area.LSA.map"))
list.shp[[1]] <- area.GCI
list.shp[[2]] <- area.GCI.off
list.shp[[3]] <- area.GCI.map
list.shp[[4]] <- area.GCI.off.map
list.shp[[5]] <- area.CIP.map
list.shp[[6]] <- area.LSA.map
# convert longitudes for mapping
for (i in 3:length(list.shp)){ # "3" because not all shapefiles need their longs converted
  for (j in 1:length(list.shp[[i]])){
    for (k in 1:length(list.shp[[i]][[j]])){
      for (l in 1:dim(list.shp[[i]][[j]][[k]])[1]){
        if (list.shp[[i]][[j]][[k]][l,1] < 0){
          list.shp[[i]][[j]][[k]][l,1] <- list.shp[[i]][[j]][[k]][l,1]+360}
      }
    }
  }
}
#
#save(list.shp, file="Processed data objects/list.shp.rda")

####
# Raster grids for UD analyses
#
sf_poly <- st_sf(value = 1, geometry = list.shp["area.GCI"]$area.GCI) # Convert sfc_POLYGON to sf_polygon
r <- raster(extent(sf_poly), res = 2000) # empty raster of polygon extent, 2 kilometer resolution
crs(r) <- st_crs(sf_poly)$proj4string # set CRS to equal area projection
r_poly <- rasterize(sf_poly, r, field = "value", background = NA) # rasterize the GCI polygon
spdf.area.GCI <- as(r_poly, "SpatialPixelsDataFrame")
#save(spdf.area.GCI, file = "Processed data objects/spdf.area.GCI.rda")

sf_poly <- st_sf(value = 1, geometry = list.shp["area.GCI.off"]$area.GCI.off) # Convert sfc_POLYGON to sf_polygon
r <- raster(extent(sf_poly), res = 2000) # empty raster of polygon extent, 2 kilometer resolution
crs(r) <- st_crs(sf_poly)$proj4string # set CRS to equal area projection
r_poly <- rasterize(sf_poly, r, field = "value", background = NA) # rasterize the GCI polygon
spdf.area.GCI.off <- as(r_poly, "SpatialPixelsDataFrame")
#save(spdf.area.GCI.off, file = "Processed data objects/spdf.area.GCI.off.rda")

list.rst <- setNames(vector("list", 2), # empty list of file names
                     c("spdf.area.GCI","spdf.area.GCI.off"))
list.rst[[1]] <- spdf.area.GCI
list.rst[[2]] <- spdf.area.GCI.off
#save(list.rst, file="Processed data objects/list.rst.rda")

# END --------------------------------------------------------------------------



####
##
# Track timing data by species -------------------------------------------------------
####
##
#

# upload list_tracks and list.shp


####
##
# construct df.ebird

# df.all <- read_ebd("EBD/ebd_US-AK_smp_relJul-2026.txt", # unique deletes repeat checklists, rollup converts all entries to species level
#                    unique = TRUE, rollup = TRUE) %>%
#                     filter(,all_species_reported == TRUE) %>%
#                     mutate(Loc.long.sf = longitude,
#                            Loc.lat.sf = latitude) %>%
#                     st_as_sf(coords = c("Loc.long.sf", "Loc.lat.sf"), crs = 4326) %>%
#                     st_transform(crs = "+proj=laea +lat_0=59.6 +lon_0=-152.6 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") %>%
#                     st_filter(list.shp[["area.GCI"]]) # subset those in Great CIPA, using equal area projection
# df.ebird <- data.frame(checklist_id = df.all$checklist_id,
#                        common_name = df.all$common_name,
#                        scientific_name = df.all$scientific_name,
#                        observation_count = df.all$observation_count,
#                        latitude = df.all$latitude,
#                        longitude = df.all$longitude,
#                        observation_date = df.all$observation_date,
#                        time_continuous = as.numeric(format(df.all$observation_date, "%j")),
#                        all_species_reported = df.all$all_species_reported,
#                        group_identifier = df.all$group_identifier) # use group_id to count checklists
# write.fst(df.ebird,"Processed data objects/ebird.GCIPA.EBD.fst")
df.ebird <- read_fst("Processed data objects/ebird.GCIPA.EBD.fst")
#
# df.samp <- read_sampling("EBD/ebd_US-AK_smp_relJul-2026_sampling.txt", unique = TRUE) %>%
#                           filter(,all_species_reported == TRUE) %>%
#                           mutate(Loc.long.sf = longitude,
#                                  Loc.lat.sf = latitude) %>%
#                           st_as_sf(coords = c("Loc.long.sf", "Loc.lat.sf"), crs = 4326) %>%
#                           st_transform(crs = "+proj=laea +lat_0=59.6 +lon_0=-152.6 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") %>%
#                           st_filter(list.shp[["area.GCI"]]) # subset those in Great CIPA, using equal area projection
# df.ebird.samp <- data.frame(checklist_id = df.samp$checklist_id,
#                             latitude = df.samp$latitude,
#                             longitude = df.samp$longitude,
#                             observation_date = df.samp$observation_date,
#                             all_species_reported = df.samp$all_species_reported,
#                             group_identifier = df.samp$group_identifier)
# write.fst(df.ebird.samp,"Processed data objects/ebird.GCIPA.EBD.sample.fst")
# df.ebird.samp <- read_fst("Processed data objects/ebird.GCIPA.EBD.sample.fst")
# #
# paste(length(unique(df.ebird$group_identifier)),length(unique(df.ebird.samp$group_identifier))) # confirm same length, and plot points

####
##
# list.track.plots

list.track.plots <- list()
df.comp.plot.og <- data.frame(Dates = c("1–7 Jan","8–14 Jan","15–21 Jan","22–31 Jan",
                                        "1–7 Feb","8–14 Feb","15–21 Feb","22–28 Feb",
                                        "1–7 Mar","8–14 Mar","15–21 Mar","22–31 Mar",
                                        "1–7 Apr","8–14 Apr","15–21 Apr","22–30 Apr",
                                        "1–7 May","8–14 May","15–21 May","22–31 May",
                                        "1–7 Jun","8–14 Jun","15–21 Jun","22–30 Jun",
                                        "1–7 Jul","8–14 Jul","15–21 Jul","22–31 Jul",
                                        "1–7 Aug","8–14 Aug","15–21 Aug","22–31 Aug",
                                        "1–7 Sep","8–14 Sep","15–21 Sep","22–30 Sep",
                                        "1–7 Oct","8–14 Oct","15–21 Oct","22–31 Oct",
                                        "1–7 Nov","8–14 Nov","15–21 Nov","22–30 Nov",
                                        "1–7 Dec","8–14 Dec","15–21 Dec","22–31 Dec"),
                              Start.julian = c(1,8,15,22,
                                               32,39,46,53,
                                               60,67,74,81,
                                               91,98,105,112,
                                               121,128,135,142,
                                               152,159,166,173,
                                               182,189,196,203,
                                               213,220,227,234,
                                               244,251,258,265,
                                               274,281,288,295,
                                               305,312,319,326,
                                               335,342,349,356),
                              End.julian = c(7,14,21,31,
                                             38,45,52,59,
                                             66,73,80,90,
                                             97,104,111,120,
                                             127,134,141,151,
                                             158,165,172,181,
                                             188,195,202,212,
                                             219,226,233,243,
                                             250,257,264,273,
                                             280,287,294,304,
                                             311,318,325,334,
                                             341,348,355,366),
                              Week = 1:48,
                              Species = NA,
                              Num.tracks = NA,
                              Num.years = NA,
                              Num.indi = NA,
                              Num.first.loc = NA,
                              Num.last.loc = NA,
                              Num.in.CI = NA,
                              Perc.in.CI = NA,
                              Tot.ebird.lists = NA,
                              Det.ebird.lists = NA,
                              Perc.ebird = NA)
###
taxa <- names(list_tracks)
for (i in 1:length(taxa)) { # subset by species
  # construct "length of stay" df
  df <- bind_rows(list_tracks[c(taxa[i])]) %>%
    mutate(Loc.long.sf = location.long) %>%
    mutate(Loc.lat.sf = location.lat) %>%
    st_as_sf(coords = c("Loc.long.sf", "Loc.lat.sf"), crs = 4326) %>%
    st_transform(crs = "+proj=laea +lat_0=59.6 +lon_0=-152.6 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
  df$In.CI <- st_within(df,list.shp[["area.GCI"]], sparse = FALSE)[,1] # TRUE/FALSE: is location in the cook inlet?
  df$First.loc <- FALSE
  df$Last.loc <- FALSE
  df.comp <- df[1,]
  individuals <- unique(df$individual.id)
  for (j in 1:length(individuals)){ # subset by individual and arrange in chronological order
    df.sub <- df[df$individual.id==individuals[j],] %>%
      arrange(Year, Time_continuous)
    for (k in 1:dim(df.sub)[1]){ # cycle through rows and identify first/last locations in the CI
      if ((k==1) && (df.sub$In.CI[k]==TRUE)){
        df.sub$First.loc[k] <- TRUE}
      if ((k>1) && (df.sub$In.CI[k]==TRUE) && (df.sub$In.CI[k-1]==FALSE)){
        df.sub$First.loc[k] <- TRUE}
      if ((k<dim(df.sub)[1]) && (df.sub$In.CI[k]==TRUE) && (df.sub$In.CI[k+1]==FALSE)){
        df.sub$Last.loc[k] <- TRUE}
      if ((k==dim(df.sub)[1]) && (df.sub$In.CI[k]==TRUE)){
        df.sub$Last.loc[k] <- TRUE}
    }
    df.comp <- rbind(df.comp,df.sub) # compile modified df
  }
  df <- df.comp[-1,] # remove placeholder
  #
  df.los <- data.frame(Species = df[df$First.loc==T,]$Species,
                       Individual.id = df[df$First.loc==T,]$individual.id,
                       Year = df[df$First.loc==T,]$Year,
                       First.jul = df[df$First.loc==T,]$Time_continuous,
                       Last.jul = df[df$Last.loc==T,]$Time_continuous,
                       Min.los = NA)
  for (l in 1:dim(df.los)[1]){ # cycle through rows df.los
    if (df.los$Last.jul[l] > df.los$First.jul[l]){
      df.los$Min.los[l] <- df.los$Last.jul[l] - df.los$First.jul[l]}
    if (df.los$Last.jul[l] < df.los$First.jul[l]){
      df.los$Min.los[l] <- df.los$Last.jul[l] + (365 - df.los$First.jul[l])}
    if (is.na(df.los$Min.los[l])){
      df.los$Min.los[l] <- 1}
  }
  # construct "timing data" df
  df.comp <- df.comp.plot.og
  df.comp$Species <- df$Species[1]
  for (m in 1:dim(df.comp)[1]) { # cycle through df.comp
    df.sub <- df[(df$Time_continuous >= df.comp$Start.julian[m]) & 
                   (df$Time_continuous <= df.comp$End.julian[m]),]
    df.comp$Num.tracks[m] <- dim(distinct(df.sub,Year,individual.id))[1]
    df.comp$Num.years[m] <- dim(distinct(df.sub,Year))[1]
    df.comp$Num.indi[m] <- dim(distinct(df.sub,individual.id))[1]
    df.comp$Num.first.loc[m] <- dim(distinct(df.los[(df.los$First.jul >= df.comp$Start.julian[m]) &
                                             (df.los$First.jul <= df.comp$End.julian[m]),],Year,Individual.id))[1]
    df.comp$Num.last.loc[m] <- dim(distinct(df.los[(df.los$Last.jul >= df.comp$Start.julian[m]) &
                                            (df.los$Last.jul <= df.comp$End.julian[m]),],Year,Individual.id))[1]
    df.comp$Num.in.CI[m] <- dim(distinct(df.sub[df.sub$In.CI==TRUE,],Year,individual.id))[1]
    df.comp$Perc.in.CI[m] <- if (dim(distinct(df.sub[df.sub$In.CI==TRUE,],Year,individual.id))[1]==0){0}
    else {round(dim(distinct(df.sub[df.sub$In.CI==TRUE,],Year,individual.id))[1] / 
                  dim(distinct(df.sub,Year,individual.id))[1] * 100, digits = 2)}
    df.comp$Tot.ebird.lists[m] <- length(unique(df.ebird[(df.ebird$time_continuous >= df.comp$Start.julian[m]) & 
                                                           (df.ebird$time_continuous <= df.comp$End.julian[m]),]$group_identifier))
    df.comp$Det.ebird.lists[m] <- dim(df.ebird[(df.ebird$time_continuous >= df.comp$Start.julian[m]) & 
                                                 (df.ebird$time_continuous <= df.comp$End.julian[m]) &
                                                 (df.ebird$common_name == df.comp$Species[m]),])[1]
    df.comp$Perc.ebird[m] <- round(length(unique(df.ebird[(df.ebird$time_continuous >= df.comp$Start.julian[m]) & 
                                                            (df.ebird$time_continuous <= df.comp$End.julian[m]) &
                                                            (df.ebird$common_name == df.comp$Species[m]),]$group_identifier))
                                   /
                                     length(unique(df.ebird[(df.ebird$time_continuous >= df.comp$Start.julian[m]) & 
                                                              (df.ebird$time_continuous <= df.comp$End.julian[m]),]$group_identifier)) * 100,
                                   digits = 2)
  }
  
  x <- list(length.of.stay = df.los,
            timing.data = df.comp)  
  
  list.track.plots[[i]] <- x
}

names(list.track.plots) <- names(list_tracks)
#save(list.track.plots, file="Processed data objects/list.track.plots.rda")


# END --------------------------------------------------------------------------




