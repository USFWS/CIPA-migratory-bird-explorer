

####
##
# Clear environment, load packages, set working directory
rm(list=ls())
z<-c("birdscanR","dplyr")
lapply(z, library, character.only=TRUE)




# Extracting Radar data --------------------------------------------------------------

### identify where the scans reside and how many there are
  ## sift through folders
### March through November
### Minimum 8 hours after evening civil twilight (sun >= 6 deg below horizon)
### If no civil twilight, take last recorded civil twilight
### If duration < 8 hours, extend for 8 hours
### dates correspond to the date of sunset and proceed into the following day 
### e.g., 2 Oct includes the sunset on 2 Oct through to sunrise on 3 Oct


### Create a df of sampling events (sampling events are in UTC time)
df.events <- data.frame(Year.UTC = rep(1995:2025,each=275),
                       Month.UTC = rep(c(rep(3,31),rep(4,30),rep(5,31),
                                 rep(6,30),rep(7,31),rep(8,31),
                                 rep(9,30),rep(10,31),rep(11,30)),31),
                       Day.UTC = rep(c(1,2,3,4,5,6,7,8,9,10:31,
                                   1,2,3,4,5,6,7,8,9,10:30,
                                   1,2,3,4,5,6,7,8,9,10:31,
                                   1,2,3,4,5,6,7,8,9,10:30,
                                   1,2,3,4,5,6,7,8,9,10:31,
                                   1,2,3,4,5,6,7,8,9,10:31,
                                   1,2,3,4,5,6,7,8,9,10:30,
                                   1,2,3,4,5,6,7,8,9,10:31,
                                   1,2,3,4,5,6,7,8,9,10:30),31),
                       Site = "PAHG",
                       Longitude = -151.35139,
                       Latitude = 60.725833,
                       Start.civil.UTC = NA,
                       End.civil.UTC = NA,
                       Dur.civil.hrs = NA,
                       Start.analysis.UTC = NA,
                       End.analysis.UTC = NA,
                       Dur.analysis.hrs = NA,
                       Num.of.scans = NA)


df.comp <- df.events[1,]
yrs <- unique(df.events$Year.UTC)
for (i in 1:length(yrs)){ ## define the "nocturnal period"
  df.sub <- df.events[df.events$Year.UTC==yrs[i],]
  ts <- twilight(timeRange = c(as.POSIXct(paste0(df.sub$Year.UTC[1],"-",df.sub$Month.UTC[1],"-",df.sub$Day.UTC[1]," ","00:00:01"),tz="UTC", format = "%Y-%m-%d %H:%M:%S"), 
                               as.POSIXct(paste0(df.sub$Year.UTC[dim(df.sub)[1]],"-",df.sub$Month.UTC[dim(df.sub)[1]],"-",df.sub$Day.UTC[dim(df.sub)[1]]," ","23:59:59"),tz="UTC", format = "%Y-%m-%d %H:%M:%S")),
                 latLon    = c(df.sub$Latitude[1], df.sub$Longitude[1]),
                 timeZone  = "UTC") %>% filter(,is_night==1)
  df.sub$Start.civil.UTC <- ts$civilStart[1:275]
  df.sub$End.civil.UTC <- ts$civilStop[1:275]
  df.sub$Dur.civil.hrs <- as.numeric(difftime(df.sub$End.civil.UTC, df.sub$Start.civil.UTC, units = "hours"))
  for (j in 1:dim(df.sub)[1]){
    if (is.na(df.sub$Start.civil.UTC[j])==TRUE){
      df.sub$Start.civil.UTC[j] <- df.sub$Start.civil.UTC[j-1]
      df.sub$Dur.civil.hrs[j] <- 0}
    if (is.na(df.sub$End.civil.UTC[j])==TRUE){
      df.sub$Dur.civil.hrs[j] <- 0}
    df.sub$Start.analysis.UTC[j] <- as.numeric(paste0(format(df.sub$Start.civil.UTC[j], "%H"),
                                           format(df.sub$Start.civil.UTC[j], "%M"),
                                           format(df.sub$Start.civil.UTC[j], "%S")))
    if (df.sub$Dur.civil.hrs[j] >= 8){
      df.sub$End.analysis.UTC[j] <- as.numeric(paste0(format(df.sub$End.civil.UTC[j], "%H"),
                                           format(df.sub$End.civil.UTC[j], "%M"),
                                           format(df.sub$End.civil.UTC[j], "%S")))
      df.sub$Dur.analysis.hrs[j] <- df.sub$Dur.civil.hrs[j]}
    else {df.sub$End.analysis.UTC[j] <- as.numeric(paste0(format(df.sub$Start.civil.UTC[j] + 8*60*60, "%H"),
                                               format(df.sub$Start.civil.UTC[j] + 8*60*60, "%M"),
                                               format(df.sub$Start.civil.UTC[j] + 8*60*60, "%S")))
    df.sub$Dur.analysis.hrs[j] <- 8}
  }
  df.comp <- rbind(df.comp,df.sub)
}
df.events <- df.comp[-1,]


## create a df of radar scans (including their file paths)
## tally the number of scans per date (i.e., event) that occur in the night [note "night" ranges ]
archives <- list.files("NEXRAD profile data") # vector of archive names
files.comp <- NA
for (i in 1:length(archives)){
  files <- untar(paste("NEXRAD profile data/",paste0("",archives[i],""),sep=""),
                 list=TRUE) # include ".bz2" here but not in folder
  PAHG.files <- files[grep("PAHG", files)] # subset PAHG file paths
  files.comp <- c(files.comp,PAHG.files) # comp across years
}
file.paths <- as.data.frame(files.comp[-1])
# save(file.paths,file="file.paths.rda")
# load("file.paths.rda")


####
###
df.scans <- data.frame(Year.UTC = as.numeric(substr(file.paths[,1],10,13)),
                       Month.UTC = as.numeric(substr(file.paths[,1],15,16)),
                       Day.UTC = as.numeric(substr(file.paths[,1],18,19)),
                       Site = substr(file.paths[,1],21,24),
                       Longitude = -151.35139,
                       Latitude = 60.725833,
                       Scan.UTC = as.numeric(substr(file.paths[,1],39,44)),
                       Nocturnal = NA,
                       File.path = file.paths[,1])
df.scans <- df.scans[df.scans$Month.UTC>2 &
                        df.scans$Month.UTC<12,] # subset march through november


### 
### Identify and tally nocturnal radar scans
df.comp <- df.scans[1,]
for (i in 1:dim(df.events)[1]){
  df.sub <- df.scans[df.scans$Year.UTC==df.events$Year.UTC[i] &
                         df.scans$Month.UTC==df.events$Month.UTC[i] &
                         df.scans$Day.UTC==df.events$Day.UTC[i],]
 if (dim(df.sub)[1]>0){
    for (j in 1:dim(df.sub)[1]){
      if ((df.sub$Scan.UTC[j] >= df.events$Start.analysis.UTC[i]) &
          (df.sub$Scan.UTC[j] <= df.events$End.analysis.UTC[i])){
        df.sub$Nocturnal[j] <- TRUE}
      else {df.sub$Nocturnal[j] <- FALSE}
    }
    df.sub.noct <- df.sub[df.sub$Nocturnal==TRUE,]
    df.events$Num.of.scans[i] <- dim(df.sub.noct)[1]
    if (dim(df.sub.noct)[1] >=48){
      df.comp <- rbind(df.comp,df.sub.noct)}
    }
}
df.scans.noct <- df.comp[-1,]
df.events.noct <- df.events[is.na(df.events$Num.of.scans)==FALSE &
                         df.events$Num.of.scans >= 48,] # minimum of 48 scans per night
                                                        # radar scans every 10 minutes in clear and 8 minutes in rainy conditions
                                                        # minimum length of "night" is 8 hrs
sum(df.events.noct$Num.of.scans) == dim(df.scans.noct)[1]
# save(df.scans.noct,file="df.scans.noct.rda")
# load("df.scans.noct.rda")
# save(df.events.noct,file="df.events.noct.rda")
# load("df.events.noct.rda")


###
### Extract and compile nocturnal and diurnal scan data
noct.paths <- df.scans.noct[,c("Year.UTC","File.path")]
# diur.paths <- df.scans[df.scans$Nocturnal==FALSE,][,c("Year.UTC","File.path")]
#
noct.yr <- unique(noct.paths$Year.UTC)
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[1:600], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[601:1200], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[1201:1800], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[1801:2400], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[2401:3000], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[3001:3600], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[3601:4200], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[4201:4800], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[4801:5400], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[5401:6000], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[6001:6600], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[6601:7200], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[7201:7800], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[7801:8400], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[8401:9000], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[9001:9600], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[9601:10200], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[10201:10800], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[10801:11400], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[11401:12000], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[12001:12600], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[12601:13200], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[13201:13800], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[13801:14400], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[14401:15000], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
noct.yr <- noct.yr[noct.yr!=1996 &
                     noct.yr!=1997 &
                     noct.yr!=1998 &
                     noct.yr!=1999 &
                     noct.yr!=2013]
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[15001:15600], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[15601:16200], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[16201:16800], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[16801:17400], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[17401:18000], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[18001:18600], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[18601:19200], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[19201:19800], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[19801:20400], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[20401:21000], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[21001:21600], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[21601:22200], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[22201:22800], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
noct.yr <- noct.yr <- noct.yr[noct.yr!=2019 &
                                noct.yr!=2020 &
                                noct.yr!=2021 &
                                noct.yr!=2022 &
                                noct.yr!=2024]
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[22801:23400], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[23401:24000], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[24001:24600], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[24601:25200], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[25201:25800], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[25801:26400], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[26401:27000], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[27001:27600], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[27601:28200], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[28201:28800], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[28801:29400], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[29401:30000], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[30001:30600], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[30601:31200], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[31201:31800], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
for (i in 1:length(noct.yr)){
  paths <- noct.paths[noct.paths$Year.UTC==noct.yr[i],]
  untar(paste("NEXRAD profile data/profiles_",paste0("",noct.yr[i],".tar.bz2"),sep=""),
        files = paths$File.path[31801:32278], # max at 32278
        exdir = "NEXRAD nocturnal scans")
}
####
###
###
# diur.yr <- unique(diur.paths$Year.UTC)
# for (i in 1:length(diur.yr)){
#   paths <- diur.paths[diur.paths$Year.UTC==diur.yr[i],]$File.path
#   untar(paste("NEXRAD profile data/profiles_",paste0("",diur.yr[i],".tar.bz2"),sep=""),
#         files = paths,
#         exdir = "NEXRAD diurnal scans")
# }


####
###
###
df <- data.frame(bin_lower = 9,
                 height = 9,
                 linear_eta = 9,
                 nbins = 9,
                 direction = 9,
                 speed = 9,
                 u = 9,
                 v = 9,
                 rmse = 9,
                 elev1 = 9,
                 nvolumes_gr35_e1 = 9,
                 elev2 = 9,
                 nvolumes_gr35_e2 = 9,
                 vcp = 9,
                 linear_eta_unfiltered = 9,
                 percent_rain = 9,
                 Year.UTC = 9,
                 Month.UTC = 9,
                 Day.UTC = 9,
                 Scan.UTC = 9,
                 Site = NA
                 )
#
# df.comp <- df #proofed: 1996-1999, 2013 ### STILL NEED TO DIVIDE BY 30 TO CONFIRM
# for (i in 1:dim(noct.paths2)[1]){
#   tryCatch({
#     scn <- read.csv(paste("NEXRAD nocturnal scans/",paste0("",noct.paths2$File.path[i],""),sep=""))
#     scn$Year.UTC <- as.numeric(substr(noct.paths2$File.path[i],10,13))
#     scn$Month.UTC <- as.numeric(substr(noct.paths2$File.path[i],15,16))
#     scn$Day.UTC <- as.numeric(substr(noct.paths2$File.path[i],18,19))
#     scn$Scan.UTC <- as.numeric(substr(noct.paths2$File.path[i],39,44))
#     scn$Site <- substr(noct.paths2$File.path[i],21,24)
#     df.comp <- rbind(df.comp,scn)
#   })
# }
# df.noct.data <- df.comp[-1,]
# dim(unique(df.noct.data[c("Month.UTC", "Day.UTC", "Scan.UTC")]))[1]
#
noct.yr <- unique(noct.paths$Year.UTC)
for (j in 1:length(noct.yr)){
  df.comp <- df
  noct.paths2 <- noct.paths[noct.paths$Year.UTC==noct.yr[j],]
  for (i in 1:dim(noct.paths2)[1]){
    tryCatch({
      scn <- read.csv(paste("NEXRAD nocturnal scans/",paste0("",noct.paths2$File.path[i],""),sep=""))
      if (dim(scn)[1]==30){
        scn$Year.UTC <- as.numeric(substr(noct.paths2$File.path[i],10,13))
        scn$Month.UTC <- as.numeric(substr(noct.paths2$File.path[i],15,16))
        scn$Day.UTC <- as.numeric(substr(noct.paths2$File.path[i],18,19))
        scn$Scan.UTC <- as.numeric(substr(noct.paths2$File.path[i],39,44))
        scn$Site <- substr(noct.paths2$File.path[i],21,24)
        df.comp <- rbind(df.comp,scn)}
    },error=function(e){})
  }
  df.noct.data <- df.comp[-1,]
  write.csv(df.noct.data, file=paste("Nocturnal_radar_data_",paste0("",noct.yr[j],""),".csv",sep=""))
}


# # df.scans that passed all of our filters
# flt.scns <- df.noct.data[!duplicated(df.noct.data[,c("Year.UTC","Month.UTC","Day.UTC","Scan.UTC")]),]
# df.noct.scans <- df.scans[paste(df.scans$Year.UTC, df.scans$Month.UTC, df.scans$Day.UTC, df.scans$Scan.UTC) %in%
#                             paste(flt.scns$Year.UTC, flt.scns$Month.UTC, flt.scns$Day.UTC, flt.scns$Scan.UTC),]
# write.csv(df.noct.scans,file="Nocturnal_radar_scans.csv")
# # the number of scans (that passed our filters) per nocturnal "event"
# flt.events <- df.noct.scans[!duplicated(df.noct.scans[,c("Year.UTC","Month.UTC","Day.UTC")]),]
# df.noct.events <- df.events[paste(df.events$Year.UTC, df.events$Month.UTC, df.events$Day.UTC) %in%
#                               paste(flt.events$Year.UTC, flt.events$Month.UTC, flt.events$Day.UTC),]
# for (i in 1:dim(df.noct.events)[1]){
#   sub <- df.noct.scans[paste(df.noct.scans$Year.UTC, df.noct.scans$Month.UTC, df.noct.scans$Day.UTC) %in%
#                          paste(df.noct.events$Year.UTC[i], df.noct.events$Month.UTC[i], df.noct.events$Day.UTC[i]),]
#   df.noct.events$Num.of.scans[i] <- dim(sub)[1]
# }
# write.csv(df.noct.events,file="Nocturnal_radar_events.csv")
#
# df.comp <- df
# for (i in 1:dim(diur.paths)[1]){
#   scn <- read.csv(paste("NEXRAD diurnal scans/",paste0("",diur.paths$File.path[i],""),sep=""))
#   if (dim(scn)[1]==30){
#     scn$Year.UTC <- as.numeric(substr(diur.paths$File.path[i],10,13))
#     scn$Month.UTC <- as.numeric(substr(diur.paths$File.path[i],15,16))
#     scn$Day.UTC <- as.numeric(substr(diur.paths$File.path[i],18,19))
#     scn$Scan.UTC <- as.numeric(substr(diur.paths$File.path[i],39,44))
#     scn$Site <- substr(diur.paths$File.path[i],21,24)
#     df.comp <- rbind(df.comp,scn)}
# }
# df.diur.data <- df.comp[-1,]
# write.csv(df.diur.data,file="Diurnal_radar_data.csv")


# END: Extracting Radar data ---------------------------------------------------------



# Processing Radar data ---------------------------------------------------

####
##
#
df <- data.frame(Ordinal.date = rep(NA,31),
                 Year = NA,
                 Num.scans = NA,
                 Bin.lower = c(seq(2900,0,by=-100),"sum"),
                 Reflectivity.mean = NA,
                 Reflectivity.prop = NA,
                 Direction = NA)
df.z <- df[1,]
#
yr.files <- list.files("NEXRAD nocturnal scans/scans by year") # vector of archive names
for (i in 1:length(yr.files)){ # loop through year-specific scan data
  scns <- read.csv(paste("NEXRAD nocturnal scans/scans by year/",paste0("",yr.files[i],""),sep=""))
  scns$Date <- as.Date(paste(scns$Year.UTC, scns$Month.UTC, scns$Day.UTC, sep = "-")) 
  scns$Ordinal.date <- format(as.Date(scns$Date), "%j")
  scns$Scan.ID <- paste(scns$Ordinal.date,scns$Scan.UTC,sep="_")
  #
  days <- unique(scns$Ordinal.date) # unique days per year
  for (j in 1:length(days)[1]) { # loop through days per year
    df.comp <- df # day-specific results
    df.comp$Ordinal.date <- days[j]
    df.comp$Year <- scns$Year.UTC[1]
    sub.day <- scns[scns$Ordinal.date==days[j],] # day-specific data
    samples <- unique(sub.day$Scan.ID) # unique scans per day
    df.comp$Num.scans <- length(samples)
    #
    sums <- rep(NA,length(samples))
    for (k in 1:length(samples)) { # sum reflectivity per scan (entire 3km-tall column)
      sub.samples <- sub.day[sub.day$Scan.ID==samples[k],] # scan-specific data
      sums[k] <- sum(sub.samples$linear_eta)}
    df.comp$Reflectivity.mean[31] <- mean(sums)
    df.comp$Reflectivity.prop[31] <- 1
    #
    for (l in 1:dim(df.comp)[1]-1){
      df.comp$Reflectivity.mean[l] <- mean(sub.day[sub.day$bin_lower==df.comp$Bin.lower[l],]$linear_eta)
      df.comp$Reflectivity.prop[l] <- df.comp$Reflectivity.mean[l] / df.comp$Reflectivity.mean[31]
      df.comp$Direction[l] <- mean(sub.day[sub.day$bin_lower==df.comp$Bin.lower[l],]$direction)}
    #
    dirs <- rep(NA,30)
    for (m in 1:dim(df.comp)[1]-1) {
      dirs[m] <- df.comp$Reflectivity.prop[m] * df.comp$Direction[m]}
    df.comp$Direction[31] <- sum(dirs)
    #
    df.z <- rbind(df.z,df.comp)
  }
}
#
df.noct.data.all <- df.z[-1,]
#
# write.csv(df.noct.data.all, file="Nocturnal_radar_data_all.csv")

table(df.noct.data.all$Ordinal.date)

# END: Processing Radar data ---------------------------------------------------



### MODIFY METHODS
## END OF CIVIL TWILIGHT TO ONSET OF CIVIL TWILIGHT (SUN ANGLE < -6), NOT SUNSET TO SUNRISE (SUN ANGLE < 0(?))
## REFLECTIVITY, NOT REFLECTIVITY TRAFFIC RATE







