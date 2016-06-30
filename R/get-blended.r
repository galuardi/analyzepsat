 #=========================================================================================#
 # Date: 10-6-10
 # Author: Ben Galuardi and Chi (Tim) Lam
 # get.blended.ukf is based on Chi Lam's code get-reynolds.r
 # This function draws belnded SST from the ERDDAP NOAA server. It already has land removed. This extracts 8-day imagery but may be modified to draw out 5 day imagery. 
 #=========================================================================================#

 get.blended.ukf <- function(track, folder = tempdir(), daycomp = 8, save = F)
{
  # Metadata: http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdBAssta8day.html
  require(date)
  require(ncdf)
  fmtDate <- function(date) {
        x <- date.mdy(date)
        paste(x$year, formatC(x$month, digits = 1, flag = "0", 
            format = "d"), formatC(x$day, digits = 1, flag = "0", 
            format = "d"), sep = "-")
    }
  fmtDay <- function(day) {
        formatC(day, digits = 2, flag = "0", format = "d")
    }
	
sstfolder <- paste(folder, "sst_files_BL", sep="/") 
testdir <- file.info(sstfolder)$isdir
  if (is.na(testdir)) {
        dir.create(sstfolder)
  }else {
        if (!testdir) 
            stop("The folder name supplied is in fact a filename")
  }
  unlink(paste(sstfolder, "/*", sep=""), F)
	
  if (is.data.frame(track)) track <- list(track)
  minDate <- min(unlist(lapply(track, function(x) mdy.date(x[1, 
        2], x[1, 1], x[1, 3]))))
  maxDate <- max(unlist(lapply(track, function(x) mdy.date(x[nrow(x), 
        2], x[nrow(x), 1], x[nrow(x), 3]))))

date1 = as.Date(as.date(minDate))
date2 = as.Date(as.date(maxDate))

  minLon <- min(unlist(lapply(track, function(x) min(x[, 4],na.rm=T)))) -  2
  maxLon <- max(unlist(lapply(track, function(x) max(x[, 4],na.rm=T)))) + 2
  minLat <- min(unlist(lapply(track, function(x) min(x[, 5],na.rm=T)))) - 4
  maxLat <- max(unlist(lapply(track, function(x) max(x[, 5],na.rm=T)))) + 4
  latlow <- ifelse(minLat < -89.5, -89.5, trunc(minLat)-0.5)
  lathigh <- ifelse(maxLat > 89.5, 89.5, trunc(maxLat)+0.5)
  lonlow <- ifelse(minLon < 0, trunc(minLon)+360-0.5, trunc(minLon)-0.5)
  lonhigh <- ifelse(maxLon < 0, trunc(maxLon)+360+0.5, trunc(maxLon)+0.5)
  
  cat(paste("Contacting server for SST images within this date range: ", date1, " - ", date2, "\n\n" , sep=""))

daycomp = daycomp

link = "http://coastwatch.pfel.noaa.gov/erddap/griddap/erdBAsstaDAYCOMPday.nc?sst[(YEARLOW-MONTHLOW-DAYLOWT00:00:00Z):DAYCOMP:(YEARHIGH-MONTHHIGH-DAYHIGHT00:00:00Z)][(0.0)][(LATLOW):(LATHIGH)][(LONLOW):(LONHIGH)]%26.draw=surface%26.vars=longitude|latitude|sst%26.colorBar=Rainbow|C|Linear|0|32|"
  opt = link
  opt <- sub("DAYCOMP", daycomp, opt)
  opt <- sub("DAYCOMP", daycomp, opt)
  opt <- sub("LATLOW", latlow, opt)
  opt <- sub("LATHIGH", lathigh, opt)
  opt <- sub("LONLOW", lonlow, opt)
  opt <- sub("LONHIGH", lonhigh, opt)
  opt <- sub("YEARLOW", format(as.Date(date1), "%Y"), opt)
  opt <- sub("MONTHLOW", format(as.Date(date1), "%m"), opt)
  opt <- sub("DAYLOW", as.numeric(format(as.Date(date1), "%d")), opt)
  opt <- sub("YEARHIGH", format(as.Date(date2), "%Y"), opt)
  opt <- sub("MONTHHIGH", format(as.Date(date2), "%m"), opt)
  opt <- sub("DAYHIGH", as.numeric(format(as.Date(date2), "%d")), opt)
  fname = paste(folder, "request.nc", sep = "/")
  download.file(opt, fname, mode="wb")
    # unlink(fname)

  nc <- open.ncdf(fname)
  lon <- get.var.ncdf(nc, varid="longitude")
  lat <- get.var.ncdf(nc, varid="latitude")
  dates = as.POSIXct(origin = "1970-01-01",get.var.ncdf(nc, varid="time"))
  every.day <- 8
  vv      <- nc$var[[1]]
  varsize <- vv$varsize
  ndims   <- vv$ndims
  nt      <- varsize[ndims]  # Remember timelike dim is always the LAST dimension!
  for (i in 1:nt){
        # Initialize start and count to read one timestep of the variable.
        start <- rep(1,ndims)   # begin with start=(1,1,1,...,1)
        start[ndims] <- i       # change to start=(1,1,1,...,i) to read timestep i
        count <- varsize        # begin w/count=(nx,ny,nz,...,nt), reads entire var
        count[ndims] <- 1       # change to count=(nx,ny,nz,...,1) to read 1 tstep
        sst <- round(t(get.var.ncdf(nc, vv, start=start, count=count )),2)
        # Prepare an individual xyz file
        xyz <- rbind(rep(NA, 3))
        d <- mdy.date(as.numeric(format(dates[i], "%m")),
                      as.numeric(format(dates[i], "%d")),
                      as.numeric(format(dates[i], "%Y")))
        # !! The date constructs for the filename are different - the first date is the image date
        # !! Unlike old code that centers the image date in between d1 and d2 (+/- days to position it)
	y1 <- date.mdy(d)$year
        d1 <- d - mdy.date(month = 1, day = 1, year = y1) + 1
        y2 <- date.mdy(d + every.day - 1)$year
        d2 <- (d + every.day -1) - mdy.date(month = 1, 
               day = 1, year = y2) + 1
        filename <- paste("BL", y1, fmtDay(d1), 
                    "_", y2, fmtDay(d2), "_", "sst", ".xyz", sep = "")
        dest <- paste(sstfolder, filename, sep = "/")
        for (j in 1:length(lon)) {
             xyz <- rbind(xyz, cbind(lat, lon[j], sst[,j]))
        }
        xyz <- na.omit(xyz)
        #if (removeland) xyz <- xyz[which(xyz[,4]==1), -4] 
        write.table(xyz, file = dest, 
                    quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
  close.ncdf(nc)
   cat("And repackaged to", length(dir(sstfolder)), "xyz files in:\n\n  ", sstfolder, "\n\n")
  cat(paste(rep("=", options()$width), collapse = ""), "\n\n")
  .sstFileVector <<- paste(sstfolder, dir(sstfolder), sep = "/")
  if(save==F) unlink(fname)
  return(sstfolder)
  }
  
  
 get.blended.ukf.trackit <- function(track, lonlow, lonhigh, latlow, lathigh, folder = tempdir(), daycomp = 8, save = F)
{
  # Metadata: http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdBAssta8day.html
  require(date)
  require(ncdf)
  fmtDate <- function(date) {
        x <- date.mdy(date)
        paste(x$year, formatC(x$month, digits = 1, flag = "0", 
            format = "d"), formatC(x$day, digits = 1, flag = "0", 
            format = "d"), sep = "-")
    }
  fmtDay <- function(day) {
        formatC(day, digits = 2, flag = "0", format = "d")
    }
	
sstfolder <- paste(folder, "sst_files_BL", sep="/") 
testdir <- file.info(sstfolder)$isdir
  if (is.na(testdir)) {
        dir.create(sstfolder)
  }else {
        if (!testdir) 
            stop("The folder name supplied is in fact a filename")
  }
  unlink(paste(sstfolder, "/*", sep=""), F)
	
  if (is.data.frame(track)) track <- list(track)
  minDate <- min(unlist(lapply(track, function(x) mdy.date(x[1, 
        2], x[1, 3], x[1, 1]))))
  maxDate <- max(unlist(lapply(track, function(x) mdy.date(x[nrow(x), 
        2], x[nrow(x), 3], x[nrow(x), 1]))))

date1 = as.Date(as.date(minDate))
date2 = as.Date(as.date(maxDate))

  # minLon <- min(unlist(lapply(track, function(x) min(x[, 4],na.rm=T)))) -  2
  # maxLon <- max(unlist(lapply(track, function(x) max(x[, 4],na.rm=T)))) + 2
  # minLat <- min(unlist(lapply(track, function(x) min(x[, 5],na.rm=T)))) - 4
  # maxLat <- max(unlist(lapply(track, function(x) max(x[, 5],na.rm=T)))) + 4
  latlow <- ifelse(latlow < -89.5, -89.5, trunc(latlow)-0.5) -4
  lathigh <- ifelse(lathigh > 89.5, 89.5, trunc(lathigh)+0.5) +4
  lonlow <- ifelse(lonlow < 0, trunc(lonlow)+360-0.5, trunc(lonlow)-0.5) -2
  lonhigh <- ifelse(lonhigh < 0, trunc(lonhigh)+360+0.5, trunc(lonhigh)+0.5)+2
  
  cat(paste("Contacting server for SST images within this date range: ", date1, " - ", date2, "\n\n" , sep=""))

daycomp = daycomp

link = "http://coastwatch.pfel.noaa.gov/erddap/griddap/erdBAsstaDAYCOMPday.nc?sst[(YEARLOW-MONTHLOW-DAYLOWT00:00:00Z):DAYCOMP:(YEARHIGH-MONTHHIGH-DAYHIGHT00:00:00Z)][(0.0)][(LATLOW):(LATHIGH)][(LONLOW):(LONHIGH)]%26.draw=surface%26.vars=longitude|latitude|sst%26.colorBar=Rainbow|C|Linear|0|32|"
  opt = link
  opt <- sub("DAYCOMP", daycomp, opt)
  opt <- sub("DAYCOMP", daycomp, opt)
  opt <- sub("LATLOW", latlow, opt)
  opt <- sub("LATHIGH", lathigh, opt)
  opt <- sub("LONLOW", lonlow, opt)
  opt <- sub("LONHIGH", lonhigh, opt)
  opt <- sub("YEARLOW", format(as.Date(date1), "%Y"), opt)
  opt <- sub("MONTHLOW", format(as.Date(date1), "%m"), opt)
  opt <- sub("DAYLOW", as.numeric(format(as.Date(date1), "%d")), opt)
  opt <- sub("YEARHIGH", format(as.Date(date2), "%Y"), opt)
  opt <- sub("MONTHHIGH", format(as.Date(date2), "%m"), opt)
  opt <- sub("DAYHIGH", as.numeric(format(as.Date(date2), "%d")), opt)
  fname = paste(folder, "request.nc", sep = "/")
  download.file(opt, fname, mode="wb")
    # unlink(fname)

  nc <- open.ncdf(fname)
  lon <- get.var.ncdf(nc, varid="longitude")
  lat <- get.var.ncdf(nc, varid="latitude")
  dates = as.POSIXct(origin = "1970-01-01",get.var.ncdf(nc, varid="time"))
  every.day <- 8
  vv      <- nc$var[[1]]
  varsize <- vv$varsize
  ndims   <- vv$ndims
  nt      <- varsize[ndims]  # Remember timelike dim is always the LAST dimension!
  for (i in 1:nt){
        # Initialize start and count to read one timestep of the variable.
        start <- rep(1,ndims)   # begin with start=(1,1,1,...,1)
        start[ndims] <- i       # change to start=(1,1,1,...,i) to read timestep i
        count <- varsize        # begin w/count=(nx,ny,nz,...,nt), reads entire var
        count[ndims] <- 1       # change to count=(nx,ny,nz,...,1) to read 1 tstep
        sst <- round(t(get.var.ncdf(nc, vv, start=start, count=count )),2)
        # Prepare an individual xyz file
        xyz <- rbind(rep(NA, 3))
        d <- mdy.date(as.numeric(format(dates[i], "%m")),
                      as.numeric(format(dates[i], "%d")),
                      as.numeric(format(dates[i], "%Y")))
        # !! The date constructs for the filename are different - the first date is the image date
        # !! Unlike old code that centers the image date in between d1 and d2 (+/- days to position it)
	y1 <- date.mdy(d)$year
        d1 <- d - mdy.date(month = 1, day = 1, year = y1) + 1
        y2 <- date.mdy(d + every.day - 1)$year
        d2 <- (d + every.day -1) - mdy.date(month = 1, 
               day = 1, year = y2) + 1
        filename <- paste("BL", y1, fmtDay(d1), 
                    "_", y2, fmtDay(d2), "_", "sst", ".xyz", sep = "")
        dest <- paste(sstfolder, filename, sep = "/")
        for (j in 1:length(lon)) {
             xyz <- rbind(xyz, cbind(lat, lon[j], sst[,j]))
        }
        xyz <- na.omit(xyz)
        #if (removeland) xyz <- xyz[which(xyz[,4]==1), -4] 
        write.table(xyz, file = dest, 
                    quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
  close.ncdf(nc)
   cat("And repackaged to", length(dir(sstfolder)), "xyz files in:\n\n  ", sstfolder, "\n\n")
  cat(paste(rep("=", options()$width), collapse = ""), "\n\n")
  .sstFileVector <<- paste(sstfolder, dir(sstfolder), sep = "/")
  if(save==F) unlink(fname)
  return(sstfolder)
  }
  
  # get.blended.ukf.trackit (track, 282,298,0,70, folder='C:\\Ben\\UNH\\PROJECTS\\PSAT\\DFO\\2010\\96077\\BSST')