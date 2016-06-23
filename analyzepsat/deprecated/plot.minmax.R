get.reynolds <- function(track, folder = tempdir(), removeland = TRUE)
{
  # Metadata: http://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.html
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
  testdir <- file.info(folder)$isdir
  if (is.na(testdir)) {
        dir.create(folder)
    }
  else {
        if (!testdir) 
            stop("The folder name supplied is in fact a filename")
  }
  unlink(paste(folder, "/*", sep=""), F)
  sstfolder <- paste(folder, "sst_files", sep="/")
  testdir <- file.info(sstfolder)$isdir
  if (is.na(testdir)) {
        dir.create(sstfolder)
  }
  else {
        if (!testdir) 
            stop("The folder name supplied is in fact a filename")
  }
  unlink(paste(sstfolder, "/*", sep=""), F)
  if (is.data.frame(track)) track <- list(track)
  minDate <- min(unlist(lapply(track, function(x) mdy.date(x[1, 
        2], x[1, 1], x[1, 3]))))
  maxDate <- max(unlist(lapply(track, function(x) mdy.date(x[nrow(x), 
        2], x[nrow(x), 1], x[nrow(x), 3]))))
  minLon