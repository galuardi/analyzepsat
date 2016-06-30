satellite.kml <-
function (track, folder = getwd(), local = F, 
    product = "TBAssta", interval = 5, 
    server = "http://coastwatch.pfeg.noaa.gov/coastwatch/CWBrowserWW360.jsp?get=gridData&dataSet=")
{
    require(date)
    fmtDate <- function(date) {
        x <- date.mdy(date)
        paste(x$year, formatC(x$month, digits = 1, flag = "0", 
            format = "d"), formatC(x$day, digits = 1, flag = "0", 
            format = "d"), sep = "-")
    }
    fmtDay <- function(day) {
        formatC(day, digits = 2, flag = "0", format = "d")
    }
    fl <- dir(folder)
    if (length(fl) != 0) {
        folder <- paste(folder, "images", sep = "/")
        dir.create(folder)
    }
    substring.location <- function (text, string, restrict) 
    {
    if (length(text) > 1) 
        stop("only works with a single character string")
    l.text <- nchar(text)
    l.string <- nchar(string)
    if (l.string > l.text) 
        return(list(first = 0, last = 0))
    if (l.string == l.text) 
        return(if (text == string) list(first = 1, last = l.text) else list(first = 0, 
            last = 0))
    is <- 1:(l.text - l.string + 1)
    ss <- substring(text, is, is + l.string - 1)
    k <- ss == string
    if (!any(k)) 
        return(list(first = 0, last = 0))
    k <- is[k]
    if (!missing(restrict)) 
        k <- k[k >= restrict[1] & k <= restrict[2]]
    if (length(k) == 0) 
        return(list(first = 0, last = 0))
    list(first = k, last = k + l.string - 1)
    }
    dat.period <- function(file)
    {
     beginopt <- which(file=="<!--BeginOptions-->")
     endopt <- which(file=="<!--EndOptions-->")
     list1 <- list()
     list2 <- list()
     for (i in 1:(endopt - beginopt - 1))
     {
     starti <- substring.location(file[beginopt+i], "centeredTime=\">")$last+1
     endi <- substring.location(file[beginopt+i], "</a>")$first-1
     list1[i] <- substr(file[beginopt+i], starti, endi)
     list2[i] <- gsub("day", "", list1[i])
     list2[i] <- as.numeric(gsub("1month", "30", list2[i]))
     }
     if (length(which(is.na(list2)))>0) {list1 <- list1[-which(is.na(list2))]}
     list2 <- na.omit(list2)
     return(list(txt=unlist(list1), num=unlist(list2)))
    }
    dat.times <- function(file)
    {
     beginopt <- which(file=="<!--BeginOptions-->")
     endopt <- which(file=="<!--EndOptions-->")
     list1 <- list()
     for (i in 1:(endopt - beginopt - 1))
     {
     starti <- substring.location(file[beginopt+i], "minLon=null\">")$last+1
     endi <- substring.location(file[beginopt+i], "</a>")$first-1
     list1[i] <- substr(file[beginopt+i], starti, endi)
     dvec <- as.numeric(unlist(strsplit(unlist(strsplit(unlist(list1[i]), "T"))[1],"-")))
     if (i ==1)
      {list2 <- mdy.date(dvec[2], dvec[3], dvec[1])
      }else {list2[i] <- mdy.date(dvec[2], dvec[3], dvec[1])}
     }
     return(list(txt=unlist(list1), dates=list2))
    }
    server <- paste(server, product, sep="")   
    ### Check with server the available intervals
    dlink <- paste(server, "&timePeriod=", sep="")
    download.file(dlink, "interval.html", mode = "wb")
    dfile <- readLines("interval.html")
    unlink("interval.html")
    dinfo <- dat.period(dfile)
    nfreq <- dinfo$num
    cat("\n--- Querying the sampling time interval --- \n\n")
    cat("Sampling period available from server: ", dinfo$txt, "\n")
    deltat <- length(which(nfreq <= interval))
    deltat <- ifelse(deltat==0, 1, deltat)
    nday  <- dinfo$txt[deltat]
    cat("Your specified interval is:", interval, " day(s)\n")
    cat("\n--- Auto-selecting the sampling period:", nday, " ---\n\n")
    ### Check with server the available times
    dlink <- paste(dlink, nday, "&centeredTime=", sep="")
    download.file(dlink, "times.html", mode = "wb")
    dfile <- readLines("times.html")
    unlink("times.html")
    dinfo <- dat.times(dfile)   
    ### Get request range from track
    if (is.data.frame(track)) 
        track <- list(track)
    minDate <- min(unlist(lapply(track, function(x) mdy.date(x[1, 
        2], x[1, 3], x[1, 1]))))
    maxDate <- max(unlist(lapply(track, function(x) mdy.date(x[nrow(x), 
        2], x[nrow(x), 3], x[nrow(x), 1]))))
    minLon <- min(unlist(lapply(track, function(x) min(x[, 4])))) - 
        5
    maxLon <- max(unlist(lapply(track, function(x) max(x[, 4])))) + 
        5
    minLat <- min(unlist(lapply(track, function(x) min(x[, 5])))) - 
        5
    maxLat <- max(unlist(lapply(track, function(x) max(x[, 5])))) + 
        5
    idxDmin <- length(which(dinfo$dates<= minDate))
    idxDmax <- length(which(dinfo$dates<= maxDate))+ 
               ifelse(((maxDate>=max(dinfo$dates))|
(maxDate<min(dinfo$dates))|
(minDate>max(dinfo$dates))),0,1)
    if (!(idxDmin==idxDmax)){ 
      if (minDate < min(dinfo$dates)) {
ifelse(idxDmax>0, idxDmin <- 1)
warning("Your track(s) start(s) before any
satellite imagery is available, which begins at", 
min(dinfo$dates))}
  if (maxDate > max(dinfo$dates)) {
warning("Your track(s) end(s) after the available
satellite imagery series , which finishes at", 
max(dinfo$dates))}
     }else warning("\n\n No satellite imagery is available for \"", product,
   "\" during your data range!!\n\n  ",
   "Please check CoastWatch website for listings at \n", 
   "       http://coastwatch.pfeg.noaa.gov/coastwatch/CWBrowserWW360.jsp", "\n")
    dsteps <- seq(idxDmin, idxDmax+1, by = nfreq[deltat])
    if (idxDmin == idxDmax) {dsteps <- 0}
    missed <- which((c(dinfo$dates[-1],dinfo$dates[length(dinfo$dates)]) 
     -dinfo$dates)>(nfreq[deltat]-1))
    missed <- missed[which(missed>idxDmin)]
    missed <- missed[which(missed<idxDmax)]
    all <- c(dsteps, missed)
    datesteps <- na.omit(dinfo$txt[sort(all)])
    ### Downloading imagery
    op <- "&timePeriod=NDAY&centeredTime=~DATE&maxLat=MAXLAT&minLon=MINLON&maxLon=MAXLON&minLat=MINLAT&filetype=GoogleEarth"
    for (d in datesteps) {
        opt <- sub("NDAY", nday, op)
        opt <- sub("DATE", d, opt)
        opt <- sub("MAXLAT", maxLat, opt)
        opt <- sub("MINLON", minLon, opt)
        opt <- sub("MAXLON", maxLon, opt)
        opt <- sub("MINLAT", minLat, opt)
        link <- paste(server, opt, sep = "")
requestdate <- unlist(strsplit(d, "T"))[1]
        filename <- paste(product, requestdate, ".kml", sep = "")
        dest <- paste(folder, filename, sep = "/")
        download.file(link, dest, mode = "wb")
        kmlfile <- readLines(dest)
unlink(dest)
        ### Add TimeSpan to kml
        idx1 <- which(kmlfile == "  <GroundOverlay>")+1
idx2 <- which(kmlfile == "</kml>")
cday<- as.Date(substr(kmlfile[6], 26, 35))
begind <- cday - trunc(nfreq[deltat]/2) 
endd <- (trunc(nfreq[deltat]/2) - ifelse(nfreq[deltat]%%2, 0, 1))*60*60*24
ctime1 <- substr(kmlfile[6], 37, 44)
ctime2 <- substr(kmlfile[6], 26, 44)
ctime2 <- format(strptime(ctime2, "%Y-%m-%d %H:%M:%S", 
  tz="GMT")+ endd + 60*60*24 -1, 
  usetz=F, format="%Y-%m-%dT%H:%M:%SZ")
addtag    <- "<TimeSpan>"
addtag[2] <- paste("<begin>", begind, "T", ctime1, "Z</begin>", sep="")
addtag[3] <- paste("<end>", ctime2, "</end>", sep="")
addtag[4] <- "</TimeSpan>"
idx3 <- which(kmlfile == "    <name>Logo</name>")+4
kmlfile[idx3] <- gsub("0.005", "0.97", kmlfile[idx3])
kmlfile[idx3+1] <- gsub("0.005", "0.97", kmlfile[idx3+1])
kmlfile[idx3] <- gsub(".04", "0.1", kmlfile[idx3])
kmlfile[idx3+1] <- gsub(".04", "0.1", kmlfile[idx3+1])
### Download image png files to a local folder
if (local==T) 
{
iname <- paste("img", substr(kmlfile[idx1], 
   substring.location(kmlfile[idx1], "<name>")$last+1, 
   substring.location(kmlfile[idx1], "_x")$first-1), ".png", sep="")
idest <- paste(folder, iname, sep = "/")
ilink <- gsub("GoogleEarth", "transparent.png", link)
download.file(ilink, idest, mode = "wb")
kmlfile[idx1+2] <- paste("<href>", iname, "</href>", sep="")
} else{
kmlfile[idx1+2] <- gsub("Time=", "Time=~", kmlfile[idx1+2])
}
kmlfile <- c(kmlfile[1:idx1], addtag, kmlfile[(idx1+1):idx2]) 
dest <- paste(folder, "/", product, as.character(cday), 
  ".kml", sep = "")
cat("to ", dest,"\n\n")
writeLines(kmlfile, dest)
    }
    cat(paste(rep(".", options()$width), collapse = ""), "\n\n")
    cat("Downloaded", length(dir(folder, pattern=product)), "files to:\n\n  ", 
        folder, "\n\n")
    return(list(folder=folder, nday=nday, nodataflag=ifelse((idxDmax==idxDmin),1,0)))
}

