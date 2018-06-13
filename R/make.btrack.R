#' make.btrack
#'
#' @param fmat 
#' @param bathy 
#' @param save.samp 
#' @param mintype 
#' @param ci 
#' @param npoints 
#' @param fulldist 
#'
#' @return
#' @export
#'
#' @examples
make.btrack = function (fmat, bathy, save.samp = F, mintype = 2, ci = 0.95, 
    npoints = 300, fulldist = T) 
{
    len = length(fmat[, 1])
    ntrack = as.data.frame(matrix(0, len, 6))
    ntrack[1, ] = c(0, 0, 0, 0, fmat[1, 8:9])
    ntrack[len, ] = c(0, 0, 0, 0, fmat[len, 8:9])
    sptmp = NULL
    for (i in 2:(length(fmat[, 1]) - 1)) {
        print(paste("Bathymetric point ", i, sep = ""))
        point = fmat[i, ]
        samp = .get.samp(point[4:9], npoints, ci = ci)
        samp.bath = sapply(1:length(samp[, 1]), function(j) .get.bath(samp[j, 
            1], samp[j, 2], bathy))
        sidx = samp.bath <= as.numeric(point[10])
        samp = samp[sidx, ]
        if (length(samp[sidx]) < 3) {
            samp = sptmp[[i - 1]]
			samp[,1] = jitter(samp[,1])
			samp[,2] = jitter(samp[,2])  # 5% jitter if we use the same sampling as previous time step
        }
        if (mintype == 2) 
            ntrack[i, 5:6] = .get.min2(ntrack[i - 1, 5], ntrack[i - 
                1, 6], .denselect(samp)[1], .denselect(samp)[2], 
                samp)
        if (mintype == 3) 
            ntrack[i, 5:6] = .get.min3(ntrack[i + 1, 5], ntrack[i + 
                1, 6], ntrack[i - 1, 5], ntrack[i - 1, 6], .denselect(samp)[1], 
                .denselect(samp)[2], samp)
        if (mintype == 4) 
            ntrack[i, 5:6] = .get.min3(ntrack[i + 1, 5], ntrack[i + 
                1, 6], .denselect(samp)[1], .denselect(samp)[2], 
                samp)
        sptmp[[i]] = samp
        b.init = .get.bath(as.numeric(point[8]), as.numeric(point[9]), 
            bathy)
        print(c(b.init - as.numeric(point[10])))
        if (b.init <= as.numeric(point[10]) & fulldist == F) {
            ntrack[i, ] = fmat[i, 4:9]
        }
        else {
            tcov = sqrt(cov(samp))
            tcov[is.nan(tcov)] = 0
            ntrack[i, 1:4] = as.vector(tcov)
        }
    }
    btrack = cbind(fmat[, 1:3], ntrack, fmat[, 10:11])
    names(btrack) = c("Year","Month", "Day",  "V11", "V12", "V21", 
        "V22", "Lon_E", "Lat_N", "maxz", "maxt")
	attr(btrack,'Header') = "#Bathymetric corrected track"
    if (save.samp) {
        list(btrack, sptmp)
    }
    else {
        btrack
    }
}

# print.btrack<-function(x,...){ 
  # "%+%"<-function(s1, s2)paste(s1, s2, sep="")
  # out<-"\n\n#Bathymetric corrected kf/ukf track\n"

     # head(x)
# }

# now we use the MASS library mvrnorm function 
.get.samp<-
function (vec, npoints, ci = 0.95) 
{
    vec = as.numeric(vec)
    Sigma <- matrix(vec[1:4], 2, 2) * ci
    mu <- c(vec[5:6])
    if (sum(Sigma) > 0) {
        ndata <- mvrnorm(npoints, mu, Sigma)
        return(ndata)
    }
}

# .get.samp <-
# function(vec,npoints,ci=.95){
	# require(QRMlib)
	# vec=as.numeric(vec)
	# Sigma <- matrix(vec[1:4],2,2)*ci; 
	# mu <- c(vec[5:6]); 
	# if(sum(Sigma)>0){ 
	# ndata <- .rmnorm2(npoints,Sigma,mu);
	# return(ndata)
	# }
# }

.get.min <-
function(lon,lat,samp){
  idx<-which.min((lon-samp[,1])^2+(lat-samp[,2])^2)
  samp[idx,]
}

.get.min2 <-
function(lon1,lat1,lon2,lat2,samp){
  idx<-which.min((lon1-samp[,1])^2+(lat1-samp[,2])^2+(lon2-samp[,1])^2+(lat2-samp[,2])^2)
  c(samp[idx,1],samp[idx,2])
}

.get.min3 <-
function(lon1,lat1,lon2,lat2,lon3,lat3,samp){
  idx<-which.min((lon1-samp[,1])^2+(lat1-samp[,2])^2+(lon2-samp[,1])^2+(lat2-samp[,2])^2+(lon3-samp[,1])^2+(lat3-samp[,2])^2)
  c(samp[idx,1],samp[idx,2])
}

.rmnorm2 <-
function (n, Sigma = equicorr(d, rho), mu = rep(0, d), d = 2, 
    rho = 0.7) {
    d <- dim(Sigma)[1]
    A <- t(chol(Sigma,pivot=T))
    X <- matrix(rnorm(n * d), nrow = n, ncol = d)
    mu.matrix <- matrix(mu, nrow = n, ncol = d, byrow = TRUE)
    return(t(A %*% t(X)) + mu.matrix)
}

.denselect <-
function(samp){
	samp=samp[!is.na(samp[,1]),]
	samp=samp[!is.na(samp[,2]),]
	dd1=density(samp[,1])
	idx1=dd1$y==max(dd1$y)
	xout=dd1$x[idx1]
	dd2=density(samp[,2])
	idx2=dd2$y==max(dd2$y)
	yout=dd2$x[idx2]
	cbind(xout,yout)
}

.get.bath <-
function(lon,lat,BATH){
  X=as.vector(BATH$lon)
  Y=as.vector(BATH$lat)
  xidx=which.min((lon-X)^2)
  yidx=which.min((lat-Y)^2)
  BATH$data[yidx,xidx]
}

.makeCI=
	function (x, level = 0.95, npoints = 100, col = rgb(.7,.7,.7,alpha=.9), border = 1,
	    density = 20, lwd = 0.1 * par("lwd"), saveobj=F,...)
	{
	    t.quan <- sqrt(qchisq(level, 2))
	    centre <- x[5:6]
	    x <- matrix(x[1:4], 2, 2)
	    r <- x[1, 2]
	    scale <- sqrt(diag(x))
	    if (scale[1] > 0) {
		r <- r/scale[1]
	    }
	    if (scale[2] > 0) {
		r <- r/scale[2]
	    }
	    r <- min(max(r, -1), 1)
	    d <- acos(r)
	    a <- seq(0, 2 * pi, len = npoints)
	    polymat=(matrix(c(t.quan * scale[1] * cos(a + d/2) + centre[1],
                t.quan * scale[2] * cos(a - d/2) + centre[2]), npoints,2))
   if(saveobj==T){return(polymat)} 
   else {polygon(polymat, col = col, border = border, density = NA, lwd = lwd,...)}	    
      #polygon(polymat, col = col, border = border, density = NA, lwd = lwd,...)	    
#	    polygon(matrix(c(t.quan * scale[1] * cos(a + d/2) + centre[1],
#		t.quan * scale[2] * cos(a - d/2) + centre[2]), npoints,
#		2), col = col, border = border, density = NA, lwd = lwd,
#		...)
	}

.btrack.bs <- function (fmat, bathy, save.samp = F, mintype = 2, ci = 0.95, 
    npoints = 300, fulldist = T) 
{
    len = length(fmat[, 1])
    ntrack = as.data.frame(matrix(0, len, 6))
    ntrack[1, ] = c(0, 0, 0, 0, fmat[1, 8:9])
    ntrack[len, ] = c(0, 0, 0, 0, fmat[len, 8:9])
    sptmp = NULL
    for (i in rev(2:(length(fmat[, 1]) - 1))) {
        print(paste("Bathymetric point ", i, sep = ""))
        point = fmat[i, ]
        samp = .get.samp(point[4:9], npoints, ci = ci)
        samp.bath = sapply(1:length(samp[, 1]), function(j) .get.bath(samp[j, 
            1], samp[j, 2], bathy))
        sidx = samp.bath <= as.numeric(point[10])
        samp = samp[sidx, ]
        if (length(samp[sidx]) < 3) {
            samp = sptmp[[i + 1]]
        }
        if (mintype == 2) 
            ntrack[i, 5:6] = .get.min2(ntrack[i - 1, 5], ntrack[i - 
                1, 6], .denselect(samp)[1], .denselect(samp)[2], 
                samp)
        if (mintype == 3) 
            ntrack[i, 5:6] = .get.min3(ntrack[i + 1, 5], ntrack[i + 
                1, 6], ntrack[i - 1, 5], ntrack[i - 1, 6], .denselect(samp)[1], 
                .denselect(samp)[2], samp)
        if (mintype == 4) 
            ntrack[i, 5:6] = .get.min3(ntrack[i + 1, 5], ntrack[i + 
                1, 6], .denselect(samp)[1], .denselect(samp)[2], 
                samp)
        sptmp[[i]] = samp
        b.init = .get.bath(as.numeric(point[8]), as.numeric(point[9]), 
            bathy)
        print(c(b.init - as.numeric(point[10])))
        if (b.init <= as.numeric(point[10]) & fulldist == F) {
            ntrack[i, ] = fmat[i, 4:9]
        }
        else {
            tcov = sqrt(cov(samp))
            tcov[is.nan(tcov)] = 0
            ntrack[i, 1:4] = as.vector(tcov)
        }
    }
    btrack = cbind(fmat[, 1:3], ntrack, fmat[, 10:11])
    names(btrack) = c("Day", "Month", "Year", "V11", "V12", "V21", 
        "V22", "Lon_E", "Lat_N", "maxz", "maxt")
    if (save.samp) {
        return(btrack, sptmp)
    }
    else {
        return(btrack)
    }
}