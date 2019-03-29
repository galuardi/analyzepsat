###############################################################################################
## OLD version of merge.tz function
###############################################################################################

# merge.tz <- function (btrack, psat, tagID = NULL)
# {
    # require(maptools)
    # allTZ = NULL
    # if (class(btrack) != "list")
        # btrack = list(btrack)
    # if (names(psat)[1] == "tagID") {
        # psat = list(psat)
        # names(psat) = tagID
    # }
    # for (j in 1:length(psat)) {
        # print(paste("Now merging tag# ", names(psat)[j], sep = ""))
        # day0 = psat[[j]]$day0
        # dayT = psat[[j]]$dayT
        # dim1 = dim(psat[[j]]$T)[1]
        # dim2 = dim(psat[[j]]$T)[2]
        # tdates = as.POSIXct(round(seq(psat[[j]]$day0b, psat[[j]]$dayTb,
            # length = dim1 * dim2), "mins"), tz = "GMT")

		# tdays = date = as.POSIXct(trunc(tdates, 'day'))
# match days of the track with days of the temp and depth..
		# attach(btrack[[j]])
		# btdays = date=as.POSIXct(trunc(ISOdate(Year, Month, Day), 'day'))
		# detach(btrack[[j]])

		# btidx = tdays<=btdays[length(btdays)]
		# tdays = tdays[btidx]
		# tdates = tdates[btidx]

		# dateidx =  match(tdays, btdays)
        # depth = as.vector(t(psat[[j]]$Z))[btidx]
        # Ext_T = as.vector(t(psat[[j]]$T))[btidx]
        # tzdat = data.frame(Date = tdates, depth, Ext_T)
        # mlat = btrack[[j]][, 9]
        # mlon = btrack[[j]][, 8]
        # mpos = matrix(c(mlon, mlat), nrow = length(mlon))
# use all positions, not a mean position!
		# allpos = matrix(0, nrow=length(tdays), ncol = 2)
		# allpos[,1:2] = mpos[dateidx,1:2]

        # sr = crepuscule(allpos, tdates, solarDep = 6, direction = "dawn",
            # POSIXct.out = T)$time

        # ss = crepuscule(allpos, tdates, solarDep = 6, direction = "dusk",
            # POSIXct.out = T)$time

        # dn = numeric(nrow(tzdat))
		# idx = tzdat[, 1] > sr & tzdat[, 1] < ss
		# dn[idx==T] = "Day"
		# dn[idx==F] = "Night"

        # diel = data.frame(sr, ss, tzdat$Date, dn)
        # subdat = cbind(tzdat, diel, rep(names(psat)[j], nrow(tzdat)))
        # allTZ = rbind(allTZ, subdat)
    # }
    # allTZ$dn = as.character(allTZ$dn)
    # allTZ[, 8] = as.character(allTZ[, 8])
    # tidx = which(allTZ$depth > 1)
    # allTZ$depth[tidx] = allTZ$depth[tidx] * -1
    # names(allTZ)[8] = "tagID"
    # allTZ
# }

###############################################################################################
## new version of merge.tz function
###############################################################################################

#' merge.tz
#'
#' @param btrack
#' @param psat
#' @param tagID
#'
#' @return
#' @export
#'
#' @examples
merge.tz = function (btrack, psat, tagID = NULL){
require(maptools)
    allTZ = NULL
    if (class(btrack) != "list")
        btrack = list(btrack)
    if (names(psat)[1] == "tagID") {
        psat = list(psat)
        names(psat) = tagID
    }

   for (j in 1:length(psat)) {
        print(paste("Now merging tag# ", names(psat)[j], sep = ""))
        day0 = psat[[j]]$day0
        dayT = psat[[j]]$dayT
        dim1 = dim(psat[[j]]$T)[1]
        dim2 = dim(psat[[j]]$T)[2]
        tdates = as.POSIXct(round(seq(psat[[j]]$day0b, psat[[j]]$dayTb,
            length = dim1 * dim2), "mins"), tz = "GMT")
        tdays  = as.POSIXct(trunc(tdates, "day"))
        attach(btrack[[j]])
        btdays = as.POSIXct(trunc(ISOdate(Year, Month,
            Day), "day"))
        detach(btrack[[j]])
        btidx = tdays <= btdays[length(btdays)]
        tdays = tdays[btidx]
        tdates = tdates[btidx]

        dateidx = match(tdays, btdays)
		nidx = !is.na(dateidx)			# new line 4-1-2014

        depth = as.vector(t(psat[[j]]$Z))[btidx]
        Ext_T = as.vector(t(psat[[j]]$T))[btidx]
        tzdat = data.frame(Date = tdates, depth, Ext_T)
        mlat = btrack[[j]][, 9]
        mlon = btrack[[j]][, 8]
        mpos = matrix(c(mlon, mlat), nrow = length(mlon))
        allpos = matrix(0, nrow = length(tdays), ncol = 2)

        allpos[nidx, 1:2] = mpos[dateidx[nidx], 1:2]  # new line 4-1-2014. ignores NA's...

        sr = crepuscule(allpos, tdates, solarDep = 6, direction = "dawn",
            POSIXct.out = T)$time
        ss = crepuscule(allpos, tdates, solarDep = 6, direction = "dusk",
            POSIXct.out = T)$time
        dn = numeric(nrow(tzdat))
        idx = tzdat[, 1] > sr & tzdat[, 1] < ss
        dn[idx == T] = "Day"
        dn[idx == F] = "Night"
        diel = data.frame(sr, ss, tzdat$Date, dn)
        subdat = cbind(tzdat, diel, rep(names(psat)[j], nrow(tzdat)))
        allTZ = rbind(allTZ, subdat)
   }

    allTZ$dn = as.character(allTZ$dn)
    allTZ[, 8] = as.character(allTZ[, 8])
    tidx = which(allTZ$depth > 1)
    allTZ$depth[tidx] = allTZ$depth[tidx] * -1
    names(allTZ)[8] = "tagID"
    allTZ
}


merge.tz.wctb <- function (btrack, tagdata, tagID = NULL)
{
    require(maptools)
    allTZ = NULL
    # if (class(btrack) != "list") btrack = list(tagID = btrack)
        tdates = tagdata$DateTime
		tdays =  as.POSIXct(trunc(tdates, 'day', tz='GMT'))
# match days of the track with days of the temp and depth..
		attach(btrack)
		btdays = date=as.POSIXct(trunc(ISOdate(Year, Month, Day), 'day'))
		detach(btrack)
		depth = tagdata$depth
        Ext_T = tagdata$Ext_T
        tzdat = data.frame(Date = tdates, depth, Ext_T)


		dateidx =  match(tdays, btdays)
        mlat = btrack[, 9]
        mlon = btrack[, 8]
        mpos = matrix(c(mlon, mlat), nrow = length(mlon))
# use all positions, not a mean position!
		allpos = matrix(0, nrow=length(tdays), ncol = 2)
		allpos[,1:2] = mpos[dateidx,1:2]

        sr = crepuscule(allpos, tdates, solarDep = 6, direction = "dawn",
            POSIXct.out = T)$time

        ss = crepuscule(allpos, tdates, solarDep = 6, direction = "dusk",
            POSIXct.out = T)$time

        dn = numeric(nrow(tzdat))
		idx = tzdat[, 1] > sr & tzdat[, 1] < ss
		dn[idx==T] = "Day"
		dn[idx==F] = "Night"

        diel = data.frame(sr, ss, tzdat$Date, dn)
        subdat = cbind(tzdat, diel, tagID = rep(tagID, nrow(tzdat)))
        allTZ = rbind(allTZ, subdat)
    allTZ$dn = as.character(allTZ$dn)
    allTZ[, 8] = as.character(allTZ[, 8])
    tidx = which(allTZ$depth > 1)
    allTZ$depth[tidx] = allTZ$depth[tidx] * -1
    names(allTZ)[8] = "tagID"
    allTZ
}
