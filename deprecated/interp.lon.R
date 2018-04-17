interp.lon <-
function(xtrack, ukf = F, span = .25, fill.sst = F, span.sst = .25){
	if(ukf) xtrack[,4] = xtrack[,4]+360
		span = span
		len = length(xtrack[, 1])
		vec = 1:len
		fidx = is.na(xtrack[, 5])
		xtrack[fidx, 4] = NA
		if (any(fidx == T)) {
			ltmp = loess(xtrack[, 4] ~ vec, span = span)
			xtrack[fidx, 4] = predict(ltmp, newdata = vec[fidx])
			resids = xtrack[,4]-(predict(ltmp, newdata = 1:len))
			bound = 2*sd(resids, na.rm = T)
			ridx = resids < bound*-1 |  resids > bound
			xtrack[ridx,4] = NA 
					if (any(ridx == T)) {
						 ltmp = loess(xtrack[, 4] ~ vec, span = span)
						 xtrack[ridx, 4] = predict(ltmp, newdata = vec[ridx])
					 }
		}
		
	if(fill.sst) xtrack[,6] = fill.vals(xtrack[,6], span = span)
	xtrack
}

