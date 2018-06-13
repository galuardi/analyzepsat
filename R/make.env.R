#' make.env
#'
#' @param allhv 
#' @param fixz 
#' @param plot 
#' @param mbz 
#' @param mbt 
#' @param mint 
#' @param minz 
#' @param log 
#' @param mmar 
#' @param mcex 
#' @param col 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
make.env <- function (allhv, fixz = F, plot = F, mbz = c(-250), mbt = c(25), mint = 0, minz = 0, 
    log = T, mmar = c(4, 6, 2, 6), mcex = 1.2, col = jet.colors(100), 
    ...) 
{
    if (fixz) {
        allhv$hour = as.numeric(format(allhv$Date, "%H"))
        utags = unique(allhv$tagID)
        alltemp = NULL
        sumidx = sumgidx = 0
        for (i in utags) {
            tagidx = allhv$tagID == i
            temp = allhv[tagidx, ]
            Ztmp = temp$depth
            zdiff = c(0, diff(Ztmp))
            gidx = temp$hour == 0 | temp$hour == 6 | temp$hour == 
                12 | temp$hour == 18
            delta = c(0, 0, 0, diff(temp$depth, 4))
            bidx = which(abs(delta[gidx == F]) > 85)
            sumidx = sumidx + length(bidx)
            sumgidx = sumgidx + sum(gidx == T) + length(which(abs(delta[gidx == 
                F]) <= 85))
            temp$depth[bidx] = NaN
            alltemp = rbind(alltemp, temp)
        }
        allhv = alltemp
    }
    dbin = allhv$depth
    dbin[dbin < (mbz)] = mbz - 1
    brks = (seq(mbz - 5, 0, 5))
    dbin = brks[findInterval(round(dbin), brks)]
    allhv$dbin = dbin
    rm(dbin)
    par(mar = mmar)
    env = table(as.vector(round(allhv$Ext_T)), round(as.vector(allhv$dbin)))
    if (log) {
        env = log(env)
    }
    env[!is.finite(env)] = 0
    env.x = as.numeric(attributes(env)$dimnames[[1]])
    env.y = as.numeric(attributes(env)$dimnames[[2]])
    if (plot) {
        require(fields)
        par(cex = mcex)
        if (log) {
            image(env.x, env.y, env, zlim = c(0.1, 8), col = col, 
                xlab = "Temperature(C)", ylab = "depth (m)", 
                axes = T, ylim = c(mbz, minz), xlim = c(mint, mbt))
            abline(h = seq(mbz, minz, 50), v = seq(5, mbt, 5), col = "grey90", 
                lty = 2)
            image(env.x, env.y, env, zlim = c(0.1, 8), col = col, 
                xlab = "", ylab = "", axes = F, ylim = c(mbz, 
                  minz), xlim = c(mint, mbt), add = T)
            box()
            image.plot(env, zlim = c(0, 8), add = F, legend.only = T, 
                horizontal = F, legend.shrink = 1, legend.args = list(text = "ln(frequency)", 
                  side = 4, line = 1.5, cex = 1.2), col = col)
        }
        else {
            image(env.x, env.y, env, zlim = c(min(env), max(env)), 
                col = col, xlab = "Temperature(C)", ylab = "depth (m)", 
                axes = T, ylim = c(mbz, minz), xlim = c(mint, mbt))
            abline(h = seq(mbz, minz, 50), v = seq(5, mbt, 5), col = "grey90", 
                lty = 2)
            image(env.x, env.y, env, zlim = c(min(env), max(env)), 
                col = col, xlab = "", ylab = "", axes = F, ylim = c(mbz, 
                  0), xlim = c(mint, mbt), add = T)
            box()
            image.plot(env, zlim = c(min(env), max(env)), add = F, 
                legend.only = T, horizontal = F, legend.shrink = 1, 
                legend.args = list(text = "frequency", side = 4, 
                  line = 1.5, cex = 1.2), col = col)
        }
    }
    list(env.x, env.y, env)
}