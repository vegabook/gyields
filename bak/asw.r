bigasw <- function(cCodes = cl, years = 1, numPCs = 6, withgraphics = T, tobrowser = F, sighl = 2, mindata = NA) {
    # cross markets asset swap comparator
    # numPCs -> number of PCs to regress against for residuals. Any number from 0 upwards. 0 = no PC removal
    # mindata -> only consider bonds with this much days of data, otherwise all
    # withgraphics -> plot the graphics
    # tobrowser -> plot in browser
    # years -> how many years of data (max)
    flushprint("calculating")
    asw <- sapply(cCodes, function(x) aa <- getasw(x, years))
    isins <- do.call(c, sapply(asw, colnames)) # get isins

    # insert human column names
    asw <- lapply(cl, function(x) {
                      aa <- asw[[x]] 
                      colnames(aa) <- isinLabel(x, colnames(aa), seper = "_"); 
                      aa
    })
    asw <- do.call(cbind, asw)
    asw <- asw * 10000
    if(!(is.na(mindata))) {
        enough <- apply(asw, 2, function(x) length(na.omit(x)) >= mindata)
        asw <- asw[, enough]
    }
    #bad data points
    naughtylist <- list("2017-07-11" = c("DE_0.5_15Aug27")) 
    for(d in names(naughtylist)) {
        asw[as.Date(d), naughtylist[[d]]] <- NA
    }
    asw <- na.locf(asw) 
    # now prepare to do data-adjusted correlations
    datahave <- apply(asw, 2, function(x) length(na.omit(x)))
    uniquehave <- unique(datahave)
    uniquehave <- uniquehave[order(uniquehave)] 
    stats <- lapply(uniquehave, function(x) {
                 thishave <- asw[, datahave >= x]
                 thishave <- na.omit(thishave)
                 thiscor <- cor(thishave)
                 thiseig <- eigen(thiscor)
                 thisPCs <- thishave %*% thiseig$vectors
                 lmPCs <- as.data.frame(thisPCs[, 1:numPCs])
                 if(numPCs > 0) {
                     returner <- apply(na.omit(asw[, datahave == x]), 2, function(x) lm(x ~ ., data = lmPCs)$residuals)
                 } else {
                     returner <- asw[, datahave == x]
                 }
                 returner <- xts(returner, order.by = last(index(asw), nrow(returner)))
                 list(pcs = thisPCs, resids = returner, eig = thiseig)
    })
    resids <- lapply(stats, function(x) x$resids)
    pcs <- lapply(stats, function(x) x$pcs)
    eigs <- lapply(stats, function(x) x$eig)
    names(pcs) <- uniquehave
    resids <- do.call(cbind, resids)
    resids <- resids[, colnames(asw)]
    zs <- apply(resids, 2, function(x) {xx <- na.omit(x); (last(xx) - mean(xx)) / sd(xx)})
    if(withgraphics) {
        if(tobrowser) {
            svg(paste(realhtmlplace, "bigasw.svg", sep = ""), width = 30, height = 20)
        } else {
            windows(30, 20)
        }
        par(mfrow = c(2, 1))
        layout(matrix(c(1, 2)), heights = c(3, 1))
        par(mar = c(0, 1, 1, 1), oma = c(2, 2, 2, 2))
        boxplot(as.matrix(resids), axes = F, outcex = 1, 
                whisklty = "dotted", whiskcol = "orange", boxcol = "orange", medcol = "orange", staplecol = "orange",
                outcol = addAlpha("orange", 0.3))
        axis(2)
        abline(h = 0, lty = "dashed", col = "grey")
        # now signaliser
        apply(resids, 2, function(x) quicksimsig(as.matrix(x), 130, sighl)) -> ll
        ll <- scale(ll) # mean zero sd 1
        ll[is.na(ll)] <- 0
        ll <- as.numeric(ll)
        pchs <- rep(19, ncol(resids))
        pchs[ll > 2] <- 24
        pchs[ll < -2] <- 25
        cols <- rep("red", ncol(resids))
        cols[ll > 2] <- "magenta"
        cols[ll < -2] <- "cyan3"
        cexs <- rep(1, ncol(resids))
        cexs[ll > 2] <- ll[ll > 2] / 2
        cexs[ll < -2] <- abs(ll[ll < -2] / 2)
        points(1:ncol(resids), as.numeric(last(resids)), pch = pchs, col = cols, cex = cexs, bg = cols)
        # now labels above and below 
        evenrange <- 1:ncol(asw)
        yesplot <- (evenrange %% 2) == 0
        text(evenrange[yesplot], as.numeric(last(resids))[yesplot], paste(colnames(asw)[yesplot], "  "), 
             srt = 90, cex = 0.7, pos = 2, offset = 0)
        yesplot <- (evenrange %% 2) != 0
        text(evenrange[yesplot], as.numeric(last(resids))[yesplot], paste("  ", colnames(asw)[yesplot]), 
             srt = 90, cex = 0.7, pos = 4, offset = 0)
        title(paste("Asset swap spread residuals to ", max(numPCs), " principal component", ifelse(max(numPCs) == 1, "", "s"), 
                    " (sample ", nrow(asw), " days)", sep = ""))
        legend("bottomright", fill = c("red", "green", "magenta", "cyan3"), 
               legend = c("residual spread % (lhs)", "z-score (rhs)", "accelerated up", "accelerated down"))
        # now zs
        zs <- apply(resids, 2, function(x) {xx <- na.omit(x); (last(xx) - mean(xx)) / sd(xx)})
        b <- barplot(zs, col = "grey", names.arg = NA, ylim = c(-4.5, 4.5), border = NA)
        poslabplace <- sapply(zs, function(x) max(x, 0))
        neglabplace <- sapply(zs, function(x) min(x, 0))
        yesplot <- (evenrange %% 2) == 0
        text(b[yesplot], poslabplace[yesplot], paste("  ", colnames(asw)[yesplot]), srt = 90, cex = 0.6, pos = 4, offset = 0)
        yesplot <- (evenrange %% 2) != 0
        text(b[yesplot], neglabplace[yesplot], paste(colnames(asw)[yesplot], "  "), srt = 90, cex = 0.6, pos = 2, offset = 0)
        abline(h = c(-2, 2), col = "grey")
        title("z-score")
        if(tobrowser) {
            dev.off()
            curdir <- getwd()
            setwd(realhtmlplace)
            browseURL("bigasw.svg")
            setwd(curdir)
        }
    }
    return(list(asw = asw, resids = resids, pcs = pcs, zs = zs, eig = eigs, isins = isins, countries = cCodes))
}

zbox <- function(mx) {
# takes matrix-like mx and gives z of every column lm to every other column
# will naomit if necessary
    resids <- apply(mx, 2, function(x) apply(mx, 2, function(y) {
            xx <- as.numeric(na.omit(x))
            yy <- as.numeric(na.omit(y))
            xx <- tail(xx, length(yy))
            yy <- tail(yy, length(xx))
            thislm <- lm(yy ~ xx)
            resid <- thislm$residuals
            slope <- thislm$coefficients[-1]
            list(bps = last(resid), z = last(resid) / sd(resid), slope = slope, resids = resid)
    }))
    slopes <- sapply(names(resids), function(x) sapply(names(resids[[x]]), function(y) resids[[x]][[y]]$slope))
    zs <- sapply(names(resids), function(x) sapply(names(resids[[x]]), function(y) resids[[x]][[y]]$z))
    bps <- sapply(names(resids), function(x) sapply(names(resids[[x]]), function(y) resids[[x]][[y]]$bps))
    bond1 <- sapply(names(resids), function(x) sapply(names(resids[[x]]), function(y) x))
    bond2 <- sapply(names(resids), function(x) sapply(names(resids[[x]]), function(y) y))
    list(slopes = slopes, zs = zs, bps = bps, bond1 = bond1, bond2 = bond2, resids = resids)
}


corbox <- function(mx, pcs) {
# takes matrix-like mx and gives z of every column lm to every other column
    resids <- apply(mx, 2, function(x) apply(mx, 2, function(y) {
            xx <- as.numeric(na.omit(x))
            yy <- as.numeric(na.omit(y))
            xx <- tail(xx, length(yy))
            yy <- tail(yy, length(xx))
            resid <- lm(yy ~ xx)$rezbox_addcorrels
            cor(diffret(resid), diffret(tail(pc, length(resid))))
    }))
}

aswbox <- function(bigobj) {
# takes a bigobj from bigasw then creates a big data.table that can be queried

}

