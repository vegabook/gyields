    #+++++++++++++++++++++++++++++++++++#
    #+++++++++CRVM BACKTEST+++++++++++++#
    #+++++++++++++++++++++++++++++++++++#
    # STEPS:                            #
    # kickbt to get factor objects      #
    # run allbondcd to get cd and pc    #
    # run btzmeasure on this to get zs  #
    # run your rule to get portfolio    #
    # price your portfolio              #
    #+++++++++++++++++++++++++++++++++++#



datayears = 7
sampleyears = 0.25
backtestyears = datayears - sampleyears


bt <- function() {
    source("gyields.r")
    source("backtest.r")
    consoleColour("powderblue")
}

kickbt <- function(years = datayears) {
# kicks off the backtest data, basically getting 10 years of data for all bonds into the factors matrices
    getAllBondFactors(ac, days = 260 * years, current = FALSE)
}

saveCDs <- function(cCodes, savestr, useDecay = TRUE, decayhl = 260, usePC2 = TRUE, pcamats = keymats, model = "yerrsvBoth") {
# does all the cheap dears for a bunch of countries
    for(cCode in cCodes) {
        bt <- allbondcd(cCode, useDecay = useDecay, decayhl = decayhl, usePC2 = usePC2, pcamats = pcamats, model = model)
        save(bt, file = paste(cCode, savestr, ".dat", sep = ""))
    }
}

allbondcd <- function(cCode, years = backtestyears, useDecay = TRUE,
                      decayhl = 260, usePC2 = TRUE, pcamats = NULL, model = "yerrsvBoth") {
# this will get cd matrix for every day in years for country cCode
    factors = get(paste(cCode, "factors", sep = ""))
    days = wdaylist(Sys.Date() - 365 * backtestyears)
    thelot <- lapply(days, function(enddate) {
        flushprint(paste(cCode, enddate))
        startdate <- enddate - sampleyears * 365
        sendfactors <- factors[(as.Date(factors$date) > startdate) & (as.Date(factors$date) <= enddate), ]
        cdmatrix(infactors = sendfactors, cCode = cCode, useDecay = useDecay, decayhl = decayhl, usePC2 = usePC2,
                 pcamats = pcamats, returnpca = TRUE)
    })
    names(thelot) <- days
    return(thelot)
}

allbondnames <- function(btobj) {
    return(unique(unlist(sapply(btobj, function(x) colnames(x$cdmat)))))
}

loadbt <- function(cCodes = ac, doz = TRUE) {
    lapply(cCodes, function(cCode) {
        setwd("y:/r/gyields/backtestdata")
        lapply(dir()[grep(paste("^", toupper(cCode), sep = ""), dir())], function(f) {
            flushprint(f)
            load(f)
            assign(strsplit(f, "\\.")[[1]][1], bt, envir = globalenv())
            if(doz) {
                btz <- btzmeasure(bt)
                pf <- simplez(btz[[1]], bt)
                assign(paste(strsplit(f, "\\.")[[1]][1], "pf", sep = ""), pf, envir = globalenv())
                assign(paste(strsplit(f, "\\.")[[1]][1], "btz", sep = ""), btz, envir = globalenv())
            }
        })
        setwd("y:/r/gyields")
    })
    consoleColour("powderblue")
}


btzmeasure <- function(btobj, decayhl = NULL, minsample = 65, mat = "cdmat") {
# z score measures for backtest
    allbonds <- allbondnames(btobj) # get all the bond names
    # go by dates and get each at that date's sd
    allzeds <- lapply(btobj, function(x) {   # each day
        considermat <- x[[mat]]   # pcmat or cdmat
        zeds <- apply(considermat, 2, function(y) {    # all the zed scores by bond
            sdseries <- na.omit(y)
            lenseries <- length(sdseries)
            if(is.null(decayhl)) decayer <- rep(1, lenseries) else decayer <- decay(lenseries, decayhl)
            if(lenseries >= minsample) return(-((sdseries[lenseries] - mean(sdseries)) / wt.sd(sdseries, decayer))) else return(NA)
        })
    })
    # now put them all into a clean matrix
    names(allzeds) <- names(btobj) # same names
    zedmat <- sapply(names(btobj), function(d) { # get the zs for each date
        dzeds <- allzeds[[d]]
        dzednames <- names(dzeds)
        dayzeds <- sapply(allbonds, function(x) {
            if(x %in% dzednames) as.numeric(dzeds[x]) else NA
        })
        #names(dayzeds) <- allbonds
    })
    zedmat <- t(zedmat) # transpose the sapply
    zedmat <- xts(zedmat, order.by = as.Date(rownames(zedmat)))
    # now how many bps per z
    allsdbp <- lapply(btobj, function(x) {   # each day
        considermat <- x[[mat]]   # pcmat or cdmat
        zeds <- apply(considermat, 2, function(y) {    # all the zed scores by bond
            sdseries <- na.omit(y)
            lenseries <- length(sdseries)
            if(is.null(decayhl)) decayer <- rep(1, lenseries) else decayer <- decay(lenseries, decayhl)
            if(lenseries >= minsample) return (wt.sd(sdseries, decayer)) else return(NA)
        })
    })
    # now put them all into a clean matrix
    names(allsdbp) <- names(btobj) # same names
    bpsdmat <- sapply(names(btobj), function(d) { # get the zs for each date
        dzeds <- allsdbp[[d]]
        dzednames <- names(dzeds)
        dayzeds <- sapply(allbonds, function(x) {
            if(x %in% dzednames) as.numeric(dzeds[x]) else NA
        })
        #names(dayzeds) <- allbonds
    })
    bpsdmat <- t(bpsdmat) # transpose the sapply
    bpsdmat <- xts(bpsdmat, order.by = as.Date(rownames(bpsdmat)))
    # now copy paste the whole thing again for just the final cheap dear. Sloppy coding but no time ;)
    allcds <- lapply(btobj, function(x) {   # each day
        considermat <- x[[mat]]   # pcmat or cdmat
        zeds <- apply(considermat, 2, function(y) {    # all the zed scores by bond
            sdseries <- na.omit(y)
            lenseries <- length(sdseries)
            if(is.null(decayhl)) decayer <- rep(1, lenseries) else decayer <- decay(lenseries, decayhl)
            if(lenseries >= minsample) return(sdseries[lenseries]) else return(NA)
        })
    })
    # now put them all into a clean matrix
    names(allcds) <- names(btobj) # same names
    cdmat <- sapply(names(btobj), function(d) { # get the zs for each date
        dcds <- allcds[[d]]
        dcdnames <- names(dcds)
        daycds <- sapply(allbonds, function(x) {
            if(x %in% dcdnames) as.numeric(dcds[x]) else NA
        })
        #names(dayzeds) <- allbonds
    })
    cdmat <- t(cdmat)
    cdmat <- xts(cdmat, order.by = as.Date(rownames(cdmat)))
    return(list(cds = cdmat, zs = zedmat, bpsd = bpsdmat))
}



simplez <- function (zobj, btobj, zthresh = 2, killthresh = 1.5, posisize = 20, stopthresh = 3) {
# posi matrix creation from z scores
# simple buy bond if z above zthresh, sell bond again if drops below killthresh
# no market condition rules
# assume can capture the cdmat using an appropriate barbell (not taken into account here)
    if(length(zobj) == 3) zobj <- zobj[[2]]
    bonds <- colnames(zobj)
    dates <- index(zobj)
    posimat <- zobj # make a copy of the zobj
    for(bondnow in bonds) {
        currentposi <- 0 # posi starts out at zero
        flushprint(bondnow)
        for(dd in dates) {
            datenow <- as.Date(dd)
            currentz <- zobj[datenow, bondnow] # get the current z for that date and that bond
            if(is.na(currentz)) currentz <- 0
            #flushprint(paste("currentz: ", currentz, "datenow: ", datenow, "bondnow: ", bondnow, "currentposi: ", currentposi))
            if(currentposi != 0) {         # we have a posi, check if we must remove it
                if (abs(currentz) < killthresh) {
                    posimat[datenow, bondnow] <- 0
                #} else if(abs(currentz) > stopthresh) {
                #    posimat[datenow, bondnow] <- 0
                } else {
                    posimat[datenow, bondnow] <- currentposi # carry it over
                }
            } else {                       # we don't have a posi so check if we should have one
                if(currentz < -zthresh) {
                    posimat[datenow, bondnow] <- -posisize
                } else if (currentz > zthresh) {
                    posimat[datenow, bondnow] <- +posisize
                } else {
                    posimat[datenow, bondnow] <- 0
                }
            }
            currentposi <- posimat[datenow, bondnow]
        }
    }
    return(posimat)
}


live3 <- function(zobj, bj, btobj, zthresh = 2, killthresh = 1.5, posisize = 20, stopthresh = 3) {
# posi matrix creation from z scores : 3 best trades in each row
# simple buy bond if z above zthresh, sell bond again if drops below killthresh
# no market condition rules
# assume can capture the cdmat using an appropriate barbell (not taken into account here)
    if(length(zobj) == 3) zobj <- zobj[[2]] # get the right zobj if sent the whole list 
    posimat <- zobj
    for(x in 1:nrow(zobj)) {
        currentvol
        rowz <- zobj[x, ]
    }
}


pricefolio <- function(posimat, yieldmat, titlemain) {
    yieldchange <- diff(yieldmat)[-1, ] # get changes
    ycclean <- apply(yieldchange, 2, function(x) {
                     y <- x
                     y[is.na(y)] <- 0
                     return(y)
                })
    posiduring <- posimat[-nrow(posimat), ] # take out last line
    plmat <- posiduring * ycclean
    daypl <- apply(plmat, 1, sum)
    pl <- cumsum(daypl) * 10
    pl <- xts(pl, order.by = as.Date(names(pl)))
    plot(pl["2007/"], minor.ticks = FALSE, major.format = "%Y", main = titlemain)
    return(pl)
}

## ----------------------------------------------- FLY RV ------------------------------------------------ ##

backfly <- function(cCodes = ac, wingmax = 3, wingration = 2, virs = FALSE, pcmat = TRUE, useDecay = FALSE, model = "svBoth", 
                    decayhl = 260, usePC2 = TRUE, pcamats = keymats, mindays = 65, bodyrange = NA) {
    days = wdaylist(Sys.Date() - 365 * backtestyears)
    allcountries <- lapply(cCodes, function(cCode) {
        factors = get(paste(cCode, "factors", sep = ""))
        cy <- couponYields(cCode)
        thelot <- lapply(days, function(enddate) {
            flushprint(paste(cCode, enddate))
            startdate <- enddate - sampleyears * 365
            sendfactors <- factors[(as.Date(factors$date) > startdate) & (as.Date(factors$date) <= enddate), ]
            cdm <- cdmatrix(infactors = sendfactors, cCode = cCode)
            sendcy <- couponYields(cCode)[index(cdm$cdmat), ]
            db <- length(days) - match(last(index(sendcy)), days)
            pc <- getPCAs(daysback = db, series = TRUE, model = model, decayhl = decayhl, usedecay = useDecay, days = nrow(sendcy))
            flushprint(paste(last(index(pc[[1]])), last(index(sendcy)), last(index(cdm$pcmat))))
            flushprint(paste(length(index(pc[[1]])), length(index(sendcy)), length(index(cdm$pcmat))))
            flymax(cCode, incdmat = cdm, inpcs = pc, incy = sendcy, plotit = FALSE)
        })
        names(thelot) <- days
    })
    names(allcountries) <- cCodes
    return(allcountries)
}





                              



            
    































































    


















