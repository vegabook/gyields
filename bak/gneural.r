# ~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~~ FACTOR CREATOR ~~~~ #
# ~~~~~~~~~~~~~~~~~~~~~~~~ #

buckets = list("2" = c(0, 2.5),           #maturity buckets
               "3" = c(2.5, 4),
               "5" = c(4, 6),
               "7" = c(6, 8.5),
               "10" = c(8.5, 12.5),
               "15" = c(12.5, 17.5),
               "20" = c(17.5, 25),
               "30" = c(25, 35),
               "50" = c(35, 100))


gn <- function() source("gneural.r")


bondFactors <- function(cCode, bond, isin = TRUE, days = 260 * 3) {
    if(!isin) bond <- bb2isin(cCode, bond) 
    bbg <- isin2bb(cCode, bond)
    cData <- get(paste(cCode, "data", sep = ""))
    factors <- sapply((length(cData$actives) - days + 1):length(cData$actives), function(x) {
        onrun <- if(bond %in% rownames(cData$ns[[x]]$yerrors)) TRUE else 
                 if(bond %in% rownames(cData$nsOff[[x]]$yerrors)) FALSE else NA
        if(!is.na(onrun)) {
            yerrdl <- if(onrun) cData$dl[[x]]$yerrors[bond, 2] else cData$dlOff[[x]]$yerrors[bond, 2]
            yerrns <- if(onrun) cData$ns[[x]]$yerrors[bond, 2] else cData$nsOff[[x]]$yerrors[bond, 2]
            yerrsv <- if(onrun) cData$sv[[x]]$yerrors[bond, 2] else cData$svOff[[x]]$yerrors[bond, 2]
            yerrdlBoth <- cData$dlBoth[[x]]$yerrors[bond, 2]
            yerrnsBoth <- cData$nsBoth[[x]]$yerrors[bond, 2]
            yerrsvBoth <- cData$svBoth[[x]]$yerrors[bond, 2]
            yerrasvBoth <- cData$asvBoth[[x]]$yerrors[bond, 2] # asv
            if(is.na(cData$csBoth) || is.na(cData$csBoth[[x]]$yerrors)) {
                yerrcsBoth <- cData$asvBoth[[x]]$yerrors[bond, 2] # use asvBoth if csBoth is not there
            } else {
                yerrcsBoth <- cData$csBoth[[x]]$yerrors[bond, 2]
            }
            mat <- cData$nsBoth[[x]]$yerrors[bond, 1]
            bucket <- names(buckets)[sapply(names(buckets), function(y)
                ifelse((mat > buckets[[y]][1] & mat <= buckets[[y]][2]), TRUE, FALSE))]
            bidask <- cData$historicData$PX_ASK[x, bbg] - cData$historicData$PX_BID[x, bbg]
            yld <- cData$nsBoth[[x]]$y[bond, 2]
            prc <- cData$nsBoth[[x]]$p[bond]
            coupon <- cData$staticData[isin2bb(cCode, bond), "COUPON"]
        } else {                     # then all NA
            yerrdl <- yerrns <- yerrsv <- yerrdlBoth <- yerrnsBoth <- yerrsvBoth <- yerrasvBoth <- yerrcsBoth <- NA # asv
            mat <- bucket <- bidask <- yld <- prc <- year <- coupon  <- NA
        }
        returner <- c(yerrdl, yerrns, yerrsv, yerrdlBoth, yerrnsBoth, yerrsvBoth, yerrasvBoth,
        yerrcsBoth, yld, prc, onrun, mat, bucket, bidask, coupon)
        if(length(returner) == 13) {
            flushprint("ERROR: length returner mismatch in bondFactors")
            flushprint(paste("bad index", x))
            flushprint("please note that number and press c <enter> to continue")
            browser()
        }
        return(returner)
    })
    factors <- data.frame(as.numeric(factors[1, ]), as.numeric(factors[2, ]), 
                as.numeric(factors[3, ]), as.numeric(factors[4, ]),
                as.numeric(factors[5, ]), as.numeric(factors[6, ]),
                as.numeric(factors[7, ]), as.numeric(factors[8, ]), as.numeric(factors[9, ]), as.numeric(factors[10, ]), # asv
                as.logical(factors[11, ]), as.numeric(factors[12, ]), # asv
                as.numeric(factors[13, ]), as.numeric(factors[14, ]), as.numeric(factors[15, ])) # asv
    years <- last(format(as.Date(names(cData$actives)), "%Y"), days)
    factors <- cbind(factors, years)
    bondNameVec <- rep(bond, length(years))
    factors <- cbind(bondNameVec, factors) # add in the bond name if we merge bondFactors later this will be needed
    dates <- last(names(cData$actives), days)
    factors <- cbind(dates, factors) # put in the dates
    colnames(factors) <- c("date", "bond", "yerrdl", "yerrns", "yerrsv", "yerrdlBoth", "yerrnsBoth", "yerrsvBoth", 
        #"yerrcsBoth",  # no asv
        "yerrasvBoth", "yerrcsBoth",  # asv
        "y", "price", "onrun", "mat", "bucket", "bidask", "coupon", "year")
    rownames(factors) <- last(names(cData$actives), days)
    return(factors)
}


allBonds <- function(cCode, current = FALSE, onoff = "both", days = 260 * 3) {
# returns all the bond names out of an nsl object
    cData <- get(paste(cCode, "data", sep = ""))
    nsObj <- cData$nsBoth
    if(!current) {
        return(unique(unlist(lapply(last(nsObj, days), function(x) rownames(x$y)))))
    } else {
        return(rownames(last(nsObj)[[1]]$y))
    }
}


allBondFactors <- function(cCode, days = 260 * 3, merged = TRUE, naomit = TRUE, 
                           ordered = TRUE, current = TRUE) {
# returns a big data frame of all the bond factorsj
    bondNames <- allBonds(cCode, current, days = days)
    af <- lapply(bondNames, function(x) {
        bondFactors(cCode, x, TRUE, days)
    })
    if(merged == TRUE) {
        afall <- af[[1]]
        for (x in 2:length(af)) afall <- rbind(afall, af[[x]])
        if(naomit == TRUE) {
            afall <- na.omit(afall)
            if(ordered == TRUE) afall <- afall[order(afall$date), ]
        }
        return(afall)
    } else {
        names(af) <- bondNames
        return(af)
    }
}

getAllBondFactors <- function(cCodes, days = 260 * 3, current = TRUE) {
    ignored <- lapply(cCodes, function(cCode) {
        f <- allBondFactors(cCode, days = days, merged = TRUE, naomit = TRUE, 
            ordered = TRUE, current = current)
        flushprint(paste("Getting all bond factors for", cCode))
        assign(paste(cCode, "factors", sep = ""), f, envir = .GlobalEnv)
    })
    return("Done getting all bond factors")
}


upBondFactors <- function(cCodes) {
# this will update a bondfactors object for a country
    ignored <- lapply(cCodes, function(cCode) {
        if(!exists(paste(cCode, "factors", sep = ""))) {         # check if it exists    
            cat("!No factor object for country", cCode, "\n")
            return(0)
        } else {
            cFactors <- get(paste(cCode, "factors", sep = ""))
            if(as.character(last(cFactors$date)) != Sys.Date()) {    # check that it's been initialized for today   
                cat("!Check data for country", cCode, "has been intialized today")
                return(1)
            } else {                                # okay all good, we can proceed
                flushprint(paste("Updating bond factors for", cCode))
                cFactors[as.character(cFactors$date) == Sys.Date(), ] <- allBondFactors(cCode, days = 1) # do the update
                assign(paste(cCode, "factors", sep = ""), cFactors, envir = .GlobalEnv)
            }
        }
    })
    return("Updated all bond factors")
}

meltFactors <- function(cCodes = ac, model = "yerrsvBoth") {
# melt all the country factors in preparation for a unified yield curve chart
    allfactors <- lapply(cCodes, function(x) get(paste(x, "factors", sep = ""))[, c("date", "bond", "year", "mat", model)])
    names(allfactors) <- cCodes
    mfactors <- melt(allfactors, id.vars = c("date", "bond", "mat", "year"), value.name = model) # do the melt
    colnames(mfactors)[length(names(mfactors))] <- "country"
    minmats <- tapply(mfactors$mat, mfactors$bond, min) # get the minimum (current) maturity
    matcol <- sapply(mfactors$bond, function(x) minmats[x]) # create minmat column
    mfactors <- cbind(mfactors, matcol)
    colnames(mfactors)[length(names(mfactors))] <- "minmat"
    return(mfactors)
}


plotBond <- function(cCode, bond) {
    utheme = stheme
    cFactors <- get(paste(cCode, "factors", sep = ""))
    bondFactors <- cFactors[cFactors$bond == bond, ]
    windows(utheme$chartsize[[1]], 12)
    par(mfrow = c(3, 1), mar = c(2, 2, 4, 2), oma = c(1, 1, 2, 1))
    plotter <- function(streamName) {
        stream <- - bondFactors[, streamName]
        d <- density(stream)
        dmean <- mean(stream)
        dsd <- sd(stream)
        plot(as.Date(bondFactors$date), stream * 10000, 
            xlab = NA, ylab = NA, cex.axis = 0.8, type = "l")
        #plot(d$y, d$x, type = "l")
        title(streamName, font.main = 3)
    }
    #plotter("yerrns")
    #plotter("yerrnsBoth")
    plotter("yerrns")
    plotter("yerrsv")
    plotter("yerrdl")
    #plotter("yerrsvBoth")
    title(paste(cCode, makelabels(cCode, bond, fromisin = TRUE)[[1]]), line = -1, outer = TRUE)
}
    

################################################# MAINTENANCE FUNCTIONS ##################################################

plotBondBuckets <- function(cCode) {
# plots the yerrors but ablines where buckets change
    #cof <- allBondFactors(cCode)
    cofl <- allBondFactors(cCode, merged = FALSE)
    lapply(cofl, function(x) {
        dev.new()
        plot(x$yerrnsBoth, pch = 19, col = ifelse(as.logical(x$onrun), "red", "black"))
        bbcode <- isin2bb(cCode, x$bond[1])
        cashflows <- get(paste(cCode, "data", sep = ""))$cfData[[bbcode]]
        cfindex <- match(cashflows$Date, as.character(x$date))
        for(i in 2:nrow(x)) if(!is.na(x[i - 1, "bucket"])) if(x[i, "bucket"] != x[i - 1, "bucket"]) abline(v = i)
        abline(v = cfindex, col = "steelblue2", lwd = 2)
    })
}


bucketCD <- function(cCodes) {
# bucket cheap dear by country assuming "f" files exist, uses mean on tapply
    windows(12, 10)
    par(mfrow = c(2, 3))
    lapply(cCodes, function(x) {
        cof <- get(paste(x, "factors", sep = ""))
        ns <- tapply(cof$yerrns, cof$bucket, mean)
        sv <- tapply(cof$yerrsv, cof$bucket, mean)
        nsb <- tapply(cof$yerrnsBoth, cof$bucket, mean)
        svb <- tapply(cof$yerrsvBoth, cof$bucket, mean)
        bars <- cbind(ns, sv, nsb, svb)
        barplot(t(bars * 10000), beside = TRUE)
        title(x)
    })
}


bucketCDlm <- function(cCodes) {
# bucket cheap dear by country assuming "f" files exist, uses regression
    windows(12, 10)
    par(mfrow = c(2, 3))
    par(bg = "wheat1")
    lapply(cCodes, function(x) {
        cof <- get(paste(x, "factors", sep = ""))
        ns <- lm(yerrns ~ bucket, data = cof)$coefficients
        sv <- lm(yerrsv ~ bucket, data = cof)$coefficients
        nsb <- lm(yerrnsBoth ~ bucket, data = cof)$coefficients
        svb <- lm(yerrsvBoth ~ bucket, data = cof)$coefficients
        ns <- (ns + ns[1])[-1]
        sv <- (sv + sv[1])[-1]
        nsb <- (nsb + nsb[1])[-1]
        svb <- (svb + svb[1])[-1]
        bars <- cbind(ns, sv, nsb, svb)
        barplot(t(bars * 10000), beside = TRUE)
        title(x)
    })
}
        
#plotDatedCurve <- function((nsObj, startDate) {
# plot all the 
#    nsObj <- nsObj[names(nsObj)[names(nsObj)] >= startDate]
#    lapply(nsObj, function(x) {
#        plotObj <- 
        
        

    
    


