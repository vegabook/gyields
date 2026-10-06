#####################################


accruedDropDates <- function(cCode, bondcode, fromisin = F) {
# dates where accrued drops on a bond
    cData <- get(paste(cCode, "data", sep = ""))
    if(fromisin) {
        bondcode <- isin2bb(cCode, bondcode)
    }
    coupon <- cData$staticData[bondcode, "COUPON"]
    if(coupon == 0) {
        flushprint("zero coupon")
        return(NULL)
    } else {
        accrued <- cData$historicData$PX_DIRTY_BID[, bondcode] - cData$historicData$PX_BID[, bondcode]
        diffacc <- diff(accrued)
        dates <- na.omit(index(accrued)[(-diffacc) > (coupon / 10)])
        dates <- as.Date(as.character(dates))
        return(dates)
    }
}


cashflowDates <- function(cCode, bondcode, fromisin = F) {
# dates when cashflows occur on a bond
    cData <- get(paste(cCode, "data", sep = ""))
    if(fromisin) {
        bondcode <- isin2bb(cCode, bondcode)
    }
    coupon <- cData$staticData[bondcode, "COUPON"]
    if(coupon == 0) {
        flushprint("zero coupon")
        return(NULL)
    } else {
        dates <- cData$cfData[[bondcode]][, 1]
        dates <- as.Date(dates)
        return(dates)
    }
}


matchddcf <- function(cCode, bondcode, fromisin = F) {
# match accrual drop dates and cashflow dates and return a data frame
    cData <- get(paste(cCode, "data", sep = ""))
    if(fromisin) {
        bondcode <- isin2bb(cCode, bondcode)
    }
    coupon <- cData$staticData[bondcode, "COUPON"]
    if(coupon == 0) {
        flushprint("zero coupon")
        return(data.frame(NULL, NULL))
    } else {
        cfd <- cashflowDates(cCode, bondcode)
        acd <- accruedDropDates(cCode, bondcode)
        if(length(acd) == 0) {      # it's a brand new bond so now accrued drop dates yet
            return(data.frame(NULL, NULL))
        }
        if(length(cfd) == length(acd)) {
            datmat <- data.frame(acd, cfd)
        } else {
            matchmatrix <- rollapply(cfd, length(acd), function(x) x - acd)
            if(length(acd) == 1) {
                matchmatrix <- matrix(matchmatrix) # from vector back to matrix
            }
            rowsums <- abs(apply(matchmatrix, 1, sum))
            minidx <- which.min(rowsums)
            datmat <- data.frame(acd, cfd[minidx:(minidx + length(acd) - 1)])
        }
        # now check integrity
        diffs <- apply(datmat, 1, function(x) as.numeric(as.Date(x[1])) - as.numeric(as.Date(x[2])))
        if(any(abs(diffs - mean(diffs)) > 10)) {
            flushprint("may have a cashflow dates vs accrual drop dates integrity issue on")
            flushprint(paste("country", cCode))
            flushprint(paste("bond", bondcode))
            fileconn <- file("build_problems.log")
            writeLines(paste(as.character(Sys.time()), bondcode, "acrrual date drop vs cashflow date mismatch"))
            close(fileconn)
            save(datmat, file = paste(bondcode, "_bad_accrual_date.log", sep = ""))
        }
        return(datmat)
    }
}

countryddcf <- function(cCode) {
# all accrual drop v cashflow dates for a country. Calls matchddcfv
    cData <- get(paste(cCode, "data", sep = ""))
    allbonds <- unique(unlist(cData$actives))
    cflist <- lapply(allbonds, function(x) matchddcf(cCode, x))
    names(cflist) <- allbonds
    return(cflist)
}









