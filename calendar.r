#calendar functions for bloomberg instruments
pacman::p_load(lubridate)

lcal <- function() source("calendar.r")


getInstrumentCalendar <- function(instrument) {
# gets the calendar associated with an instrument
    cal <- (mybds(instrument, "CALENDAR_NON_SETTLEMENT_DATES"))[, 1]
    cal <- as.Date(cal)
    return(cal)
}


getCountryCalendar <- function(cCode) {
# gets the calendar associated with a country
    flushprint(paste("getting", cCode, "from Bloomberg"))
    cal <- mybds("EUR Curncy", "CALENDAR_NON_SETTLEMENT_DATES", 
        "SETTLEMENT_CALENDAR_CODE", cCode)
    return(as.Date(cal[, 1]))
}


getCalendars <- function(cCodes, up = FALSE) {
# load the calendar file
    try(load(paste(dataplace, "cal.dat", sep = ""), verbose = FALSE))
    if((!exists("cal")) | up) {
        cal <- lapply(cCodes, function(x) getCountryCalendar(x))
        names(cal) <- cCodes
        assign("cal", cal, envir = globalenv())
    } else {
        notThere <- cCodes[which(!cCodes %in% names(cal))]
        if(length(notThere) > 0) {
            cal <- lapply(cCodes, function(x) getCountryCalendar(x))
            names(cal) <- cCodes
        }
        assign("cal", cal, envir = globalenv())
    }
    save(cal, file = paste(dataplace, "cal.dat", sep = ""))
}


modFol <- function(cCode, inDates) {
# modified following dates using the cCode calendar in the cal list (and weekends)
    if(class(inDates) != "Date") inDates <- as.Date(inDates) # convert to dates if necessary
    fixed <- sapply(inDates, function(x) {
        inMonth <- month(x) # from lubridate
        while((weekdays(x) %in% c("Saturday", "Sunday")) | (x %in% cal[[cCode]])) 
            x <- x + 1
        if(month(x) != inMonth) { # if we have gone into the next month
            x <- x - 1 # go back one day
            while((weekdays(x) %in% c("Saturday", "Sunday")) | (x %in% cal[[cCode]])) 
                x <- x - 1 # and repeat if necessary
        }
        return(x)
    })
    return(as.Date(fixed))
}


settleDate <- function(dates, calendar = "TE", tplus = 3, tchange = NA) {
# takes the dates and finds the next settlement date. Default calendar is "target"
# tchange takes a historic data frame of dates in column 1 and t values in column 2 for t convention changes # !!! not implemented
    if(!("Date" %in% class(dates))) {
        flushprint("dates input must be of class Date")
        return(-1)
    }
    settledates <- sapply(dates, function(d) {
        settlecounted <- 0 # zero settlement days so far
        outdate <- d
        usetplus <- tplus
        if((calendar == "FR") & (d < as.Date("2012-04-02"))) usetplus <- 2
        while(settlecounted < usetplus) {
            outdate <- outdate + 1 # next day
            if(!((weekdays(outdate) %in% c("Saturday", "Sunday")) | (outdate %in% cal[[calendar]]))) {
                settlecounted <- settlecounted + 1
            }
        }
        return(outdate)
    })
    return(as.Date(settledates))
}

tradeDate <- function(dates, calendar = "TE", tplus = 3, tchange = NA) {
# takes a settlement date and find the associated trade date
# tchange takes a historic data frame of dates in column 1 and t values in column 2 for t convention changes # !!! not implemented
    if(!("Date" %in% class(dates))) {
        flushprint("dates input must be of class Date")
        return(-1)
    }
    tradedates <- sapply(dates, function(d) {
        tradecounted <- 0 # zero settlement days so far
        outdate <- d
        usetplus <- tplus
        if((calendar == "FR") & (d < as.Date("2012-04-02"))) usetplus <- 2
        while(tradecounted < usetplus) {
            outdate <- outdate - 1 # next day
            if(!((weekdays(outdate) %in% c("Saturday", "Sunday")) | (outdate %in% cal[[calendar]]))) {
                tradecounted <- tradecounted + 1
            }
        }
        return(outdate)
    })
    return(as.Date(tradedates))
}



        
fol <- function(cCode, inDates) {
# "following" dates using the cCode calendar in the cal list (and weekends)
    if(class(inDates) != "Date") inDates <- as.Date(inDates) # convert to dates if necessary
    fixed <- sapply(inDates, function(x) {
        while((weekdays(x) %in% c("Saturday", "Sunday")) | (x %in% cal[[cCode]])) 
            x <- x + 1
        return(x)
    })
    return(as.Date(fixed))
}


# debugging functions

compareCal <- function(cCode) {
    load(paste(dataplace, "mcal.dat", sep = ""))
    manCal <- mcal[mcal[, 1] == cCode, 2]
    manCal <- manCal[!weekdays(manCal) %in% c("Saturday", "Sunday")]
    autoCal <- getCountryCalendar(cCode)
    autoCal <- autoCal[autoCal <= last(manCal)]
    autoCal <- autoCal[autoCal >= first(manCal)]
    autoCal <- autoCal[!weekdays(autoCal) %in% c("Saturday", "Sunday")]
    nwautoCal <- autoCal[!weekdays(autoCal) %in% c("Saturday", "Sunday")] 
    instrCal <- getInstrumentCalendar(paste(last(last(get(paste(cCode, "data", sep = ""))$actives)[[1]]), "Corp"))
    instrCal <- instrCal[instrCal <= last(manCal)]
    instrCal <- instrCal[instrCal >= first(manCal)]
    nwinstrCal <- instrCal[!weekdays(instrCal) %in% c("Saturday", "Sunday")]

    browser()
}



        

    



   



    
