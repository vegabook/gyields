#################################################################
# COMPARISON OF GLOBAL NATIONAL AND SUPRANATIONAL YIELD CURVES  #
# EXTRACTS DATA FROM BLOOMBERG FOR EACH CURVE                   #
# FINDS THE ZERO CURVE AND NELSON SIEGEL CURVES FOR EACH DAY    #
# FINDS THE PRINCIPAL COMPONENTS OF THE CURVES                  #
# FINDS THE CHEAP DEARS FOR EACH CURVE                          #
# STARTED 30 DECEMBER 2012                                      #
#################################################################

# INTERESTING ALGOS
# Minimal tortion bets (meucci)


# TO DO
# fix dobox new dates christmas historicdata 24/25 Dec
# portfolio level pc sensitivities
# add coupons to bondfactors
# !!!!!!!!!! couponYields assumes annually quoted zeros when they are actually instantaneous zeros
# !!!!!!!!!! thus discount factors should actually be exponents


source("../rtools/common.r")
source("../rtools/tools.r")
source("../rtools/mongo.r")
source("gflows.r") # next months coupon flows
source("gdaily.r")


sysname = Sys.info()["sysname"]
nodename = Sys.info()["nodename"]

if(sysname == "Windows") {
    consoleColour("grey50")
} else if(sysname == "Darwin") {
    windows <- function(width, height) {
        quartz("", width, height, antialias = FALSE)
    }
} else {
    library("cairoDevice")
    windows = Cairo # set the windows window maker to Cairo
}

if(sysname == "Windows") {
    if(nodename == "VIRTUALWIN81") {
        dataplace <- "//VBOXSVR/Copy/data/gyields/" # where all the data is located
        bondchartplace <- "//LILJEN/crvm/html/bondcharts/"
        realhtmlplace <- "//LILJEN/crvm/html/country/"
        scratchplace <- "//LILJEN/crvm/scratch/"
        startdir <- getwd() # directory for websites etc where the wd should
    } else {
        dataplace <- "//LILJEN/share/crvm/data/" # where all the data is located
        bondchartplace <- "//LILJEN/crvm/html/bondcharts/"
        realhtmlplace <- "//LILJEN/crvm/html/country/"
        startdir <- getwd() # directory for websites etc where the wd should
    }
} else { # linux
    dataplace <- "/media/lilshare/crvm/data/"
    bondchartplace <- "/media/lilshare/crvm/html/bondcharts/"
    realhtmlplace <- "/media/lilshare/crvm/html/country/"
    scratchplace <- "/media/lilshare/crvm/html/scratch/"
    startdir <- getwd()
}

g <- function() {
    setwd(startdir)
    source("gyields.r")
}

library("xts") # for timeseries
library("RColorBrewer") # for plotting curves in debug mode
if(sysname != "Windows") source("../rtools/mongo.r") 
source("NSrates.R")
source("NelsonSiegel.R")
source("gneural.r") # this will bring in all the bond factor stuff
source("calendar.r")
#source("../rtools/mongo.r")
source("backtest.r")
library("foreach") # parallel processing
library("plyr") # for merging data frame with differing numbers of rows
library("FactoMineR") # for weighted PCA
library("ggplot2")
library("gridExtra") # for alex ggplot charts
library("gridBase") # for base graphics grid viewports used in spread analyses
library("knitr") # for html output
#library("MethComp") # for orthogonal regression
#library("rCharts") 
library("termstrc") # does most of the gruntwork of curve calcs
library("scales") # for proper ggplot2 scale formats
library("vioplot") # for violin plots
library("ridge") # for ridge regression
library("shiny")
library("RJSONIO")
library("leaps")
library("gplots") # for heatmap.2
library("RCurl")  # to ftp to India
library("httr") # for the actual result once done


# start acc dt   for start date and DES CASH FLOW ADJ for overidable date cash flows

# Bloomberg curve IDs NBNB YCGT0025 for the US doesn't work because
#   cannot seem to retrieve a bond chain
BBcurveIDs  <- list(PO = "YCGT0084 Index", #Portugal
                    DE = "YCGT0016 Index", #Germany
                    FR = "YCGT0014 Index", 
                    SP = "YCGT0061 Index",
                    IT = "YCGT0040 Index",
                    AU = "YCGT0001 Index", #Australia
                    AT = "YCGT0063 Index", #Austria
                    JP = "YCGT0018 Index",
                    GB = "YCGT0022 Index",
                    HK = "YCGT0095 Index",
                    CA = "YCGT0007 Index",
                    CH = "YCGT0082 Index",
                    NO = "YCGT0078 Index",
                    SW = "YCGT0021 Index",
                    DK = "YCGT0011 Index",
                    IR = "YCGT0062 Index",
                    BE = "YCGT0006 Index",
                    NE = "YCGT0020 index", 
                    ZA = "YCGT0090 Index",
                    PD = "YCGT0177 Index", #Poland
                    MX = "YCGT0251 Index",
                    GR = "YCGT0156 Index", 
                    FI = "YCGT0081 Index", 
                    US = "YCGT0025 Index")


gdp2012 <- list(PO = 212,
                DE = 3400, 
                FR = 2608, 
                SP = 1350,
                IT = 2014,
                AU = 1541,
                AT = 398, 
                JP = 5963,
                GB = 2440,
                HK = 263, 
                CA = 1819,
                CH = 632, 
                NO = 501, 
                SE = 526, 
                IR = 210, 
                BE = 484, 
                NE = 773, 
                ZA = 384, 
                PD = 487, 
                MX = 1177,  
                GR = 249) 


cColors <- list(PO = "aquamarine",
                DE = "#A6CEE3",
                FR = "#1F78B4",
                SP = "#EEC90080",
                IT = "#B2DF8A",
                AU = "lightgreen",
                AT = "#E31A1C",
                JP = "salmon",
                GB = "palevioletred2",
                HK = 263, 
                CA = 1819,
                CH = 632, 
                NO = 501, 
                SE = 526, 
                IR = 210, 
                BE = "#FB9A99", 
                NE = "#FDBF6F", 
                ZA = 384, 
                PD = 487, 
                MX = 1177,  
                GR = 249) 


histStartDate <- as.Date("2003-01-01") # history start date calendar
offRunMinMat <- 1 # minimum maturity of off the run bonds for them to be included
nsTau <- c(3, 20, 0.5)
svTauMost <- c(3, 20, 1, 1)
svTauLess <- c(3, 5, 1, 1)
#svTau <- list(SP = c(10, 20, 1, 5), 
svTau <- list(SP = c(3, 10, 1, 1),
              IT = svTauMost,
              FR = svTauMost,
              DE = svTauMost,
              BE = svTauMost,
              NE = svTauMost,
              AT = svTauMost,
              MX = svTauMost)

keymodel <- list(SP = "asvBoth",
                 IT = "asvBoth",
                 FR = "svBoth",
                 DE = "svBoth",
                 BE = "svBoth",
                 NE = "svBoth",
                 AT = "svBoth",
                 MX = "svBoth")


dlLambda <- lapply(1:length(BBcurveIDs), function(x) 4)
names(dlLambda) <- names(BBcurveIDs)
minMaturity <- 1.90
maxMaturity <- 35
winwidth <- 9 # inches, for plots
irsmats <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 15, 20, 30)
keymats <- c(2, 3, 5, 7, 10, 15, 20, 30) # key maturities to consider 
eurIRStickers <- paste(paste("EUSA", irsmats, sep = ""), "CMPL Curncy") 

mc <- c("DE", "FR", "IT", "SP") # main countries
ac <- c(mc, "NE", "AT", "BE") # all countries
em <- c("RU", "TR", "ZA", "PL", "HU", "CZ", "EU", "US", "MX")
em2 <- c("US", "EU", "ZA", "TR", "PL", "HU", "CZ", "MX")

#dataplace <- "//LILJEN/share/crvm/data/" # where all the data is located


gradientscale <- rescale(c(-4, -1.9, -1, 0, 1, 2.1, 4))
gradientcolours <- c("darkred", "red", "orange", "yellow", "#99BBFF", "blue", "darkblue")
#gradientscale <- rescale(c(-5, -2.2, 0, 2.2, 5))
#gradientcolours <- c("darkred", "red", "white", "blue", "darkblue")

# USEFUL HELPER TOOL FUNCTIONS ########################################

shine <- function() {
# runs the shiny user interface
    runApp("qshiny")
}


wdaylist <- function(startDate = histStartDate, endDate = Sys.Date()) {
# creates a list of weekdays starting at startDate and ending at endDate
    alldays <- as.Date(startDate:endDate) # create the list of all days
    wdays <- alldays[!(weekdays(alldays) %in% c("Saturday", "Sunday"))]
    return(wdays)
}

prcYld <- function(bCode, price, settleDate = Sys.Date()) {
# correct price to yield calculation
    if (class(settleDate) != "Date") settleDate <- as.Date(settleDate)
    sd <- format(settleDate, "%Y%m%d")
    prc <- as.character(price)
    if (length(strsplit(bCode, " ")[[1]]) < 2) bCode <- paste(bCode, "Corp")
    return(bdp(conn, bCode, "YLD_YTM_MID", override_fields = c("PX_BID", "PX_ASK", "SETTLE_DT"), 
        override_values = c(prc, prc, sd)))
}

ann_yields <- function(cashflows, m, searchint = c(-1, 1), tol = 1e-10) {
# tom browne's version of bond_yields in termstrc, must include prices
    yy <- bond_yields(cashflows, m, searchint = searchint, tol = tol)
    yy[, 2] <- exp(yy[, 2]) - 1
    return(yy)
}

isin2bb <- function(cCode, isin) {
# takes isins and converts them to bberg codes
    staticData <- get(paste(cCode, "data", sep = ""))$staticData
    rnames <- rownames(staticData)
    as.character(sapply(isin, function(x) rnames[staticData$ID_ISIN == x]))
}

bb2isin <- function(cCode, bb) {
    staticData <- get(paste(cCode, "data", sep = ""))$staticData
    return(staticData[bb, "ID_ISIN"])
}

bondsearch <- function(cCode, matmonth, matyear, coupon = NA, returnISIN = TRUE) {
# finds bond isin or bbcode by countrycode, coupon and maturity year
    if(nchar(matyear) == 2) matyear <- matyear + 2000
    cData <- get(paste(cCode, "data", sep = ""))
    isin <- cData$staticData[(year(cData$staticData$MATURITY) == matyear) 
                           & (month(cData$staticData$MATURITY) == matmonth), ]

    if(!is.na(coupon)) isin <- isin[isin$COUPON == coupon, ]
    isin <- isin[, "ID_ISIN"]
    if(!returnISIN) return(isin2bb(cCode, isin)) else return(isin)
}

maxMats <- function(cCode, numneed = 65) {
# tells me the maximum maturity for which I have at least numneed (=65) days of data
    ay <- last(allBondYields(cCode, mindata = 0, maxdata = numneed + 1, field = "mat"), numneed)
    return(round(max(first(ay), na.rm = TRUE) + 1))
}

interpIRS <- function(mats, inIRS = eurirs, inIRSmats = irsmats, historicmats = TRUE) {
# will interploate the inIRS matrix backwards according to mats
# if historicmats, then mats will be adjusted by the dates in the inIRS. inIRS must be an xts in this case. 
    if(historicmats) {
        dates <- index(inIRS)
        diffmats <- (last(as.Date(dates)) - as.Date(dates)) / 365.25
        mats <- sapply(mats, function(x) x + diffmats)
        interped <- sapply(1:nrow(inIRS), function(x) approx(inIRSmats, inIRS[x, ], mats[x, ], rule = 2)$y)
    } else {
        interped <- apply(inIRS, 1, function(x) approx(inIRSmats, x, mats, rule = 2)$y)
        if(length(mats) > 1) interped <- t(interped) # get it into the right format
    }
    if("xts" %in% class(inIRS)) interped <- xts(interped, order.by = index(inIRS))
    return(interped)
}

interpcy <- function(mats, yields, yieldmats = as.numeric(colnames(yields))) {
# returns the matrix of interpolated yields by maturities mats
# if the column names of yields are not numeric and/or do not represent the maturities, then these must 
# explicitly be specified 
    interped <- apply(yields, 1, function(x) approx(yieldmats, x, mats, rule = 2)$y)
    if(length(mats) > 1) interped <- t(interped) # ensure column
    interped <- xts(interped, order.by = index(yields))
    return(interped)
}

list2isin <- function(bondlist) {
    isins <- sapply(bondlist, function(x) {
        if(length(x) == 4) {
            bondsearch(as.character(x[1]), as.numeric(x[2]), as.numeric(x[3]), as.numeric(x[4]))
        } else {
            bondsearch(as.character(x[1]), as.numeric(x[2]), as.numeric(x[3]))
        }
    })
    return(isins)
}

genericSpread <- function(isins, weights = rep(1, length(isins)), model = "auto", days = 260 * 3) {
    bondnames <- allbonds()
    cCodes <- sapply(isins, function(i) names(bondnames)[sapply(bondnames, function(x) i %in% x)])
    mats <- sapply(1:length(isins), function(x) bdur(cCodes[x])[isins[x], "mat"])
    listyields <- lapply(1:length(mats), function(x) 
                         interpcy(mats[x], allYields(cCodes[x], model = model, days = days)[[1]]))
    dates <- index(listyields[[1]]) # extract date
    listyields <- lapply(listyields, as.numeric) # convert to numeric for the do.call coming up
    iyields <- do.call(cbind, listyields)
    genspread <- iyields %*% weights
    genspread <- xts(genspread, order.by = dates)
    return(genspread)
}

#bondData is data for a bond that will never change
bbStaticDataFields <- c("ID_ISIN",
                      "ISSUER", 
                      "COUPON",
                      "CPN_FREQ",
                      "MATURITY",
                      "CALC_TYP_DES",                    # pricing calculation type 
                      "SHORT_NAME",
                      "INFLATION_LINKED_INDICATOR",     # N or Y, in R returned as TRUE or FALSEb
                      "ISSUE_DT",
                      "FIRST_SETTLE_DT",
                      "PX_METHOD",                      # PRC or YLD 
                      "PX_DIRTY_CLEAN",                 # market convention dirty or clean
                      "DAYS_TO_SETTLE",
                      "CALLABLE",
                      "MARKET_SECTOR_DES",
                      "INDUSTRY_SECTOR",
                      "INDUSTRY_GROUP",
                      "INDUSTRY_SUBGROUP")

bbDynamicDataFields <- c("IS_STILL_CALLABLE",
                        "RTG_MOODY",
                        "RTG_MOODY_WATCH",
                        "RTG_SP",
                        "RTG_SP_WATCH",
                        "RTG_FITCH",
                        "RTG_FITCH_WATCH")

bbHistoricDataFields <- c("PX_BID",
                          "PX_ASK",
                          #"PX_CLEAN_BID",
                          #"PX_CLEAN_ASK",
                          "PX_DIRTY_BID",
                          "PX_DIRTY_ASK",
                          #"ASSET_SWAP_SPD_BID",
                          #"ASSET_SWAP_SPD_ASK",
                          "LAST_PRICE",
                          #"SETTLE_DT",
                          "YLD_YTM_MID")


addBonds <- function(cCode, addDate, inList) {
# this manually adds bonds to a country actives object when building a country or updating one
    doList <- inList
    addList <- switch(cCode, 
        "IT" = list("EJ679466 Corp" = c("2013-05-17", "2099-01-01"),
                    "EJ770441 Corp" = c("2013-07-31", "2099-01-01"), 
                    "EJ826136 Corp" = c("2013-09-12", "2099-01-01"), 
                    "EJ873702 Corp" = c("2013-10-10", "2099-01-01"), 
                    "EK017198 Corp" = c("2013-01-09", "2099-01-01"), 
                    "EK048427 Corp" = c("2014-01-29", "2099-01-01"), 
                    "EK093352 Corp" = c("2014-02-27", "2099-01-01"), 
                    "EK274354 Corp" = c("2014-05-16", "2099-01-01"), 
                    "EK324067 Corp" = c("2014-06-13", "2099-01-01"), 
                    "EK352661 Corp" = c("2014-06-26", "2099-01-01"),
                    "EK457296 Corp" = c("2014-08-26", "2099-01-01"),
                    "EK699416 Corp" = c("2015-01-16", "2099-01-01"),
                    "EK624270 Corp" = c("2014-11-05", "2099-01-01"),
                    "EJ807144 Corp" = c("2013-08-29", "2099-01-01")),
                    #"EJ6263705 Corp" = c("2013-04-10", "2099-01-01"))
        "SP" = list("EJ748178 Corp" = c("2013-07-10", "2099-01-01"), 
                    "EJ739412 Corp" = c("2013-07-02", "2099-01-01"),
                    "EJ941587 Corp" = c("2013-11-21", "2099-01-01"),
                    "EK010043 Corp" = c("2014-01-08", "2099-01-01"),
                    "EK040749 Corp" = c("2014-01-23", "2099-01-01"),
                    "EJ873876 Corp" = c("2013-10-10", "2099-01-01"),
                    "EK328970 Corp" = c("2014-06-13", "2099-01-01"),
                    "EK492047 Corp" = c("2014-09-18", "2099-01-01"),
                    "EK708195 Corp" = c("2015-01-20", "2099-01-01"),
                    "EK361187 Corp" = c("2014-07-03", "2099-01-01")), # new 30 year
        "NE" = list("EJ498904 Corp" = c("2013-01-07", "2099-01-01"),
                    "EK041463 Corp" = c("2014-02-18", "2099-01-01"),
                    "EK013840 Corp" = c("2014-01-10", "2099-01-01")),
        "BE" = list("EK023398 Corp" = c("2014-01-15", "2099-01-01")),
        "FR" = list("EJ609837 Corp" = c("2013-03-27", "2099-01-01"),
                    "EK034658 Corp" = c("2014-01-22", "2099-01-01"),
                    "EJ913944 Corp" = c("2013-03-27", "2099-01-01")),
        "DE" = list("EJ808025 Corp" = c("2013-09-03", "2099-01-01"),
                    "EC215301 Corp" = c("2005-01-01", "2099-01-01")))
    lapply(names(addList), function(x) if((addDate >= addList[[x]][1]) & (addDate <= addList[[x]][2])) {
        if(!(x %in% inList)) {
            doList <<- append(doList, x)
            flushprint(paste("manually adding", x))
        }
    })
    return(doList)
}


getActives <- function(cCode, test = FALSE, upFrom = NULL) {
# this function is going to get all the data out of bloomberg that we need for a
# country, and update it if ncessary
    # list of bonds by country that should NOT be included...
    outbonds <- list(FR = paste(c("GG711526", "EJ284137", "EJ2841371"), "Corp"), 
                     DE = paste(c("ZZ207104", "ZZ207101"), "Corp"), 
                     BE = paste(c("EH210510", "EC565224"), "Corp"))
    flushprint(cCode)
    if (test == TRUE) startDate <- as.Date("2014-03-15") else startDate <- histStartDate  # if we are testing
    # first get all the curve members for history
    wdays <- wdaylist(startDate, Sys.Date()) # create the list of working days from startdate
    actives <- lapply(wdays, function(y) {
        x <- unlist(bds(conn, BBcurveIDs[cCode], "CURVE_MEMBERS", override_fields = "CURVE_DATE",
            override_values = format(y, "%Y%m%d")))
        if(is.null(x)) {   # try again because sometimes bloomberg gives a null
            alarm() # beep
            flushprint("!! TRYING AGAIN !!")
            x <- unlist(bds(conn, BBcurveIDs[cCode], "CURVE_MEMBERS", override_fields = "CURVE_DATE",
            override_values = format(y, "%Y%m%d")))
        }
        if(length(outbonds[[cCode]]) > 0) {         # remove outbonds
            nullret <- sapply(outbonds[[cCode]], function(outbond) {
                if(outbond %in% x) {
                    x <<- x[-index(x)[!is.na(match(x, outbond))]]
                }
            })
        }
        flushprint(y)
        names(x) <- NULL
        flushprint(x)
        x <- x[sapply(strsplit(x, " "), "[[", 2) == "Corp"] # take out non corps
        x <- addBonds(cCode, y, x) # some bonds need manually to be added
        return(x)
    })
    save(actives, file = paste(dataplace, cCode, "actives.dat", sep = "")) # save the file
    uniqueActives <- unique(unlist(actives)) # there will be puhlenty duplicates. Get rid of them
    staticData <- bdp(conn, uniqueActives, bbStaticDataFields)
    uniqueActives <- uniqueActives[staticData[, "COUPON"] != 0]
    staticData <- staticData[staticData[, "COUPON"] != 0, ]
    # now check that staticData is unique
    if(length(staticData$ID_ISIN) != length(unique(staticData$ID_ISIN))) {
        flushprint("non unique bonds")
        browser()
    }
    actives <- lapply(actives, function(x) x[x %in% uniqueActives]) # kill zero coupons and tbills
    actives <- lapply(1:length(actives), function(i) {       # now kill actives with out of range maturities
        curActives <- actives[[i]]
        curDay <- wdays[i]
        mats <- staticData[curActives, "MATURITY"]
        mats <- (as.Date(mats) - curDay) / 365
        curActives <- curActives[(mats >= minMaturity) & (mats <= maxMaturity)]
        return(curActives)
    })
    return(actives)
}


bbGetCountry <- function(cCode, test = FALSE, upFrom = NULL) {
# this function is going to get all the data out of bloomberg that we need for a
# country, and update it if ncessary
    # list of bonds by country that should NOT be included...
    outbonds <- list(FR = paste(c("GG711526", "EJ284137", "EJ2841371"), "Corp"), 
                     DE = paste(c("ZZ207104", "ZZ207101"), "Corp"), 
                     BE = paste(c("EH210510", " Corp")))
    flushprint(cCode)
    if (test == TRUE) startDate <- as.Date("2010-01-01") else startDate <- histStartDate  # if we are testing
    # first get all the curve members for history
    wdays <- wdaylist(startDate, Sys.Date()) # create the list of working days from startdate
    actives <- lapply(wdays, function(y) {
        x <- unlist(bds(conn, BBcurveIDs[cCode], "CURVE_MEMBERS", override_fields = "CURVE_DATE",
            override_values = format(y, "%Y%m%d")))
        if(is.null(x)) {   # try again because sometimes bloomberg gives a null
            alarm() # beep
            flushprint("!! TRYING AGAIN !!")
            x <- unlist(bds(conn, BBcurveIDs[cCode], "CURVE_MEMBERS", override_fields = "CURVE_DATE",
            override_values = format(y, "%Y%m%d")))
        }
        if(length(outbonds[[cCode]]) > 0) {         # remove outbonds
            nullret <- sapply(outbonds[[cCode]], function(outbond) {
                if(outbond %in% x) {
                    x <<- x[-index(x)[!is.na(match(x, outbond))]]
                }
            })
        }
        flushprint(y)
        names(x) <- NULL
        flushprint(x)
        x <- x[sapply(strsplit(x, " "), "[[", 2) == "Corp"] # take out non corps
        x <- addBonds(cCode, y, x) # some bonds need manually to be added
        return(x)
    })
    save(actives, file = paste("data/", cCode, "actives.dat", sep = "")) # save the file
    uniqueActives <- unique(unlist(actives)) # there will be puhlenty duplicates. Get rid of them
    #!!!!!!!!!!! this is where we must re-engineer the bond chain to get new bonds in    
    #referenceBond <- paste(strsplit(last(last(actives)[[1]]), " ")[[1]][[1]], "Govt") # get a reference bond for getting the bond chain
    #bondChain <- bds(conn, referenceBond, "bond_chain")[, 1] # get the current bond chain
    #!!!!!!!!!!! this is where it ends
    # now get the unchanging bond data
    staticData <- bdp(conn, uniqueActives, bbStaticDataFields)
    uniqueActives <- uniqueActives[staticData[, "COUPON"] != 0]
    staticData <- staticData[staticData[, "COUPON"] != 0, ]
    # now check that staticData is unique
    if(length(staticData$ID_ISIN) != length(unique(staticData$ID_ISIN))) {
        flushprint("non unique bonds")
        browser()
    }
    actives <- lapply(actives, function(x) x[x %in% uniqueActives]) # kill zero coupons and tbills
    actives <- lapply(1:length(actives), function(i) {       # now kill actives with out of range maturities
        curActives <- actives[[i]]
        curDay <- wdays[i]
        mats <- staticData[curActives, "MATURITY"]
        mats <- (as.Date(mats) - curDay) / 365
        curActives <- curActives[(mats >= minMaturity) & (mats <= maxMaturity)]
        return(curActives)
    })
    names(actives) <- wdays
    # now get the cash flowdata
    cfData <- lapply(uniqueActives, function(x) {
        bds(conn, x, "DES_CASH_FLOW_ADJ", override_fields = "SETTLE_DT", 
            override_values = format(as.Date(staticData[x, "FIRST_SETTLE_DT"]), "%Y%m%d"))
    })
    # Okay we have a major problem with data returns from the next line
    historicData <- lapply(bbHistoricDataFields, function(x) {
        bbdh(uniqueActives, flds = x, startDate = startDate)
    })
    names(historicData) <- bbHistoricDataFields   # put the names in otherwise we get a numbered list
    # now get the settle dates
    allDates <- as.Date(index(historicData$LAST_PRICE)) # all the dates we will find settlement dates for for all bonds. No posix
    # now do the off the run bonds
    dates <- names(actives)
    offruns <- lapply(dates, function(x) {
        currentDate <- as.Date(x)
        bondsLive <- subset(staticData, ((as.Date(FIRST_SETTLE_DT) < currentDate) & 
            ((as.Date(MATURITY) - as.difftime(offRunMinMat * 52, unit = "weeks")) > currentDate)), select = c("FIRST_SETTLE_DT", "MATURITY"))
        return(setdiff(rownames(bondsLive), actives[[x]]))
    })
    offruns <- lapply(1:length(offruns), function(i) {       # now kill offruns with out of range maturities
        curOffruns <- offruns[[i]]
        curDay <- wdays[i]
        mats <- staticData[curOffruns, "MATURITY"]
        mats <- (as.Date(mats) - curDay) / 365
        curOffruns <- curOffruns[(mats >= minMaturity) & (mats <= maxMaturity)]
        return(curOffruns)
    })
    names(offruns) <- dates
    uniqueActives <- sapply(strsplit(uniqueActives, " "), "[[", 1) # whipped out the corp bid
    rownames(staticData) <- sapply(strsplit(rownames(staticData), " "), function(x) x[[1]]) # take out corp bita
    names(cfData) <- uniqueActives
    offruns <- lapply(offruns, function(x) sapply(strsplit(x, " "), "[[", 1))
    actives <- lapply(actives, function(x) sapply(strsplit(x, " "), "[[", 1))
    cData <- list(actives = actives, offruns = offruns, staticData = staticData, cfData = cfData, 
        historicData = historicData) # put it into the global environment
    # save in case weget a problem later
    flushprint("now creating couponBonds objects")
    # k now do the couponBonds
    dynOn <- lapply(names(cData$actives), function(x) createCouponBonds(cCode, x, "on", cData = cData))
    dynOff <- lapply(names(cData$actives), function(x) createCouponBonds(cCode, x, "off", cData = cData))
    dynBoth <- lapply(names(cData$actives), function(x) createCouponBonds(cCode, x, "both", cData = cData))
    names(dynOn) <- names(actives)
    names(dynOff) <- names(actives)
    names(dynBoth) <- names(actives)
    cData <- list(actives = actives, offruns = offruns, staticData = staticData, cfData = cfData, 
        historicData = historicData) # put it into the global environment
    cData$dynOn <- dynOn
    cData$dynOff <- dynOff
    cData$dynBoth <- dynBoth
    #temp save for debugging
    save(cData, file = paste(cCode, "data.tmp", sep = ""))
    #now do the Nelson Siegels and Svenssons
    flushprint("Doing both...cs")
    csBoth <- lapply(dynBoth, function(dynOne) tryCatch(estim_cs(dynOne, cCode), 
        error = function(ee) {
            flushprint("cs error")
            return(NA)
        }))
    csBoth <- tryCatch(lightns(csBoth), error = function(ee) NA)
    if(!is.na(csBoth)) names(csBoth) <- names(actives)
    cData$csBoth <- csBoth
    flushprint("Doing dl....")
    dl <- lightns(dons(dynOn, meth = "dl", lambdayrs = dlLambda[[cCode]]))
    names(dl) <- names(actives)
    cData$dl <- dl
    flushprint("Doing ns....")
    ns <- lightns(dons(dynOn, meth = "ns", taucon = list(c(nsTau))))
    names(ns) <- names(actives)
    cData$ns <- ns
    flushprint("Doing sv....")
    sv <- try(lightns(dons(dynOn, meth = "sv", taucon = list(svTau[[cCode]]))))
    names(sv) <- names(actives)
    cData$sv <- sv
    flushprint("Doing offrun pricing....")
    nsOff <- lapply(1:length(dynOff), function(x) priceOff(dynOff[[x]], ns[[x]]))
    names(nsOff) <- names(actives)
    cData$nsOff <- nsOff
    svOff <- lapply(1:length(dynOff), function(x) priceOff(dynOff[[x]], sv[[x]]))
    names(svOff) <- names(actives)
    cData$svOff <- svOff
    dlOff <- lapply(1:length(dynOff), function(x) priceOff(dynOff[[x]], dl[[x]]))
    names(dlOff) <- names(actives)
    cData$dlOff <- dlOff
    flushprint("Doing both...dl")
    dlBoth <- lightns(dons(dynBoth, meth = "dl", lambda = dlLambda[[cCode]]))
    names(dlBoth) <- names(actives)
    cData$dlBoth <- dlBoth
    flushprint("Doing both...ns")
    nsBoth <- lightns(dons(dynBoth, meth = "ns", taucon = list(c(nsTau))))
    names(nsBoth) <- names(actives)
    cData$nsBoth <- nsBoth
    flushprint("Doing both...sv")
    svBoth <- lightns(dons(dynBoth, meth = "sv", taucon = list(c(svTau[[cCode]]))))
    names(svBoth) <- names(actives)
    cData$svBoth <- svBoth
    flushprint("Doing both...asv")
    asvBoth <- lightns(dons(dynBoth, meth = "asv", taucon = list(c(svTau[[cCode]]))))
    names(asvBoth) <- names(actives)
    cData$asvBoth <- asvBoth
    assign(paste(cCode, "data", sep = ""), cData, pos = 1) # put it into the global environment
    saveCountry(cCode) # save the country
}


upCountry <- function(cCode, minmat = minMaturity) {
# does a full check to update a country data structure from the last time it was updated
    cData <- get(paste(cCode, "data", sep = ""))      # get the country data
    startDate <- last(names(cData$actives))
    wdays <- wdaylist(as.Date(startDate), Sys.Date()) # create the list of working days from startdate
    actives <- lapply(wdays, function(x) {            # active
        y <- unlist(bds(conn, BBcurveIDs[cCode], "CURVE_MEMBERS", override_fields = "CURVE_DATE",
            override_values = format(x, "%Y%m%d")))
        y <- addBonds(cCode, x, y)
        y1 <- sapply(strsplit(y, " "), "[[", 1)
        y2 <- sapply(strsplit(y, " "), "[[", 2)
        y1 <- y1[y2 == "Corp"]
        names(y1) <- NULL
        return(y1)
    })
    uniqueActives <- unique(unlist(actives))
    coupons <- bdp(conn, paste(uniqueActives, "Corp"), "COUPON")
    uniqueActives <- uniqueActives[coupons != 0]  # take out zero coupons
    actives <- lapply(actives, function(x) x[x %in% uniqueActives])
    names(actives) <- wdays
    newBondsEst <- uniqueActives[!(uniqueActives %in% names(cData$cfData))] # any new bonds...estimate because bb name may duplicate
    if(length(newBondsEst) > 0) {
        newBondsISIN <- bdp(conn, paste(newBondsEst, "Corp"), "ID_ISIN")[, 1] # get their ISINs 
        newBonds <- newBondsEst[!newBondsISIN %in% bb2isin(cCode, names(cData$cfData))] # and see if any duplicates
    } else newBonds <- newBondsEst
    if(length(newBonds) != length(newBondsEst)) {
        flushprint(paste(cCode, " new bonds but looks like duplicate bonds"))
        flushprint("newBondsEst")
        flushprint(newBondsEst)
        flushprint("newBonds")
        flushprint(newBonds)
    }
    #newBonds <- NULL #!!!!!!!!!!!!!!!!!!!!!!!!
    if(length(newBonds) > 0) {    # if there are new bonds we need to get cashflow, static, and historic data
        flushprint("NEW BONDS!!")
        flushprint(newBonds)
        newBondsCorp <- paste(newBonds, "Corp") # we are going to get data so get the corp in
        staticData <- bdp(conn, newBondsCorp, bbStaticDataFields) # get the staticData
        cfData <- lapply(newBondsCorp, function(x) {
            bds(conn, x, "DES_CASH_FLOW_ADJ", override_fields = "SETTLE_DT", 
                override_values = format(as.Date(staticData[x, "FIRST_SETTLE_DT"]), "%Y%m%d"))
        })
        historicData <- lapply(bbHistoricDataFields, function(x) {
            pxdata <- bbdh(newBondsCorp, flds = x,  
                startDate = first(names(cData$actives))) # we get data all the way back to the same dates in cData
            colnames(pxdata) <- newBonds
            pxdata <- xts(pxdata, 
                order.by = as.Date(as.POSIXct(index(pxdata), tz = "Europe/London"), tz = "Europe/London")) # remove POSIXct class
            return(pxdata)
        })
        names(historicData) <- bbHistoricDataFields
        # now fix the name
        names(cfData) <- newBonds
        rownames(staticData) <- sapply(strsplit(rownames(staticData), " "), function(x) x[[1]]) # take out corp bits
        # and now merge them
        cData$cfData <- append(cData$cfData, cfData)
        cData$staticData <- rbind(cData$staticData, staticData)
        cData$historicData <- lapply(names(cData$historicData), function(x) {    # apply to each of PX BID etc
            flushprint("entering!")
            cbinData <- cbind(cData$historicData[[x]], 
                first(historicData[[x]], nrow(cData$historicData[[x]]))) # cbind-em but only the first n rows
            return(cbinData)
        })
        names(cData$historicData) <- bbHistoricDataFields
    } 
    # so all done if we had new bonds now we do the generic stuff even without new bonds
    flushprint("getting new historic data")
    newHistoricData <- lapply(bbHistoricDataFields, function(x) {
        allBondNames <- colnames(cData$historicData[[1]]) # all the bonds we have ever had
        nonMaturedTest <- cData$staticData[allBondNames, "MATURITY"] >= startDate # which are not matured
        currBondNames <- allBondNames[nonMaturedTest] # so these are the ones we need to get
        bret <- tryCatch(bbdh(paste(currBondNames, "Corp"), flds = x, startDate = startDate), 
            error = function(w) return(rep(NA, length(wdays))))
        return(bret)
        })
    cDataNames <- names(cData$historicData) # save the names
    cData$historicData <- lapply(1:length(cData$historicData), function(x) { # now merge them
        oldData <- cData$historicData[[x]][-nrow(cData$historicData[[x]]), ]
        newData <- newHistoricData[[x]]
        datesData <- c(as.POSIXct(index(oldData)), as.POSIXct(index(newData)))
        as.xts(rbind.fill(as.data.frame(oldData), as.data.frame(newData)), 
             order.by = as.Date(datesData, tz = "Europe/London")) # specifiy Europe/London otherwise tz bugs
        })
    names(cData$historicData) <- cDataNames # restore the names 
    # now we gotta do ye olde offruns
    offruns <- lapply(1:length(wdays), function(x) {   
        currentDate <- as.Date(wdays[x])
        bondsLive <- subset(cData$staticData, ((as.Date(FIRST_SETTLE_DT) < currentDate) &      
            ((as.Date(MATURITY) - as.difftime(offRunMinMat * 52, unit = "weeks")) > currentDate)), 
                select = c("FIRST_SETTLE_DT", "MATURITY"))
        return(setdiff(rownames(bondsLive), actives[[x]]))
    })
    names(offruns) <- wdays
    cData$actives <- append(cData$actives[-length(cData$actives)], actives)  # stick new actives in
    cData$offruns <- append(cData$offruns[-length(cData$offruns)], offruns)  # stick new offruns in
    assign(paste(cCode, "data", sep = ""), cData, pos = 1) # assign now so we can do dynCBs
    # now we do the new dyncbs
    flushprint("updating dynCouponBonds")
    dynOn <- lapply(wdays, function(x) createCouponBonds(cCode, as.character(x), "on", minmat = minmat))
    names(dynOn) <- wdays
    dynOff <- lapply(wdays, function(x) createCouponBonds(cCode, as.character(x), "off", minmat = minmat))
    names(dynOff) <- last(wdays, length(dynOff)) # because there may be less here
    dynBoth <- lapply(wdays, function(x) createCouponBonds(cCode, as.character(x), "both", minmat = minmat))
    names(dynBoth) <- wdays
    cData$dynOn <- append(cData$dynOn[-length(cData$dynOn)], dynOn)  # stick new offruns in
    cData$dynOff <- append(cData$dynOff[-length(cData$dynOff)], dynOff)  # stick new offruns in
    cData$dynBoth <- append(cData$dynBoth[-length(cData$dynBoth)], dynBoth)  # stick new offruns in
    # now we do the svs and ns
    flushprint("updating dl")
    dl <- lightns(dons(dynOn, meth = "dl", lambdayrs = dlLambda[[cCode]], snow = FALSE, pcores = 1))
    names(dl) <- wdays
    flushprint("updating ns")
    ns <- lightns(dons(dynOn, meth = "ns", taucon = list(nsTau), snow = FALSE, pcores = 7))
    names(ns) <- wdays
    flushprint("updating sv")
    sv <- lightns(dons(dynOn, meth = "sv", taucon = list(svTau[[cCode]]), snow = FALSE, pcores = 7))
    names(sv) <- wdays
    flushprint("updating dlOff")
    dlOff <- lapply(1:length(dl), function(x) priceOff(dynOff[[x]], dl[[x]]))
    names(dlOff) <- wdays
    flushprint("updating nsOff")
    nsOff <- lapply(1:length(ns), function(x) priceOff(dynOff[[x]], ns[[x]]))
    names(nsOff) <- wdays
    flushprint("updating svOff")
    svOff <- lapply(1:length(sv), function(x) priceOff(dynOff[[x]], sv[[x]]))
    names(svOff) <- wdays
    flushprint("updating dlBoth")
    dlBoth <- lightns(dons(dynBoth, meth = "dl", lambdayrs = dlLambda[[cCode]], snow = FALSE, pcores = 7))
    names(dlBoth) <- wdays
    flushprint("updating nsBoth")
    nsBoth <- lightns(dons(dynBoth, meth = "ns", taucon = list(nsTau), snow = FALSE, pcores = 7))
    names(nsBoth) <- wdays
    flushprint("updating svBoth")
    svBoth <- lightns(dons(dynBoth, meth = "sv", taucon = list(svTau[[cCode]]), snow = FALSE, pcores = 7))
    names(svBoth) <- wdays
    flushprint("updating asvBoth")
    asvBoth <- lightns(dons(dynBoth, meth = "asv", taucon = list(svTau[[cCode]]), snow = FALSE, pcores = 7))
    names(asvBoth) <- wdays
    flushprint("updating csBoth")
    csBoth <- lightns(lapply(dynBoth, function(x) estim_cs(x, cCode)))
    cData$dl <- append(cData$dl[-length(cData$dl)], dl)  # stick new dl in  
    cData$ns <- append(cData$ns[-length(cData$ns)], ns)  # stick new ns in  
    cData$sv <- append(cData$sv[-length(cData$sv)], sv)  # stick new sv in  
    cData$dlOff <- append(cData$dlOff[-length(cData$dlOff)], dlOff)  # stick new nsOff in
    cData$nsOff <- append(cData$nsOff[-length(cData$nsOff)], nsOff)  # stick new nsOff in
    cData$svOff <- append(cData$svOff[-length(cData$svOff)], svOff)  # stick new svOff in
    cData$dlBoth <- append(cData$dlBoth[-length(cData$dlBoth)], dlBoth)  # stick new nsBoth in  
    cData$nsBoth <- append(cData$nsBoth[-length(cData$nsBoth)], nsBoth)  # stick new nsBoth in  
    cData$svBoth <- append(cData$svBoth[-length(cData$svBoth)], svBoth)  # stick new svBoth in  
    cData$asvBoth <- append(cData$asvBoth[-length(cData$asvBoth)], asvBoth)  # stick new svBoth in  
    cData$csBoth <- append(cData$csBoth[-length(cData$csBoth)], csBoth)  # stick new svBoth in  
    #now we return the new cData!
    assign(paste(cCode, "data", sep = ""), cData, pos = 1)
    flushprint("done")
}



id <- function(cCodes = cl, minmat = minMaturity, quick = FALSE, fromongo = FALSE, updata = TRUE) { 
#id means "IntraDay"
# this will update all the intraday prices
    cDatas <- lapply(cCodes, function(x) get(paste(x, "data", sep = ""))) # get all the country data
    names(cDatas) <- cCodes
    today <- Sys.Date()
    lapply(cDatas, function(x) {          # check if they are up to date
        if(last(names(x$actives)) != Sys.Date()) {
            cat("Country", x$ns[[1]]$names, "is not up to date \n")
            cat("You must run kick daily before running id")
            stop()
        }
    })
    if(updata) { # if we need to get new data (sometimes no if bberg data temporarily bad)
        flushprint("getting Bloomberg data")
        upBonds <- lapply(cDatas, function(x) c(last(x$offruns), last(x$actives)))
        if(fromongo) {  # if we are coming from Monb
            for(cCode in cCodes) {
                flushprint(paste("updating from mongo:", cCode))
                # get all the bonds for this country code
                cData <- cDatas[[cCode]]
                bondcodes <- c(last(cData$actives)[[1]], last(cData$offruns)[[1]])
                bondisins <- bb2isin(cCode, bondcodes)
                bondlabels <- isinLabel(cCode, bondisins)
                last_prices <- bbmongolatest(paste(bondisins, "Corp"))
                if(any(last_prices[, "close"] < 20)) {
                    flushprint ("bad bond price")
                    browser()
                }
                if(nrow(last_prices) < length(bondisins)) {
                    flushprint("Cannot get live data from mongodb for the following bonds")
                    notgot  <- bondlabels[!(paste(bondisins, "Corp") %in% rownames(last_prices))]
                    notgotisins  <- bondisins[!(paste(bondisins, "Corp") %in% rownames(last_prices))]
                    flushprint(notgot)
                    flushprint("Would you like to add them? [y/n]")
                    if(as.character(toupper(scan(, what = character()))) == "Y") {
                        for (ng in notgotisins) {
                            browser()
                            bbmongoadd(paste(ng, "Corp"), bbcheck = FALSE)
                        }
                        flushprint("Okay added them. You should wait 30 seconds before continuing or")
                        flushprint("check the capture tool to make sure they are now being subscribed")
                        flushprint("Press enter to continue......")
                        scan(n = 1)
                    } else {
                        flushprint("stopping")
                        stop()
                    }
                }
                framerows <- nrow(cData$historicData$LAST_PRICE) # number of rows
                for(bcn in 1:length(bondcodes)) { # now put all the data in 
                    bondcode = bondcodes[bcn]
                    bondisin = bondisins[bcn]
                    bondprice = last_prices[paste(bondisin, "Corp"), "close"]
                    cDatas[[cCode]]$historicData$LAST_PRICE[framerows, bondcode] <- bondprice
                }
            }
        } else {   # get data from Bloomberg if we don't get from mongo
            upData <- lapply(bbHistoricDataFields, function(x) {
                bdata <- bdp(conn, paste(unlist(upBonds), "Corp"), x)
                colnames(bdata) <- lapply(strsplit(colnames(bdata), " "), "[[", 1)
                return(bdata)
            })
            flushprint("inserting bloomberg data into country structures")
            cDatas <- lapply(cDatas, function(x) {
                x$historicData <- lapply(1:length(x$historicData), function(y) {
                    upNames <- colnames(x$historicData[[y]])
                    x$historicData[[y]][nrow(x$historicData[[y]]), upNames] <- upData[[y]][upNames, ]
                    return(x$historicData[[y]])
                })
                names(x$historicData) <- bbHistoricDataFields
                return(x)
            })
        }
    }
    flushprint("Updating curves")
    for(x in 1:length(cDatas)) {
        flushprint(cCodes[x])
        len <- length(cDatas[[x]]$actives)  
        dynOn <- createCouponBonds(cCodes[x], as.character(today), "on", cData = cDatas[[x]], minmat = minmat, useLAST_PRICE = fromongo)
        dynOff <- createCouponBonds(cCodes[x], as.character(today), "off", cData = cDatas[[x]], minmat = minmat, useLAST_PRICE = fromongo)
        dynBoth <- createCouponBonds(cCodes[x], as.character(today), "both", cData = cDatas[[x]], minmat = minmat, useLAST_PRICE = fromongo)
        len <- length(cDatas[[x]]$actives)
        #browser()
        dl <- try(lightns(dons(list(dynOn), meth = "dl", lambdayrs = dlLambda[[cCodes[x]]], 
            snow = FALSE, pcores = 1)))
        if("try-error" %in% class(dl)) {
            flushprint("error in dl")
            browser()
        }
        dlBoth <- try(lightns(dons(list(dynBoth), meth = "dl", lambdayrs = dlLambda[[cCodes[x]]], 
            snow = FALSE, pcores = 1)))
        if("try-error" %in% class(dlBoth)) {
            flushprint("error in dlBoth")
            browser()
        }
        if(quick) {
            ns <- lightns(dons(list(dynOn), meth = "ns", taucon = list(nsTau),
                sparam = t(cDatas[[x]]$ns[[len]]$startparam), 
                snow = FALSE, pcores = 1)) # start param for speed
            sv <- try(lightns(dons(list(dynOn), meth = "sv", taucon = list(svTau[[cCodes[x]]]),
                sparam = t(cDatas[[x]]$sv[[len]]$startparam), 
                snow = FALSE, pcores = 1)))
            if("try-error" %in% class(sv)) { # if error
                flushprint("error when using previous start parameters in sv. Trying with null start parameters....")
                sv <- try(lightns(dons(list(dynOn), meth = "sv", taucon = list(svTau[[cCodes[x]]]),
                    snow = FALSE, pcores = 1)))
            }
            nsBoth <- lightns(dons(list(dynBoth), meth = "ns", taucon = list(nsTau),
                sparam = t(cDatas[[x]]$nsBoth[[len]]$startparam), 
                snow = FALSE, pcores = 1)) # start param for speed
            svBoth <- try(lightns(dons(list(dynBoth), meth = "sv", taucon = list(svTau[[cCodes[x]]]),
                sparam = t(cDatas[[x]]$svBoth[[len-1]]$startparam), 
                snow = FALSE, pcores = 1)))
            if("try-error" %in% class(svBoth)) {
                flushprint("error when using previous start parameters in svBoth. Trying with null start parameters....")
                svBoth <- try(lightns(dons(list(dynBoth), meth = "sv", taucon = list(svTau[[cCodes[x]]]),
                    snow = FALSE, pcores = 1)))
            }
            asvBoth <- try(lightns(dons(list(dynBoth), meth = "asv", taucon = list(svTau[[cCodes[x]]]),
                sparam = t(cDatas[[x]]$svBoth[[len-1]]$startparam), 
                snow = FALSE, pcores = 1)))
            if("try-error" %in% class(asvBoth)) {
                flushprint("error when using previous start parameters in asvBoth. Trying with null start parameters....")
                asvBoth <- try(lightns(dons(list(dynBoth), meth = "asv", taucon = list(svTau[[cCodes[x]]]),
                    snow = FALSE, pcores = 1)))
            }
        } else {
            ns <- lightns(dons(list(dynOn), meth = "ns", taucon = list(nsTau),
                snow = FALSE, pcores = 1)) 
            sv <- lightns(dons(list(dynOn), meth = "sv", taucon = list(svTau[[cCodes[x]]]),
                snow = FALSE, pcores = 1))
            dlBoth <- lightns(dons(list(dynBoth), meth = "dl", lambdayrs = dlLambda[[cCodes[x]]], 
                snow = FALSE, pcores = 1))
            nsBoth <- lightns(dons(list(dynBoth), meth = "ns", taucon = list(nsTau),
                snow = FALSE, pcores = 1))
            svBoth <- lightns(dons(list(dynBoth), meth = "sv", taucon = list(svTau[[cCodes[x]]]),
                snow = FALSE, pcores = 1))
            asvBoth <- lightns(dons(list(dynBoth), meth = "asv", taucon = list(svTau[[cCodes[x]]]),
                snow = FALSE, pcores = 1))
        }

        #asvBoth <- try(lightns(dons(list(dynBoth), meth = "asv", taucon = list(svTau[[cCodes[x]]]),
        #    snow = FALSE, pcores = 1)))
        #csBoth <- lightns(lapply(dynBoth, function(y) {
        #    class(y) <- "couponbonds"
        #    estim_cs(y, cCodes[x])
        #}))
        dlOff <- list(priceOff(dynOff, dl[[1]]))
        nsOff <- list(priceOff(dynOff, ns[[1]]))
        svOff <- list(priceOff(dynOff, sv[[1]]))
        cDatas[[x]]$dynOn[len] <- list(dynOn)
        cDatas[[x]]$dynOff[len] <- list(dynOff)
        cDatas[[x]]$dynBoth[len] <- list(dynBoth)
        cDatas[[x]]$dl[len] <- dl
        cDatas[[x]]$ns[len] <- ns
        cDatas[[x]]$sv[len] <- sv
        cDatas[[x]]$dlOff[len] <- dlOff
        cDatas[[x]]$nsOff[len] <- nsOff
        cDatas[[x]]$svOff[len] <- svOff
        cDatas[[x]]$dlBoth[len] <- dlBoth
        cDatas[[x]]$nsBoth[len] <- nsBoth
        cDatas[[x]]$svBoth[len] <- svBoth
        cDatas[[x]]$asvBoth[len] <- asvBoth
        #cDatas[[x]]$csBoth[len] <- csBoth
    }
    for(x in 1:length(cCodes)) assign(paste(cCodes[x], "data", sep = ""), cDatas[[x]], pos = 1)
    upBondFactors(cCodes) # update bond factors
    if(fromongo) {
        nowirs <- bbmongolatest(eurIRStickers)[, "close"]
    } else {
        nowirs <- bdp(conn, eurIRStickers, "LAST PRICE")[, 1]
    }
    eurirs[nrow(eurirs), ] <<- nowirs # update irs
    flushprint(Sys.time())
}

checkIntegrity <- function(cCodes) {
# this will count the various bits of a country structure to make sure their numbers are the same
# used mainly on country load
    for(cCode in cCodes) {
        cData <- get(paste(cCode, "data", sep = ""))
        # now check integrity
        lengths <- sapply(cData, length)
        lengths <- c(lengths, sapply(cData$historicData, nrow))
        noCheck <- c("staticData", "cfData", "historicData")
        lengthsCheck <- lengths[setdiff(names(lengths), noCheck)]
        if(!all(lengthsCheck == lengthsCheck[[1]])) {
            flushprint(cCode)
            flushprint("!!! Possible Integrity Issue !!!")
            flushprint(lengthsCheck)
            scan()
        } else flushcat(paste(cCode, "integrity OK\n"))
    }
}


loadCountry <- function(cCode) {
# loads country dataa from file
    load(paste(dataplace, cCode, "data.dat", sep = ""), envir = globalenv())
    checkIntegrity(cCode)
    # now add to country list
    if(!exists("cl")) assign("cl", cCode, pos = 1) else if(is.na(match(cCode, cl))) cl <<- c(cl, cCode)
}


saveCountry <- function(cCode) {
# saves a country data to file
    save(list = paste(cCode, "data", sep = ""), file = paste(dataplace, cCode, "data.dat", sep = ""))
}


offRuns <- function(cCode) {
# will show me for a country which off the run bonds are not in the current actives
    cdata <- get(paste(cCode, "data", sep = ""))
    dates <- names(cdata$actives)
    oruns <- lapply(dates, function(x) {
        currentDate <- as.Date(x)
        bondsLive <- subset(cdata$staticData, ((as.Date(FIRST_SETTLE_DT) < currentDate) & 
            ((as.Date(MATURITY) - as.difftime(offRunMinMat * 52, unit = "weeks")) > currentDate)), select = c("FIRST_SETTLE_DT", "MATURITY"))
        ordf <- data.frame(setdiff(rownames(bondsLive), cdata$actives[[x]])) 
        colnames(ordf) <- "offruns"
        return(ordf)
    })
    names(oruns) <- dates
    return(oruns)
}


rollConvention <- function(cCode, dates) {
# returns the date rolling convention for the country
    return(modFol(cCode, dates))
}

settleDays <- function(cCode, bondlist, today, cData = NULL) {
# settlement day count for each bond - inputs and outputs bloomberg bond IDs
    today <- as.Date(today) # jsut in case
    if(is.null(cData)) cData <- get(paste(cCode, "data", sep = "")) 
    sdays <- cData$staticData[bondlist, "DAYS_TO_SETTLE"] 
    if(cCode == "FR") {
        # if we're after 2 april 2012, btans settle T+3 also
        isBTAN <- substr(cData$staticData[bondlist, "SHORT_NAME"], 1, 4) == "BTAN"
        sdays <- ifelse(isBTAN, 1, 3)
        if(today >= as.Date("2012-04-02")) sdays <- rep(3, length(sdays)) 
    }
    if((cCode %in% c("TE", "DE", "FR", "IT", "SP", "NE", "AT", "BE")) & (today >= as.Date("2014-10-06"))) {
        sdays <- rep(2, length(sdays))
    }
    sdates <- sapply(sdays, function(x) {
        daycount <- 0
        settleDate <- today
        while(daycount < x) {    # find today's settlement date
                settleDate <- settleDate + 1
                if (!((weekdays(settleDate) %in% c("Saturday", "Sunday")) 
                | (settleDate %in% cal[[cCode]]))) daycount <- daycount + 1
            }
        return(settleDate)
    })
    sdays <- data.frame(sdays, as.Date(sdates))
    rownames(sdays) <- bondlist
    return(sdays)
}


createCouponBonds <- function(cCode, dateString, onoff = "both", minmat = minMaturity, maxmat = maxMaturity, cData = NULL, 
                              zeroAccrued = FALSE, useLAST_PRICE = FALSE) {
# function to create a couonbonds object from the bberg data, onoff is whether onruns, or offruns or both
    if(is.null(cData)) cdata <- get(paste(cCode, "data", sep = "")) else cdata <- cData # get the data set
    today <- as.Date(dateString)
    goodbonds <- subset(cdata$staticData, COUPON != 0 & INFLATION_LINKED_INDICATOR == FALSE) # clean out zeros and tbills
    if(onoff == "off") {           
    goodbonds <- goodbonds[rownames(goodbonds) %in% cdata$offruns[[dateString]], ]
    } else if(onoff == "on") {
        goodbonds <- goodbonds[rownames(goodbonds) %in% cdata$actives[[dateString]], ]
    } else if(onoff == "both") {
        goodbonds <- rbind(goodbonds[rownames(goodbonds) %in% cdata$offruns[[dateString]], ],
                                goodbonds[rownames(goodbonds) %in% cdata$actives[[dateString]], ])
    } else {
        goodbonds <- goodbonds[inbonds, ]
    }
    # now kill the maturities lower than minmat(urity)
    goodbonds <- goodbonds[((as.Date(goodbonds$MATURITY) - today)/ 365) > minmat, ]
    if(!is.null(maxmat)) goodbonds <- goodbonds[((as.Date(goodbonds$MATURITY) - today) / 365) < maxmat, ]
    if(nrow(goodbonds) > 0) {
        stripnames <- rownames(goodbonds)
        pxbid <- cdata$historicData$PX_BID[today, stripnames]
        pxask <- cdata$historicData$PX_ASK[today, stripnames]
        pxdbid <- cdata$historicData$PX_DIRTY_BID[today, stripnames]
        pxdask <- cdata$historicData$PX_DIRTY_ASK[today, stripnames]
        if(useLAST_PRICE) {
            price <- as.numeric(cdata$historicData$LAST_PRICE[today, stripnames])
        } else {
            price <- as.numeric((pxbid + pxask) / 2)
        }
        if(zeroAccrued) accrued <- rep(0, length(price)) else accrued <- round(as.numeric(pxdbid - pxbid), 3)
        settleDates <- settleDays(cCode, rownames(goodbonds), today, cData = cData)
        cashflows <- lapply(rownames(goodbonds), function(x) {
            goodflows <- cdata$cfData[[x]][as.Date(cdata$cfData[[x]][, "Date"]) > settleDates[x, 2], ]
            principalsize <- goodflows[nrow(goodflows), "Principal"]
            #gfstipnames <- sapply(strsplit(rownames(goodflows), " "), function(x) x[1]) dunno if I need this
            isin <- rep(cdata$staticData[x, "ID_ISIN"], nrow(goodflows))
            cf <- apply(goodflows[, 2:3], 1, sum) / principalsize * 100
            dt <- rollConvention(cCode, as.Date(goodflows[, 1]))
            return(list(isin = isin, cf = cf, dt = dt))
        })
        isinvec <- unlist(lapply(cashflows, function(x) x$isin))
        cfvec <- as.numeric(unlist(lapply(cashflows, function(x) x$cf)))
        datevec <- as.Date(unlist(lapply(cashflows, function(x) x$dt))) # must redo as.Date as unlist kills dates
        govbonds <- list(ISIN = goodbonds$ID_ISIN, 
                         MATURITYDATE = as.Date(goodbonds$MATURITY),
                         ISSUEDATE = as.Date(goodbonds$FIRST_SETTLE_DT),
                         COUPONRATE = as.numeric(goodbonds$COUPON) / 100,
                         PRICE = price,
                         ACCRUED = accrued,
                         CASHFLOWS = list(ISIN = isinvec, CF = cfvec, 
                                          DATE = if (is.null(datevec)) NA else datevec),
                         TODAY = today)
        govbonds <- list(govbonds)
        names(govbonds) <- cCode
        class(govbonds) <- "couponbonds"
        # now check if this bond has good price data
        badBonds <- is.na(govbonds[[cCode]]$PRICE) | is.na(govbonds[[cCode]]$ACCRUED) # which bonds might be bad
        if(sum(badBonds) > 0) {
            badISINs <- govbonds[[cCode]]$ISIN[badBonds]
            flushprint(paste("**** Got bad bonds on", today, ":", badISINs))
            govbonds <- rm_bond(govbonds, cCode, badISINs)
        }
    } else govbonds <- NULL
    return(govbonds)
}

startRedis <- function(address = "192.168.1.30") {
    library(doRedis)
    registerDoRedis("jobs1", host = address)
    startLocalWorkers(n = 7, queue = "jobs1", address)
}

stopRedis <- function() {
    removeQueue("jobs1")
    redisClose()
}

dons <- function(lobj, numdo = NULL, pcores = 6, meth = "ns", taucon = list(nsTau), lambdayrs = NULL, 
                 sparam = NULL, snow = TRUE, rdis = FALSE) {
# this function will parallel run estim_nss code across a country"l" object
# optional numdo parameter does last numdo of them, pcores is how many cores 1 = no parallel processing
    if(!is.null(lambdayrs)) lambdayrs = (30 / lambdayrs) * 0.0609
    if(snow & rdis) {
        print("!!! cannot have doSNOW and doRedis at the same time")
        return()
    }
    if((pcores > 1) & (snow) ) {           # setup parallel processing with snow
        library("doSNOW")
        cl <- makeCluster(pcores)
		registerDoSNOW(cl)
    }
    if(meth == "cs") { # if we're using the estim_cs routine
        runfun <- function(cbobj) {  # function to run in parallel if cs
            tryCatch(estim_cs(cbobj, names(cbobj)), error = function(ee) NA)
        }
        if (!is.null(numdo)) doobj <- last(lobj, numdo) else doobj <- lobj
        if(pcores > 1) {
            nsret <- foreach(i = 1:length(doobj), .packages = "termstrc") %dopar% {
                runfun(doobj[[i]]) # doing it parallel
            }
            if(snow) stopCluster(cl)
        } else {
            nsret <- foreach(i = 1:length(doobj), .packages = "termstrc") %do% {
                runfun(doobj[[i]]) # doing it non parallel
            }
        }  
    } else { # if we're using the estim_nss routine
        runfun <- function(cbobj, met, tauc, lda, sp) {      # define the function to run parallel
            tryCatch(estim_nss(cbobj, names(cbobj), method = met, tauconstr = tauc, startparam = sp, lambda = lda), 
                error = function(ee) NA)
        }
        if (!is.null(numdo)) doobj <- last(lobj, numdo) else doobj <- lobj
        if(pcores > 1) {
            nsret <- foreach(i = 1:length(doobj), .packages = "termstrc") %dopar% {
                runfun(doobj[[i]], met = meth, tauc = taucon, lda = lambdayrs, sp = sparam) # doing it parallel
            }
            if(snow) stopCluster(cl)
        } else {
            nsret <- foreach(i = 1:length(doobj), .packages = "termstrc") %do% {
                runfun(doobj[[i]], met = meth, tauc = taucon, lda = lambdayrs, sp = sparam) # doing it non parallel
            }
        }  
    }
    return(nsret)
}


priceOff <- function(dynObj, nsObj) {
# prices a couponbonds object off another NS curve
    if(!(is.null(dynObj) | is.null(nsObj))) {
        meth <- nsObj$method   # what's the method ns or sv
        beta <- nsObj$startparam  # what are the parameters for the model
        lda <- nsObj$lambda
        name <- nsObj$name
        cf <- create_cashflows_matrix(dynObj[[name]])
        m <- create_maturities_matrix(dynObj[[name]])
        phat <- bond_prices(method = meth, beta, m, cf, lda)$bond_prices # get fair prices
        cfp <- rbind(-phat, cf) # now put the prices into the cashflows matrix
        mp <- rbind(rep(0, ncol(m)), m) # and zero for the front maturity (the price)
        yhat <- ann_yields(cfp, mp) # yet the annual yields of one bonds priced off another
        cfOrig <- create_cashflows_matrix(dynObj[[name]], include_price = TRUE)
        cfOrig[1, ][is.na(cfOrig[1, ])] <- -phat[is.na(cfOrig[1, ])] # if bad prices, put in ns prices
        y <- ann_yields(cfOrig, mp) 
        p <- -cfOrig[1, ]
        matOrder <- order(y[, 1])
        y <- y[matOrder, ]
        yhat <- yhat[matOrder, ]
        p <- p[matOrder]
        phat <- phat[matOrder]
        if(length(yhat) > 2) {
            yerrors <- yhat - y
            yerrors[, 1] <- yhat[, 1] #put the maturities back properly
        } else {                      # special case if only one bonds then
            y <- t(as.matrix(y))      # we must it back from numeric to matrix
            yhat <- t(as.matrix(yhat))
            yerrors <- yhat - y
            yerrors[, 1] <- yhat[, 1]
            rownames(yerrors) <- rownames(y) <- rownames(yhat) <- names(p) 
        }
        perrors <- phat - p
        return(list(name = name, y = y, yhat = yhat, yerrors = yerrors, 
                    p = p, phat = phat, perrors = perrors))
    } else return(NULL)
}


lightns <- function(nsobj) {
# takes those massive termstr_nss objects and makes them *much* smaller by chucking out tons of redundant data
    lightobj <- lapply(nsobj, function(x) {
        #first convert them to annual yields
        if(is.na(x)) NA else {
            yann <- x$y[[1]]
            yann[, 2] <- exp(yann[, 2]) - 1
            yhatann <- x$yhat[[1]] 
            yhatann[, 2] <- exp(yhatann[, 2]) - 1
            yerrorsann <- x$yerrors[[1]]
            yerrorsann[, 2] <- yhatann[, 2] - yann[, 2]
            colnames(yerrorsann) <- colnames(yann) # because termstrc estim forgets this
            startparam <- if(is.null(x$opt_result)) NA else as.numeric(x$opt_result[[1]][[1]]) # startparameters if exist
            lambda <- ifelse(is.null(x$lambda), NA, x$lambda) # startparameters if exist
            method <- ifelse(is.null(x$method), "cs", x$method) # startparameters if exist
            if(method == "cs") { # then get spot rates out
                spot <- x$spot[[1]] # get the spot rates out
                class(spot) <- NULL # whip all that rates class garbage out
                spot <- spot[spot[, 1] == round(spot[, 1], 1), ]  # interpolate otherwise data too big
            } else spot <- NULL
            list(name = x$group, method = method, startparam = startparam,
                tau = ifelse(is.null(x$spsearch[[1]]), NA, 
                list(c(min(x$spsearch[[1]]$tau), max(x$spsearch[[1]]$tau), x$spsearch[[1]]$tau[2] - x$spsearch[[1]]$tau[1]))),
                lambda = lambda,
                y = yann, yhat = yhatann, yerrors = yerrorsann,
                p = x$p[[1]], phat = x$phat[[1]], perrors = x$perrors[[1]], spot = spot)
        }
    })
    if(!is.null(names(lightobj))) names(lightobj) <- names(nsobj)
    lightobj <- addCouponYields(lightobj)
    return(lightobj)
}


matrixzs <- function(incd, days = 21, windowl = 260, mindata = 65, decayhl = NA, meanone = TRUE) {
# takes a matrix and gives final row z scores (zscores - added for searchability) for it
    if(is.na(decayhl)) decayer <- (rep(1, nrow(incd)) / ifelse(meanone, nrow(incd), 1))
        else decayer <- decay(nrow(incd), decayhl, meanone = meanone)
    usecd <- last(incd, windowl + days)
    sds <- rollapply(usecd, windowl, function(x) if(length(na.omit(x)) > mindata) wt.sd(na.omit(x), last(decayer, length(na.omit(x)))) else NA)
    means <- rollapply(usecd, windowl, function(x) if(length(na.omit(x)) > mindata) wt.mean(na.omit(x), last(decayer, length(na.omit(x)))) 
                       else NA)
    lasts <- last(usecd, days)
    zs <- (lasts - means) / sds
    return(list(zs = zs, sds = sds, means = means, lasts = lasts))
}


cdmatrix <- function(cCode, model = paste("yerr", keymodel[[cCode]], sep = ""),
                     useDecay = TRUE, decayhl = 260, usePC2 = TRUE, pcamats = keymats, 
                     infactors = NULL, returnpca = FALSE, showcall = FALSE) {
    if(showcall) flushprint(paste("cCode", cCode, "model", model, "useDecay", useDecay, "decyahl", decayhl, 
                                  "usePC2", usePC2, "pcamats", pcamats,
                     "infactors", infactors, "returnpca", returnpca))
    #makes a cheap dear matrix for each bond, and also gives the PC1 divergence vector, and the PCA adjusted cheap dears
    if(is.null(infactors)) factors <- get(paste(cCode, "factors", sep = "")) else factors <- infactors
    # now refactor
    factors$bond <- as.factor(as.character(factors$bond))
    factors$date <- as.factor(as.Date(factors$date))
    s <- split(factors, factors$date) # split so each date has its own list
    s <- lapply(s, function(x) {
        rownames(x) <- x$bond
        x
    })
    ss <- s
    names <- rownames(last(s)[[1]]) # names of bonds that we want
    cdmat <- t(sapply(s, function(x) x[names, model]))
    colnames(cdmat) <- names
    cdmat <- xts(cdmat, order.by = as.Date(rownames(cdmat))) # the cheap dear matrix for bonds  
    ymat <- t(sapply(s, function(x) x[names, "y"]))
    colnames(ymat) <- names
    ymat <- xts(ymat, order.by = as.Date(index(cdmat)))
    matmat <- t(sapply(s, function(x) x[names, "mat"]))
    colnames(matmat) <- names
    matmat <- xts(matmat, order.by = as.Date(index(cdmat)))
    # now apply years
    avgdev <- apply(cdmat, 1, function(x) mean(abs(na.omit(x)))) # the average deviation from the curve
    avgdev <- xts(avgdev, order.by = as.Date(index(cdmat)))
    cdm <- t(sapply(s, function(x) approx(x$mat, x[[model]], 1:35, rule = 2)$y)) # cheap dear by maturity 1:35
    cdmkey <- cdm <- xts(cdm, order.by = as.Date(rownames(cdm))) # separage cdm and cdmkey because PCA might not be done on all 1:35
    if (!is.null(pcamats)) cdmkey <- cdmkey[, pcamats] 
    if(useDecay == TRUE) {
        pca <- PCA(cdmkey, graph = FALSE, row.w = decay(nrow(cdm), decayhl)) # pca of the maturity cd matrix   
        #print(pca$eig)
    } else {
        pca <- PCA(cdmkey, graph = FALSE)
    }
    l1 <- pca$var$coord[, 1] / sqrt(pca$eig[1, 1])
    l2 <- pca$var$coord[, 2] / sqrt(pca$eig[2, 1])
    l3 <- pca$var$coord[, 3] / sqrt(pca$eig[3, 1])
    pc1 = cdmkey %*% l1
    pc2 = cdmkey %*% l2
    pc3 = cdmkey %*% l3
    pc1 <- xts(pc1, order.by = index(cdmat))
    pc2 <- xts(pc2, order.by = index(cdmat))
    pc3 <- xts(pc3, order.by = index(cdmat))
    cdpcdata = list(pca = pca, pcs = cbind(pc1, pc2, pc3))
    if (usePC2 == TRUE) load <- l1 + l2 + l3 else load <- l1
    pcx <- cdmkey %*% load  # so we have the first principal component
    pcx <- xts(pcx, order.by = index(cdmat))
    i <- index(cdm)
    cdmpc <- sapply(1:35, function(x) {   # now do regressions and check the PC1-defined place for each maturity
        reg <- lm(cdm[, x] ~ pcx)
        intercept <- reg$coefficients[1] # get the intercept of the regression  
        coeff <- reg$coefficients[2] # get the coefficients of the regression
        return(pcx * coeff + intercept)
    })
    colnames(cdmpc) <- 1:35
    cdmpc <- xts(cdmpc, order.by = index(cdm))
    #????????????????? would be interesting here to put in the correlation of each bond to the 3 pcs
    # now for each bond, interpoloate where in cdmpc its maturity was, and find out where it's pc1-defined place was
    bondlist <- split(factors, factors$bond) # split factors by bond
    fullbondmat <- cdmat
    bondmat <- sapply(bondlist, function(x) {
        df <- data.frame(rep(NA, nrow(cdmat)))
        rownames(df) <- index(cdmat)
        df[as.character(x[, "date"]), 1] <- x[, "mat"]
        return(df[, 1])
    })
    bondmat <- bondmat[, names]  # only the ones we're interested in, pal
    dbond <- sapply(1:ncol(cdmat), function(x) {
        cdcol <- cdmat[, x]
        matcol <- bondmat[, x]
        matcol <- matcol[!is.na(matcol)]
        cdcol <- as.numeric(cdcol[!is.na(cdcol)])
        pccol <- last(pc1, length(cdcol))
        pccol2 <- last(pc2, length(cdcol))
        pccol3 <- last(pc3, length(cdcol))
        resids <- lm(cdcol ~ pccol:matcol)$residuals
        resids <- c(rep(NA, nrow(cdmat) - length(cdcol)), resids)
    })
    dbond <- xts(dbond, order.by = as.Date(index(cdm)))
    colnames(dbond) <- colnames(cdmat)
    r2m <- sapply(1:35, function(x) summary(lm(cdm[, x] ~ pc1))$r.squared)
    coupons <- sapply(colnames(dbond), function(x) bondlist[[x]]$coupon[1])
    if(returnpca) {
        return(list(cdmat = cdmat, pcmat = dbond, ymat = ymat, matmat = matmat, 
                    cdm = cdm, pc1 = pc1, avgdev = avgdev, r2m = r2m, pca = pca, coupons = coupons))
    } else {
        return(list(cdmat = cdmat, pcmat = dbond, ymat = ymat, matmat = matmat, 
                    cdm = cdm, cdpcdata = cdpcdata, avgdev = avgdev, r2m = r2m, coupons = coupons))
    }
}


maturityCD <- function(cCodes = ac, series = TRUE, days = 780, whichpcs = 1:3, mats = 2:35, 
                   decayhl = 260, usedecay = TRUE, scale.unit = FALSE, model = "auto") {
# this will do cheap dear on maturities based on principal components, and will also do bond cheap dear
    pcs <- getPCAs(ac, days = days, series = series, colSelect = mats, decayhl = decayhl, 
                    usedecay = usedecay, scale.unit = scale.unit, model = model)
    residmats <- lapply(cCodes, function(cCode) {
        dynObj <- get(paste(cCode, "data", sep = ""))[[model]]
        yields <- last(couponYields(dynObj)[, mats], days) * 100 * 100
        cpcs <- do.call(cbind, lapply(pcs[whichpcs], function(x) x[, cCode])) # combine the country's PCs
        if(!identical(index(yields), index(cpcs))) {
            flushprint("index mismatch")
            #browser()
        }
        if(usedecay) decayer <- decay(days, decayhl) else decayer <- rep(1, days)
        linresids <- apply(yields, 2, function(x) lm(x ~ cpcs)$residuals)
        linresids <- xts(linresids, order.by = as.Date(rownames(linresids)))
        return(linresids)
    })
    names(residmats) <- cCodes
    return(residmats)
}


quantileOfCD <- function(inmat, basemat, scale.unit = FALSE, whichpcs = 1:3, 
                         whichquantiles = seq(0, 1, 1/20), plotit = TRUE) {
# take an inmatrix, return its residuals against inmat PCs, then the quantiles of those residuals
    if(nrow(inmat) != nrow(basemat)) {
        flushprint("vectors must have the same number of rows")
        return(-1)
    } else {
        if(scale.unit) {
            pca = PCA(basemat, scale.unit = TRUE, graph = FALSE)
            baseeigenvecs <- sapply(whichpcs, function(w) pca$var$coord[, w] / sqrt(pca$eig[w, 1]))
            baseeigenvals <- pca$eig
            #baseeigenvecs <- as.matrix(eigen(cor(basemat))$vectors[, whichpcs]) # get the loading; matrix for single pc case
        } else {
            pca = PCA(basemat, scale.unit = FALSE, graph = FALSE)
            baseeigenvecs <- sapply(whichpcs, function(w) pca$var$coord[, w] / sqrt(pca$eig[w, 1]))
            baseeigenvals <- pca$eig
            #baseeigenvecs <- as.matrix(eigen(cov(basemat))$vectors[, whichpcs]) # should be same as above
        }
        basepcs <- apply(baseeigenvecs, 2, function(y) y %*% t(basemat)) # create the vectors
        # tryout
        lms <- apply(inmat, 2, function(x) lm(x ~ basepcs))
        resids <- sapply(lms, function(x) x$residuals)
        resids <- xts(resids, order.by = as.Date(rownames(resids)))
        coeffs <- lapply(lms, coef)
        quantiles <- apply(resids, 2, function(x) quantile(x, whichquantiles))
        sds <- apply(resids, 2, sd)
        if(plotit) {
            windows(10, 10)
            plot(quantiles[1, ], ylim = c(min(quantiles), max(quantiles)), type = "l")
            apply(quantiles, 1, lines)
            lines(resids[nrow(resids), ], col = addAlpha("red", 0.75), lwd = 2)
            lines(sds * 2, col = "green4", lwd = 2)
            lines(- sds * 2, col = "green4", lwd = 2)
        }
    }
    basepcs <- xts(basepcs, order.by = index(inmat))
    return(list(resids = resids, quantiles = quantiles, pcs = basepcs, baseeigenvecs = baseeigenvecs, 
                baseeigenvals = baseeigenvals, inmat = inmat, basemat = basemat))
}


makeQuantilePolyDF <- function(quantiledf) {
# will return a ggplot-friendly polygon df
# must intput a data frame, with percentages along the rows, and maturies along the columnds
# percentages must be ordered ascending, be equidistant, and odd number
    xvals = as.numeric(colnames(quantiledf))
    percs = as.numeric(sub("%", "", rownames(quantiledf))) / 100
    # now check if percents are good
    if((min(diff(percs)) - max(diff(percs))) > 0.0001) {
        flushprint("percentiles gaps must be the same")
        return(-1)
    }
    if((length(percs) %% 2) == 0) {
        flushprint("must have an odd number of percentiles")
        return(-1)
    }
    # now make sure densities are correct by doing cumulative percentiles and also reversing them
    percents <- cumsum(diff(percs))
    percents <- percents[percents < 0.5]
    percents <- c(percents, rev(percents)) + 0.1
    # now ready to starty building polygons
    ncols <- ncol(quantiledf) # number of x values
    polydf <- lapply(2:nrow(quantiledf), function(x) {       # start at row 2 because need 2 rows per poly
        onepoly <- rbind(cbind(xvals, quantiledf[x - 1, ], rep(x - 1, ncols), rep(percents[x - 1], ncols)), 
        cbind(rev(xvals), rev(quantiledf[x, ]), rep(x - 1, ncols), rep(percents[x - 1], ncols)))
        return(onepoly)
    })
    polydf <- data.frame(do.call(rbind, polydf))
    colnames(polydf) <- c("xvals", "yvals", "id", "value")
    return(polydf)
}

crossLines <- function(dependent, barriers)  {
# will take a line (dependent) and see where it crosses the barriers
# will return a list of lines of different IDs which represent the segments outside and between the barriers
# inputs a dataframe or matrix for dependent with x and y; lists of the same for the barriers
# this is for the coloured line in function ggcd
    numbarriers <- length(barriers)
    numregions <- numbarriers + 1
    knotpoints <- dependent$x # all xs here
    for(b in barriers) knotpoints <- append(knotpoints, b$x) 
    knotpoints <- unique(knotpoints)
    knotpoints <- knotpoints[order(knotpoints)]
    dependinterp <- approx(dependent$x, dependent$y, knotpoints, rule = 2)$y
    barrierinterp <- sapply(barriers, function(b) approx(b$x, b$y, knotpoints, rule = 2)$y)
    regionmatrix <- cbind(knotpoints, dependinterp, barrierinterp)
    regionfinder <- rbind(apply(regionmatrix, 1, function(rr) rr[2] > rr[3:length(rr)]), rep(TRUE, length(knotpoints)))
    regions <- apply(regionfinder, 2, function(x) match(TRUE, x))
    rleregions <- rle(regions) # run length encode
    crosspoints <- c(1, cumsum(rleregions$lengths)) # where line crosses into regions
    allines <- lapply(2:length(crosspoints), function(x) {
                              list(x = knotpoints[crosspoints[x - 1]:crosspoints[x]],
                                   y = dependinterp[crosspoints[x - 1]:crosspoints[x]],
                                   col = rleregions$values[x - 1])
                })
}

              
ggCD <- function(cCode, compccodes = ac, mindata = 65, days = 520, subdays = 260, model = "auto", 
                     whichpcs = 1:4, howmuchrecent = 5, usepcmat = FALSE, smooth = TRUE, plotit = TRUE, 
                     plotbonds = FALSE, zwindow = 260, zdecay = 130, scale.unit = TRUE) {
# going to plot a ggplot country maturity and bond cd object
# TODO
#   * bug in spain crossover point when coming from below
#   * allow weekly history lines and dots; create framework for evolution
#   * make sure history is appropriate: rather than using 520 days consider using much more with halflife
#   * include comparator country and PCA analysis diagnostis
#   * PCA row weighted by the bucket importance in the benchmark index
#   * flymax overlay
    howmuchrecent <- howmuchrecent + 1 # to get in previous day's close too to make it really howmuchrecent
    outerthresh <- c("2.5%", "97.5%")
    innerthresh <- c("15%", "85%")
    regioncols1 = c("seagreen3", "cyan3", "dodgerblue3", "darkorchid3", "firebrick3")
    regioncols2 = c("seagreen1", "cyan1", "dodgerblue1", "darkorchid1", "firebrick1")
    maxmat <- maxMats(cCode, mindata) # what's this country's maximum yield curve maturity for dat
    quandata <- quantileOfCD(inmat = allYields(cCode, days = days, model = model, mats = 2:maxmat)[[1]] * 10000,
                             basemat = do.call(cbind, allYields(compccodes, days = days, model = model, mats = keymats)) * 10000, 
                             #basemat = do.call(cbind, allYields(compccodes, days = days, model = model, mats = 2:maxmat)) * 10000, 
                             whichpcs = whichpcs, plotit = FALSE, whichquantiles = seq(0.025, 0.975, by = 0.025), 
                             scale.unit = scale.unit)
    quanmelt <- melt(quandata$quantiles)
    bkts <- sapply(quanmelt[, 2], function(x) names(buckets)[sapply(buckets, function(y) (x > y[1] && x <= y[2]))])
    quanmelt <- cbind(quanmelt, bkts)
    colnames(quanmelt) <- c("percentile", "mat", "value", "bucket")
    polys <- makeQuantilePolyDF(quandata$quantiles) # that's for all the polygons
    # now create today's line
    todayline <- last(quandata$resids, howmuchrecent)
    todayline <- as.data.frame(todayline)
    rownames(todayline) <- rev(1:howmuchrecent) - 1
    todayline <- melt(as.matrix(todayline))
    colnames(todayline) <- c("daysback", "mat", "value")
    todayline$daysback <- factor(todayline$daysback, levels = rev(1:howmuchrecent) - 1)
    # now find the different lines in different z score regions
    regionlines <- crossLines(data.frame(x = todayline[todayline$daysback == 0, "mat"],
                                         y = todayline[todayline$daysback == 0, "value"]),
                              lapply(rev(c(outerthresh[1], innerthresh, outerthresh[2])), function(thresh) {
                                     data.frame(x = as.numeric(colnames(quandata$quantiles)),
                                                y = quandata$quantiles[thresh, ])
                                     }))
    # now get the boxplots
    infactors = get(paste(cCode, "factors", sep = "")) # usually 3 years of factor data
    if(length(unique(infactors$date)) < days) { # but check that we have enough and if not, get a new one
        infactors <- allBondFactors(cCode, days = days)
    }
    cdm <- cdmatrix(cCode, model = paste("yerr", ifelse(model == "auto", keymodel[[cCode]], model), sep = ""), 
                    useDecay = FALSE, infactors = infactors)
    if(usepcmat) cheapmat <- - cdm$pcmat * 10000 else cheapmat <- - cdm$cdmat * 10000 # negate because returned the other way
    mats <- last(cdm$matmat, days) # get the maturities
    lmats <- matrix(last(mats)) # going to use this to add maturities to the meldted matrices
    rownames(lmats) <- colnames(mats) # to create a lookup table
    # now we must add the last line of resids (curve cheap dear) to each bond.
    allbondcurvecd <- t(sapply(1:nrow(mats), function(x) 
                approx(colnames(quandata$resids), quandata$resids[x, ], mats[x, ], rule = 2)$y))
    colnames(allbondcurvecd) <- colnames(mats) # put the bond names in
    allbondcurvecd <- xts(allbondcurvecd, order.by = index(mats))
    allbondbothcd <- allbondcurvecd + last(cheapmat, nrow(allbondcurvecd))
    bondzs <- matrixzs(last(cheapmat, days), windowl = zwindow, decayhl = zdecay)$zs # get zs
    curvebybondzs <- matrixzs(allbondcurvecd, windowl = zwindow, decayhl = zdecay)$zs # get zs
    bothbybondmatzs <- matrixzs(allbondbothcd, windowl = zwindow, decayhl = zdecay, 
                                days = (days - zwindow) - 1) # not just zs because later we'll need the sds
    bothbybondzs <- bothbybondmatzs$zs # get the zs out
    curvebymatzs <- as.data.frame(last(matrixzs(quandata$resids, windowl = zwindow, decayhl = zdecay)$zs, howmuchrecent))
    colnames(curvebymatzs) <- 2:maxmat
    rownames(curvebymatzs) <- rev(1:howmuchrecent) - 1
    meltcurvebymatzs <- melt(as.matrix(curvebymatzs))
    todayline <- cbind(todayline, meltcurvebymatzs[, 3])
    colnames(todayline)[ncol(todayline)] <- "zscore"
    # now start plotting this bad boy
    bybondcdcurve <- cbind(melt(as.matrix(last(allbondcurvecd, days))), rep("curve", days))
    bybondcdbond <- cbind(melt(as.matrix(last(cheapmat, days))), rep("bond", days))
    bybondcdboth <- cbind(melt(as.matrix(last(allbondbothcd, days))), rep("both", days))
    colnames(bybondcdboth) <- colnames(bybondcdcurve) <- colnames(bybondcdbond) <- c("date", "bond", "value", "source")
    bybondcd <- rbind(bybondcdcurve, bybondcdbond, bybondcdboth)
    bybondcd$bond <- factor(as.character(bybondcd$bond), levels = colnames(mats))
    bybondbuckets <- sapply(sapply(bybondcd$bond, function(x) lmats[x]), 
            function(y) names(buckets)[sapply(buckets, function(z) (y > z[1] && y <= z[2]))]) # get the buckets
    labels <- paste(paste(" ", isinLabel(cCode, rownames(lmats), couponAfter = TRUE, withcCode = FALSE), sep = ""), " ", sep = "")
    labels <- toupper(labels)
    bybondlabels <- sapply(bybondcd$bond, function(x) labels[x])
    bybondcd <- cbind(bybondcd, bucket = bybondbuckets, label = bybondlabels) 
    bybondcd$bucket <- factor(bybondcd$bucket, levels = names(buckets)) # order the buckets
    bybondcd$label <- factor(bybondcd$label, levels = labels) # order the buckets
    gg1 <- ggplot(bybondcd[bybondcd$date == as.character(max(as.character(bybondcd$date))), ], 
                  aes(x = label, y = value, fill = factor(source))) + 
                  geom_bar(alpha = 0.7, position = "dodge")
    gg1 <- gg1 + facet_grid(~ bucket, scale = "free_x", space = "free_x")
    gg1 <- gg1 + theme(axis.text.x = element_text(angle = 90)) 
    gg1 <- gg1 + theme(legend.title = element_blank())
    gg1 <- gg1 + scale_x_discrete(name = "bonds")
    gg1 <- gg1 + scale_y_continuous(name = "bps")
    gg1 <- gg1 + ggtitle(cCode)
    #dev.new()
    #plot(gg1)
    # create the bond cd and curve cd at bond maturity point, melted frame
    boxadder <- matrix(last(allbondcurvecd))
    rownames(boxadder) <- colnames(mats) # create lookup table
    boxadder <- boxadder[colnames(cheapmat), ]
    sweepcdmat <- sweep(cheapmat, 2, boxadder, FUN = "+") # add all this stuff
    # make long and short cdmats for two boxplots (if we want two that is - will mainly use longcdmat)
    longcdmat <- last(sweepcdmat, days)
    shortcdmat <- last(sweepcdmat, subdays)
    # now we rip out the outliers
    #longstats <- boxplot(data.frame(longcdmat), plot = FALSE)$stats
    #for (x in 1:ncol(longcdmat)) {
    #    longcdmat[(longcdmat[, x] > max(longstats[, x])), x] <- NA
    #    longcdmat[(longcdmat[, x] < min(longstats[, x])), x] <- NA
    #}
    # melt these suckers for ogplot2
    meltcdlong <- melt(as.matrix(longcdmat))
    meltcdlong <- cbind(meltcdlong, sapply(as.character(meltcdlong$Var2), function(x) lmats[x, ])) # apply the maturities
    meltcdshort <- melt(as.matrix(shortcdmat))
    meltcdshort <- cbind(meltcdshort, sapply(as.character(meltcdshort$Var2), function(x) lmats[x, ])) # apply the maturities
    colnames(meltcdshort) <- colnames(meltcdlong) <- c("date","bond","value", "mat")
    # now create a small meltcdlong called meltcdrecent to which we can add z scores
    howmuchdates <- last(index(sweepcdmat), howmuchrecent)
    meltcdrecent <- meltcdlong[as.character(meltcdlong$date) %in% as.character(howmuchdates), ]
    meltcdrecent <- cbind(meltcdrecent, apply(meltcdrecent, 1, function(x) as.numeric(bothbybondzs[x[1], x[2]])))
    colnames(meltcdrecent)[ncol(meltcdrecent)] <- "zscore"
    howmuchdatesindex <- data.frame(howmuchdates, (howmuchrecent - 1):0)
    meltcdrecent <- cbind(meltcdrecent, sapply(as.character(meltcdrecent$date),  # stick in a daysback index
                                               function(x) howmuchdatesindex[howmuchdatesindex[, 1] == x, 2]))
    colnames(meltcdrecent)[ncol(meltcdrecent)] <- "daysback"
    # x axis labels by buckets
    xaxtickpos <- as.numeric(names(buckets))
    # now latest cheapdear points
    latestcdpoint <- meltcdlong[as.character(meltcdlong$date) == max(as.character(meltcdlong$date)), ]
    latestcdpoint <- cbind(latestcdpoint, labels)
    colnames(latestcdpoint)[ncol(latestcdpoint)] <- "label"
    # now figure out the bond label positions using the old boxplot
    dummybox <- boxplot(as.matrix(last(sweepcdmat, days)), plot = FALSE)$stats
    labelposies <- sapply(1:ncol(dummybox), 
                          function(x) if ((abs(x) %% 2) < .Machine$double.eps) c(dummybox[1, x], 0) else c(dummybox[5, x], 1))
    latestcdpoint <- cbind(latestcdpoint, labelposies[1, ])
    colnames(latestcdpoint)[ncol(latestcdpoint)] <- "labelposies"
    latestcdpoint <- cbind(latestcdpoint, labelposies[2, ])
    colnames(latestcdpoint)[ncol(latestcdpoint)] <- "adjustvertvec"
    ggp <- ggplot(polys, aes(x = xvals, y = yvals)) + 
         #geom_polygon(aes(fill = - value, group = id, alpha = value)) + # lovely blue
         geom_polygon(aes(fill = value, group = id, alpha = value)) + # lovely shiny light blue middle draw me in
         scale_x_log10(breaks = xaxtickpos, minor_breaks = NULL) + 
         theme(legend.position = "none", panel.background = element_rect(fill = "grey85", colour = NA)) + 
         xlab("maturity") + ylab("bps")
    z2frame <- quanmelt[quanmelt[, "percentile"] %in% outerthresh, ]
    ggp <- ggp + geom_line(data = z2frame, 
                           aes(x = mat, y = value, group = percentile), colour = "white", size = 2)
    ggp <- ggp + geom_line(data = quanmelt[quanmelt[, "percentile"] %in% innerthresh, ], 
                           aes(x = mat, y = value, group = percentile), colour = "white", size = 1,
                           linetype = "dotted")
    # put 2z labels around; first calc them then insert them
    z2lab1 <- z2frame[z2frame[, "percentile"] == outerthresh[1], ]
    z2lab2 <- z2frame[z2frame[, "percentile"] == outerthresh[2], ]
    z2lab1 <- c(max(z2lab1[, "mat"]), z2lab1[z2lab1[, "mat"] == max(z2lab1[, "mat"]), "value"])
    z2lab2 <- c(max(z2lab2[, "mat"]), z2lab2[z2lab2[, "mat"] == max(z2lab2[, "mat"]), "value"])
    #ggp <- ggp + annotate("text", x = z2lab1[1] + 3, y = z2lab1[2], label = "-2z", col = "white", size = 3)
    #ggp <- ggp + annotate("text", x = z2lab2[1] + 3, y = z2lab2[2], label = "+2z", col = "white", size = 3)
    #add last few days line/today (this doesn't work very well hence commented out)
    todayback <- todayline[todayline$daysback == (howmuchrecent - 1), ] # get this historic lines
    ggp <- ggp + geom_smooth(data = todayback, aes(x = mat, y = value, group = daysback), 
                             colour = "darkred", linetype = "dashed", 
                             se = FALSE, size = 1, method = "loess", span = (ifelse(smooth, 0.3, 0.1)))
    #add boxplot
    ggp <- ggp + geom_boxplot(data = meltcdlong, aes(x = mat, y = value, group = bond), outlier.size = NA, 
                              colour = "grey30", alpha = 0.5, size = 0.2, width = 0.025)

    # add the latest point
    ggp <- ggp + geom_point(data = latestcdpoint, aes(x = mat, y = value, group = bond)) 
    # now do labels (twice - one for above, one for below)
    ggp <- ggp + geom_text(data = latestcdpoint[latestcdpoint$adjustvertvec == 1, ], aes(x = mat, y = labelposies, label = label), 
                           angle = 90, colour = "grey20", size = 3, hjust = 0, alpha = 0.5)
    ggp <- ggp + geom_text(data = latestcdpoint[latestcdpoint$adjustvertvec == 0, ], aes(x = mat, y = labelposies, label = label), 
                           angle = 270, colour = "grey20", size = 3, hjust = 0, alpha = 0.5)
    #now print a nice z-score graded colour line for the curve 
    todaytoday <- todayline[todayline$daysback == 0, ]
    minz <- min(rescale(todaytoday[, "zscore"])) # for scaling of z-score line gradient colours
    maxz <- max(rescale(todaytoday[, "zscore"]))
    bpspline <- smooth.spline(todaytoday$mat, todaytoday$value, spar = 0.4) # Smooth out the curve with lots of points
    zscorespline <- smooth.spline(todaytoday$mat, todaytoday$zscore) # and smooth out the zscores too
    xplot <- seq(2, maxmat, by = 0.1)
    todayplotter <- data.frame(mat = xplot, value = predict(bpspline, xplot)$y, 
                               zscore = rescale(c(-5, 5, predict(zscorespline, xplot)$y))[-1:-2]) # build the plotter
    ggp <- ggp + geom_path(data = todayplotter, aes(x = mat, y = value, colour = zscore), size = 2, linejoin = "bevel") +
                 scale_colour_gradientn(colours = gradientcolours, values = gradientscale, limits = c(minz, maxz))
    #and the title
    ggp <- ggp + ggtitle(cCode)
    # now the test chart
    mm <<- meltcdrecent[meltcdrecent$daysback == 0, ]
    ggp <- ggp + geom_point(data = mm, aes(x = mat, y = value, colour = rescale(c(-5, 5, zscore))[-1:-2]), size = 6) +
           scale_colour_gradientn(colours = gradientcolours, values = gradientscale, limits = c(0, 1))
    ggp <- ggp + geom_point(data = mm, aes(x = mat, y = value), colour = "black", size = 4.5)
    ggp <- ggp + geom_text(data = mm, aes(x = mat, y = value), label = round(mm$zscore, 1), colour = "white", size = 2, alpha = 0.7)
    # that's it, now plot or return data
    if(plotit) {
        plot (ggp) 
        if(plotbonds) { # plot all the bonds to disk
            # btw bybondcd is the DADDY of cd. daddy.
            # also bothbybondmatzs
            bb <- split(bybondcd, bybondcd[, "bond"])
            lapply(bb, function(b) {
                isin <- b[1, "bond"]
                flushprint(isin)
                sdboth <- as.numeric(last(bothbybondmatzs$sds[, isin])) # get the standard deviation
                bcrv <- last(b[b[, "source"] == "curve", ], days - zwindow)
                bbnd <- last(b[b[, "source"] == "bond", ], days - zwindow)
                bbth <- last(b[b[, "source"] == "both", ], days - zwindow)
                sds <- last(bothbybondmatzs$sds[, isin], days - zwindow)
                sds <- na.locf(sds, fromLast = TRUE) # carry first non NA value if there are NAs
                means <- last(bothbybondmatzs$means[, isin], days - zwindow)
                means <- na.locf(means, fromLast = TRUE)
                sdup <- means + 2 * sds
                sddown <- means - 2 * sds
                sdup <- data.frame(date = as.Date(index(sdup)), z2 = as.numeric(sdup))
                sddown <- data.frame(date = as.Date(index(sddown)), z2 = as.numeric(sddown))
                bbth[["sdup"]] <- sdup[, "z2"]
                bbth[["sddown"]] <- sddown[, "z2"]
                ggbth <- ggplot(bbth, aes(x = as.Date(as.character(date)), y = value, colour = source)) 
                ggbth <- ggbth + geom_line(data = bcrv, size = 0.6)
                ggbth <- ggbth + geom_line(data = bbnd, size = 0.6)
                ggbth <- ggbth + geom_line(data = bbth, size = 1.5, alpha = 0.8)
                ggbth <- ggbth + theme(axis.title.x = element_blank())
                ggbth <- ggbth + ylab("bps")
                ggbth <- ggbth + ggtitle(paste(isinLabel(cCode, isin), "RV"))
                if(!all(is.na(sds))) {
                    ggbth <- ggbth + geom_line(aes(y = sdup), size = 0.6, linetype = "dashed", colour = "grey", size = 1)
                    ggbth <- ggbth + geom_line(aes(y = sddown), size = 0.6, linetype = "dashed", colour = "grey", size = 1)
                    ggbth <- ggbth + annotate("text", x = as.Date(bbth[1, "date"]), y = bbth[1, "sdup"], 
                                              label = "2z", size = 5, colour = "darkgrey")
                    ggbth <- ggbth + annotate("text", x = as.Date(bbth[1, "date"]), y = bbth[1, "sddown"], 
                                              label = "-2z", size = 5, colour = "darkgrey")
                }
                ggsave(ggbth, filename = paste(bondchartplace, isin, ".png", sep = ""), width = 6, height = 5, 
                       units = "in", dpi = 120)
            })
        }
    } else return(list(cdChart = ggp, 
                       cdData = list(quandata = quandata, quanmelt = quanmelt, cheapmat = cheapmat,
                                     allbondcurvecd = allbondcurvecd, allbondbothcd = allbondbothcd, 
                                     bondzs = bondzs, curvebybondzs = curvebybondzs, 
                                     bothbybondzs = bothbybondzs,
                                     curvebymatzs = curvebymatzs, meltcurvebymatzs = meltcurvebymatzs,
                                     bybondcdcurve = bybondcdcurve, bybondcdbond = bybondcdbond, 
                                     bybondcdboth = bybondcdboth, bybondcd = bybondcd, 
                                     meltcdlong = meltcdlong,  meltcdshort = meltcdshort, 
                                     meltcdrecent = meltcdrecent, 
                                     latestcdpoint = latestcdpoint, gradientcolours = gradientcolours, 
                                     cCode = cCode, compccodes = compccodes, whichpcs = whichpcs)))
}

ggPCs <- function(ggData) {
# produces plots of the principal components and loadings of a ggCD object
    cdData <- ggData$cdData
    pcvecs <- cdData$quandata$baseeigenvecs
    pcvals <- cdData$quandata$baseeigenvals
    pcs <- cdData$quandata$pc
    basemat <- cdData$quandata$basemat
    # now check if pc3 is oriented correctly for slope
    if(mean(pcs[, 3]) < 0) {
        pcs[, 3] <- -pcs[, 3]
        pcvecs[, 3] <- -pcvecs[, 3]
    }
    # now we normalise the pcs so that they make sense
    for (x in 1:ncol(pcs)) pcs[, x] <- pcs[, x] / sum(abs(pcvecs[, x]))
    # now carry on
    cCodes <- cdData$compccodes
    whichpcs = cdData$whichpcs
    cols <- unlist(lapply(cCodes, function(x) rep(cColors[[x]], length(keymats))))
    rownames(pcvecs) <- colnames(basemat) <- unlist(lapply(cCodes, function(x) paste(x, keymats, sep = "")))
    pclabels <- c(" (level)", " (periph spreads)", " (slope)", "", "")
    colnames(pcvecs) <- paste("PC", whichpcs, sep = "")
    colnames(pcvecs) <- paste(colnames(pcvecs), pclabels[1:ncol(pcvecs)], sep = "")
    dframe <- data.frame(country = unlist(lapply(cCodes, function(x) rep(x, length(keymats)))),
                    mat = unlist(lapply(cCodes, function(x) keymats)))
    dframe <- cbind(dframe, pcvecs)
    dframe <- melt(dframe, id.vars = c("country", "mat"))
    dframe$country <- factor(dframe$country, levels = cCodes)
    dframe$mat <- factor(dframe$mat, levels = keymats)
    dframe$variable <- factor(dframe$variable, levels = colnames(pcvecs))
    loadplots <- lapply(colnames(pcvecs), function(x) {
        g <- ggplot(dframe[dframe$variable == x, ], aes(x = mat, y = value))
        g <- g + geom_bar(colour = "darkgrey", stat = "identity", fill = "grey")
        g <- g + facet_grid(~ country, scale = "free")
        g <- g + ggtitle(paste(x, "loading"))
        g <- g + ylab("weight")
        g <- g + theme(axis.title.x = element_blank())
        return(g)
    })
    # plot the pc levels
    pclabels <- c(" (level)", " (periph spreads)", " (slope)", "", "")
    pcplots <- lapply(1:ncol(pcs), function(x) {
        dfpcval <- data.frame(date = index(pcs), pcval = pcs[, x])
        g <- ggplot(dfpcval, aes(x = date, y = pcval)) 
        g <- g + geom_line(colour = "black")
        g <- g + geom_point(size = 1.5, color = "black", fill = "white", pch = 21)
        g <- g + ggtitle(paste("PC", x, " value", sep = ""))
        g <- g + xlab("maturity") + ylab("bps")
        g <- g + theme(axis.title.x = element_blank())
        return(g)
    })
    pcshortplots <- lapply(1:ncol(pcs), function(x) {
        dfpcval <- data.frame(date = index(last(pcs, "2 months")), pcval = last(pcs[, x], "2 months"))
        g <- ggplot(dfpcval, aes(x = date, y = pcval)) 
        g <- g + geom_line(colour = "black")
        g <- g + geom_point(size = 1.5, color = "black", fill = "white", pch = 21)
        g <- g + ggtitle(paste("PC", x, " value", pclabels[x], sep = ""))
        g <- g + xlab("maturity") + ylab("bps")
        g <- g + theme(axis.title.x = element_blank(), 
                       axis.title.y = element_text(size = 8),
                       axis.text.x = element_text(size = 8),
                       plot.title = element_text(size = 10))

        return(g)
    })
    recentpcs <- data.frame(last(pcs, "2 months"))
    recentpcs[["Date"]] <- as.Date(rownames(recentpcs))
    colnames(recentpcs) <- c("PC1", "PC2", "PC3", "PC4", "Date")
    recentpcs <- melt(recentpcs, id.vars = "Date")
    recentplot <- ggplot(recentpcs, aes(x = Date, y = value)) + geom_line(colour = "black")
    recentplot <- recentplot + geom_point(fill = "white", pch = 21)
    recentplot <- recentplot + facet_wrap(~ variable, scale = "free_y")
    recentplot <- recentplot + theme(axis.title.x = element_blank()) + ylab("bps")
    recentplot <- recentplot + ggtitle("PCs in past 2 calendar months")
    return(list(loadplots = loadplots, pcplots = pcplots, recentplot = recentplot, pcshortplots = pcshortplots))
}


ggHeat <- function(cCodes = cl, plotheat = FALSE, usepcmat = TRUE, daysback = 5) {
# produces a bond heatmap out of ggcdobj
    ggcdobj <- lapply(cCodes, ggCD, plotit = FALSE, usepcmat = usepcmat, howmuchrecent = daysback)
    names(ggcdobj) <- cCodes
    countrymatzs <- sapply(ggcdobj, function(g) {
        matzs <- g$cdData$curvebymatzs
        colnames.matzs <- colnames(matzs)
        return(as.numeric(last(matzs[, colnames.matzs %in% as.character(keymats)])))
    })
    if(is.null(names(ggcdobj))) {
        colnames(countrymatzs) <- cl
    } else {
        colnames(countrymatzs) <- names(ggcdobj)
    }
    rownames(countrymatzs) <- keymats
    czs.melt <- melt(countrymatzs)
    colnames(czs.melt) <- c("maturity", "country", "zscore")
    czs.melt$maturity <- factor(czs.melt$maturity, levels = as.character(keymats))
    czs.melt$country <- factor(czs.melt$country, levels = rev(colnames(countrymatzs)))
    heat <- ggplot(czs.melt, aes(x = maturity, y = country)) + geom_tile(aes(fill = zscore))
    heat <- heat + scale_fill_gradientn(colours = gradientcolours, values = gradientscale, limits = c(-4, 4))
    heat <- heat + geom_text(data = czs.melt, aes(x = maturity, y = country, label = round(zscore, 1)), size = 4, colour = "black")
    heat <- heat + theme(legend.position = "bottom")
    heat <- heat + ggtitle("Curve relative value heatmap")
    if(plotheat) {
        dev.new()
        plot(heat)
    }
    countrymatzs <- lapply(ggcdobj, function(g) {
        matzs <- g$cdData$curvebymatzs
        colnames.matzs <- colnames(matzs)
        currentmatrix <- matzs[, colnames.matzs %in% as.character(keymats)]
        currentmatrix <- as.matrix(currentmatrix)
        rownames(currentmatrix) <- paste("T-", rownames(currentmatrix), sep = "")
        colnames(currentmatrix) <- keymats
        return(currentmatrix)
    })
    if(is.null(names(ggcdobj))) {
        names(countrymatzs) <- cl
    } else {
        names(countrymatzs) <- names(ggcdobj)
    }
    czs.melt <- melt(countrymatzs)
    colnames(czs.melt) <- c("T", "mat", "zscore", "country")
    levs <- rev(paste("T-", 0:(length(unique(as.character(czs.melt$T))) - 1), sep = "")) # calc the levels
    czs.melt$T <- factor(czs.melt$T, levels = levs) 
    czs.melt$country <- factor(as.character(czs.melt$country), levels = names(countrymatzs))
    heat2 <- ggplot(data = czs.melt, aes(x = T, y = zscore, fill = zscore, width = zscore, stat = "identity")) 
    heat2 <- heat2 + geom_bar(colour = "darkgrey", stat = "identity")
    heat2 <- heat2 + scale_fill_gradientn(colours = gradientcolours, values = gradientscale, limits = c(-4, 4))
    heat2 <- heat2 + facet_grid(country ~ mat)
    heat2 <- heat2 + theme(axis.text.x = element_blank(), legend.position = "bottom")
    heat2 <- heat2 + labs(x = paste("Each panel represents past", length(levs), "days"))
    heat2 <- heat2 + ggtitle("Curve relative value evolution")
    if(plotheat) {
        dev.new()
        plot(heat2)
    } else { 
        return(list(curvezs = czs.melt, heatmain = heat, heatevol = heat2, ggcdobj = ggcdobj))
    }
}


bucketCD <- function(cCodes = ac, model = "auto", years = 3, subyears = 0.5, useDecay = TRUE, decayhl = 130, 
                     numlast = 21) {
# cheap dear by bucket then plot
    matrices <- sapply(cCodes, function(cCode) 
                       cdmatrix(cCode, model = paste("yerr", ifelse(model == "auto", keymodel[[cCode]], model), sep = ""),
                                useDecay = useDecay, decayhl = decayhl), simplify = FALSE, USE.NAMES = TRUE)
    cdmats <- lapply(matrices, "[[", "cdmat")
    pcmats <- lapply(matrices, "[[", "pcmat")
    cdzs <- do.call(cbind, lapply(cdmats, function(x) matrixzs(x, days = numlast)$zs))
    pczs <- do.call(cbind, lapply(pcmats, function(x) matrixzs(x, days = numlast)$zs))
    cdzs <- cbind(index(cdzs), as.data.frame(cdzs))
    pczs <- cbind(index(pczs), as.data.frame(pczs))
    colnames(cdzs)[1] <- colnames(pczs)[1] <- "Date"
    cdzs <- melt(cdzs, id.vars = "Date")
    pczs <- melt(pczs, id.vars = "Date")
    # okay so we have z-scores, now we need corresponding bps levels
    cdlevel <- do.call(cbind, lapply(cdmats, last, numlast))
    pclevel <- do.call(cbind, lapply(pcmats, last, numlast))
    cdlevel <- cbind(index(cdlevel), as.data.frame(cdlevel))
    pclevel <- cbind(index(pclevel), as.data.frame(pclevel))
    colnames(cdlevel)[1] <- colnames(pclevel)[1] <- "Date"
    cdlevel <- melt(cdlevel, id.vars = "Date")
    pclevel <- melt(pclevel, id.vars = "Date")
    isinvec <- pczs[, 2] # all the isins
    ab <- allbonds() # get list of all bonds by counry so that we can classify the isins
    countries <- sapply(cdzs[, 2], function(x) names(ab)[sapply(ab, function(y) x %in% y)])
    cdzs <- cbind(countries, cdzs)
    pczs <- cbind(countries, pczs)
    # now get the maturity
    maturitycd <- maturityCD(cCodes = cCodes, model = model, days = years * 260, 
                             usedecay = useDecay, decayhl = decayhl, scale.unit = FALSE, mats = keymats)
    maturityzs <- lapply(maturitycd, function(x) matrixzs(x, days = numlast)$zs)
    maturityzs <- lapply(maturityzs, function(x) {y <- x; colnames(y) <- keymats; y})
    labels <- apply(cdzs[, c("countries", "variable")], 1, function(x) 
                    isinLabel(x[1], x[2], shortlab = FALSE, seper = "", withcCode = FALSE))
    mats <- apply(cdzs[, c("countries", "variable")], 1, function(x) bondMats(x[1], x[2]))
    bucks <- sapply(mats, function(x) names(buckets)[sapply(buckets, function(y) (x >= y[1]) && (x < y[2]))])
    cdcomplete <- cbind(cdzs, cdlevel$value, mats, bucks)
    sectorcds <- apply(cdcomplete, 1, function(x) maturitycd[[x[1]]][x[2], x[7]]) # get corresponding sector cds
    sectorzs <- apply(cdcomplete, 1, function(x) maturityzs[[x[1]]][x[2], x[7]]) # get corresponding sector zs
    cdcomplete <- cbind(labels, cdcomplete, sectorcds, sectorzs, pczs$value, pclevel$value)
    colnames(cdcomplete) <- c("label", "country", "date", "isin", "cdz", "cd", 
                              "mat", "bucket", "sectorcd", "sectorz", "pcz", "pc")
    return(cdcomplete)
    # now we have the maturities we must bind them into the larger vectors
    # melt them myself (screw reshape and ply)

}


zeroYields <- function(nsObj, mats = 1:35) {
# gets the zero curve yields from a dl, ns, or sv object
    meth = nsObj[[1]]$method # get the method
    zy <- t(sapply(nsObj, function(x) {
        switch(meth, 
            "ns" = spr_ns(x$startparam, mats) / 100,
            "sv" = spr_sv(x$startparam, mats) / 100,
            "asv" = spr_asv(x$startparam, mats) / 100,
            "dl" = spr_dl(x$startparam, mats, x$lambda) / 100,
            "cs" = splinefun(x$spot[, 1], x$spot[, 2])(mats)
        )
    }))
    if(!is.null(names(nsObj))) {
        zy <- xts(zy, order.by = as.Date(rownames(zy))) 
    }
    colnames(zy) <- mats
    return(zy)
}


fwdYields <- function(nsObj, mats = 1:35) {
# gets the forward curve yields from a dl, ns, or sv object
    meth = nsObj[[1]]$method # get the method
    zy <- t(sapply(nsObj, function(x) {
        switch(meth, 
            "ns" = fwr_ns(x$startparam, mats) / 100,
            "sv" = fwr_sv(x$startparam, mats) / 100,
            "asv" = fwr_asv(x$startparam, mats) / 100,
            "dl" = fwr_dl(x$startparam, mats, x$lambda) / 100,

        )
    }))
    if(!is.null(names(nsObj))) {
        zy <- xts(zy, order.by = as.Date(rownames(zy))) 
    }
    colnames(zy) <- mats
    return(zy)
}


emirstickers <- function(cCode, source = "CMPL") {
    maturities <- unique(c(1:10))
    ticker <- switch(cCode, 
        RU = "RRUSSW", 
        TR = "TYUSSW",
        ZA = "SASW",
        PL = "PZSW",
        HU = "HFSW",
        CZ = "CKSW",
        EU = "EUSA",
        US = "USSA", 
        MX = "MPSW")
    irst <- paste(ticker, maturities, sep = "")
    irst <- paste(irst, "CMPL")
    irst <- paste(irst, "Curncy")
}

emfratickers <- function(cCode, source = "CMPL") {
    maturities <- unique(c(1:10))
    ticker <- switch(cCode, 
        RU = "RRUSSW", 
        TR = "TYUSSW",
        ZA = "SASW",
        PL = "PZSW",
        HU = "HFSW",
        CZ = "CKSW",
        EU = "EUSA",
        US = "USSA", 
        MX = "MPSW")
    irst <- paste(ticker, maturities, sep = "")
    irst <- paste(irst, "CMPL")
    irst <- paste(irst, "Curncy")
}

emRates <- function(cCode, years = 3) {
    irst = emirstickers(cCode)
    flushprint(irst)
    yields <- bbdh(irst, years = 3)
    colnames(yields) <- maturities
    return(yields)
}

emhair <- function(inmatrix, col65 = colours()[548]) {
# plot a hair chart of the inmatrix, but also make sure that the colnames are the maturities
    ylims = c(min(inmatrix), max(inmatrix))
    mats <- as.numeric(colnames(inmatrix))
    plot(mats, inmatrix[1, ], col = "white", ylim = ylims, cex.axis = 0.8, cex.names = 0.8, xlab = "", ylab = "")
    rows <- nrow(inmatrix)
    cols = c(rep(addAlpha("grey80", 0.2), rows - 65), rep(addAlpha(col65, 0.2), 65))
    sapply(1:rows, function(x) xspline(mats, inmatrix[x, ], shape = 0.5, border = cols[x]))
    lines(spline(mats, inmatrix[rows, ]), lwd = 2, col = "black")
}

emhairmatrix <- function(cCodes = em) {
    par(mfrow = c(length(cCodes), length(cCodes)))
    par(oma = c(2, 2, 2, 2))
    par(mar = c(3, 1.25, 1.5, 1.25))
    rates <- lapply(cCodes, function(x) emRates(x, 2 * 260))
    names(rates) <- cCodes
    for (x in cCodes) {
        for(y in cCodes) {
            if(x != y) {
                plotmat <- rates[[x]] - rates[[y]]
                emhair(plotmat)
                title(paste(x, "-", y, sep = ""), font.main = 3, col.main = "grey30")
            } else {
                plotmat <- rates[[x]]
                emhair(plotmat)
                title(x, font.main = 2, col.main = "black")
                uu <- par("usr")
                rect(uu[1], uu[3], uu[2], uu[4], col = addAlpha("grey", 0.1))
            }
            abline(h = 0, col = colours()[613])
        }
    }
}

empca <- function(cCodes = em) {
    rates <- lapply(cCodes, function(x) emRates(x, 2 * 260))
    names(rates) <- cCodes
    pcs <- lapply(rates, function(x) {
        pc <- PCA(x, graph = FALSE)
        l1 <- pc$var$coord[, 1] / sqrt(pc$eig[1, 1])
        l2 <- pc$var$coord[, 2] / sqrt(pc$eig[2, 1])
        l3 <- pc$var$coord[, 3] / sqrt(pc$eig[3, 1])
        pc1 <- x %*% l1 / sqrt(ncol(x))
        pc2 <- x %*% l2 / sqrt(ncol(x))
        pc3 <- x %*% l3 / sqrt(ncol(x))
        returner <- cbind(pc1, pc2, pc3)
        colnames(returner) <- c("pc1", "pc2", "pc3")
        return(returner)
    })
    names(pcs) <- cCodes
    pc1s <- sapply(pcs, function(x) x[, 1])
    pc2s <- sapply(pcs, function(x) x[, 2])
    pc3s <- sapply(pcs, function(x) x[, 3])
    colnames(pc1s) <- paste(cCodes, "PC1")
    colnames(pc2s) <- paste(cCodes, "PC2")
    colnames(pc3s) <- paste(cCodes, "PC3")
    return(list(pc1s = pc1s, pc2s = pc2s, pc3s = pc3s))
}


emspreadget <- function(cCodes = em, spreads = list(c(2, 5), c(5, 10), c(2, 5, 10), c(2, 10)), years = 3, bps = TRUE) {
# for each spread, return an xts matrix of it for each country 
    if(class(spreads) != "list") spreads <- list(spreads)
    couponylds <- lapply(cCodes, function(cCode) last(emRates(cCode), years * 260))
    spreadvecs <- lapply(spreads, function(spread) {             # for each spread
        intervec <- lapply(couponylds, function(y) {                         # and for each country
            if(length(spread) == 2) {
                return(y[, spread[2]] - y[, spread[1]])
            } else {
                return(- y[, spread[1]] + 2 * y[, spread[2]] - y[, spread[3]])
            }
        })
        intervec <- do.call(cbind, intervec)
        names(intervec) <- cCodes
        if(bps) intervec <- intervec * 100 # turn to bps
        return(intervec)
    })
    names(spreadvecs) <- lapply(spreads, function(spread) {
        lab <- ""
        for(x in 1:(length(spread) - 1)) lab <- paste(lab, as.character(spread[x]), "-", sep = "")
        lab <- paste(lab, spread[length(spread)], sep = "")
        return(lab)
    })
    return(spreadvecs)
}


emspreadout <- function(spreadlist = emspreadget(), sethresh = 1.5, howmuchret = 21, win = TRUE, decayhl = 130) {
# this will take a spreadlist and find which spreads are out of whack using regression
    if(is.na(decayhl)) decayer <- rep(1, nrow(spreadlist[[1]])) else decayer <- decay(nrow(spreadlist[[1]]), decayhl) # decay vector 
    numcountry <- ncol(spreadlist[[1]])
    alloutboth <- lapply(names(spreadlist), function(x) {         # all the spreads
        zout <- sapply(colnames(spreadlist[[x]]), function(y) {      # all the countries
            rss <- regsubsets(spreadlist[[x]][, y] ~ ., 
                data = spreadlist[[x]][, -index(names(spreadlist[[x]]))[names(spreadlist[[x]]) == y]], weights = decayer) 
            rsswhich <- summary(rss)$which
            rssadjr2 <- summary(rss)$adjr2
            maxar2 <- max(rssadjr2)
            selector <- rsswhich[index(rssadjr2)[rssadjr2 == maxar2], -1] # all this to get the selctor
            selector <- names(selector)[selector] # just get the inclusion countries                
            lmodel <- lm(spreadlist[[x]][, y] ~ spreadlist[[x]][, selector], weights = decayer) # build the linear model
            resid <- lmodel$residuals #
            resid <- xts(as.numeric(resid), order.by = as.Date(index(resid)))
            sds <- na.omit(rollapply(resid, length(resid) - howmuchret + 1, function(x) wt.sd(x, last(decayer, length(x))))) # history of sd
            serr <- last(resid, howmuchret) / sds # standards errors
            return(list(serr, last(resid, howmuchret)))
        })
        squarese <- sapply(names(spreadlist[[x]]), function(m) {
            sapply(names(spreadlist[[x]]), function(p) {
                if (m == p) return(0)
                rr <- as.numeric(lm(spreadlist[[x]][, m] ~ spreadlist[[x]][, p], weights = decayer)$residuals)
                return(last(rr) / wt.sd(rr, decayer))
            })
        })
        return(list(zout, squarese))
    })
    allout <- lapply(alloutboth, "[[", 1) # get the spread by country z scores and residuals
    squareout <- lapply(alloutboth, "[[", 2) # get the country by country z score matrices
    names(allout) <- names(squareout) <- names(spreadlist)
    allzout <- lapply(allout, function(x) sapply(x[1, ], function(y) y)) # extract z scores
    allrout <- lapply(allout, function(x) sapply(x[2, ], function(y) y)) # extract actual bps
    names(allzout) <- names(allrout) <- names(spreadlist)
    mainplot <- sapply(allzout, last) # get the most recent for each one
    minn <- min(c(-2, sapply(allzout, min))) # get axis minimum
    maxx <- max(c(2, sapply(allzout, max))) # get axis maximum
    mzout <- melt(allzout) # for ggplot
    # now plot these guys
    pal <- brewer.pal(ncol(spreadlist[[1]]) + 3, "PRGn")[c(-3, -4, -6)]
    if (win) windows(9, 5)
    par(mar = c(1.5, 3, 2, 3), oma = c(1.5, 0, 0, 0))
    barplot(mainplot, beside = TRUE, col = "white", ylim = c(minn, maxx), border = "white", axes = FALSE, 
        space = c(0, 3)) # dummy plot
    u <- par("usr") # for the background grey shading rectangle
    rect(u[1], -sethresh, u[2], sethresh, col = "grey96", border = NA) # draw the rectangle
    barplot(mainplot, beside = TRUE, col = pal, ylim = c(minn, maxx), border = "grey50", add = TRUE, 
        space = c(0, 3)) -> bb
    abline(h = c(-sethresh, sethresh), col = addAlpha("grey50", 0.5), lty = "dashed")
    legend("topright", legend = colnames(spreadlist[[1]]), col = pal, pch = 15, bg = addAlpha("white", 0.5), box.lwd = 0, 
        cex = 0.8)
    title("Spread Z scores", col.main = "grey50", line = -0.5)
    # now plot all the subcharts
    lapply(names(allzout), function(x) {
        if (win) windows(9, 5)
        par(mar = c(1.5, 3, 2, 3), oma = c(1.5, 0, 0, 0))
        cols <- sapply(pal, function(p) c(rep("grey88", howmuchret - 1), p))
        wids <- c(rep(1, howmuchret - 1), 3)
        barplot(allzout[[x]], beside = TRUE, space = c(0, 10), col = "white", ylim = c(minn, maxx), width = wids, axes = FALSE,
            border = "white", names.arg = rep("", ncol(allzout[[x]])))
        u <- par("usr") # coords for the plot area 
        rect(u[1], -sethresh, u[2], sethresh, col = "grey96", border = NA) # draw the background rectangle
        barplot(allzout[[x]], beside = TRUE, space = c(0, 10), col = cols, ylim = c(minn, maxx), width = wids, 
            border = "grey50", names.arg = rep("", ncol(allzout[[x]])), add = TRUE) -> bb
        text(apply(bb, 2, mean), -(sethresh * 1.2), colnames(allzout[[x]]))
        axis(2)
        labelfactors <- last(allrout[[x]]) / last(allzout[[x]]) # work out the multipler for the individual bps axes
        axispos <- apply(bb, 2, last) + 3
        sapply(1:ncol(allrout[[x]]), function(i) {
            pty <- pretty(c(allrout[[x]][, i], 0))
            ptyat <- pty / labelfactors[i]
            excludepty <- ((ptyat > max(axTicks(2))) | (ptyat < min(axTicks(2)))) # where will mini axis be outside of main axis
            pty <- pty[!excludepty] # exclude that
            ptyat <- ptyat[!excludepty] # and exclude that too
            axis(4, at = ptyat, labels = pty, pos = axispos[i], padj = -2.5, tcl = -0.22, col = "grey70", cex.axis = 0.6, col.axis = "grey70")
            if(i == 1) text(axispos[1] + 2.55, max(ptyat), "bps", pos = 3, offset = 0.5, cex = 0.6, col = "grey70")
        })
        abline(h = c(-sethresh, sethresh), col = addAlpha("grey70", 0.5), lty = "dashed")
        title(x, col.main = "grey50", line = 0.0)
        title(paste(howmuchret, "day z-score history"), col.main = "grey50", cex.main = 0.8, line = -1)
        mtext(paste("  ", format(Sys.time(), "%Y-%m-%d  %Hh%M %Z")), side = 1, line = 0, outer = TRUE, 
            adj = 0, cex = 0.7, col = "grey", font = 3)
    })
    if(win) return(list(allout, squareout))
}



couponYields <- function(nsObj, create = FALSE, mats = 1:35) {
#returns a coupon yield curve from a zero yield curve
    if(class(nsObj) %in% "character") nsObj <- get(paste(nsObj, "data", sep = ""))[[keymodel[[nsObj]]]]
    if(create) {
        if(!all(diff(mats) == 1)) {
            flushprint("Can only calculate coupon yields for maturities of unit differences")
            return(FALSE)
        }
        matSize <- length(mats)
        zYields <- zeroYields(nsObj, mats)# start off yields at zero yields
        cz <- t(apply(zYields, 1, function(z) {
            sapply(1:matSize, function(x) {
                optimize(function(y) abs(100 - sum(sapply(1:x, function(i) 
                    ifelse(i == x, 100 + y * 100, y * 100) / (1 + z[i]) ^ i))), interval = c(0, 1))$minimum
            })
        }))
    } else {
        cz <- t(sapply(nsObj, function(x) x$ycoupon))
    }
    if(!is.null(names(nsObj))) {
        cz <- xts(cz, order.by = as.Date(rownames(cz)))
    }
    colnames(cz) <- mats
    return(cz)
}


bondMats <- function(cCode, isins, showinyears = TRUE) {
    statics <- get(paste(cCode, "data", sep = ""))$staticData
    mats <- sapply(isins, function(x) statics[statics[, "ID_ISIN"] == x, "MATURITY"])
    if(showinyears) {
        return((as.Date(mats) - Sys.Date()) / 365.25)
    } else {
        return(as.Date(mats))
    }
}


bdur <- function(cCode, ddate = NA)  {
# duration, modified duration, and DV01 of each bond for a country
    cData <- get(paste(cCode, "data", sep = ""))
    if(is.na(ddate)) obj <- last(cData$dynBoth)[[1]] else obj <- cData$dynBoth[[as.character(ddate)]]
    cf <- create_cashflows_matrix(obj[[1]], include_price = TRUE)   
    m <- create_maturities_matrix(obj[[1]], include_price = TRUE)
    y <- bond_yields(cf, m)
    ann <- ann_yields(cf, m) # annualised yield
    dur <- duration(cf, m, y[, 2])
    prc <- -cf[1, ]
    dv01 <- dur[, 2] * prc / 100
    mat <- apply(m, 2, max)
    ret <- cbind(mat, dur[, -3], dv01, prc, y[, "Yield"], ann[, "Yield"])
    colnames(ret) <- c("mat", "dur", "mdur", "dv01", "price", "yield", "annyield")
    return(ret)
}

cydur <- function(cCode, ddate = NA, model = keymodel[[cCode]]) {
# duration, modified duration, and DV01 of each maturity in the genericy couponyields of a country
    cData <- get(paste(cCode, "data", sep = ""))
    cy <- couponYields(cData[[model]])
    if(is.na(ddate)) y <- as.numeric(last(cy)) * 100 else y <- as.numeric(cy[ddate, ]) * 100
    cf <- matrix(rep(0, length(y) ^ 2), nrow = length(y), ncol = length(y))
    m <- matrix(rep(0, length(y) ^ 2), nrow = length(y), ncol = length(y))
    for(x in 1:length(y)) {
        for(z in 1:length(y)) {
            if(z < x) {
                cf[z, x] <- y[x] 
                m[z, x] <- z
            } else if(z == x) {
                cf[z, x] <- 100 + y[x]
                m[z, x] <- z
            } else {
                cf[z, x] <- 0
                m[z, x] <- 0
            }
        }
    }
    cf <- rbind(rep(-100, length(y)), cf)
    m <- rbind(rep(0, length(y)), m)
    colnames(m) <- colnames(cf) <- as.character(1:length(y))
    yy <- bond_yields(cf, m)
    dur <- duration(cf, m, yy[, 2])
    prc <- -cf[1, ]
    dv01 <- dur[, 2] * prc / 100
    mat <- apply(m, 2, max)
    ret <- cbind(mat, dur[, -3], dv01, prc, yy[, "Yield"], y / 100)
    colnames(ret) <- c("mat", "dur", "mdur", "dv01", "price", "yield", "annyield")
    return(ret)
}


allYieldsNew <- function(cCodes = ac, model = "auto", mats = 1:35, days = 260 * 3, daysback = 0, 
                         type = c("coupon", "zero", "fwd")) {
# a list of all the coupon yields for all countries
    yieldfun <- switch(type[1], 
        "coupon" = couponYields,
        "zero" = zeroYields,
        "fwd" = fwdYields)
    ay <- lapply(cCodes, function(cCode) {
        cModel <- get(paste(cCode, "data", sep = ""))[[ifelse(model == "auto", keymodel[[cCode]], model)]]
        return(first(last(yieldfun(cModel, mats = mats), days + daysback), days))
    })
    names(ay) <- cCodes
    return(ay)
}


allYields <- function(cCodes = ac, model = "auto", mats = 1:35, days = 260 * 3, daysback = 0, combine = FALSE) {
# a list of all the coupon yields for all countries
    ay <- lapply(cCodes, function(cCode) {
        cModel <- get(paste(cCode, "data", sep = ""))[[ifelse(model == "auto", keymodel[[cCode]], model)]]
        return(first(last(couponYields(cModel)[, mats], days + daysback), days))
    })
    names(ay) <- cCodes
    if(combine) {
        ay <- do.call(cbind, ay)
        colnames(ay) <- do.call(c, lapply(cCodes, function(cCode) paste(cCode, as.character(mats), sep = "")))
    }

    return(ay)
}

    

addCouponYields <- function(nsObj) {
# adds coupon yields
    cy <- couponYields(nsObj, create = TRUE)
    added <- lapply(1:length(nsObj), function(x) {nsObj[[x]]$ycoupon <- cy[x, ]; nsObj[[x]]})
    names(added) <- names(nsObj)
    return(added)
}


getBetas <- function(cCodes, days = 260 * 3, dlBoth = TRUE) {
# get the first 3 diebold li factors from the country objects specified
    betas <- lapply(cCodes, function(cCode) {
        dlObj <- if(dlBoth) last(get(paste(cCode, "data", sep = ""))$dlBoth, days) # get the correct dl object
            else last(get(paste(cCode, "data", sep = ""))$dl, days)
        t(sapply(dlObj, function(x) x$startparam))
    })
    names(betas) <- cCodes
    #!!!!! must check if dates corresond when more than one country
    return(betas)
}


getPCAs <- function(cCodes = ac, days = 260 * 3, model = "auto", decayhl = 260, zeros = FALSE, fwds = FALSE, series = FALSE,
                    colSelect = as.numeric(names(buckets)), silent = TRUE, usedecay = TRUE, daysback = 0, scale.unit = TRUE) {
    if(usedecay) decayer <- decay(days, decayhl) else decayer <- rep(1, days)
    pcs <- lapply(cCodes, function(cCode) {
        svObj <- first(last(get(paste(cCode, "data", sep = ""))[[ifelse(model == "auto", keymodel[[cCode]], model)]], 
                            days + daysback), days) # get the correct dl object, adjust days
        if(zeros) {
            rateMat <- zeroYields(svObj) 
        } else if(fwds) {
            rateMat <- fwdYields(svObj)
        } else {
            rateMat <- couponYields(svObj)
            if(!("xts" %in% class(rateMat))) {
                rateMat <- as.xts(rateMat, order.by = as.Date(rownames(rateMat)))
            }
        }
        rateMat <- rateMat[, colSelect] # select the right columns to use so as not to overweight long end
        if(!series) diffMat <- diffret(rateMat) else diffMat <- rateMat # off returns? or outright?  
        pca <- PCA(diffMat, graph = FALSE, row.w = decayer, 
            scale.unit = scale.unit, ncp = 7)
        load1 <- pca$var$coord[, 1] / sqrt(pca$eig[1, 1])# / ncol(diffMat) # level component
        if(mean(load1) < 0) load1 <- -load1 # ensure positive loadings
        load2 <- pca$var$coord[, 2] / sqrt(pca$eig[2, 1])# / ncol(diffMat) # slope component
        if(last(load2) < 0) load2 <- -load2 # ensure positive slopes
        load3 <- pca$var$coord[, 3] / sqrt(pca$eig[3, 1])# / ncol(diffMat) # curvature component
        if(length(rle(as.numeric(load3) > 0)) == 3) { # if it is indeed pc3
            if(last(load3) > 0) {
                load3 <- -load3
            }
        }
        load4 <- pca$var$coord[, 4] / sqrt(pca$eig[4, 1])# / ncol(diffMat) # waviness component
        load5 <- pca$var$coord[, 5] / sqrt(pca$eig[5, 1])# / ncol(diffMat) # waviness component
        load6 <- pca$var$coord[, 6] / sqrt(pca$eig[6, 1])# / ncol(diffMat) # waviness component
        load7 <- pca$var$coord[, 7] / sqrt(pca$eig[7, 1])# / ncol(diffMat) # waviness component
        if(!silent) {
            flushprint(cCode)
            flushprint(pca$var$coord)
        }
        pc1 <- diffMat %*% load1 / sum(abs(load1)) # divide by abs so makese sense versus inputs
        pc2 <- diffMat %*% load2 / sum(abs(load2))
        pc3 <- diffMat %*% load3 / sum(abs(load3))
        pc4 <- diffMat %*% load4 / sum(abs(load4))
        pc5 <- diffMat %*% load5 / sum(abs(load5))
        pc6 <- diffMat %*% load6 / sum(abs(load6))
        pc7 <- diffMat %*% load7 / sum(abs(load7))
        all4 <- xts(cbind(pc1, pc2, pc3, pc4, pc5, pc6, pc7), order.by = last(index(rateMat), length(pc1)))
        allloads <- cbind(load1, load2, load3, load4, load5, load6, load7)
        return(list(allpcs = all4, alloads = allloads, eigs = pca$eig[, 1], yields = rateMat))
    })
    countryloads <- lapply(pcs, "[[", 2)
    countryeigs <- sapply(pcs, "[[", 3)
    colnames(countryeigs) <- cCodes
    countryyields <- lapply(pcs, "[[", 4)
    names(countryyields) <- cCodes
    pcs <- lapply(pcs, "[[", 1)
    names(countryloads) <- cCodes
    names(pcs) <- cCodes
    pcsi <- lapply(1:ncol(pcs[[1]]), function(i) {
        do.call(cbind, lapply(pcs, function(x) x[, i]))
    }) # transpose
    if(usedecay) decayer <- decay(nrow(pcsi[[1]]), decayhl) else decayer <- rep(1, nrow(pcsi[[1]]))
    # now do pc of pcs
    pcsi <- lapply(pcsi, function(x) {   # put in colnames and do the cross country PCAs
        pca <-PCA(x, graph = FALSE, row.w = decayer, scale.unit = scale.unit)
        load1 <- pca$var$coord[, 1] / sqrt(pca$eig[1, 1])
        if(!silent) flushprint(pca$var$coord)
        pc1 <- x %*% load1
        x <- cbind(x, pc1)
        colnames(x) <- c(cCodes, "ALL")
        return(x)
    })
    # now do cross country PCA
    ay <- do.call(cbind, allYields(cCodes = cCodes, mats = colSelect, model = model, days = days))
    if(usedecay) decayer <- decay(nrow(ay), decayhl) else decayer <- rep(1, nrow(ay))
    if(!series) ay <- diffret(ay) # rets if necessary
    colnames(ay) <- unlist(lapply(cCodes, function(x) paste(x, colSelect, sep = "")))
    pca <- PCA(ay, graph = FALSE, row.w = decayer, scale.unit = scale.unit, ncp = 10)
    load1 <- pca$var$coord[, 1] / sqrt(pca$eig[1, 1])
    load2 <- pca$var$coord[, 2] / sqrt(pca$eig[2, 1])
    load3 <- pca$var$coord[, 3] / sqrt(pca$eig[3, 1])
    load4 <- pca$var$coord[, 4] / sqrt(pca$eig[4, 1])
    load5 <- pca$var$coord[, 5] / sqrt(pca$eig[5, 1])
    load6 <- pca$var$coord[, 6] / sqrt(pca$eig[6, 1])
    load7 <- pca$var$coord[, 7] / sqrt(pca$eig[7, 1])
    load8 <- pca$var$coord[, 8] / sqrt(pca$eig[8, 1])
    load9 <- pca$var$coord[, 9] / sqrt(pca$eig[9, 1])
    load10 <- pca$var$coord[, 10] / sqrt(pca$eig[10, 1])
    pc1 <- ay %*% load1 / sum(abs(load1))
    if(mean(pc1) < 0) { # pivot to make sense
        load1 <- -load1
        pc1 <- -pc1
    }
    pc2 <- ay %*% load2 / sum(abs(load2))
    if(mean(pc2) < 0) { # pivot to make sense
        load2 <- -load2
        pc2 <- -pc2
    }
    pc3 <- ay %*% load3 / sum(abs(load3))
    if(mean(pc3) < 0) { # pivot to make sense
        load3 <- -load3
        pc3 <- -pc3
    }
    pc4 <- ay %*% load4 / sum(abs(load4))
    if(mean(pc4) < 0) { # pivot to make sense
        load4 <- -load4
        pc4 <- -pc4
    }
    pc5 <- ay %*% load5 / sum(abs(load5))
    pc6 <- ay %*% load6 / sum(abs(load6))
    pc7 <- ay %*% load7 / sum(abs(load7))
    pc8 <- ay %*% load8 / sum(abs(load8))
    pc9 <- ay %*% load9 / sum(abs(load9))
    pc10 <- ay %*% load10 / sum(abs(load10))
    crosspc <- cbind(pc1, pc2, pc3, pc4, pc5, pc6, pc7, pc8, pc9, pc10)
    crosspc <- xts(crosspc, order.by = as.Date(index(ay)))
    crosseigs <- pca$eig[, 1]
    crossyields <- ay
    pcsi[[8]] <- countryloads
    pcsi[[9]] <- countryeigs
    pcsi[[10]] <- countryyields
    pcsi[[11]] <- crosspc
    crossloads <- cbind(load1, load2, load3, load4, load5, load6, load7, load8, load9, load10)
    pcsi[[12]] <- crossloads
    pcsi[[13]] <- crosseigs
    pcsi[[14]] <- crossyields
    names(pcsi) <- c("pc1", "pc2", "pc3", "pc4", "pc5", "pc6", "pc7", 
                     "countryloads", "countryeigs", "countryyields", "crosscountry", "crossloads", "crosseigs", "crossyields")
    return(pcsi)
}

eigenRoll <- function(inmat, rollwindow = 65, scale.unit = FALSE) {
# gets all the eigenvectors and eigenvalues evolution
# returns the eigenvectors, each eigenvalue, and the summ100 cumsum eigenvalues
    if(scale.unit) corfun <- cor else corfun <- cov # use correlation or covariance
    eigenvalues <- na.omit(rollapply(inmat, rollwindow, function(x) eigen(corfun(x))$values, by.column = FALSE)) # all eigenvalues
    eigenvecs <- lapply(1:ncol(inmat), function(ee) {                # matrix for each eigvenvector
        fixEigen(na.omit(rollapply(inmat, rollwindow, function(x) eigen(corfun(x))$vectors[, ee], by.column = FALSE)))
    })
    names(eigenvecs) <- 1:ncol(inmat)
    cumulvalues <- t(apply(eigenvalues, 1, function(x) cumsum(x) / sum(x)))
    percentvalues <- t(apply(eigenvalues, 1, function(x) (x / sum(x)) * 100))
    fulleigen <- eigen(corfun(inmat))
    # now orient fulleigen properly
    if("xts" %in% class(inmat)) {
        dates <- last(index(inmat), nrow(eigenvalues))
        cumulvalues <- xts(cumulvalues, order.by = dates)
        eigenvalues <- xts(eigenvalues, order.by = dates)
        percentvalues <- xts(percentvalues, order.by = dates)
    }
    return(list(eigenvalues = eigenvalues, eigenvectors = eigenvecs, cumulvalues = cumulvalues, 
                percentvalues = percentvalues, rollwindow = rollwindow))
}

eigenRollPlot <- function(eigenobj, axislabels = NA, prelabel = "", maxplot = 5, win = TRUE, logy = FALSE) {
    bluedays = 262
    yellowdays = 65
    numvecs <- length(eigenobj$eigenvectors)
    plotcol = addAlpha("grey", 0.07)
    todaycol = "green4"
    yellowcol = addAlpha("firebrick2", 0.15)
    bluecol = addAlpha("turquoise3", 0.15)
    for(x in 1:(min(maxplot, numvecs))) {
        if(win) windows(8, 10)
        par(mfrow = c(3, 1))
        layout(matrix(c(1, 3)), heights = c(3, 1))
        plot(1:numvecs, rep(1, numvecs), col = "white", ylim = c(-1, 1), axes = FALSE, xlab = "", ylab = "")
        apply(eigenobj$eigenvectors[[x]], 1, function(x) lines(1:numvecs, x, col = plotcol))
        apply(last(eigenobj$eigenvectors[[x]], bluedays), 1, function(x) lines(1:numvecs, x, col = bluecol))
        apply(last(eigenobj$eigenvectors[[x]], yellowdays), 1, function(x) lines(1:numvecs, x, col = yellowcol))
        lines(1:numvecs, eigenobj$eigenvectors[[x]][nrow(eigenobj$eigenvectors[[x]]), ], lwd = 2, col = todaycol)
        grid()
        abline(h = 0)
        points(1:numvecs, eigenobj$eigenvectors[[x]][nrow(eigenobj$eigenvectors[[x]]), ], pch = 15, col = todaycol)
        axis(2)
        if(is.na(axislabels)) axis(1) else axis(1, labels = axislabels, at = 1:numvecs)
        title(paste(prelabel, "loading", x, "evolution"))
        title(paste(eigenobj$rollwindow, "-day rolling window", sep = ""), line = 0, cex.main = 1, 
              col.main = "grey50", font.main = 3)
        legend("bottomright", legend = c(paste(nrow(eigenobj$eigenvectors[[x]]), "-day history", sep = ""),
               paste(yellowdays, "-day history", sep = ""),
               paste(bluedays, "-day history", sep = ""), "today"),
               fill = c(plotcol, yellowcol, bluecol, todaycol))
        plot(eigenobj$percentvalues[, x], xlab = "", ylab = "log scale", main = "", log = "y",
             minor.ticks = FALSE, major.format = "%Y", cex.axis = 0.8, cex.lab = 0.8, col.lab = "grey")
        points(last(eigenobj$percentvalues[, x], pch = 19))
        text(last(eigenobj$percentvalues[, x]), last(eigenobj$percentvalues[, x]))
        title(paste("eigenvalue", x, "% of total"))
        # now plot the actual variable
        browser()

    }
}

eigAnalysis <- function(cCodes, model = "auto", rollwindow = 65, diff = TRUE, 
                        scale.unit = FALSE, maxplot = 7, win = TRUE, mats = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 15, 20, 25, 30),
                        maxdays = 2620) {
    lapply(cCodes, function(cCode) {
        nsobj <- get(paste(cCode, "data", sep = ""))[[keymodel[[cCode]]]]
        if(diff) mat <- diffret(last(couponYields(nsobj), maxdays)[, mats]) 
            else mat <- last(couponYields(nsobj), maxdays)[, mats]
        eigenRollPlot(eigenRoll(mat, rollwindow = rollwindow, scale.unit = scale.unit),
            axislabels = format(mats), prelabel = paste(cCode, keymodel[[cCode]], ifelse(diff, "return, ", "level, ")), maxplot = maxplot, win = win)
    })
}



doAll <- function(cCode, method, offon, accrued = FALSE, pcores = 8) {
# do all the list create, ns creation, and saving, for a country
    loadCountry(cCode)
    cdata <- get(paste(cCode, "data", sep = "")) # get the data set
    flushcat("loaded", cCode, "\n")
    dyncb <- lapply(names(cdata$actives), function(x) createCouponBonds(cCode, x, offon, accrued))
    names(dyncb) <- names(cdata$actives) # let's retain the names ;
    flushprint("done dyncb")
    ns <- dons(dyncb, meth = method, taucon = (if(method == "sv") list(svTau[[cCode]]) 
         else list(nsTau)), pcores = pcores)
    names(ns) <- names(dyncb) # and let's STILl retain the names
    flushcat("done curve fitting", "\n", "saving.....")
    save(dyncb, file = paste("data/", cCode, offon, "dyncb.dat", sep = ""))
    save(ns,file = paste("data/", cCode, method, offon, ".dat", sep = ""))
    flushcat("all done", cCode, "\n")
}

getSven <- function(cCode, model = keymodel[[cCode]]) {
# gets the Svensson (or other model) coefficients. CCode can be a ccode or else an nsobj directly
    if("list" %in% class(cCode)) { # if we were sent an 
        nsobj <- cCode
    } else {
        cData <- get(paste(cCode, "data", sep = ""))
        nsobj <- cData[[model]]
    }
    sven <- t(sapply(nsobj, function(x) x$startparam))
    method <- first(nsobj)[[1]]$method
    cols <- switch(method, 
        "ns" = c("beta0", "beta1", "beta2", "tau1"),
        "sv" = c("beta0", "beta1", "beta2", "tau1", "beta3", "tau2"),
        "asv" = c("beta0", "beta1", "beta2", "tau1", "beta3", "tau2"),
        "dl" = c("beta0", "beta1", "beta2", "lambda"))
    if(method == "dl") {
        lambda <- sapply(nsobj, function(x) x$lambda)
        sven <- cbind(sven, lambda)
    }
    colnames(sven) <- cols
    return(sven)
}


# ----------------------------------------- GRAPHING FUNCTIONS --------------------------------------------------- #

stheme <- list(chartsize = c(8, 8), 
                   outermargins = c(3, 2),
                   mainyield = "black", 
                   mainlwd = 2, 
                   curyield = addAlpha("red", 0.65), 
                   curyielddecay = addAlpha("deeppink", 0.65),
                   curlwd = 2.5, 
                   curcex = 1.4, 
                   curpch = 19, 
                   curoutline = addAlpha("red", 0.3), 
                   offruncol = addAlpha("orange", 0.65),
                   offrunfair = addAlpha("purple", 0.65),
                   hair = addAlpha("darkgray", 0.1),
                   hairlegend = addAlpha("darkgray", 0.5),
                   hairRecent = addAlpha("darkgoldenrod4", 0.12), 
                   hairRecentlegend = addAlpha("darkgoldenrod4", 0.25),
                   daysbackcol = "darkgoldenrod1",
                   daysbackstyle = "dashed",
                   titlefont = 1,
                   titlesize = 1.2, 
                   titlecol = "grey20", 
                   subtitlesize = 1, 
                   axiscol = "Azure3",
                   labelfont = 1, 
                   labelsize = 0.7,
                   timestampsize = 0.7,
                   timestampfont = 3,
                   timestampcol = "Azure3",
                   legendback = "#EEEEEEDD", 
                   boxcol = addAlpha("grey30"),
                   boxback = addAlpha("grey50"),
                   subboxcol = addAlpha("grey", 0.7),
                   subboxborder = addAlpha("royalblue1", 0.8),
                   subboxwidth = 0.4,
                   boxhistpch = 4, 
                   gridcol = addAlpha("azure3", 0.5),
                   gridlines = "solid")


# will price 

makelabels <- function(cCode, bbcodes, fromisin = FALSE) {
# recall bbcodes (or isins) *must* all come from same country
    if(is.na(cCode)) {
        if(!fromisin) {
            flushprint("cannot calculate country from bbcode --- please use ISIN")
            return(-1)
        } else {
            bondnames <- allbonds()
            cCode <- names(bondnames)[sapply(bondnames, function(x) bbcodes[1] %in% x)]
        }
    }
    if(fromisin) bbcodes <- isin2bb(cCode, bbcodes)
    staticdata <- get(paste(cCode, "data", sep = ""))$staticData
    bondmatdates <- as.Date(staticdata[bbcodes, "MATURITY"])  # label maturity
    #bondmatdates <- bondmatdates[order(bondmatdates)]
    bondcoupons <- staticdata[bbcodes, "COUPON"] # label coupon
    labels = paste(format(round(bondcoupons, 2)), "% ", format(bondmatdates, "%b%y"), sep = "") # make the labels
    return(list(labels = labels, coupons = bondcoupons, mats = bondmatdates))
}        


isinSearch <- function(isin, withcoupon = FALSE) {
    ab <- allbonds()
    cCode <- names(ab)[sapply(ab, function(x) isin %in% x)]
    bbCode <- isin2bb(cCode, isin)
    staticdata <- get(paste(cCode, "data", sep = ""))$staticData
    maturity <- as.Date(staticdata[bbCode, "MATURITY"])  # label maturity
    #bondmatdates <- bondmatdates[order(bondmatdates)]
    coupon <- round(as.numeric(staticdata[bbCode, "COUPON"]), 3)
    isinmonth <- as.numeric(format(maturity, "%m"))
    isinyear <- as.numeric(format(maturity, "%y"))
    if(withcoupon) {
        return(list(cCode, isinmonth, isinyear, coupon))
    } else {
        return(list(cCode, isinmonth, isinyear))
    }
}


isActive <- function(cCode, bondList, fromisin = TRUE) {
# checks the last active list in the cData and sees which of bonList is in it. IE which are ative. 
    if(fromisin) bondList <- isin2bb(cCode, bondList) # convert to bloomberg codes
    cData <- get(paste(cCode, "data", sep = ""))
    lastActives <- last(cData$actives)[[1]]
    return(bondList %in% lastActives)
}
    

lightplotgrid <- function(cCode, model = keymodel[[cCode]], years = 3, daysback = 1, log = FALSE, bplot = FALSE, 
    smooth = TRUE, win = TRUE, ylims = "auto") {
# this is going to plot a lightns object
    # first create the chart theme list. stheme is the default SAGB style theme
    cData <- get(paste(cCode, "data", sep = ""))
    lightobj <- cData[[model]]
    utheme <- stheme # use theme 
    if(win) windows(winwidth, 12) # create window otherwise it's been pdf'd away
    vp1 <- viewport(x = 0.5, y = 0.625, width = 1, height = 0.75)
    vp2 <- viewport(x = 0.5, y = 0.125, width = 1, height = 0.25)
    pushViewport(vp1)
    par(new = TRUE, fig = gridFIG())
    par(mar = c(3, utheme$outermargins[1], 6, utheme$outermargins[2])) # put a big margin at the bottom
    recentdays <- 21 * 3 # 3 months
    plotobj <- last(lightobj, years * 260) # obj containing years of data to plot
    couponylds <- couponYields(plotobj) * 100
    mats <- unlist(sapply(plotobj, function(x) x$yhat[, 1])) # maturities
    xlims <- c(min(mats), max(mats)) # x axis limits
    yields <- unlist(sapply(plotobj, function(x) x$yhat[, 2])) * 100 # all the yields
    if(ylims == "auto") ylims <- c(min(yields), max(yields)) # y axis limits
    latest <- last(plotobj)[[1]]
    latest$yhat[, 2] <- latest$yhat[, 2] * 100 # for plotting big numbers not small ones    
    latest$y[, 2] <- latest$y[, 2] * 100 # same again
    plot(latest$yhat, col = "white", xlim = xlims, ylim = ylims, axes = FALSE, log = ifelse(log, "x", ""),
        xlab = "", ylab = "") # dummy plot to get the coords right
    abline(v = latest$yhat[, 1], col = utheme$gridcol, lty = utheme$gridlines) # vertical gridlines corresponding to bonds
    colhair <- rep(utheme$hair, length(plotobj))
    colhair[(length(colhair) - recentdays):length(colhair)] <- utheme$hairRecent
    if(smooth) {
        intmax <- trunc(xlims[2])
        lapply(1:nrow(couponylds), function(x) lines(minMaturity:intmax, couponylds[x, minMaturity:intmax], col = colhair[x]))
    } else {
        for(x in 1:length(plotobj)) xspline(plotobj[[x]]$yhat[, 1], plotobj[[x]]$yhat[, 2] * 100, 
            shape = -0.5, border = colhair[x]) # plot the hair chart
    }
    dbackline <- plotobj[[length(plotobj) - daysback]]$yhat # days back line
    dbackline[, 2] <- dbackline[, 2] * 100 # for plotting
    xspline(dbackline[, 1], dbackline[, 2], shape = 0, border = utheme$daysbackcol, lwd = utheme$mainlwd) # daysback line
    xspline(latest$yhat[, 1], latest$yhat[, 2], shape = -0, border = utheme$mainyield, lwd = utheme$mainlwd) # now line
    xspline(latest$y, border = utheme$curyield, lwd = utheme$curlwd) # draw thw actual yield curve
    points(latest$y, col = utheme$curoutline, bg = utheme$curyield, cex = utheme$curcex, pch = utheme$curpch) # points on actual yield curve
    bondisins <- rownames(latest$yhat) # get the isins of the lastest bonds
    staticdata <- get(paste(latest$name, "data", sep = ""))$staticData # get the static data - will need it for label info
    bondmatdates <- as.Date(staticdata[isin2bb(latest$name, bondisins), "MATURITY"])  # label maturity
    bondmatdates <- bondmatdates[order(bondmatdates)]
    bondcoupons <- staticdata[isin2bb(latest$name, bondisins), "COUPON"] # label coupon
    labels = paste(format(round(bondcoupons, 2)), format(bondmatdates, "%b %y")) # make the labels
    labels1 <- labels[rep(1:2, length(labels)/2) == 1] # offset label set 1
    labels2 <- labels[!(labels %in% labels1)] # offset label set 2
    labelpos1 <- latest$yhat[, 1][labels %in% labels1] # offset position 1
    labelpos2 <- latest$yhat[, 1][labels %in% labels2] # offset position 2
    axis(1, col = utheme$axiscol, at = labelpos1, labels = labels1, las = 2, cex.axis = utheme$labelsize) # first set of labels
    axis(3, at = labelpos2, labels = labels2, las = 2, cex.axis = utheme$labelsize, col = utheme$axiscol) # second set of labels
    axis(3, line = -2, col = utheme$axiscol, las = 2, cex.axis = utheme$labelsize)
    abline(h = 0, col = utheme$axiscol, lty = "dashed")
    axis(2, col = utheme$axiscol, cex.axis = utheme$labelsize) # and of course, a yield axia
    #!! must stick legend in here
    title(paste(latest$name, ifelse(log, "- logscale", "")), line = 5, col.main = utheme$titlecol,              
        font.main = utheme$titlefont, cex.main = utheme$titlesize)     # plot the title
    legend("bottomright", c(paste(years, "year history"), paste(recentdays, "day history"),
           "NS fair curve", "Actual curve", paste("NS fair", daysback, "days ago")), 
           col = c(utheme$hairlegend, utheme$hairRecentlegend, utheme$mainyield, utheme$curyield, 
           utheme$daysbackcol), lwd = 2, lty = c(rep("solid", 4), utheme$daysbackstyle), 
           cex = utheme$labelsize, bg = utheme$legendback, box.col = utheme$axiscol)
    # NOW LET's chuck in the GREENIES latest yield
    if(bplot == TRUE) {
        cData <- get(paste(latest$name, "data", sep = ""))
        bonds <- last(cData$actives)[[1]]
        yy <- last(cData$historicData$YLD_YTM_MID[, bonds])
        mm <- (as.Date(cData$staticData[bonds, "MATURITY"]) - Sys.Date()) / 365
        points(mm, yy, col = "green")
        lines(mm, yy, col = "green")
    }
    popViewport()
    pushViewport(vp2)
    par(new = TRUE, fig = gridFIG())
    # NOW we plot the change chart
    par(mar = c(3.5, utheme$outermargins[1], 3, utheme$outermargins[2])) # put a big margin at the bottom
    commonisin <- intersect(rownames(latest$y), rownames(dbackline)) # bonds in both history and today
    change <- (latest$yhat[commonisin, 2] - dbackline[commonisin, 2]) * 100 # calculate change, turn into bps
    changeMats <- latest$yhat[commonisin, 1]
    bchange <- (latest$y[commonisin, 2] - plotobj[[length(plotobj) - daysback]]$y[commonisin, 2] * 100) * 100 # bond change
    yChangeLims <- c(min(c(change, bchange)), max(c(change, bchange))) # for axis calculation
    yChangeRange <- yChangeLims[2] - yChangeLims[1]
    yChangeAdd <- yChangeRange * 0.2
    if(yChangeLims[1] < 0) yChangeLims[1] <- yChangeLims[1] - yChangeAdd
    if(yChangeLims[2] > 0) yChangeLims[2] <- yChangeLims[2] + yChangeAdd
    plot(changeMats, bchange, xlim = xlims, ylim = yChangeLims, frame = FALSE, xlab = NA, cex = 0, xaxt = "n",
        yaxt = "n", log = ifelse(log, "x", "")) # plot a blank frame
    grid()
    polygon(c(changeMats, rev(changeMats)), c(rep(0, length(change)), rev(change)), 
        col = addAlpha(utheme$axiscol, 0.5), border = addAlpha(utheme$axiscol, 0.70))
    rect(changeMats - 0.2, 0, changeMats + 0.2, bchange, col = addAlpha(utheme$axiscol, 0.8), border = utheme$axiscol)
    axis(1, las = 2, cex.axis = utheme$labelsize, col = utheme$axiscol) # second set of labels
    #p <- (change / abs(change)) + 2 #this decides whether labels go above or below change bars
    #p[is.na(p)] <- 1 # if no change in yield today then plot above the change bars
    axis(2, col = utheme$axiscol, cex.axis = utheme$labelsize)
    title(paste("Change from", daysback, ifelse(daysback == 1, "day ago", "days ago")), 
        col.main = utheme$titlecol, font.main = utheme$titlefont, cex.main = utheme$titlesize * 0.7,
        line = 1)
    # now the time stamp
    popViewport()
    mtext(format(Sys.time(), "%Y-%m-%d %Hh%M %Z"), side = 1, line = 1.5, outer = FALSE, 
        adj = 0, cex = utheme$timestampsize, col = utheme$timestampcol, font = utheme$timestampfont)
}

lightplot <- function(cCode, model = keymodel[[cCode]], years = 3, daysback = 1, log = FALSE, bplot = FALSE, 
    smooth = TRUE, win = TRUE, ylims = "auto") {
# this is going to plot a lightns object
    # first create the chart theme list. stheme is the default SAGB style theme
    cData <- get(paste(cCode, "data", sep = ""))
    lightobj <- cData[[model]]
    utheme <- stheme # use theme 
    if(win) windows(winwidth, 12) # create window otherwise it's been pdf'd away
    par(mar = c(4, utheme$outermargins[1], 7, utheme$outermargins[2])) # put a big margin at the bottom
    par(mfrow = c(2, 1))
    layout(matrix(c(1, 2)), heights = c(3, 1))
    recentdays <- 21 * 3 # 3 months
    plotobj <- last(lightobj, years * 260) # obj containing years of data to plot
    couponylds <- couponYields(plotobj) * 100
    mats <- unlist(sapply(plotobj, function(x) x$yhat[, 1])) # maturities
    xlims <- c(min(mats), max(mats)) # x axis limits
    yields <- unlist(sapply(plotobj, function(x) x$yhat[, 2])) * 100 # all the yields
    if(ylims == "auto") ylims <- c(min(yields), max(yields)) # y axis limits
    latest <- last(plotobj)[[1]]
    latest$yhat[, 2] <- latest$yhat[, 2] * 100 # for plotting big numbers not small ones    
    latest$y[, 2] <- latest$y[, 2] * 100 # same again
    plot(latest$yhat, col = "white", xlim = xlims, ylim = ylims, axes = FALSE, log = ifelse(log, "x", ""),
        xlab = "", ylab = "") # dummy plot to get the coords right
    abline(v = latest$yhat[, 1], col = utheme$gridcol, lty = utheme$gridlines) # vertical gridlines corresponding to bonds
    colhair <- rep(utheme$hair, length(plotobj))
    colhair[(length(colhair) - recentdays):length(colhair)] <- utheme$hairRecent
    if(smooth) {
        intmax <- trunc(xlims[2])
        lapply(1:nrow(couponylds), function(x) lines(minMaturity:intmax, couponylds[x, minMaturity:intmax], col = colhair[x]))
    } else {
        for(x in 1:length(plotobj)) xspline(plotobj[[x]]$yhat[, 1], plotobj[[x]]$yhat[, 2] * 100, 
            shape = -0.5, border = colhair[x]) # plot the hair chart
    }
    dbackline <- plotobj[[length(plotobj) - daysback]]$yhat # days back line
    dbackline[, 2] <- dbackline[, 2] * 100 # for plotting
    xspline(dbackline[, 1], dbackline[, 2], shape = 0, border = utheme$daysbackcol, lwd = utheme$mainlwd) # daysback line
    xspline(latest$yhat[, 1], latest$yhat[, 2], shape = -0, border = utheme$mainyield, lwd = utheme$mainlwd) # now line
    xspline(latest$y, border = utheme$curyield, lwd = utheme$curlwd) # draw thw actual yield curve
    points(latest$y, col = utheme$curoutline, bg = utheme$curyield, cex = utheme$curcex, pch = utheme$curpch) # points on actual yield curve
    bondisins <- rownames(latest$yhat) # get the isins of the lastest bonds
    staticdata <- get(paste(latest$name, "data", sep = ""))$staticData # get the static data - will need it for label info
    bondmatdates <- as.Date(staticdata[isin2bb(latest$name, bondisins), "MATURITY"])  # label maturity
    bondmatdates <- bondmatdates[order(bondmatdates)]
    bondcoupons <- staticdata[isin2bb(latest$name, bondisins), "COUPON"] # label coupon
    labels = paste(format(bondcoupons), format(bondmatdates, "%b %y")) # make the labels
    labels1 <- labels[rep(1:2, length(labels)/2) == 1] # offset label set 1
    labels2 <- labels[!(labels %in% labels1)] # offset label set 2
    labelpos1 <- latest$yhat[, 1][labels %in% labels1] # offset position 1
    labelpos2 <- latest$yhat[, 1][labels %in% labels2] # offset position 2
    axis(1, col = utheme$axiscol, at = labelpos1, labels = labels1, las = 2, cex.axis = utheme$labelsize) # first set of labels
    axis(3, at = labelpos2, labels = labels2, las = 2, cex.axis = utheme$labelsize, col = utheme$axiscol) # second set of labels
    axis(3, line = -2, col = utheme$axiscol, las = 2, cex.axis = utheme$labelsize)
    abline(h = 0, col = utheme$axiscol, lty = "dashed")
    axis(2, col = utheme$axiscol, cex.axis = utheme$labelsize) # and of course, a yield axia
    #!! must stick legend in here
    title(paste(latest$name, ifelse(log, "- logscale", "")), line = 5, col.main = utheme$titlecol,              
        font.main = utheme$titlefont, cex.main = utheme$titlesize)     # plot the title
    legend("bottomright", c(paste(years, "year history"), paste(recentdays, "day history"),
           "NS fair curve", "Actual curve", paste("NS fair", daysback, "days ago")), 
           col = c(utheme$hairlegend, utheme$hairRecentlegend, utheme$mainyield, utheme$curyield, 
           utheme$daysbackcol), lwd = 2, lty = c(rep("solid", 4), utheme$daysbackstyle), 
           cex = utheme$labelsize, bg = utheme$legendback, box.col = utheme$axiscol)
    # NOW LET's chuck in the GREENIES latest yield
    if(bplot == TRUE) {
        cData <- get(paste(latest$name, "data", sep = ""))
        bonds <- last(cData$actives)[[1]]
        yy <- last(cData$historicData$YLD_YTM_MID[, bonds])
        mm <- (as.Date(cData$staticData[bonds, "MATURITY"]) - Sys.Date()) / 365
        points(mm, yy, col = "green")
        lines(mm, yy, col = "green")
    }
    # NOW we plot the change chart
    par(mar = c(3.5, utheme$outermargins[1], 3, utheme$outermargins[2])) # put a big margin at the bottom
    commonisin <- intersect(rownames(latest$y), rownames(dbackline)) # bonds in both history and today
    change <- (latest$yhat[commonisin, 2] - dbackline[commonisin, 2]) * 100 # calculate change, turn into bps
    changeMats <- latest$yhat[commonisin, 1]
    bchange <- (latest$y[commonisin, 2] - plotobj[[length(plotobj) - daysback]]$y[commonisin, 2] * 100) * 100 # bond change
    yChangeLims <- c(min(c(change, bchange)), max(c(change, bchange))) # for axis calculation
    yChangeRange <- yChangeLims[2] - yChangeLims[1]
    yChangeAdd <- yChangeRange * 0.2
    if(yChangeLims[1] < 0) yChangeLims[1] <- yChangeLims[1] - yChangeAdd
    if(yChangeLims[2] > 0) yChangeLims[2] <- yChangeLims[2] + yChangeAdd
    plot(changeMats, bchange, xlim = xlims, ylim = yChangeLims, frame = FALSE, xlab = NA, cex = 0, xaxt = "n",
        yaxt = "n", log = ifelse(log, "x", "")) # plot a blank frame
    grid()
    polygon(c(changeMats, rev(changeMats)), c(rep(0, length(change)), rev(change)), 
        col = addAlpha(utheme$axiscol, 0.5), border = addAlpha(utheme$axiscol, 0.70))
    rect(changeMats - 0.2, 0, changeMats + 0.2, bchange, col = addAlpha(utheme$axiscol, 0.8), border = utheme$axiscol)
    axis(1, las = 2, cex.axis = utheme$labelsize, col = utheme$axiscol) # second set of labels
    #p <- (change / abs(change)) + 2 #this decides whether labels go above or below change bars
    #p[is.na(p)] <- 1 # if no change in yield today then plot above the change bars
    axis(2, col = utheme$axiscol, cex.axis = utheme$labelsize)
    title(paste("Change from", daysback, ifelse(daysback == 1, "day ago", "days ago")), 
        col.main = utheme$titlecol, font.main = utheme$titlefont, cex.main = utheme$titlesize * 0.7,
        line = 1)
    # now the time stamp
    mtext(format(Sys.time(), "%Y-%m-%d %Hh%M %Z"), side = 1, line = 1.5, outer = FALSE, 
        adj = 0, cex = utheme$timestampsize, col = utheme$timestampcol, font = utheme$timestampfont)
}

matzed <- function(zmatrix, zrange = nrow(zmatrix), minsample = 65, halflife = NA) {
# gives the zscore of the last line in a matrix, versus the last zrange lines in the matrix, minimum sample = minsample
    if (zrange < minsample) {
        print("length of range is less than the cutoff amount")
        return(-1)
    } else {
        if(is.na(halflife)) decayer <- rep(1, zrange) else decayer <- decay(zrange, halflife, meanone = TRUE)
        consider <- tail(zmatrix, zrange) # matrix to work on 
        means <- apply(consider, 2, function(x) wt.mean(na.omit(x), last(decayer, length(na.omit(x))))) # colum means
        consider <- t(apply(consider, 1, function(x) x - means)) # remove means
        sds <- apply(consider, 2, function(x) wt.sd(na.omit(x), last(decayer, length(na.omit(x))))) # standard deviations
        lastline <- consider[nrow(consider), ] 
        returner <- lastline/ sds
        valid <- apply(consider, 2, function(x) length(na.omit(x)) >= minsample)
        returner[!valid] <- NA
        return(returner)
    }
}


lightboxgrid <- function(cCode, model = keymodel[[cCode]], years = 3, subyears = 0.5, daysback = 5, usePC = FALSE, 
                     usePC2 = TRUE, useDecay = TRUE, decayhl = 260, win = TRUE, outliers = FALSE, pcamats = keymats) {
# create a cheap dear boxplot
    utheme <- stheme # colour theme
    if(win) windows(winwidth, 12)
    vp1 <- viewport(x = 0.5, y = 0.625, width = 1, height = 0.75)
    vp2 <- viewport(x = 0.5, y = 0.125, width = 1, height = 0.25)
    pushViewport(vp1)
    par(new = TRUE, fig = gridFIG())
    par(mar = c(3, utheme$outermargins[1], 6, utheme$outermargins[2])) # put a big margin at the bottom
    cdm <<- cdmatrix(cCode, paste("yerr", model, sep = ""), 
                    useDecay = useDecay, usePC2 = usePC2, decayhl = decayhl, pcamats = pcamats)
    if(usePC == TRUE) {
        cdmat <- - last(cdm$pcmat, years * 260) # make the cheap dear matrix, invert also
    } else {
        cdmat <- - last(cdm$cdmat, years * 260) # make the cheap dear matrix, invert also
    }
    cdmat <- as.matrix(cdmat)
    pc1 <- cdm$pc1
    isins <- colnames(cdmat)
    fillcols <- ifelse(isActive(cCode, isins), utheme$boxback, 
        "white")
    lbls <- makelabels(cCode, isins, TRUE) # make the label
    par(mar = c(6, utheme$outermargins[1], 6, utheme$outermargins[2])) # put a big margin at the bottom
    current <- cdmat[nrow(cdmat), ] * 100 * 100 # and for zscores we'll need the current
    dbak <- cdmat[nrow(cdmat) - daysback, ] * 100 * 100 # this is for plotting the daysback point
    zscores <- matzed(cdmat, halflife = subyears * 260) # and here's the zScore!!
    valid <- apply(cdmat, 2, function(x) length(na.omit(x)) >= 65) # did we have enough data for a valid zscore
    zscores <- zscores * valid # kill invalid zscores
    zlabels <- sapply(zscores, function(x) ifelse(x == 0, "--", format(round(x, digits = 1), nsmall = 1)))
    boxplot(cdmat * 100 * 100, col = fillcols, axes = FALSE, outpch = "", outline = FALSE, boxwex = 0) # dummy boxplothhhhhh
    abline(v = 1:ncol(cdmat), col = utheme$gridcol, lty = utheme$gridlines)
    boxplot(cdmat * 100 * 100, axes = FALSE, add = TRUE, 
        outcex = 0.7, outpch = 19, outcol = addAlpha(utheme$axiscol, 0.4), outline = outliers, at = 1:ncol(cdmat), 
        col = "white", border = "white") # k plot the boxplot
    #background violin plot
    for(x in 1:ncol(cdmat)) {
        vioplotter <- na.omit(tail(cdmat[, x] * 100 * 100, years * 260))
        vioplot(vioplotter, drawRect = FALSE, col = utheme$subboxcol, border = NA, ##border = utheme$subboxborder, 
        at = x, na.rm = TRUE, add = TRUE)
    }
    coords <- par("usr")
    rect(coords[1], coords[3], coords[2], coords[4], col = NA, border = "white", lwd = 2) # kill the vioplot rect 
    boxplot(tail(cdmat * 100 * 100, subyears * 260), border = utheme$boxcol, col = utheme$boxback, axes = FALSE, add = TRUE, 
        outpch = "", outline = outliers, at = 1:ncol(cdmat), lty = "solid", boxwex = 0.3)# replot it so we don't get outliers obscuring the whiskers
    axis(1, at = 1:ncol(cdmat), labels = lbls$labels, col = utheme$axiscol, 
        cex.axis = utheme$labelsize, las = 2) # now all the axes, separately
    axis(2, col = utheme$axiscol, cex.axis = utheme$labelsize)
    points(1:ncol(cdmat), current, pch = utheme$curpch, col = (ifelse(useDecay, utheme$curyielddecay, utheme$curyield)), 
        cex = utheme$curcex) # current dotk
    points(1:ncol(cdmat), dbak, pch = utheme$boxhistpch, col = utheme$daysbackcol, bg = addAlpha(utheme$daysbackcol, 0.4), 
        cex = utheme$curcex)
    abline(h = 0, col = utheme$gridcol, lty = utheme$gridlines)
    title(paste(cCode, "cheap dear range", ifelse(usePC, "(residual after eliminating directional influences)", "")), 
        line = 4.5, col.main = utheme$titlecol, font.main = utheme$titlefont, cex.main = utheme$titlesize) # plot the title
    axis(3, at = 1:ncol(cdmat), labels = zlabels, las = 2, cex.axis = utheme$labelsize, 
        col = utheme$axiscol ) # plot the nice zscores axis
    title(main = paste(years, "y / ", round(subyears * 12), "m halflife weighed zscores ( -- when less than 65 days of data)", sep = ""), 
        cex.main = utheme$labelsize, line = 2.5, font.main = utheme$labelfont)
    legend("bottomright", legend = c(paste("(up to) ", years, "y history", sep = ""),
                                     paste(round(subyears * 12), "month history"), 
                                     "current",
                                     paste(daysback, ifelse(daysback == 1, "day ago", "days ago"))), 
        col = c(utheme$subboxcol, utheme$boxback, utheme$curyield, utheme$daysbackcol),
        pch = c(15, 15, utheme$curpch, utheme$boxhistpch), cex = utheme$labelsize,
        bg = utheme$legendback, box.col = utheme$axiscol)
    # okay now we plot the change chart 
    popViewport()
    pushViewport(vp2)
    par(new = TRUE, fig = gridFIG())
    change <- current - dbak
    par(mar = c(2, utheme$outermargins[1], 2, utheme$outermargins[2])) # put a big margin at the bottom
    b <- barplot(change, axisnames = FALSE, axes = FALSE, space = 0.5, border = "grey", col = fillcols)
    axis(2, col = utheme$axiscol, cex.axis = utheme$labelsize)
    axisticks = pretty(change)
    abline(h = axisticks, col = utheme$axiscol, lty = "dotted")
    title(paste("Change from", daysback, ifelse(daysback == 1, "day ago", "days ago")), 
        col.main = utheme$titlecol, font.main = utheme$titlefont, cex.main = utheme$titlesize * 0.7,
        line = 1)
    mtext(format(Sys.time(), "%Y-%m-%d %Hh%M %Z"), side = 1, line = 0, outer = FALSE, 
        adj = 0, cex = utheme$timestampsize, col = utheme$timestampcol, font = utheme$timestampfont)
    popViewport()
}



lightbox <- function(cCode, model = keymodel[[cCode]], years = 3, subyears = 0.5, daysback = 5, usePC = FALSE, 
                     usePC2 = TRUE, useDecay = TRUE, decayhl = 260, win = TRUE, outliers = FALSE, pcamats = keymats) {
# create a cheap dear boxplot
    utheme <- stheme # colour theme
    if(win) windows(winwidth, 12)
    par(mar = c(4, utheme$outermargins[1], 7, utheme$outermargins[2])) # put a big margin at the bottom
    par(mfrow = c(2, 1))
    layout(matrix(c(1, 2)), heights = c(3, 1))
    cdm <<- cdmatrix(cCode, paste("yerr", model, sep = ""), 
                    useDecay = useDecay, usePC2 = usePC2, decayhl = decayhl, pcamats = pcamats)
    if(usePC == TRUE) {
        cdmat <- - last(cdm$pcmat, years * 260) # make the cheap dear matrix, invert also
    } else {
        cdmat <- - last(cdm$cdmat, years * 260) # make the cheap dear matrix, invert also
    }
    cdmat <- as.matrix(cdmat)
    pc1 <- cdm$pc1
    isins <- colnames(cdmat)
    fillcols <- ifelse(isActive(cCode, isins), utheme$boxback, 
        "white")
    lbls <- makelabels(cCode, isins, TRUE) # make the label
    par(mar = c(6, utheme$outermargins[1], 6, utheme$outermargins[2])) # put a big margin at the bottom
    current <- cdmat[nrow(cdmat), ] * 100 * 100 # and for zscores we'll need the current
    dbak <- cdmat[nrow(cdmat) - daysback, ] * 100 * 100 # this is for plotting the daysback point
    zscores <- matzed(cdmat, halflife = subyears * 260) # and here's the zScore!!
    valid <- apply(cdmat, 2, function(x) length(na.omit(x)) >= 65) # did we have enough data for a valid zscore
    zscores <- zscores * valid # kill invalid zscores
    zlabels <- sapply(zscores, function(x) ifelse(x == 0, "--", format(round(x, digits = 1), nsmall = 1)))
    boxplot(cdmat * 100 * 100, col = fillcols, axes = FALSE, outpch = "", outline = FALSE, boxwex = 0) # dummy boxplothhhhhh
    abline(v = 1:ncol(cdmat), col = utheme$gridcol, lty = utheme$gridlines)
    boxplot(cdmat * 100 * 100, axes = FALSE, add = TRUE, 
        outcex = 0.7, outpch = 19, outcol = addAlpha(utheme$axiscol, 0.4), outline = outliers, at = 1:ncol(cdmat), 
        col = "white", border = "white") # k plot the boxplot
    #background violin plot
    for(x in 1:ncol(cdmat)) {
        vioplotter <- na.omit(tail(cdmat[, x] * 100 * 100, years * 260))
        vioplot(vioplotter, drawRect = FALSE, col = utheme$subboxcol, border = NA, ##border = utheme$subboxborder, 
        at = x, na.rm = TRUE, add = TRUE)
    }
    coords <- par("usr")
    rect(coords[1], coords[3], coords[2], coords[4], col = NA, border = "white", lwd = 2) # kill the vioplot rect 
    boxplot(tail(cdmat * 100 * 100, subyears * 260), border = utheme$boxcol, col = utheme$boxback, axes = FALSE, add = TRUE, 
        outpch = "", outline = outliers, at = 1:ncol(cdmat), lty = "solid", boxwex = 0.3)# replot it so we don't get outliers obscuring the whiskers
    axis(1, at = 1:ncol(cdmat), labels = lbls$labels, col = utheme$axiscol, 
        cex.axis = utheme$labelsize, las = 2) # now all the axes, separately
    axis(2, col = utheme$axiscol, cex.axis = utheme$labelsize)
    points(1:ncol(cdmat), current, pch = utheme$curpch, col = (ifelse(useDecay, utheme$curyielddecay, utheme$curyield)), 
        cex = utheme$curcex) # current dotk
    points(1:ncol(cdmat), dbak, pch = utheme$boxhistpch, col = utheme$daysbackcol, bg = addAlpha(utheme$daysbackcol, 0.4), 
        cex = utheme$curcex)
    abline(h = 0, col = utheme$gridcol, lty = utheme$gridlines)
    title(paste(cCode, "cheap dear range", ifelse(usePC, "(residual after eliminating directional influences)", "")), 
        line = 4.5, col.main = utheme$titlecol, font.main = utheme$titlefont, cex.main = utheme$titlesize) # plot the title
    axis(3, at = 1:ncol(cdmat), labels = zlabels, las = 2, cex.axis = utheme$labelsize, 
        col = utheme$axiscol ) # plot the nice zscores axis
    title(main = paste(years, "y / ", round(subyears * 12), "m halflife weighed zscores ( -- when less than 65 days of data)", sep = ""), 
        cex.main = utheme$labelsize, line = 2.5, font.main = utheme$labelfont)
    legend("bottomright", legend = c(paste("(up to) ", years, "y history", sep = ""),
                                     paste(round(subyears * 12), "month history"), 
                                     "current",
                                     paste(daysback, ifelse(daysback == 1, "day ago", "days ago"))), 
        col = c(utheme$subboxcol, utheme$boxback, utheme$curyield, utheme$daysbackcol),
        pch = c(15, 15, utheme$curpch, utheme$boxhistpch), cex = utheme$labelsize,
        bg = utheme$legendback, box.col = utheme$axiscol)
    # okay now we plot the change chart 
    change <- current - dbak
    par(mar = c(2, utheme$outermargins[1], 2, utheme$outermargins[2])) # put a big margin at the bottom
    b <- barplot(change, axisnames = FALSE, axes = FALSE, space = 0.5, border = "grey", col = fillcols)
    axis(2, col = utheme$axiscol, cex.axis = utheme$labelsize)
    axisticks = pretty(change)
    abline(h = axisticks, col = utheme$axiscol, lty = "dotted")
    title(paste("Change from", daysback, ifelse(daysback == 1, "day ago", "days ago")), 
        col.main = utheme$titlecol, font.main = utheme$titlefont, cex.main = utheme$titlesize * 0.7,
        line = 1)
    mtext(format(Sys.time(), "%Y-%m-%d %Hh%M %Z"), side = 1, line = 0, outer = FALSE, 
        adj = 0, cex = utheme$timestampsize, col = utheme$timestampcol, font = utheme$timestampfont)
}
                                

plotbonds <- function(cCode, model = keymodel[[cCode]], years = 3, pdfit = FALSE, auctions = FALSE, win = TRUE) {
    modelnames <- list(yerrsvBoth = "Svensson on and off the run",
                       yerrnsBoth = "Nelson Siegel on and off the run", 
                       yerrdlBoth = "Diebold Li on and off the run",
                       yerrcsBoth = "Cubic spline on and off the run",
                       yerrsv = "Svensson on the run",
                       yerrns = "Nelson Siegel on the run",
                       yerrdl = "Diebold Li on the run") # list of all the names of the models
    wwidth <- 9
    wheight <- 8
    utheme <- stheme # colour theme
    utheme$labelsize <- utheme$labelsize 
    cdm <- cdmatrix(cCode, model = paste("yerr", model, sep = ""))
    pcmat <- last(cdm$pcmat, years * 260)
    cdmat <- last(cdm$cdmat, years * 260)
    cdmat <- cdmat[, apply(cdmat, 2, function(zz) length(na.omit(zz)) > 2)] # kill cols where < 2 data poitns
    pcmat <- pcmat[, apply(pcmat, 2, function(zz) length(na.omit(zz)) > 2)] # kill cols where < 2 data points
    labs <- makelabels(cCode, colnames(cdmat), fromisin = TRUE)
    factors <- get(paste(cCode, "factors", sep = ""))
    if(pdfit) pdf(paste(cCode, "bonds.pdf", sep = ""), wwidth, wheight)
    if(auctions) { # if we need to have auction tap schedule)
        bonds <- paste(isin2bb(cCode, colnames(cdmat)), "Corp") # get the bb tickers
        taps <- lapply(bonds, function(x) {
            aucs <- bds(conn, x, "HISTORY_OF_REOP_TAPS_OF_THE_BD")[, 2]
            if(is.null(aucs)) NULL else as.Date(aucs)
        })
        names(taps) <- colnames(cdmat)
    }
    lapply(1:ncol(pcmat), function(x) {
        flushprint(colnames(pcmat)[x])
        if(pdfit) {
            plot.new() 
        } else if (win) {
            windows(wwidth, wheight)
        } 
        par(mar = c(2, 2, 1, 1))
        par(oma = c(2, 1, 3, 1))
        par(mfcol = c(2, 3))
        layout(t(matrix(c(1, 2, 3, 4, 5, 6), 2, 3)), widths = c(3, 1), heights = c(1, 1, 1))
        cdplot <- - na.omit(cdmat[, x] * 10000)
        pcplot <- - na.omit(pcmat[, x] * 10000)
        cdlims <- c(min(cdplot), max(cdplot))
        pclims <- c(min(pcplot), max(pcplot))
        pret <- pretty(c(cdplot, pcplot))
        dcd <- density(cdplot)
        dpc <- density(pcplot)
        pointsizes <- rep(0, length(cdplot))
        pointsizes[length(pointsizes)] <- 1
        plot(cdplot, type = "l", main = "", ylim = cdlims, major.format = "%b %Y", minor.ticks = FALSE, 
            cex.axis = utheme$labelsize)
        if (auctions) {
            points(cdplot[taps[[x]], ], col = "red", pch = 2)
            if(length(intersect(index(cdplot), taps[[x]])) > 0) {
                legend("topleft", "auction / tap", pch = 2, col = "red", bty = "n")
            }
        }
        points(cdplot, pch = 19, col = "red", cex = pointsizes)
        abline(h = 0, lty = "dashed")
        title(modelnames[[model]], line = -1, font.main = utheme$titlefont,
            col.main = addAlpha("black", 0.5))
        plot(dcd$y, dcd$x, type = "l", ylim = cdlims, axes = FALSE)
        axis(2, cex.axis = utheme$labelsize)
        abline(h = last(cdplot), col = "red", lty = "dashed")
        plot(pcplot, type = "l", main = "", ylim = pclims, major.format = "%b %Y", minor.ticks = FALSE, 
            cex.axis = utheme$labelsize)
        points(pcplot, pch = 19, col = "red", cex = pointsizes)
        abline(h = 0, lty = "dashed")
        title(paste(modelnames[[model]], "risk adjusted"), line = -1, font.main = utheme$titlefont, 
            col.main = addAlpha("black", 0.5))
        plot(dpc$y, dpc$x, type = "l", ylim = pclims, axes = FALSE)
        axis(2, cex.axis = utheme$labelsize)
        abline(h = last(pcplot), col = "red", lty = "dashed")
        isin <- colnames(pcmat)[x]
        yields <- subset(factors, bond == isin, y)
        yields <- xts(yields, order.by = as.Date(rownames(yields)))
        yields <- last(yields, years * 260)
        plot(yields * 100, cex.axis = utheme$labelsize, main = "", major.format = "%b %Y", minor.ticks = FALSE, log = "x")
        title("yield", line = -1, font.main = utheme$titlefont, 
            col.main = addAlpha("black", 0.5))
        title(paste(cCode, labs$labels[x]), cex.main = 1.5, font.main = utheme$titlefont, outer = TRUE)
    })
    if(pdfit) dev.off()
}
    

tdog <- function(update = FALSE, cCodes = ac, model = "auto", smooth = TRUE, knitit = TRUE, 
    pdfit = FALSE, years = 3) {
    if (update) id(cCodes)
    if(knitit) {
        knit("crvm.rhtml")
        shell("crvm.html", wait = FALSE)
    } else {
        if(pdfit) pdf("tdog1.pdf", width = 9, height = 11)
        lapply(cCodes, function(x) {
            if(model == "auto") usemodel <- keymodel[[cCode]] else usemodel <- model
            lightplot(x, model = usemodel,
                      daysback = 1, smooth = smooth, win = ifelse(pdfit, FALSE, TRUE), years = years)
            lightbox(x, model = usemodel, daysback = 5, usePC = FALSE, win = ifelse(pdfit, FALSE, TRUE), years = years)
            lightbox(x, model = usemodel, daysback = 5, usePC = TRUE, win = ifelse(pdfit, FALSE, TRUE), years = years)
        })
        if(pdfit) dev.off()
    }
}

rdog <- function() {
# outputs the real money product
    knit("crvmreal.rhtml")
    shell("crvmreal.html", wait = FALSE)
}

serve <- function() {
# serves CRVM continuously to directories
    flushprint("Press escape at any time to stop")
    flushprint("Please ensure that you have run a kick every morning before doing this...")
    flushprint("... the main kick must be run on Philip's machine, and thereafter it")
    flushprint("should be run on server Calculon with quick = TRUE parameter")
    
    if(sysname == "Windows") {
        setwd("//LILJEN/share/crvm/")
    } else if((sysname == "Linux") && (nodename == "Calculon")) {
        setwd(realhtmlplace)
    }
    while(TRUE) {
        flushprint("..........................")
        flushprint(Sys.time())
        flushprint("..........................")
        id(fromongo = TRUE)
        for(cCode in tolower(c(cl, "cc"))) {
            savedir <- getwd()
            setwd(cCode)
            knit(paste(cCode, ".rhtml", sep = ""))
            setwd(savedir)
        }
        doFTP()
        #knit("crvmreal.rhtml")
        #knit("crvm.rhtml")
        #knit("flymax.rhtml")
    }
}

doFTP <- function(basedir = "/media/lilshare/crvm/html/", 
                  destdir = "ftp://54.78.142.128/files/", 
                  credentials = "ftpuser:itcmarkets2014",
                  trigger = "http://54.78.142.128/crvm/api/country/all/update") {
    oldwd <- getwd()
    setwd(basedir)
    zip("country.zip", "./country")
    ftpresult <- ftpUpload("country.zip", 
                           paste(destdir, "country.zip", sep = ""),
                           userpwd = credentials)
    POST(trigger)
    setwd(oldwd)
}


lightcd <- function(lightobj, years = 3, daysback = 5) {}


activecd <- function(cCode) {
    cdata <- get(paste(cCode, "data", sep = "")) # get the data set
    cdlist <- lapply(cdata$ns, function(x) x$yerrors[[cCode]])
    currentcd <- rownames(last(cdlist)[[1]])
    curcdlist <- lapply(1:length(cdlist), function(x) cdlist[[x]][rownames(cdlist[[x]]) %in% currentcd, ])
}  


kick <- function(countries = ac, save = TRUE, quick = FALSE) {
# start off every day function. loads, updates, saves each country
    if(!quick & !exists("conn")) {
        gobb()
    } else {
        gomongo("192.168.1.32")
    }
    if(sysname == "Windows") {
        getCalendars(c(countries, "TE")) # also get target settlement calendar "TE"
    } else {
        load(paste(dataplace, "cal.dat", sep = ""), envir = globalenv())
    }        
    lapply(countries, function(x) {
        flushprint(paste("Doing", x))
        loadCountry(x)
        if(!quick) {
            upCountry(x)
            checkIntegrity(x)
            if(save) saveCountry(x)
        }
    })
    getAllBondFactors(cl)
    load(paste(dataplace, "trades.dat", sep = ""), envir = globalenv())
    if(quick) {
        load(paste(dataplace, "eurirs.dat", sep = ""), envir = globalenv())
    } else {
        eurirs <<- bbdh(eurIRStickers, 10)
        save(eurirs, file = paste(dataplace, "eurirs.dat", sep = ""))
    }   
}

savetrades <- function() {
    save(trades, file = paste(dataplace, "trades.dat", sep = ""))
}


changer <- function(cCodes = ac, daysBack = 1, decayhl = 130, sample = 3 * 260, model = "auto", win = TRUE, 
                    usecc = TRUE, returnplots = FALSE) {
# plots the change charts outright and PCA adjusted; usecc = use cross country rather than individual pcas
    winheight <- 3.5
    cy <- lapply(cCodes, function(cCode) {          # get the countries
        cData <- get(paste(cCode, "data", sep = ""))
        cc <- couponYields(cData[[ifelse(model == "auto", keymodel[[cCode]], model)]])
        cc <- cc[, as.numeric(names(buckets))]
        cc <- last(cc, sample)
        cc <- diffret(cc)
        return(cc)
    })
    names(cy) <- cCodes
    pcas <- getPCAs(cCodes, days = sample, decayhl = decayhl, silent = TRUE, model = model, scale.unit = FALSE)
    decayer <- decay(nrow(pcas[[1]]), decayhl)
    cypca <- lapply(cCodes, function(cCode) {   
        if(usecc) {
            res <- apply(cy[[cCode]], 2, function(x) lm(x ~ pcas$crosscountry[, 1:4], weights = decayer)$residuals)
        } else {
            res <- apply(cy[[cCode]], 2, function(x) lm(x ~ pcas[[1]]$ALL + pcas[[2]]$ALL +pcas[[3]]$ALL, weights = decayer)$residuals)
        }
        res <- xts(res, order.by = index(cy[[cCode]]))
        return(res)
    })
    names(cypca) <- cCodes
    rets <- lapply(cCodes, function(cCode) {
        nplot <- apply(last(cy[[cCode]], daysBack), 2, sum) * 10000
        pplot <- apply(last(cypca[[cCode]], daysBack), 2, sum) * 10000
        return(cbind(nplot, pplot))
    })
    names(rets) <- cCodes
    xmelt <- melt(rets)
    xmelt$L1 <- factor(xmelt$L1, levels = unique(xmelt$L1), ordered = TRUE)
    levels(xmelt$Var2) <- c("Outright", "Alpha residual")
    if (win) windows(winwidth, winheight)
    g <- ggplot(xmelt, aes(x=factor(Var1),value, row=L1, col=Var2, group=Var2, fill=factor(Var1))) + 
        geom_bar(stat="identity") + facet_grid(Var2 ~ L1, scale="free") +  
        xlab("Maturities") + ylab("") + theme(legend.position = "none", axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 7), plot.title= element_text(size=9), axis.title = element_text(size = 9)) + 
        scale_fill_grey(start=0.8,end=0.3) + ggtitle("today")
    # now do the weekly charts
    endpts <- last(endpoints(cy[[1]], "weeks"), 6)
    endpts[2] <- nrow(cy[[1]]) - 21 # past month
    enddates <- sapply(1:length(endpts), function(x) {
        mnth <- substr(months(index(cy[[1]])[endpts][x], FALSE), 1, 3)
        dy <- day(index(cy[[1]])[endpts][x])
        paste(dy, mnth)
    })
    weeklabels <- c(paste("rolling month (", enddates[2], "-today)", sep = ""), 
                    paste("previous 2 weeks (", enddates[3], "-", enddates[5], ")", sep = ""),
                    paste("previous week (", enddates[4], "-", enddates[5], ")", sep = ""),
                    paste("week to today (", enddates[5], " close - now)", sep = ""))
    wk <- lapply(cCodes, function(cCode) {
        nwk <- rbind(apply(cy[[cCode]][endpts[2]:endpts[6], ], 2, sum),
                     apply(cy[[cCode]][endpts[3]:endpts[5], ], 2, sum),
                     apply(cy[[cCode]][endpts[4]:endpts[5], ], 2, sum),
                     apply(cy[[cCode]][endpts[5]:endpts[6], ], 2, sum))
        nwk <- cbind(rep("outright", nrow(nwk)), weeklabels, as.data.frame(nwk))
        colnames(nwk)[1:2] <- c("type", "weeknum")
        pwk <- rbind(apply(cypca[[cCode]][endpts[2]:endpts[6], ], 2, sum),
                     apply(cypca[[cCode]][endpts[3]:endpts[5], ], 2, sum),
                     apply(cypca[[cCode]][endpts[4]:endpts[5], ], 2, sum),
                     apply(cypca[[cCode]][endpts[5]:endpts[6], ], 2, sum))
        pwk <- cbind(rep("residual", nrow(pwk)), weeklabels, as.data.frame(pwk))
        colnames(pwk)[1:2] <- c("type", "weeknum")
        return(rbind(nwk, pwk))
    })
    names(wk) <- cCodes
    wk <- melt(wk, id = c("weeknum", "type"))
    wk$L1 <- factor(wk$L1, levels=unique(wk$L1), ordered = TRUE) # factor it up and ensure label correctness
    #------------------------------------------------------------- ggplot
    ggweek <- function(whichweek, scalefree = TRUE) {
    #function to plot a week change
        if(win) windows(winwidth, winheight)
        a <- ggplot(wk, aes(variable, value*10000, group=weeknum, fill=factor(variable))) + 
        facet_grid(type~L1, scale="free") + 
        ###############outright##################
        geom_bar(data=wk[wk$type=="outright"&wk$weeknum==whichweek,], stat="identity", color="#F8766D") + 
        geom_bar(data=wk[wk$type=="outright"&wk$weeknum==weeklabels[4],], stat="identity", alpha=0, linetype=0) + 
        geom_bar(data=wk[wk$type=="outright"&wk$weeknum==weeklabels[3],], stat="identity", alpha=0, linetype=0) +
        geom_bar(data=wk[wk$type=="outright"&wk$weeknum==weeklabels[2],], stat="identity", alpha=0, linetype=0) +
        geom_bar(data=wk[wk$type=="outright"&wk$weeknum==weeklabels[1],], stat="identity", alpha=0, linetype=0) +
        ######################residual###########
        geom_bar(data=wk[wk$type=="residual"&wk$weeknum == whichweek,], stat="identity", color = "#00BFC4") +
        geom_bar(data=wk[wk$type=="residual"&wk$weeknum==weeklabels[4],], stat="identity", alpha=0, linetype=0) +
        geom_bar(data=wk[wk$type=="residual"&wk$weeknum==weeklabels[3],], stat="identity", alpha=0, linetype=0) +
        geom_bar(data=wk[wk$type=="residual"&wk$weeknum==weeklabels[2],], stat="identity", alpha=0, linetype=0) +
        geom_bar(data=wk[wk$type=="residual"&wk$weeknum==weeklabels[1],], stat="identity", alpha=0, linetype=0) +
        ##############aesthetic##################
        scale_fill_grey(start=0.8,end=0.3) +
        theme(legend.position = "none", plot.title= element_text(size=9), axis.text.x = element_text(size=7), 
            axis.text.y = element_text(size=7)) +
        xlab("") + ylab("") + ggtitle(whichweek)
        return(a)
    }
    # okay now plot each week
    if(weekdays(Sys.Date()) != "Monday") {
		b  <- ggweek(rev(weeklabels)[[1]])
	}
    c <- ggweek(rev(weeklabels)[[2]])
    d <- ggweek(rev(weeklabels)[[3]])
    e <- ggweek(rev(weeklabels)[[4]])
	if(!returnplots) {
		plot(g)
		if(weekdays(Sys.Date()) != "Monday") plot(b)
		plot(c)
		plot(d)
		plot(e)
	} else {
		if(weekdays(Sys.Date()) != "Monday") {
			return(list("g" = g, "b" = b, "c" = c, "d" = d, "e" = e))
		} else {
			return(list("g" = g, "c" = c, "d" = d, "e" = e))
		}
	}
}


cfmat <- function(cCodes, days = 260 * 3, hl = 130, method = "ns", daysback = 1, zeros = FALSE) {
# compareFactors
# will compare level, slope, and curvature and plot the curves in a nice matrix
    btheme <- list (
        diag = "cornsilk2",
        #diag = "grey90",
        barup = "darksalmon",
        bardown = "darkseagreen",
        #background = "white"
        background = "cornsilk1"
    )
    tenors = c(2, 5, 10, 20, 30)
    utheme <- stheme # set the colours theme
    cDatas <- lapply(cCodes, function(x) get(paste(x, "data", sep  = "")))
    # get the country zero curves
    zeps <- lapply(cDatas, function(x) {
        scc <- last(x$ns, days)
        if(method == "ns") {
            scc <- last(x$ns, days)
            t(sapply(scc, function(y) spr_ns(y$startparam, seq(2, 30))))
        } else {
            scc <- last(x$sv, days)
            t(sapply(scc, function(y) spr_sv(y$startparam, seq(2, 30))))
        }
    })
    #get the country yields
    yields <- lapply(cDatas, function(x) {
        if(method == "ns") scc <- last(x$ns, days) else scc <- last(x$sv, days)
        lapply(scc, function(y) y$y[y$y[, 1] < 33, ])
    })
    # smooth the yields and get the yields spreads
    smoothyields <- lapply(yields, function(x) {
        lapply(yields, function(y) {
            lapply(1:days, function(z) {
                m1 <- x[[z]][, 1]
                m2 <- y[[z]][, 1]
                y1 <- x[[z]][, 2]
                y2 <- y[[z]][, 2]
                min1 <- max(min(m1), min(m2))
                max1 <- min(max(m1), max(m2))
                mm <- c(m1, m2)
                mm <- mm[mm >= min1]
                mm <- mm[mm <= max1]
                mm <- mm[order(mm)]
                mm <- unique(mm)
                int1 <- approx(m1, y1, mm)
                int2 <- approx(m2, y2, mm)
                if (identical(x, y)) cbind(mm, int1$y * 100) else cbind(mm, (int1$y - int2$y) * 100)
            })
        })
    })
    ylims <- function() { 
    # gets the diagonal and lower triangle y axis limits for yields curves
        sylim <- lapply(smoothyields, function(x) {
            lapply(x, function(y) {
                d1 <- y[[days]] 
                d2 <- y[[days - daysback]]
                # now extend maturities to 30 if we have a 25 to 30y point but no 30
                if((d1[nrow(d1), 1] > 25) & (d1[nrow(d1), 1] < 30)) d1[nrow(d1), 1] <- 30
                if((d2[nrow(d2), 1] > 25) & (d2[nrow(d2), 1] < 30)) d2[nrow(d2), 1] <- 30
                if((d1[1, 1] > 2) & (d1[1, 1] < 3)) d1[1, 1] <- 2
                if((d2[1, 1] > 2) & (d2[1, 1] < 3)) d2[1, 1] <- 2
                d1 <- approx(d1[, 1], d1[, 2], tenors)$y
                d2 <- approx(d2[, 1], d2[, 2], tenors)$y
                (d1 - d2) * 100
            })
        })
        diags <- sapply(1:length(cCodes), function(m) {
            sapply(1:length(cCodes), function(n) {
                if (m == n) sylim[[m]][[n]]
            })
        })
        lowertriag <- sapply(1:length(cCodes), function(m) {
            sapply(1:length(cCodes), function(n) {
                if (m > n) sylim[[m]][[n]]
            })
        })
        return(list(changes = sylim, countryLims = c(min(na.omit(unlist(diags))), max(na.omit(unlist(diags)))),
                    spreadLims = c(min(na.omit(unlist(lowertriag))), max(na.omit(unlist(lowertriag))))))
    }
    # get the zero curve spreads
    smoothzeros <- lapply(zeps, function(x) {
        lapply(zeps, function(y) {
            if(identical(x, y)) x else x - y
        })
    })

    zlims <- function() {
    # gets the diagonal and lower triangle y axis limits for zero curves
        zylim <- lapply(smoothzeros, function(x) {
            lapply(x, function(y) {
                return((y[days, tenors - 1] - y[days - daysback, tenors - 1]) * 100)
            })
        })
        diags <- sapply(1:length(cCodes), function(m) {
            sapply(1:length(cCodes), function(n) {
                if(m == n) zylim[[m]][[n]]
            })
        })
        lowertriag <- sapply(1:length(cCodes), function(m) {
            sapply(1:length(cCodes), function(n) {
                if (m > n) zylim[[m]][[n]]
            })
        })
        return(list(changes = zylim, countryLims = c(min(unlist(diags)), max(unlist(diags))),
                    spreadLims = c(min(unlist(lowertriag)), max(unlist(lowertriag)))))
    }
    
    pcs <- lapply(zeps, function(x) {
    # do the PCAs
        PCA(x, graph = FALSE, row.w = decay(nrow(x), hl))
    })

    windows(10, 11)
    par(mfrow = c(length(cCodes), length(cCodes) + 1), oma = c(2, 2, 4, 2), mar = c(2, 1.5, 2, 1.5))
    haircol <- c(rep(utheme$hair, days - 65), rep(utheme$hairRecent, 65))

    plotZeros <- function(toPlot, yl, bg = NULL) {
        plot(toPlot[1, ], col = "white", ylim = c(min(toPlot), max(toPlot)), axes = FALSE)
        if(!is.null(bg)) {
            rc <- par("usr")
            rect(rc[1], rc[3], rc[2], rc[4], col = bg, border = "black")
        }
        lapply(1:days, function(x) lines(toPlot[x, ], col = haircol[x]))
        lines(toPlot[days - daysback, ], col = utheme$daysbackcol, lwd = 2)
        lines(toPlot[days, ], col = utheme$mainyield, lwd = 2)
        abline(h = 0, col = utheme$axiscol)
        axis(2)
    }

    plotYields <- function(toPlot, yl, bg = NULL) {
        ally <- sapply(toPlot, function(x) c(min(x[, 2]), max(x[, 2])))
        allm <- sapply(toPlot, function(x) c(min(x[, 1]), max(x[, 1])))
        plot(toPlot[[1]], col = "white", ylim = c(min(ally), max(ally)), 
            xlim = c(min(allm), max(allm)), axes = FALSE)
        if(!is.null(bg)) {
            rc <- par("usr")
            rect(rc[1], rc[3], rc[2], rc[4], col = bg, border = "black")
        }
        lapply(1:days, function(x) xspline(toPlot[[x]][, 1], toPlot[[x]][, 2], 
            border = haircol[x], shape = 1))
        xspline(toPlot[[days - daysback]][, 1], toPlot[[days - daysback]][, 2], 
            border = utheme$daysbackcol, lwd = 2, shape = 1)
        xspline(toPlot[[days]][, 1], toPlot[[days]][, 2], 
            border = utheme$mainyields, lwd = 2, shape = 1)
        abline(h = 0, col = utheme$axiscol)
        axis(2) #, col = utheme$axiscol)
    }
    plotBars <- function(toPlot, yl, bg = NULL, xaxis = FALSE) {
        if(!is.null(bg)) {
            barplot(toPlot, ylim = yl, border = TRUE)#, names.arg = as.character(tenors[!is.na(toPlot)]))
            rc <- par("usr")
            rect(rc[1], rc[3], rc[2], rc[4], col = bg, border = "black")
        }
        #, names.arg = as.character(tenors[!is.na(toPlot)]))
        cols = ifelse(toPlot > 0, btheme$barup, btheme$bardown)
        bb <- barplot(toPlot, ylim = yl, border = TRUE, add = ifelse(is.null(bg), FALSE, TRUE), 
            col = cols)
        if(xaxis) {
            axis(1, at = bb, labels = as.character(tenors))
        }
    }

    if(zeros) yll <- zlims() else yll <- ylims() # find the y limits for the whole plot

    for(x in 1:length(cCodes)) {
        for(y in 1:(length(cCodes) + 1)) {
            if(y == x) {
                par(bg = btheme$background)
                if(zeros) plotZeros(smoothzeros[[x]][[y]], yll[[1]], bg = btheme$diag) else 
                    plotYields(smoothyields[[x]][[y]], yll[[1]], bg = btheme$diag)
                title(cCodes[y], line = 1)
            } else if (y == (x + 1)) {
                plotBars(yll[[1]][[x]][[x]], yll[[2]], bg = btheme$diag, xaxis = ((x == length(cCodes)) & (y == (x + 1))))
                title(paste(cCodes[x], "chg"), line = 1)
            } else if (y > (x + 1)) {
                plotBars( - yll[[1]][[x]][[y-1]], yll[[3]])
                title(paste(cCodes[y-1], "-", cCodes[x], " chg", sep = ""), font.main = 3, line = 1)
            } else {
                if(zeros) plotZeros(smoothzeros[[x]][[y]], yll[[1]]) else
                    plotYields(smoothyields[[x]][[y]], yll[[2]])
                title(paste(cCodes[x], "-", cCodes[y], sep = ""), line = 1, font.main = 3)
            }
            if((x == length(cCodes)) & (y <= x)) axis(1)
        }
    }
    #legend("bottomright", legend = c(paste("past", days/260, "years"), "past 3 months",
            #paste(daysback, ifelse(daysback == 1, "day ago", "days ago")),
            #"now"), col = c(utheme$hairlegend, utheme$hairRecentlegend, utheme$daysbackcol, 
            #utheme$mainyield), lwd = 2, bg = utheme$legendback, box.col = utheme$axiscol)
    # NOW LET's chuck in the GREENIES latest yield
    title(paste(ifelse(zeros, "Zero", "Yield"), "curve and spread matrix"), outer = TRUE, 
        font.main = utheme$titlefont, cex.main = utheme$titlesize * 1.5, col = utheme$titlecol)
}


pyields <- function(ccode, nsobj, datestring) {
# this is going to compare yields on the date from the ns obj, to the ytms from bberg
    cdata <- get(paste(ccode, "data", sep = ""))
    bonds <- cdata$actives[[datestring]]
    # get the bloomberg yields and maturities
    yy <- cdata$historicdata$yld_ytm_mid[datestring, bonds]/s
    mm <- (as.date(cdata$staticdata[bonds, "maturity"]) - sys.date()) /  365.25
    # now calclulate my yields
    nowdyn <- cdata$dynon[[datestring]][[1]]
    cfm <- create_cashflows_matrix(nowdyn, include_price = TRUE)
    matm <- create_maturities_matrix(nowdyn, include_price = TRUE)
    myy <- ann_yields(cfm, matm)
    myy[, 2] <- ((1 + myy[, 2]) ^2 - 1) / 2
    myy[, 2] <- myy[, 2] * 100
    plot(mm, yy, pch = 19)
    points(myy, col = "red")
}


errmat <- function(ns, nsoff = na, daysback = 260 * 3) {
# going to get an xts of all the bond errors
    bonds <- c(rownames(last(ns)[[1]]$yerrors), (if(is.na(nsoff)) null else rownames(last(nsoff)[[1]]$yerrors)))
    len <- length(ns) # number of days in this thing
    # okay get the cheap dear list interspersed with on/off run test vectors
    cdlist <- lapply((len - daysback + 1):len, function(x) {
        yerrs <- rbind(ns[[x]]$yerrors, (if(is.na(nsoff)) null else nsoff[[x]]$yerrors)) # current yerrors
        yesvec <- bonds %in% rownames(yerrs) # which bonds are in there vector
        berrs <- yesvec # create a matrix of TRUE FALSE
        berrs[yesvec] <- yerrs[bonds[yesvec], 2] # get the bond data into berrs for this day
        berrs[!berrs] <- na # and the rest are na
        # now matrix of TRUEs for each day where current onrun was onrun at this time:w
        ison <- !is.na(berrs) # and this is the TRUE FALSE matrix that we will return
        ison[!is.na(berrs)] <- bonds[!is.na(berrs)] %in% rownames(ns[[x]]$yerrors) 
        return(list(berrs = berrs, ison = ison))
    })
    cd <- t(sapply(cdlist, "[[", 1))
    ison <- t(sapply(cdlist, "[[", 2))
    colnames(ison) <- bonds
    colnames(cd) <- bonds
    ison <- ison[, !is.na(last(cd))] # remove zero coupon or too short columns
    cd <- cd[, !is.na(last(cd))]
    #ison <- xts(ison, order.by = as.date(last(names(cdata$actives), daysback)))
    #cd <- xts(cd, order.by = as.date(last(names(cdata$actives), daysback)))
    return(list(cd = cd, ison = ison))
}


spreadget <- function(cCodes = ac, spreads = list(c(2, 5), c(5, 10), c(7, 10), c(5, 20), c(2, 5, 10), c(2, 10), c(10, 15), 
                      c(10, 20), c(10, 30), c(5, 10, 15), c(5, 10, 30), c(10, 15, 30)),
                      model = "auto", years = 3, bps = TRUE,
                      daysback = 0) {
# for each spread, return an xts matrix of it for each country 
    if(class(spreads) != "list") spreads <- list(spreads)
    couponylds <- allYields(cCodes = cCodes, days = years * 260, daysback = daysback, model = model)
    spreadvecs <- lapply(spreads, function(spread) {             # for each spread
        intervec <- lapply(couponylds, function(y) {                         # and for each country
            if(length(spread) == 2) {
                return(y[, spread[2]] - y[, spread[1]])
            } else {
                return(- y[, spread[1]] + 2 * y[, spread[2]] - y[, spread[3]])
            }
        })
        intervec <- do.call(cbind, intervec)
        names(intervec) <- cCodes
        if(bps) intervec <- intervec * 10000 # turn to bps
        intervec
    })
    names(spreadvecs) <- lapply(spreads, function(spread) {
        lab <- ""
        for(x in 1:(length(spread) - 1)) lab <- paste(lab, as.character(spread[x]), "-", sep = "")
        lab <- paste(lab, spread[length(spread)], sep = "")
        return(lab)
    })
    return(spreadvecs)
}


catlabels <- function(listtocat, sepchar = "-") {
    if(length(listtocat) > 2) {
        return(paste(listtocat[1], catlabels(listtocat[-1], sepchar = sepchar), sep = sepchar))
    } else {
        return(paste(listtocat[1], listtocat[2], sep = sepchar))
    }

}


flymax <- function(cCode = "DE", incdmat = NULL, inpcs = NULL, incy = NULL, wingmax = 5, wingratio = 3, virs = FALSE, 
                   pcmat = FALSE, useDecay = FALSE, model = "auto",
                   decayhl = 260, usePC2 = TRUE, pcamats = keymats, mindays = 130, bodyrange = NA, 
                   cost = switch(cCode, "DE" = 0.5, "FR" = 0.7, "IT" = 2, "SP" = 1.5, "NE" = 1, "BE" = 1.5, "AT" = 1.5), 
                   plotit = TRUE, win = TRUE, plotavg = TRUE, plotthresh = 2, costthresh = 3 * cost, scoremin = 3, 
                   specific = NULL, specificany = FALSE,
                   reversepolarity = FALSE, maxyears = 1.25, ccPCA = FALSE, minorPCA = FALSE,
                   exclude = list()) {
# find tbe best flys. wingmax is the maximum duration distance between each wing and body, wingration is max duration gap ratio between
# wings and body, lots of data for the cd matrix (decay etc) and also the sds of barbells, then cost is what the total cost would be
# if specific, then a specific fly will be plotted. Please provide ISINs
# maxyears cuts off too much data if not NA
    # first get all the cheap dear
    if(is.null(incdmat)) # has the cdmat been provided?
        cdmat <- cdmatrix(cCode, useDecay = TRUE, decayhl = decayhl, usePC2 = usePC2, pcamats = keymats, 
                          model = paste("yerr", ifelse(model == "auto", keymodel[[cCode]], model), sep = ""))  
    else cdmat <- incdmat 
    if(is.null(maxyears)) {
        pca <- getPCAs(ac, ifelse(model == "auto", keymodel[[cCode]], model),
                       decayhl = decayhl, usedecay = useDecay, series = TRUE, scale.unit = TRUE)
    } else {
        pca <- getPCAs(ac, model = ifelse(model == "auto", keymodel[[cCode]], model), 
                       decayhl = decayhl, usedecay = useDecay, series = TRUE, 
                       scale.unit = TRUE, days = 260 * maxyears)
    }
    pca <<- pca
    if(is.null(incy))  # has the couponyields matrix been provided
        cy <- couponYields(cCode) # have the couponyields been provided
    else cy <- incy
    dur <- bdur(cCode, last(index(cdmat$cdmat))) # all the durations
    dur <- dur[!(rownames(dur) %in% exclude[[cCode]]), ] # take out exclusion bonds
    comb <- combn(rownames(dur), 3) # combination of all bonds
    combdurs <- apply(comb, 2, function(x) dur[x, "mdur"])
    ordercomb <- sapply(1:ncol(combdurs), function(x) comb[, x][order(combdurs[, x])]) # order them
    wingdiffs <- apply(ordercomb, 2, function(x) diff(dur[x, "mdur"])) 
    if(is.null(specific)) {
        goodflys <- ordercomb[, sapply(1:ncol(wingdiffs), function(x) {             # kill all weird flys that don't match requirements
                          durmax <- sqrt(dur[ordercomb[2, x], "mdur"])
                          return(((wingdiffs[1, x] <= durmax) & (wingdiffs[2, x] <= durmax)) 
                                & (((wingdiffs[1, x] / wingdiffs[2, x]) <= wingratio) 
                                & ((wingdiffs[2, x] / wingdiffs[1, x]) <= wingratio)))
                        })] 
    } else goodflys <- as.matrix(specific)
    if(pcmat) cd <- cdmat$pcmat * 10000 else cd <- cdmat$cdmat * 10000 # we do this but we will always use the pcmat here probably
    uncd <- cdmat$cdmat * 10000
    y <- - cdmat$ymat * 10000
    if(!is.na(maxyears)) { # if we have to limit the amount of data
        uncd <- last(uncd, maxyears * 260)
        y <- last(y, maxyears * 260)
        cd <- last(cd, maxyears * 260)
    }
    flyspreads <- apply(goodflys, 2, function(x) {
        as.numeric((cd[, x[1]] - 2 * cd[, x[2]] + cd[, x[3]]) * ifelse(reversepolarity, -1, 1)) # calc the cheap dears
    })
    unflyspreads <- apply(goodflys, 2, function(x) {
        # calc the spreads of unadjusted cheap deares
        as.numeric((+uncd[, x[1]] - 2 * uncd[, x[2]] + uncd[, x[3]]) * ifelse(reversepolarity, -1, 1))     
    })
    yspreads <- apply(goodflys, 2, function(x) {
        as.numeric((- y[, x[1]] + 2 * y[, x[2]] - y[, x[3]]) * ifelse(reversepolarity, -1, 1)) # calc the spreads of unadjusted yields
    })
    flyspreads <- xts(flyspreads, order.by = index(cd))
    unflyspreads <- xts(unflyspreads, order.by = index(uncd))
    yspreads <- xts(yspreads, order.by = index(y))
    flyzs <- apply(flyspreads, 2, function(x) { # now get the zs
        xna <- as.numeric(na.omit(x))
        if(length(xna) < mindays) c(NA, NA) else {  # make sure enough data    
            if(useDecay) {
                decayer <- decay(length(xna), decayhl)
                lastxna <- xna[length(xna)]
                sdxna <- wt.sd(xna, decayer)
                thisz <- (lastxna - wt.mean(xna, decayer)) / sdxna # get the z
            } else {
                lastxna <- xna[length(xna)]
                sdxna <- sd(xna)
                thisz <- (lastxna - mean(xna)) / sdxna #get the z
            }
            bpz <- sdxna * (abs(thisz) - 1)
            return(c(thisz, bpz))
        }
    })
    bpzs <- flyzs[2, ]
    flyzs <- flyzs[1, ]
    # now kick out the NAs from too little history
    flyspreads <- flyspreads[, !is.na(flyzs)]
    unflyspreads <- unflyspreads[, !is.na(flyzs)]
    yspreads <- yspreads[, !is.na(flyzs)]
    goodflys <- as.matrix(goodflys[, !is.na(flyzs)]) # must put in as.matrix because if specific and only one turns to vector
    flyzs <- na.omit(flyzs)
    bpzs <- na.omit(bpzs)
    # now order them by zs
    flyspreads <- flyspreads[, order(abs(flyzs))]
    unflyspreads <- unflyspreads[, order(abs(flyzs))]
    yspreads <- yspreads[, order(abs(flyzs))]
    goodflys <- as.matrix(goodflys[, order(abs(flyzs))])
    bpzs <- bpzs[order(abs(flyzs))]
    flyzs <- flyzs[order(abs(flyzs))]
    bondlabs <- apply(goodflys, 2, function(x) isinLabel(cCode, x, ""))
    shortlabs <- apply(goodflys, 2, function(x) isinLabel(cCode, x, "", shortlab = TRUE))
    # now check if specific bond was passed. If so, it's the only good fly. Else find the ones that meet criteria
    if(is.null(specific)) {
        qualifiers <- sapply(1:length(flyzs), function(x) (abs(flyzs[x]) > plotthresh) & (abs(bpzs[x]) > costthresh))
    } else {
        if(specificany) 
            qualifiers <- apply(goodflys, 2, function(x) any(x %in% specific))
        else
            qualifiers <- apply(goodflys, 2, function(x) all(specific %in% x))
    }
    # filter out the bad ones by z score and cost
    # create list of good trades
    goodlist <- lapply(1:length(qualifiers), function(x) {
        if(qualifiers[x] == TRUE) {
            plotter <- as.numeric(na.omit(flyspreads[, x]))
            plotter <- xts(plotter, order.by = as.Date(last(index(cd), length(plotter))))
            unplotter <- as.numeric(na.omit(unflyspreads[, x]))
            unplotter <- xts(unplotter, order.by = as.Date(last(index(cd), length(unplotter))))
            yplotter <- as.numeric(na.omit(yspreads[, x]))
            yplotter <- xts(yplotter, order.by = as.Date(last(index(cd), length(yplotter))))
            if(reversepolarity) 
                plotitle <- paste("+", bondlabs[1, x], "- 2*", bondlabs[2, x], "+", bondlabs[3, x])
            else
                plotitle <- paste("-", bondlabs[1, x], "+ 2*", bondlabs[2, x], "-", bondlabs[3, x])
            shortitl <-  paste(shortlabs[1, x], shortlabs[2, x], shortlabs[3, x], sep = "")
            linmod <- lm(last(plotter, mindays) ~ last(pca[[1]][, cCode], mindays) + last(pca[[2]][, cCode], mindays)) # 6 months
            linmod3 <- lm(last(unplotter, mindays) ~ last(pca[[3]][, cCode], mindays))
                                                #+ last(pca[[2]][, cCode], mindays)
                                                #+ last(pca[[3]][, cCode], mindays))
            pclmrsq <- summary(linmod)$r.squared
            pclmcoef <- coef(linmod)
            pclmse <- last(linmod$residuals) / sd(linmod$residuals)
            sefrompc <- last(linmod$residuals) / sd(linmod$residuals)
            if(flyzs[x] > 0) 
                makebp <- last(plotter)[[1]] - (mean(plotter) + sd(plotter))
            else 
                makebp <- last(plotter)[[1]] - (mean(plotter) - sd(plotter))
            flyscore <- sqrt((abs(makebp) / (ifelse(cost == 0, 1, cost))) * abs(flyzs[x]) * as.numeric(abs(pclmse)))
            flyscore3 <- abs(as.numeric(last(linmod3$residuals)))
            return(list(country = cCode, fly = plotitle, flyspread = plotter, unflyspread = unplotter, yspread = yplotter, 
                        titl = plotitle, shortitl = shortitl, pclmrsq = pclmrsq, pclmse = pclmse, pclmcoef = pclmcoef, 
                        z = flyzs[x], bpzs = bpzs[x], makebp = makebp, flyscore = flyscore, flyscore3 = flyscore3, 
                        instruments = goodflys[, x], 
                        durations = combdurs[, x], labls = bondlabs[, x]))
        }
    })
    goodlist <- goodlist[qualifiers]
    goodlist <- goodlist[order(sapply(goodlist, "[[", "flyscore3"))] # order by flyscore
    if(is.null(specific)) goodlist <- goodlist[sapply(goodlist, "[[", "flyscore3") > scoremin]
    if(plotit & length(goodlist) > 0) {
        # first plot the average deviation for the country
        bondscores <- matrixzs(cd)$zs # get the z for all the bonds
        #if(reversepolarity) bondscores <- -bondscores
        if(plotavg) {
            if(win) windows(12, 6)
            plot(last(cdmat$avgdev, "18 months") * 10000, type = "l", 
                             minor.ticks = FALSE, major.format = "%b %Y", main = "")
            points(last(cdmat$avgdev, "18 months") * 10000, pch = 21, col = "black", bg = "grey")
            points(points(cdmat$avgdev, "18 months") * 10000)
            abline(h = mean(last(cdmat$avgdev, "18 months")) * 10000, lty = "dotted")
            title(paste(cCode, ": average bond deviation, bps", sep = ""), 
                cex.main = 1.2, font.main = 3, col.main = "grey20")
        }
        for (x in length(goodlist):1) {
            # prepare data
            if(ccPCA & minorPCA) {
                if(win) windows(12, 15)
                par(mfrow = c(4, 4))
            } else if(ccPCA | minorPCA) { # if we want to include cross country PCA
                if(win) windows(12, 12)
                par(mfrow = c(3, 4))
            } else {
                if(win) windows(12, 8)
                par(mfrow = c(2, 4))
            }
            par(mar = c(3, 2, 3, 2))
            par(oma = c(2, 1, 6, 1))
            plotter <- goodlist[[x]]$flyspread
            ylims = c(min(plotter), max(plotter))
            # plot the main chart
            plot(plotter, minor.ticks = FALSE, major.format = "%b %Y", ylim = ylims, 
              main = "Fly spread versus fitted curve, 1x2x1", axes = FALSE)
            axis(1, at = axTicks(1), labels = format(as.POSIXct(axTicks(1), origin = "1970-01-01"), "%b %Y"))
            axis(2)
            abline(h = mean(plotter), lty = "dashed")
            abline(h = mean(plotter) + sd(plotter), col = "grey", lty = "dashed")
            abline(h = mean(plotter) - sd(plotter), col = "grey", lty = "dashed")
            points(last(plotter), col = "red", pch = 19)
            dens <- density(plotter)
            # plot density chart
            plot(dens$y, dens$x, ylim = ylims, type = "l")
            abline(h = mean(plotter), lty = "dashed")
            abline(h = mean(plotter) + sd(plotter), col = "grey", lty = "dashed")
            abline(h = mean(plotter) - sd(plotter), col = "grey", lty = "dashed")
            if(goodlist[[x]]$z > 0) {
                arrows(max(dens$y), last(plotter), max(dens$y), mean(plotter) + sd(plotter), code = 3, col = "red", length = 0.1, 
                       angle = 20)
                text(max(dens$y), mean(c(last(plotter)[[1]], mean(plotter) + sd(plotter))), 
                     round(goodlist[[x]]$makebp, 1), col = "red", adj = 1.2)

            } else {
                arrows(max(dens$y), last(plotter), max(dens$y), mean(plotter) - sd(plotter), code = 3, col = "red", length = 0.1, 
                       angle = 20)
                text(max(dens$y), mean(c(last(plotter)[[1]], mean(plotter) - sd(plotter))), 
                     round(goodlist[[x]]$makebp, 1), col = "red", adj = 1.2)
            }
            title(paste("Standard errors:", round(goodlist[[x]]$z, 2)))
            abline(h = last(plotter), col = "red")
            # plot support charts   
            yplotter <- - goodlist[[x]]$yspread
            plot(as.zoo(yplotter), main = "Outright yield fly spread, 1x2x1", axes = FALSE)
            axis(1, at = axTicks(1), labels = format(as.Date(axTicks(1), origin = "1970-01-01"), "%b%y"))
            axis(2)
            #points(last(as.zoo(yplotter)), col = "black", pch = 19)
            text(last(index(as.zoo(yplotter))), last(yplotter), round(last(yplotter), 1), adj = 1, col = "red", cex = 0.9)
            # plot bond zs
            barplot(- (as.matrix(bondscores[, goodlist[[x]]$instruments])), beside = TRUE, 
                    width = c(rep(1, 20), 3), col = c(rep("grey", 20), "grey30"), 
                    names.arg = isinLabel(cCode, goodlist[[x]]$instruments, coupon = FALSE, seper = " "), border = "white")
            abline(h = c(-2, 2), col = "grey", lty = "dotted")
            uu <- par("usr")
            rect(uu[1], uu[3], uu[2], uu[4])
            title("Bond z-scores (with 21 day hist)")
            # plot yield curve
            plot(2:32, last(cy)[, 2:32] * 100, type = "l", main = "Yield curve")
            points(as.numeric(last(cdmat$matmat)), as.numeric(last(cdmat$ymat) * 100), pch = 19, col = addAlpha("grey", 0.5))
            abline(v = as.numeric(last(cdmat$matmat[, goodlist[[x]]$instruments])), col = addAlpha("blue", 0.5), lty = "dotted")
            points(as.numeric(last(cdmat$matmat[, goodlist[[x]]$instruments])), 
                   as.numeric(last(cdmat$ymat[, goodlist[[x]]$instruments])) * 100,
                   pch = 19, col = addAlpha("blue", 0.5), cex = c(1.5, 2.5, 1.5))
            #now the regressions against pcs
            #pc1
            barplot(as.numeric(pca$countryloads[[cCode]][, 1]), axes = FALSE, border = NA, col = "grey80")
            par(new = TRUE)
            regress(pca[[1]][, cCode] * 100, yplotter, main = "Fly against PC1", axes = TRUE, legnd = TRUE, 
                xlab = "directional component", ylab = "fly bps", hiDays = c(5, 21, mindays))
            axis(2)
            #pc2
            barplot(as.numeric(pca$countryloads[[cCode]][, 2]), axes = FALSE, border = NA, col = "grey85")
            par(new = TRUE)
            regress(pca[[2]][, cCode] * 100, yplotter, main = "Fly against PC2", axes = TRUE, 
                    xlab = "slope component", ylab = "fly bps", 
                    hiDays = c(5, 21, mindays), legnd = FALSE)
            axis(2)
            legend("bottomright", fill = "grey80", legend = "PC loadings", bg = addAlpha("white", 0.5))
            #pc3
            barplot(as.numeric(pca$countryloads[[cCode]][, 3]), axes = FALSE, border = NA, col = "grey85")
            par(new = TRUE)
            regress(pca[[3]][, cCode] * 100, yplotter, main = "Fly against PC3", axes = TRUE, 
                    xlab = "curvature component", ylab = "fly bps", 
                    hiDays = c(5, 21, mindays), legnd = FALSE)
            axis(2)
            if(minorPCA) { # again if include cross country PCA
                barplot(as.numeric(pca$countryloads[[cCode]][, 4]), axes = FALSE, border = NA, col = "grey80")
                par(new = TRUE)
                regress(pca[[4]][, cCode] * 100, yplotter, main = "Fly against PC4", axes = TRUE, legnd = FALSE, 
                    xlab = "directional component", ylab = "fly bps", hiDays = c(5, 21, mindays))
                axis(2)
                barplot(as.numeric(pca$countryloads[[cCode]][, 5]), axes = FALSE, border = NA, col = "grey80")
                par(new = TRUE)
                regress(pca[[5]][, cCode] * 100, yplotter, main = "Fly against PC5", axes = TRUE, legnd = FALSE, 
                    xlab = "directional component", ylab = "fly bps", hiDays = c(5, 21, mindays))
                axis(2)
                barplot(as.numeric(pca$countryloads[[cCode]][, 6]), axes = FALSE, border = NA, col = "grey80")
                par(new = TRUE)
                regress(pca[[6]][, cCode] * 100, yplotter, main = "Fly against PC6", axes = TRUE, legnd = FALSE, 
                    xlab = "directional component", ylab = "fly bps", hiDays = c(5, 21, mindays))
                axis(2)
                barplot(as.numeric(pca$countryloads[[cCode]][, 7]), axes = FALSE, border = NA, col = "grey80")
                par(new = TRUE)
                regress(pca[[7]][, cCode] * 100, yplotter, main = "Fly against PC7", axes = TRUE, legnd = FALSE, 
                    xlab = "directional component", ylab = "fly bps", hiDays = c(5, 21, mindays))
                axis(2)
            }
            if(ccPCA) { # again if include cross country PCA
                regress(pca$crosscountry[, 1] * 100, yplotter, main = "Fly against cross country PC1", legnd = FALSE,
                        axes = TRUE, xlab = "curvature component", ylab = "fly bps", hiDays = c(5, 21, mindays))
                axis(2)
                regress(pca$crosscountry[, 2] * 100, yplotter, main = "Fly against cross country PC2", legnd = FALSE,
                        axes = TRUE, xlab = "curvature component", ylab = "fly bps", hiDays = c(5, 21, mindays))
                axis(2)
                regress(pca$crosscountry[, 3] * 100, yplotter, main = "Fly against cross country PC3", legnd = FALSE,
                        axes = TRUE, xlab = "curvature component", ylab = "fly bps", hiDays = c(5, 21, mindays))
                axis(2)
                regress(pca$crosscountry[, 4] * 100, yplotter, main = "Fly against cross country PC4", legnd = FALSE,
                        axes = TRUE, xlab = "curvature component", ylab = "fly bps", hiDays = c(5, 21, mindays))
                axis(2)
            }
            mtext(paste("  ", format(Sys.time(), "%Y-%m-%d  %Hh%M %Z")), side = 1, line = 0, outer = TRUE, 
                adj = 0, cex = 0.7, col = "grey", font = 3) # timestamp
            title(goodlist[[x]]$fly, outer = TRUE, cex.main = 2, font.main = 3, col.main = "grey20")
            # trade score with lm to PCA
            flyscore <- goodlist[[x]]$flyscore
            flyscore3 <- goodlist[[x]]$flyscore3
            title(paste("fly score:", round(flyscore3, 1)), 
                  col.main = "red", line = 0.5, outer = TRUE, cex.main = 1.5, font.main = 3)
        }
    }
    if(win) return(goodlist) else return("")
}


flyknit <- function() {
    knit("flymax.rhtml")
    shell("flymax.html", wait = FALSE)
}


rankmax <- function(backdays = 1, cCodes = ac, mats = keymats, selector = 3, maxhedge = 1, model = "auto", 
                    method = "linear", decayhl = 130, years = 3, wingratio = 3) {
    s0 <- spreadmax(cCodes = cCodes, mats = mats, selector = selector, maxhedge = maxhedge, 
                    model = model, method = method, 
                    decayhl = decayhl, years = years, ordermult = TRUE, daysback = 0, wingratio = wingratio)
    s1 <- spreadmax(cCodes = cCodes, mats = mats, selector = selector, maxhedge = maxhedge, 
                    model = model, method = method, 
                    decayhl = decayhl, years = years, ordermult = TRUE, daysback = backdays, wingratio = wingratio)
    s1 <- s1[rownames(s0), ]
    rankchange <- s0$rank - s1$rank
    zchange <- s0$se - s1$se
    rankchangedf <- data.frame(rank = s0$rank, rankchange = rankchange, zchange = round(zchange, 2), 
                               z = round(s0$se, 2), rsq = round(s0$rsq, 2))
    rownames(rankchangedf) <- rownames(s0)
    return(rankchangedf)
}


spreadmax <- function(cCodes = ac, mats = keymats, selector = 3, maxhedge = 1, model = "auto", method = "linear", 
                      decayhl = 130, years = 3, ordermult = TRUE, daysback = 0, wingratio = 3) {
# get all the combinations of number "selector" in vector "mats" and get the spreads, then do stats on them to see if there is anything
    comblist <- combn(mats, selector, simplify = FALSE) # all the combinations of mats
    maxispreads <- spreadget(cCodes = cCodes, spreads = comblist, model = model, years = years, daysback = daysback) # get all the spreads
    if(is.null(decayhl) | is.na(decayhl)) 
        decayer <- rep(1, nrow(maxispreads[[1]])) else decayer <- decay(nrow(maxispreads[[1]]), decayhl)
    countrycomb <- combn(cCodes, maxhedge + 1)
    durations <- lapply(cCodes, function(x) cydur(x, model = ifelse(model == "auto", keymodel[[x]], model))) # get durations so that we can kill flys with too much ratio
    names(durations) <- cCodes
    allscores <- sapply(names(maxispreads), function(spread) { # for each country
        apply(countrycomb, 2, function(cc) { # for each spread combination
            maturities <- strsplit(spread, "-")[[1]]
            dur1 <- durations[[cc[1]]][maturities, "mdur"]
            dur2 <- durations[[cc[2]]][maturities, "mdur"]
            durdiff1 <- diff(dur1)
            durdiff2 <- diff(dur2)
            if(selector == 3) {   # do the wing ratio test if the selector is butterflies, otherwise all good so all badwing test false
                badwing <- ((durdiff1[1] / durdiff1[2] > wingratio) | (durdiff1[2] / durdiff1[1] > wingratio) 
                    | (durdiff2[1] / durdiff2[2] > wingratio) | (durdiff2[2] / durdiff2[1] > wingratio)) 
            } else badwing <- FALSE
            lmmatrix <- maxispreads[[spread]][, cc]
            if(method == "quantile") {
                modq <- rq(lmmatrix[, 1] ~ ., data = lmmatrix[, -1], weights = decayer)
                r <- modq$residuals
                f <- modq$fitted
                w <- modq$weights
            } else if (method == "linear") {
                modq <- lm(lmmatrix[, 1] ~ ., data = lmmatrix[, -1], weights = decayer)
                r <- modq$residuals
                f <- modq$fitted
                w <- modq$weights
            } else if (method == "deming") {
                modq <- lm(lmmatrix[, 1] ~ ., data = lmmatrix[, -1], weights = decayer)
                r <- modq$residuals
                f <- modq$fitted
                w <- modq$weights
            }
            r <- modq$residuals
            f <- modq$fitted
            w <- modq$weights
            mss <- if (attr(modq$terms, "intercept")) {
                m <- sum(w * f/sum(w))
                sum(w * (f - m)^2)
            } else sum(w * f^2)
            rss <- sum(w * r^2)
            r <- sqrt(w) * r
            modqr2 <- mss/(mss + rss)
            se <- last(r) / sd(r)
            return(list(se = se, rsq = modqr2, coeffs = modq$coefficients, badwing = badwing))
        })
    })
    rownames(allscores) <- apply(countrycomb, 2, catlabels)
    retdf <- expand.grid(rownames(allscores), colnames(allscores))
    retdf <- cbind(retdf, apply(retdf, 1, function(x) allscores[x[1], x[2]][[1]]$rsq)) # addrsq
    retdf <- cbind(retdf, apply(retdf, 1, function(x) allscores[x[1], x[2]][[1]]$se)) # add standard errors
    retdf <- cbind(retdf, apply(retdf, 1, function(x) allscores[x[1], x[2]][[1]]$badwing)) # badwings for later removal
    retdf <- cbind(retdf, t(apply(retdf, 1, function(x) allscores[x[1], x[2]][[1]]$coeffs))) # coefficients of regression
    retdf <- retdf[!retdf[, 5], -5] # take out bad wings
    scorevec <- abs(retdf[, 4]) * retdf[, 3] # score each one by multiplying rsq by num standard errors
    ordervec <- order(scorevec) # order of the scores
    rankvec <- nrow(retdf) - rank(scorevec) + 1 # rank of the scores going downards
    retdf <- cbind(retdf, rankvec)
    colnames(retdf)[3:5] <- c("rsq", "se", "intercept")
    rownames(retdf) <- paste(retdf[, 2], retdf[, 1])
    colnames(retdf)[6:(ncol(retdf) - 1)] <- paste("coeff", 1:maxhedge, sep = "")
    colnames(retdf)[ncol(retdf)] <- "rank"
    if (ordermult == TRUE) retdf <- retdf[ordervec, ] else retdf <- retdf[order(abs(retdf$se)), ]
    return(retdf)
}


spreadout <- function(spreadlist = spreadget(), sethresh = 1.5, howmuchret = 21, win = TRUE, decayhl = 260, plotit = TRUE, 
                      meanone = FALSE, useridge = FALSE, ridgelambda = 2, usepca = FALSE, whichpcs = 1:3, plotfirst = FALSE) {
# this will take a spreadlist and find which spreads are out of whack using regression
# plotfirst: yes or no plot the summmary chart
    if(is.na(decayhl)) decayer <- rep(1, nrow(spreadlist[[1]])) 
        else decayer <- decay(nrow(spreadlist[[1]]), decayhl, meanone = meanone) # decay vector 
    numcountry <- ncol(spreadlist[[1]])
    alloutboth <- lapply(names(spreadlist), function(x) {         # all the spreads
        zout <- sapply(names(spreadlist[[x]]), function(y) {      # all the countries
            rss <- regsubsets(spreadlist[[x]][, y] ~ ., 
                data = spreadlist[[x]][, -index(names(spreadlist[[x]]))[names(spreadlist[[x]]) == y]], weights = decayer) 
            rsswhich <- summary(rss)$which
            rssadjr2 <- summary(rss)$adjr2
            maxar2 <- max(rssadjr2)
            selector <- rsswhich[index(rssadjr2)[rssadjr2 == maxar2], -1] # all this to get the selctor
            selector <- names(selector)[selector] # just get the inclusion countries                
            lmodel <- lm(spreadlist[[x]][, y] ~ spreadlist[[x]][, selector], weights = decayer) # build the linear model
            ridgemodel <- linearRidge(spreadlist[[x]][, y] ~ spreadlist[[x]][, selector], 
                                      lambda = ridgelambda) # build the linear model
            if(useridge) {
                resid <- spreadlist[[x]][, y] - spreadlist[[x]][, selector] %*% coef(ridgemodel)[-1]  # manual residuals
                resid <- resid - coef(ridgemodel)[[1]]
            } else {
                resid <- lmodel$residuals #
            }
            lmresid <- lmodel$residuals #
            resid <- xts(as.numeric(resid), order.by = as.Date(index(resid)))
            othercountries <- names(spreadlist[[x]])[- match(y, names(spreadlist[[x]]))]
            if(usepca) {
                pcas <- getPCAs(othercountries, series = TRUE, model = "auto", decayhl = decayhl, scale.unit = FALSE)
                spread = spreadlist[[x]][, y]
                pcs = pcas$crosscountry[, whichpcs]
                pcresid <- lm(spread ~ pcs, weights = decayer)$residuals
                resid <- pcresid
            }
            # now history of sd (not recreating the reg every time)
            sds <- na.omit(rollapply(resid, length(resid) - howmuchret + 1, function(x) wt.sd(x, last(decayer, length(x))))) 
            serr <- last(resid, howmuchret) / sds # standards errors
            return(list(serr, last(resid, howmuchret)))
        })
        squarese <- sapply(names(spreadlist[[x]]), function(m) {
            sapply(names(spreadlist[[x]]), function(p) {
                if (m == p) return(0)
                rr <- as.numeric(lm(spreadlist[[x]][, m] ~ spreadlist[[x]][, p], weights = decayer)$residuals)
                return(last(rr) / wt.sd(rr, decayer))
            })
        })
        return(list(zout, squarese))
    })
    allout <- lapply(alloutboth, "[[", 1) # get the spread by country z scores and residuals
    squareout <- lapply(alloutboth, "[[", 2) # get the country by country z score matrices
    names(allout) <- names(squareout) <- names(spreadlist)
    allzout <- lapply(allout, function(x) sapply(x[1, ], function(y) y)) # extract z scores
    allrout <- lapply(allout, function(x) sapply(x[2, ], function(y) y)) # extract actual bps
    names(allzout) <- names(allrout) <- names(spreadlist)
    if (plotit == TRUE) {
        pal <- unlist(cColors[colnames(spreadlist[[1]])])
        minn <- min(c(-2, sapply(allzout, min))) # get axis minimum
        maxx <- max(c(2, sapply(allzout, max))) # get axis maximum
        if(plotfirst) {
            mainplot <- sapply(allzout, last) # get the most recent for each one
            mzout <- melt(allzout) # for ggplot
            # now plot these guys
            if (win) windows(9, 5)
            par(mar = c(1.5, 3, 2, 3), oma = c(1.5, 0, 0, 0))
            barplot(mainplot, beside = TRUE, col = "white", ylim = c(minn, maxx), border = "white", axes = FALSE, 
                space = c(0, 3), cex.axis = 0.8, cex.names = 0.7) # dummy plot
            u <- par("usr") # for the background grey shading rectangle
            rect(u[1], -sethresh, u[2], sethresh, col = "grey96", border = NA) # draw the rectangle
            barplot(mainplot, beside = TRUE, col = pal, ylim = c(minn, maxx), border = "grey50", add = TRUE, 
                space = c(0, 3), cex.names = 0.7, cex.axis = 0.8) -> bb
            abline(h = c(-sethresh, sethresh), col = addAlpha("grey50", 0.5), lty = "dashed")
            legend("bottomright", legend = colnames(spreadlist[[1]]), col = pal, pch = 15, bg = addAlpha("white", 0.5), box.lwd = 0, 
                cex = 0.8)
            title("Spread Z scores", col.main = "grey50", line = -0.5)
        }
        # now plot all the subcharts
        ll <- lapply(names(allzout), function(x) {
            if (win) {
                windows(9, 5)
                par(mar = c(1.5, 3, 2, 3), oma = c(1.5, 0, 0, 0))
            }
            cols <- sapply(pal, function(p) c(rep("grey88", howmuchret - 1), p))
            wids <- c(rep(1, howmuchret - 1), 3)
            barplot(allzout[[x]], beside = TRUE, space = c(0, 10), col = "white", ylim = c(minn, maxx), width = wids, axes = FALSE,
                border = "white", names.arg = rep("", ncol(allzout[[x]])), cex.names = 0.8, cex.axis = 0.8)
            u <- par("usr") # coords for the plot area 
            rect(u[1], -sethresh, u[2], sethresh, col = "grey96", border = NA) # draw the background rectangle
            barplot(allzout[[x]], beside = TRUE, space = c(0, 10), col = cols, ylim = c(minn, maxx), width = wids, 
                border = "grey50", names.arg = rep("", ncol(allzout[[x]])), add = TRUE, cex.names = 0.8, cex.axis = 0.8) -> bb
            text(apply(bb, 2, mean), -(sethresh * 1.2), colnames(allzout[[x]]))
            labelfactors <- last(allrout[[x]]) / last(allzout[[x]]) # work out the multipler for the individual bps axes
            axispos <- apply(bb, 2, last) + 3
            ss <- sapply(1:ncol(allrout[[x]]), function(i) {
                pty <- pretty(c(allrout[[x]][, i], 0))
                ptyat <- pty / labelfactors[i]
                excludepty <- ((ptyat > max(axTicks(2))) | (ptyat < min(axTicks(2)))) # where will mini axis be outside of main axis
                pty <- pty[!excludepty] # exclude that
                ptyat <- ptyat[!excludepty] # and exclude that too
                axis(4, at = ptyat, labels = pty, pos = axispos[i], padj = -2.5, tcl = -0.22, col = "grey70", cex.axis = 0.6, col.axis = "grey70")
                if(i == 1) text(axispos[1] + 2.55, max(ptyat), "bps", pos = 3, offset = 0.5, cex = 0.6, col = "grey70")
            })
            abline(h = c(-sethresh, sethresh), col = addAlpha("grey70", 0.5), lty = "dashed")
            title(x, col.main = "grey50", line = 0.0)
            title(paste(howmuchret, "day z-score history"), col.main = "grey50", cex.main = 0.8, line = -1)
            mtext(paste("  ", format(Sys.time(), "%Y-%m-%d  %Hh%M %Z")), side = 1, line = 0, outer = TRUE, 
                adj = 0, cex = 0.7, col = "grey", font = 3)
        })
    } else return(list(allout, squareout))
}


ssquare <- function(squarelist, win = TRUE) {
# plots a square grid of the 2x country regressions from the spreadout()[[2]] output
    lapply(names(squarelist), function(squarename) {
        if(win) windows(4, 4)
        whichsquare <- squarelist[[squarename]]
        squareplot <- tableGrob(round(whichsquare, 1), 
            gpar.coretext = gpar(fontsize = 7), 
            gpar.coltext = gpar(fontsize = 7), 
            gpar.rowtext = gpar(fontsize = 7), h.even.alpha = 0.5) # ggplot table
        grid.draw(squareplot)
    })
}
    

prettyBondName <- function(country, month, year, coupon = NA) {
    paste(country, ifelse(is.na(coupon), NULL, coupon), format(month, "%b"), format(year, "%y"))
}

isinLabel <- function(cCode, isins, seper = " ", coupon = TRUE, couponAfter = FALSE, shortlab = FALSE, withcCode = TRUE) {
# make nice labels from isins; shortlab = make a compact label
    cData <- get(paste(cCode, "data", sep = ""))
    data <- sapply(isins, function(x) cData$staticData[cData$staticData$ID_ISIN == x, c("COUPON", "MATURITY")])
    if(shortlab) {
        data[2, ] <- format(as.Date(as.character(data[2, ])), "%m%y")
        labs <- apply(data, 2, function(x) paste(cCode, x[2], sep = ""))
    } else {
        data[2, ] <- format(as.Date(as.character(data[2, ])), "%b%y")
        if(coupon) {
            if(couponAfter) {
                labs <- apply(data, 2, function(x) paste(cCode, seper, x[2], seper, "(", x[1], ")", sep = ""))
            } else {
                labs <- apply(data, 2, function(x) paste(cCode, x[1], x[2], sep = seper))
            }
        } else {
            labs <- apply(data, 2, function(x) paste(cCode, x[2], sep = seper))
        }
    }
    if(!withcCode) labs <- sapply(labs, function(x) substr(x, 3 + nchar(seper), nchar(x))) # remove country if necessary
    return(labs)
}

tsBondYield <- function(cCode, isin, today = Sys.Date()) {
# gets the yield of a bond using the termstrc library
    cbobj <- createCouponBonds(cCode, as.character(today), minmat = 0)
    cfmat <- create_cashflows_matrix(cbobj[[1]], TRUE)
    matmat <- create_maturities_matrix(cbobj[[1]], TRUE)
    return(as.numeric(ann_yields(cfmat, matmat)[isin, "Yield"]))
}


dobox <- function(bonds, weights, virs = FALSE, withfit = TRUE, years = 1, model = "auto", main = "", withlegend = TRUE, plotit = TRUE, 
                  regressions = TRUE, usecy = TRUE) {
# plots a series of the bonds, by weights, against IRS if necessary, and with a chart too
# bonds is a list of lists. Each element of bonds is either a generic rate (if this list is length 2) or a specific (3 or 4)
    if(sum(weights) != 0) flushprint("Weights do not sum to zero! Continuing anyway...")
    bonds <- lapply(bonds, function(x) {   # check for isins
        if ((length(x) == 1) && (regexpr("[A-Z]{2}[0-9, A-Z]{10}", x) == 1)) {
            return(isinSearch(x, TRUE))
        } else return(x)
    })
    #if(class(bonds) != "list") bonds <- list(bonds) # don't fail if we have one bond not passed as a list
    if(length(weights) != length(bonds)) {
        flushprint("Bond list and weights vector are different lengths.")
        return()
    }
    yields <- lapply(bonds, function(x) {
        usemodel <- ifelse(model == "auto", keymodel[[x[[1]]]], model)
        if(length(x) > 2) { # if it's a bond description
            isin <- bondsearch(x[[1]], x[[2]], x[[3]], ifelse(length(x) > 3, x[[4]], NA)) # find the bond
            cfactors <- get(paste(x[[1]], "factors", sep = "")) #get the bond factors
            thisyield <- cfactors[cfactors$bond == isin, c("y", 
                                  paste("yerr", usemodel, sep = ""), "mat", "date")]
            thisyield <- last(xts(thisyield[, -4], order.by = as.Date(thisyield[, 4])), years * 260)
            #if(nrow(thisyield) < years * 260) {
            #    wdays <- wdaylist(Sys.Date() - years * 260)
            #    thisyield <- sapply(wdays, function(ty) tsBondYield(x[[1]], isin, ty))
            #    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! slow and shit but might actually work
            #}
            coupyields <- last(couponYields(get(paste(x[[1]], "data", sep = ""))[[usemodel]]), nrow(thisyield))
            if(usecy) {
                thisyield[, 2] <- sapply(1:nrow(coupyields), function(cy) approx(1:35, coupyields[cy, ], thisyield[cy, 3])$y)
            } else {
                thisyield[, 2] <- thisyield[, 1] + thisyield[, 2]
            }
            #thisyield[, 2] <- thisyield[, 1] - thisyield[, 2] 
            if(virs == TRUE) {
                forirs <- last(eurirs, nrow(thisyield))
                bondirs <- sapply(1:nrow(thisyield), function(y) approx(irsmats, forirs[y, ], thisyield[y, 3], rule = 2)$y/100) # irs for mat
                thisyield[, 1] <- thisyield[, 1] - bondirs
            }
            return(thisyield)
        } else {  # if it's just a maturity off the couponyields
            cfactors <- get(paste(x[[1]], "data", sep = ""))
            cyields <- last(couponYields(cfactors[[usemodel]])[, x[[2]]], 
                            years * 260)
            if(virs == TRUE) {
                forirs <- last(eurirs, nrow(cyields))
                modelirs <- sapply(1:nrow(cyields), function(y) approx(irsmats, forirs[y, ], x[[2]], rule = 2)$y/100) # irs for mat
                modelirs <- cyields - modelirs
            } else modelirs <- cyields
            return(cbind(modelirs, cyields)) 
            #return(cbind(cyields, cyields))
        }
    })
    minlen <- min(sapply(yields, nrow))
    yields <- lapply(yields, function(x) last(x, minlen)) # make them all the same length
    bseries <- do.call(cbind, lapply(yields, function(x) x[, 1])) #the matrix of bond series
    mseries <- do.call(cbind, lapply(yields, function(x) x[, 2])) #the matrix of fitted series
# okay this is the plotting zone --------------------
# first build the title
    # build the title
    if(main == "") {
        titleElements <- lapply(1:length(bonds), function(x) {
            if(weights[[x]] == 1) {
                wtext <- ifelse(x == 1, "", "+")
            } else if (weights[[x]] == -1) {
                wtext <- "-" 
            } else {
                wtext <- paste(ifelse(weights[[x]] >= 0, ifelse(x == 1, "", "+"), ""), weights[[x]], "x ", sep = "")
            }
            ctext <- bonds[[x]][[1]]
            if(length(bonds[[x]]) > 2) {
                btext <- paste(ifelse(length(bonds[[x]]) > 3, paste(bonds[[x]][[4]], " ", sep = ""), ""), 
                    (month.abb[bonds[[x]][[2]]]),  "",  bonds[[x]][[3]], sep = "") 
            } else {
                btext <- paste(bonds[[x]][[2]], "y", sep = "")
            }
            return(paste(wtext, ctext, btext, sep = ""))
        })
        main <- do.call(paste, titleElements)
        if(virs == TRUE) main = paste(main, "versus IRS")
    }
    if(plotit & regressions) {
        # do some regressions
        bshalf1 <- bseries[, 1:(ncol(bseries)/2)]
        bshalf2 <- bseries[, (ncol(bseries)/2 + 1):ncol(bseries)]
        whalf1 <- weights[1:(length(weights)/2)]
        whalf2 <- weights[(length(weights)/2 + 1):length(weights)]
        bw1 <- bshalf1 %*% whalf1 * 10000
        bw2 <- bshalf2 %*% whalf2 * 10000
        dev.new()
        try(regress(bw1, bw2))
        dev.new()
        try(regress(bw2, bw1))
        dev.new()
        # now do the box
    }
    bonds_curve <- bseries %*% weights * 10000
    model_curve <- mseries %*% weights * 10000
    bplot <- data.frame(bonds_curve = bonds_curve, dt = as.Date(index(bseries)))
    if(withfit) {
        bplot <- cbind(bplot, model_curve)
        colnames(bplot)[ncol(bplot)] <- "model_curve"
    } else withlegend <- FALSE
    if (plotit == TRUE) {
        meltedb <- melt(bplot, id = "dt") # melt by date for ggplot
        g <- ggplot(meltedb, aes(x = dt, y = value, colour = variable)) + 
                            geom_line() + 
                            geom_point(shape = 21, fill = "white", size = 1.7) + 
                            labs(title = main) +
                            scale_color_manual(values = c("violetred1", "dodgerblue2")) + 
                            theme_bw() + 
                            theme(axis.title.x = element_blank(), 
                                  axis.title.y = element_blank(),
                                  legend.title=element_blank()) 
        if((withlegend == FALSE) | (identical(bonds_curve, model_curve))) {
            g <- g + theme(legend.position = "none")
        } else g <- g + theme(legend.position = "bottom")
        plot(g)
    }
    bonds_curve <- xts(bonds_curve, order.by = as.Date(last(index(yields[[1]]), nrow(bonds_curve))))
    model_curve <- xts(model_curve, order.by = as.Date(last(index(yields[[1]]), nrow(model_curve))))
    return(list(bonds_curve = bonds_curve, model_curve = model_curve))
}


boxFromTrade <- function(trade, model = "auto", years = 1, regressions = TRUE, withfit = FALSE, virs = NA, 
                         overrideweights = NA) {
# plots the trade from a trade idea
    if(is.na(overrideweights)) weights = trade$weights else weights = overrideweights
    if(!is.na(virs)) usevirs <- virs else usevirs <- trade$virs
    dobox(bonds = trade$instruments, weights = weights, withfit = withfit, virs = usevirs,
          model = model, years = years, regressions = regressions)
}

ttNames <- function(trads = trades) sapply(trads, function(x) x$name)

ttBill <- function(trads = trades) {
# for billing purposes plots all the trade deets
    sapply(trades, function(x) {
        paste(c(x$name, x$trans[[1]]$entrydate))
    })
}


ttCloseTrade <- function() {
    flushprint(ttNames(trades)) # print out all trades
    flushprint("TRADE CLOSER...Press escape at any time to exit")
    flushprint("Which trade would you like to close, please type trade number and press enter")
    tradenum = as.numeric(scan(n = 1))
    flushprint(tradenum)
    ttTrade(trades[[tradenum]])
    flushprint("How many days ago (0 for today, 1 for yesterday, etc... in calendar days not business days: ")
    daysago = as.numeric(scan(n = 1))
    flushprint(daysago)
    flushprint(Sys.Date() - daysago)
    flushprint ("what is closing level: ")
    closelev = as.numeric(scan(n = 1))
    flushprint(closelev)
    flushprint("Will close the following: ")
    flushprint(ttNames()[tradenum])
    flushprint(paste("at level", closelev))
    flushprint(paste("effective close date", Sys.Date() - daysago))
    flushprint("okay? (y/n then <enter>)")
    yesno = scan(n = 1, what = character())
    if (toupper(yesno) == "Y") {
        trades[[tradenum]][["closedate"]] <- Sys.Date() - daysago
        trades[[tradenum]][["closelevel"]] <- closelev
        ttTrade(trades[[tradenum]])
        save(trades, file = paste(dataplace, "trades.dat", sep = ""))
        trades <<- trades
    }
}
    

ttAddTrade <- function(name, entrydate, entrylevel, size, carry = 0, target, revisit = NA, closedate = NA, closelevel = NA, 
                       instruments, weights, virs = FALSE, withfit = FALSE, tradedwith = NA) {
# add a trade to the trade tracker
    trades <<- append(trades, list(list(name = name, trans = list(list(entrydate = as.Date(entrydate), 
                                  entrylevel = entrylevel, size = size)), 
                                  target = target, revisit = revisit, closedate = as.Date(closedate), closelevel = closelevel, 
                                  instruments = instruments, weights = weights, 
                                  virs = virs, withfit = withfit, tradedwith = tradedwith)))
    save(trades, file = paste(dataplace, "trades.dat", sep = ""))
}


flyFromTrade <- function(trade, model = "auto", maxyears = 1.5, pcmat = FALSE, ccPCA = TRUE, minorPCA = TRUE) {
    bonds <- sapply(trade$instruments, function(x) bondsearch(x[1][[1]], x[2][[1]], x[3][[1]]))
    direction = trade$weights[2] < 0
    flymax(trade$instruments[[1]][1][[1]], specific = bonds, maxyears = maxyears, pcmat = pcmat, 
           reversepolarity = direction, ccPCA = ccPCA, minorPCA = minorPCA, model = model)
}

ttSaveTrades <- function() {
    save(trades, file = paste(dataplace, "trades.dat", sep = ""))
}



oneChart <- function(cCodes = ac, decayhl = 260, usedecay = TRUE, model = "auto", mats = keymats, days = 260 * 3, 
                     regpcs = c(1, 2, 3)) {
    p <- getPCAs(cCodes = cCodes, days = days, model == model,
                 decayhl = decayhl, usedecay = usedecay, zeros = FALSE, 
                 series = TRUE)
    ay <- allYields(cCodes = cCodes, model = model, mats = mats, days = days)
    #now get PCA residuals by country
    if(usedecay) decayer <- decay(days, decayhl) else decayer <- rep(1, days)
    ap <- lapply(cCodes, function(cCode) {
        countrypcs <- lapply(p[regpcs], function(pp) pp[, cCode])
        countrypcs <- do.call(cbind, countrypcs)
        cpc <- sapply(mats, function(y) {
            ll <- lm(ay[[cCode]][, match(y, mats)] ~ countrypcs, weights = decayer)
            flushprint(paste(cCode, "mat", y, "rsq", summary(ll)$r.squared))
            return(ll$residuals)
        })
        colnames(cpc) <- mats
        return(cpc)
    })
    names(ap) <- cCodes
    # okay now we have all the PCA residual maturity cheap dear by country, regress them with each other
    cd <- lapply(cCodes, function(cCode) {
        sapply(mats, function(mat) {
            othermats <- sapply(ap, function(cc) cc[, match(mat, mats)]) # extract the relevant in each element of ap
            othermats <- data.frame(othermats)
            colnames(othermats) <- cCodes
            consider <- othermats[, cCode]
            remainder <- as.matrix(othermats[, -match(cCode, ac)])
            resids <- lm(consider ~ remainder, weights = decayer)$residuals
            outwhack <- as.numeric(last(resids))
        })
    })
}


ttWeekly <- function() {
    sapply(ttThisWeek(), ttTrade)
    ttTotalPnL(ttThisWeek())
    ttBar()
}

ttPnL <- function(trade, years = 1.5, plotit = TRUE, useseries = TRUE) {
    if(any(sapply(trade$instruments, length) < 3)) useseries = FALSE # need to go the old "dobox" way if generic instruments
    if(useseries) {
        cCode = trade$instruments[[1]][1]
        instruments <- sapply(trade$instruments, function(x) bondsearch(x[[1]], x[[2]], x[[3]], 
                                                                        ifelse(length(x) == 3, NA, x[[4]]), returnISIN = FALSE))
        yields <- lapply(trade$instruments, function(x) { 
                        bondbbcode <- bondsearch(x[[1]], x[[2]], x[[3]], ifelse(length(x) == 3, NA, x[[4]]), returnISIN = FALSE)
                        cData <- get(paste(x[[1]], "data", sep = ""))
                        thisyield <- cData$historicData$YLD_YTM_MID[, bondbbcode]
                        # kill the series
                        thisyield <- na.omit(thisyield[paste(trade$trans[[1]]$entrydate - years * 365, "/", sep = ""), ]) 
                        if(trade$virs) {
                            mat = as.numeric(bondMats(cCode, bb2isin(cCode, bondbbcode), showinyears = TRUE))
                            interpirs <- interpIRS(mat, last(eurirs, nrow(thisyield)))
                            thisyield <- as.numeric(thisyield) - as.numeric(interpirs)
                            thisyield <- xts(thisyield, order.by = index(interpirs))
                        }
                        return(thisyield)
        })
        yields <- na.omit(do.call(cbind, yields))
        series <- xts(yields %*% trade$weights, order.by = index(yields))
        series <- data.frame(dt = as.Date(as.character(index(series))), bonds_curve = as.numeric(series) * 100)
        dotrade <- series
    } else {
        dotrade <- as.data.frame(dobox(trade$instruments, trade$weights, trade$virs, trade$withfit, 
                   years = years, plotit = FALSE))
        dotrade <- cbind(as.Date(rownames(dotrade)), dotrade)   # put the date in
        dotrade <- dotrade[, -3] # remove the model curve
        names(dotrade)[1] <- "dt"
    }
    returns <- dotrade[dotrade$dt >= trade$trans[[1]]$entrydate, ] # start returns at opendate
    rownames(returns) <- 1:nrow(returns) # remove the date from the colnames
    returns <- cbind(returns, rep(NA, nrow(returns))) # NAs for all the adds
    if(!is.na(trade$closedate)) {
        returns  <- returns[returns$dt < trade$closedate, ]
        returns <- rbind(returns, c(as.character(trade$closedate), trade$closelevel, 0))
    }
    colnames(returns)[3] <- "add"
    lapply(trade$trans, function(x) {   # now add all the start input trades
        returns <<- rbind(c(as.character(x$entrydate), x$entrylevel, x$size), returns)
    })
    returns <- returns[order(returns$dt), ]
    for(x in 2:nrow(returns)) if(is.na(returns[x, "add"])) returns[x, "add"] <- returns[x - 1, "add"] # fill out all the adds
    returns[, 2] <- as.numeric(returns[, 2]) # turn to numerics
    returns[, 3] <- as.numeric(returns[, 3]) # turn to numerics
    dailyrets <- diff(returns[, 2]) * returns[, 3][-nrow(returns)] # that's where the returns happen
    if(trade$target < trade$trans[[1]]$entrylevel) dailyrets <- - dailyrets # fix direction
    dailyrets <- c(0, dailyrets)
    totalret <- cumsum(dailyrets) # total return
    returndf <- as.data.frame(cbind(as.Date(returns[, "dt"]), dailyrets, totalret))
    returndf[, 1] <- as.Date(returndf[, 1])
    colnames(returndf)[1] <- "dt"
    if(plotit == TRUE) {
        g <- ggplot(returndf, aes(x = dt, y = totalret, colour = totalret)) +
                    geom_line() +
                    scale_x_date(labels = date_format("%b %d")) + 
                    geom_point(shape = 21, fill = "white", size = 1.7) + 
                    #geom_hline(yintercept = 0, colour = "grey50") + 
                    labs(title =  paste("P&L: ", round(last(returndf$totalret)), "k EUR", sep = "")) +
                    annotate("text", label = utctime(),
                             x = min(returndf$dt), 
                             y = min(returndf$totalret), hjust = 0, size = 3, colour = "black",
                             fontface = "italic", alpha = 0.3) + 
                    theme(axis.title.x = element_blank(),
                          axis.title.y = element_blank(), 
                          axis.text.x = element_text(size = 8), 
                          axis.text.y = element_text(size = 8),
                          plot.title = element_text(size = 11, 
                                                    colour = ifelse(last(returndf$totalret) > 0, "chartreuse3", "red")),
                          legend.position = "none")
        return(g)
    } else return(xts(returndf[, c("dailyrets", "totalret")], order.by = as.Date(returndf[, "dt"])))
}


ttTotalPnL <- function(trads = trades, plotit = TRUE, win = TRUE, livetitle = TRUE) {
    allrets <- lapply(trads, function(x) ttPnL(x, plotit = FALSE)[, "dailyrets"]) -> ll # get all the returns
    allrets <- lapply(allrets, function(x) {
        uniquerows <- unique(index(x))
        uniquerets <- sapply(uniquerows, function(y) sum(x[y]))
        xts(uniquerets, order.by = as.Date(uniquerows))
    })
    retsmat <- do.call(cbind, allrets)
    colnames(retsmat) <- 1:ncol(retsmat)
    sumrets <- sapply(allrets, sum)
    goodratio <- round(sum(sumrets >= 0) / length(sumrets), 2)
    avgreturn <- round(mean(sumrets))
    totalret <- cumsum(apply(retsmat, 1, function(x) sum(na.omit(x))))
    totalret <- xts(totalret, order.by = as.Date(names(totalret))) # make an xts
    Valrisk <- round(sd(diff(totalret)[-1]) * 1.65)
    trdf <- data.frame(ret = totalret, dt = index(totalret))
    if (plotit == TRUE) {
        g <- ggplot(trdf, aes(x = dt, y = ret)) + 
                    geom_line() +
                    scale_x_date(labels = date_format("%b %d")) + 
                    labs(title = paste(ifelse(livetitle, "P&L since", "Total portfolio P&L since"), 
                                       first(index(totalret)), "(EUR '000)")) + 
                    geom_point(shape = 21, fill = "white", size = 2) +
                    annotate("text", x = last(trdf$dt), y = last(trdf$ret), 
                             label = paste(round(last(trdf$ret)), "k", sep = ""), size = 3, hjust = 0, vjust = 1.5, colour = "grey50") + 
                    annotate("text", x = mean(trdf$dt), y = max(trdf$ret), 
                             label = paste("VaR: ", Valrisk, "k, average trade return: ", avgreturn, "k, good/bad ratio: ", goodratio, sep = ""), 
                             size = 4, vjust = 1.5) + 
                    theme(axis.title.x = element_blank(),
                          axis.title.y = element_blank(), 
                          axis.text.x = element_text(size = 8),
                          axis.text.y = element_text(size = 8),
                          plot.title = element_text(size = 15, colour = "grey20"))
        if(win) windows(7, 5)
        plot(g)
    }
    return(xts(trdf[, 1], order.by = as.Date(trdf[, 2])))
}


ttp <- function() {
# updates prices and plots PnL
    id()
    ttTotalPnL(ttLive())
}


ttTrade <- function(trade, win = TRUE, closeangle = 15, useseries = TRUE) {
# plot chart and PnL 5of a trade
    bonds <- trade$instruments
    weights <- trade$weights
    titleElements <- lapply(1:length(bonds), function(x) {
        if(weights[[x]] == 1) {
            wtext <- ifelse(x == 1, "", "+")
        } else if (weights[[x]] == -1) {
            wtext <- "-" 
        } else {
            wtext <- paste(ifelse(weights[[x]] >= 0, ifelse(x == 1, "", "+"), ""), weights[[x]], "x ", sep = "")
        }
        ctext <- bonds[[x]][[1]]
        if(length(bonds[[x]]) > 2) {
            btext <- paste(ifelse(length(bonds[[x]]) > 3, paste(bonds[[x]][[4]], " ", sep = ""), ""), 
                (month.abb[bonds[[x]][[2]]]),  "",  bonds[[x]][[3]], sep = "") 
        } else {
            btext <- paste(bonds[[x]][[2]], "y", sep = "")
        }
        return(paste(wtext, ctext, btext, sep = ""))
    })
    subtitle <- do.call(paste, titleElements) # names of bonds
    grid.arrange(ttPlotTrade(trade, closeangle = closeangle, useseries = useseries), 
                 ttPnL(trade, useseries = useseries), ncol = 2, 
                 sub = textGrob(subtitle, just = "bottom", gp = gpar(cex = 0.8)),
                 main = trade$name, widths = c(2/3, 1/3))
}


ttLive <- function(trads = trades) {
# return the live trades
    liv <- list()
    for(x in 1:length(trads)) if(is.na(trades[[x]]$closedate)) liv <- append(liv, trads[x])
    return(liv)
}


ttThisWeek <- function(trads = trades, numweeks = 1) {
# all the live trades plus the ones closed in current week
    live <- ttLive()
    pastdays <- Sys.Date() - 1:(numweeks * 7 - 1)
    lastmonday <- last(pastdays[weekdays(pastdays) == "Monday"])
    tradedates <- as.Date(sapply(trades, "[[", "closedate"))
    tradesclosedthisweek <- sapply(tradedates, function(x) {
        if((!is.null(x)) & (!is.na(x))) {
            if (x >= lastmonday) TRUE else FALSE
        } else FALSE
    })
    return(c(ttLive(), trads[tradesclosedthisweek]))
}

ttBar <- function(trads = NA, byday = FALSE, numweeks = 1) {
# bar chart of P&L  
    if(is.na(trads)) trads <- ttThisWeek(numweeks = numweeks)
    pastdays <- Sys.Date() - 1:(numweeks * 7 - 1)
    lastmonday <- last(pastdays[weekdays(pastdays) == "Monday"])
    pnls <- lapply(trads, function(x) ttPnL(x, plotit = FALSE))
    pnls <- lapply(pnls, function(x) x[, 1][paste(lastmonday, "/", sep = "")])
    pnls <- do.call(cbind, pnls)
    if(!byday) pnls <- apply(pnls, 2, function(x) sum(na.omit(x)))
    par(mar = c(20, 3, 3, 3))
    barplot(pnls, names.arg = ttNames(trads), las = 2, cex.names = 0.7)
    title(paste(paste("P&L since", format(lastmonday, "%d %b")), ifelse(byday, "", 
        paste(ifelse(sum(pnls) > 0, "+", ""), round(sum(pnls)), "k EUR", sep = ""))))
}


ttPlotTrade <- function(trade, years = 1, closeangle = 15, useseries = TRUE, plotit = TRUE) {
# plot the trade tracker
# plot the series and the P&L plus some stats
    if(any(sapply(trade$instruments, length) < 3)) useseries = FALSE # need to go the old "dobox" way if generic instruments
    if(useseries) {
        cCode = trade$instruments[[1]][1]
        instruments <- sapply(trade$instruments, function(x) bondsearch(x[[1]], x[[2]], x[[3]], 
                                                                        ifelse(length(x) == 3, NA, x[[4]]), returnISIN = FALSE))
        yields <- lapply(trade$instruments, function(x) { 
                        bondbbcode <- bondsearch(x[[1]], x[[2]], x[[3]], ifelse(length(x) == 3, NA, x[[4]]), returnISIN = FALSE)
                        cData <- get(paste(x[[1]], "data", sep = ""))
                        thisyield <- cData$historicData$YLD_YTM_MID[, bondbbcode]
                        # kill the series
                        thisyield <- na.omit(thisyield[paste(trade$trans[[1]]$entrydate - years * 365, "/", sep = ""), ]) 
                        if(trade$virs) {
                            mat = as.numeric(bondMats(cCode, bb2isin(cCode, bondbbcode), showinyears = TRUE))
                            interpirs <- interpIRS(mat, last(eurirs, nrow(thisyield)))
                            thisyield <- as.numeric(thisyield) - as.numeric(interpirs)
                            thisyield <- xts(thisyield, order.by = index(interpirs))
                        }
                        return(thisyield)
        })
        yields <- na.omit(do.call(cbind, yields))
        series <- xts(yields %*% trade$weights, order.by = index(yields))
        series <- data.frame(dt = as.Date(index(series)), bonds_curve = as.numeric(series) * 100)
        dotrade <- series
    } else {
        dotrade <- as.data.frame(dobox(trade$instruments, trade$weights, trade$virs, trade$withfit, 
                   years = years, plotit = FALSE))
        dotrade <- cbind(as.Date(rownames(dotrade)), dotrade)   # put the date in
        dotrade <- dotrade[, -3] # remove the model curve
        names(dotrade)[1] <- "dt"
    }
    if(plotit == TRUE) {
        entrydf <- data.frame(dt = as.Date(sapply(trade$trans, "[[", "entrydate")),
                              lev = sapply(trade$trans, "[[", "entrylevel"), 
                              siz = sapply(trade$trans, "[[", "size"))
        g <- ggplot(dotrade, aes(x = dt, y = bonds_curve)) +
                    geom_line(colour = addAlpha("grey40", 0.6)) +
                    #scale_x_date(labels = date_format("%b %d")) + 
                    geom_point(shape = 21, colour = addAlpha("grey40", 0.6), fill = addAlpha("white", 0.6), size = 2) +
                    labs(title = paste("Entry: ", trade$trans[[1]]$entrydate, ", Target: ", 
                                       trade$target, ", revisit: ", trade$revisit, sep = "")) + 
                    theme(axis.title.x = element_blank(),
                          axis.title.y = element_blank(), 
                          axis.text.x = element_text(size = 8),
                          axis.text.y = element_text(size = 8),
                          plot.title = element_text(size = 11, colour = "grey20"))
        # now add the entry and close points
        g <- g + geom_point(data = entrydf, aes(x = dt, y = lev), shape = 21, fill = addAlpha("chartreuse2", 0.5),  # add entry points
                            colour = "chartreuse4", size = 3)
        g <- g + geom_text(data = entrydf, aes(x = dt, y = lev, label = paste(siz, "k on ", dt, " at ", lev, sep = "")), # add entry labels
                           size = 3, col = "chartreuse4", size = 4, hjust = 1, vjust = 1.5, font = "bold", 
                           angle = 0, position = "dodge")
        # now add the target and revisit levels
        g <- g + geom_hline(yintercept = trade$target, linetype = "dashed", colour = "chartreuse3")
        g <- g + annotate("text", x = last(dotrade$dt), y = trade$target, label = "target", vjust = 1.5, color = "chartreuse3", 
                          size = 3)
        g <- g + geom_hline(yintercept = trade$revisit, linetype = "dashed", colour = "chocolate3")
        if(!is.na(trade$revisit)) {     # if a revisit level has been included
            g <- g + annotate("text", x = last(dotrade$dt), y = trade$revisit, label = "revisit", vjust = 1.5, color = "chocolate3", 
                              size = 3)
        }
        if(!is.na(trade$closedate)) {

            exitdf <- data.frame(dt = trade$closedate, lev = trade$closelevel)
            g <- g + geom_point(data = exitdf, aes(x = dt, y = lev), 
                                fill = addAlpha("red", 0.5), colour = "red", shape = 21, size = 3)
            g <- g + geom_text(data = exitdf, aes(x = dt, y = lev, label = paste("Closed ", dt, " at ", lev, sep = "")), 
                           size = 3, col = "red", size = 4, hjust = 1, vjust = 1.5, font = "bold", 
                           angle = closeangle, position = "dodge")
        }
        return(g)
    } else {
        return(dotrade)
    }
}

ttRiskHist <- function(trades, whichpcs = 1:3, model = "auto", years = 10, eperiod = "years", ek = 1, useret = FALSE) {
# go back years years and each time plot a trade's relationship to pcs 1 thru 3 of that year
    cCodes <- unique(unlist(sapply(trades, function(trade) sapply(trade$instruments, function(x) x[[1]]))))
    cCodes <- as.character(cCodes)
    isins <- unlist(sapply(trades, function(trade) unlist(list2isin(trade$instruments))))
    isins <- as.character(isins)
    weights <- sapply(trades, function(trade) {
        size <- last(trade$trans)[[1]]$size
        weight <- trade$weights
        return(- size * weight)
    })
    weights <- as.numeric(unlist(weights))
    ay <- allYields(cCodes, days = 260 * years, combine = TRUE, mats = keymats)
    ay <- ay[paste(as.character(year(Sys.Date()) - years + 1), "/", sep = ""), ] # start from 1 Jan
    genspread <- genericSpread(isins, weights, model = model, days = nrow(ay)) * 10000
    eps <- endpoints(ay, on = eperiod, k = ek)
    sapply(2:length(eps), function(x) {
        windows(12, 12)
        layout(t(matrix(c(1, 2, 3, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8), 4, 5)), 
               heights = c(3, 2, 2, 2, 2))
        par(oma = c(2, 2, 2, 2), mar = c(2, 2, 2, 2))
        diffMat <- ay[(eps[x - 1] + 1):eps[x], ]
        gg <- genspread[(eps[x - 1] + 1):eps[x], ]
        pca <- PCA(diffMat, graph = FALSE, scale.unit = TRUE)
        load1 <- pca$var$coord[, 1] / sqrt(pca$eig[1, 1])# / ncol(diffMat) # level component
        if(mean(load1) < 0) load1 <- -load1 # ensure positive loadings
        load2 <- pca$var$coord[, 2] / sqrt(pca$eig[2, 1])# / ncol(diffMat) # slope component
        if(last(load2) < 0) load2 <- -load2 # ensure positive slopes
        load3 <- pca$var$coord[, 3] / sqrt(pca$eig[3, 1])# / ncol(diffMat) # curvature component
        if(length(rle(as.numeric(load3) > 0)) == 3) { # if it is indeed pc3
            if(last(load3) > 0) {
                load3 <- -load3
            }
        }
        load4 <- pca$var$coord[, 4] / sqrt(pca$eig[4, 1])# / ncol(diffMat) # curvature component
        pc1 <- diffMat %*% load1 / sum(abs(load1)) * 100 # divide by abs so makese sense versus inputs
        pc2 <- diffMat %*% load2 / sum(abs(load2)) * 100
        pc3 <- diffMat %*% load3 / sum(abs(load3)) * 100
        pc4 <- diffMat %*% load4 / sum(abs(load4)) * 100
        if(useret) {
            for(y in years) {
                regress(diffret(pc1), diffret(gg), main = paste(year(index(gg)[1]), "pc1"), legnd = FALSE)
                regress(diffret(pc2), diffret(gg), main = paste(year(index(gg)[1]), "pc2"), legnd = FALSE)
                regress(diffret(pc3), diffret(gg), main = paste(year(index(gg)[1]), "pc3"), legnd = FALSE)
                regress(diffret(pc4), diffret(gg), main = paste(year(index(gg)[1]), "pc4"), legnd = FALSE)
            }
        } else {
            for(y in years) {
                regress(pc1, gg, main = paste(year(index(gg)[1]), "pc1"), legnd = FALSE)
                regress(pc2, gg, main = paste(year(index(gg)[1]), "pc2"), legnd = FALSE)
                regress(pc3, gg, main = paste(year(index(gg)[1]), "pc3"), legnd = FALSE)
                regress(pc4, gg, main = paste(year(index(gg)[1]), "pc4"), legnd = FALSE)
            }
        }
        barplot(load1, las = 2)
        title(paste("load1 ", round(pca$eig[1, 2], 1), "%", sep = ""), col = addAlpha("black", 0.7), line = -0.5)
        barplot(load2, las = 2)
        title(paste("load2 ", round(pca$eig[2, 2], 1), "%", sep = ""), col = addAlpha("black", 0.7), line = -0.5)
        barplot(load3, las = 2)
        title(paste("load3 ", round(pca$eig[3, 2], 1), "%", sep = ""), col = addAlpha("black", 0.7), line = -0.5)
        barplot(load4, las = 2)
        title(paste("load4 ", round(pca$eig[4, 2], 1), "%", sep = ""), col = addAlpha("black", 0.7), line = -0.5)

    })
    return(NULL)
}
    

ttRisk <- function(trades = ttLive(), days = 260 * 1, model = "auto", usedecay = FALSE, scale.unit = TRUE, pccheck = 1:6, 
                   pccountries = cl) {
    risks <- lapply(trades, function(trade) {
        cCodes <- trade$instruments[[1]][[1]]
        cCodes <- sapply(trade$instruments, function(x) x[[1]]) 
        isins <- list2isin(trade$instruments)
        size <- last(trade$trans)[[1]]$size
        weights <- trade$weights
        genspread <- genericSpread(isins, weights, model = model, days = days) * 10000
        genspread <- diffret(genspread)
        risk <- genspread * size
        return(risk)
    })
    names(risks) <- sapply(trades, function(x) x$name)
    risks <- do.call(cbind, risks)
    cumrisk <- apply(risks, 1, sum)
    cumrisk <- last(cumrisk, days)
    totalvar <- sd(cumrisk) * 1.65
    pcs <- getPCAs(cCodes = pccountries, days = days, model = model, 
                   usedecay = usedecay, scale.unit = scale.unit, series = FALSE)
    exposures <- sapply(pccheck, function(x) {
        cutrisk <- as.numeric(last(cumrisk, nrow(pcs$crosscountry)))
        linmod <- lm(cutrisk ~ pcs$crosscountry[, x])
        return(coef(linmod)[2] /10000)
    })
    pcweights <- pcs$crosseigs[pccheck]/sum(pcs$crosseigs) # something wrong here somewhere
    pcvars <- totalvar * pcweights
    sapply(pccheck, function(x) {
        windows(16, 5)
        par(oma = c(1, 1, 3, 1))
        par(mfrow = c(1, 3))
        layout(t(matrix(c(1, 2))), widths = c(1, 2))
        titl <- paste("Cumulative risk against PC", x, ":", sep = "")
        regress(pcs$crosscountry[, x], cumrisk, main = "Correlation", 
                xlab = paste("pc", x, sep = ""), ylab = "P&L", cex.main = 1, col.main = "darkgrey")
        titl <- paste(titl, " ", round(pcvars[x]), "k EUR, ", 
                      round((pcvars[x] / totalvar) * 100), "% of total VaR", sep = "")
        cols <- unlist(lapply(cl, function(x) rep(cColors[[x]], length(buckets))))
        barplot(pcs$crossloads[, x], las = 2, cex.names = 0.6, border = cols, 
                main = "Loadings", col.main = "darkgrey", cex.main = 1)
        title(titl, outer = TRUE)
    })
    # now we are going to do each trade by its relationship to the PCs
    tradepl = lapply(trades, function(x) ttPnL(x, plotit = FALSE)[, "totalret"])
    names(tradepl) <- ttNames(trades)
    pcseries <- getPCAs(cCodes = pccountries, days = days, model = model, 
                   usedecay = usedecay, scale.unit = scale.unit, series = TRUE)
    windows(length(tradepl) * 2, length(pccheck) * 2) # setup a windows by the right size
    par(oma = c(1, 3, 3, 1), mar = c(1, 1, 1, 1))
    layout(t(matrix(1:(length(tradepl) * length(pccheck)), nrow = length(tradepl), ncol = length(pccheck))))
    for (y in 1:length(pccheck)) {
        for (x in 1:length(tradepl)) {
            regress(pcseries$crosscountry[, y], tradepl[[x]], cex.axes = 0.1, col.axis = "white",
                    legnd = FALSE, hiDays = c(1, 5, 10))
            par(new = TRUE)
            plot.zoo(last(pcseries$crosscountry[, y], 
                          length(na.omit(tradepl[[x]]))), col = "grey", axes = FALSE)
            par(new = TRUE)
            plot.zoo(na.omit(tradepl[[x]]), col = "red", axes = FALSE)
        }
    }
}


cashflowCalendar <- function(cCodes = ac, months = 3, plotit = TRUE) {
    startDate <- Sys.Date()
    endDate <- Sys.Date() + months * 30.5
    
    cflist <- lapply(cCodes, function(cCode) {
        cData <- get(paste(cCode, "data", sep = ""))
        return(cData$cfData)
    })
    names(cflist) <- cCodes
    cfdf <- melt(cflist)
    cfdf <- cfdf[(cfdf$Date >= startDate) & (cfdf$Date <= endDate), ] # only the dates from now until within "months" arg
    cfdf <- cfdf[cfdf$value > 0, ] # only nonzerocashflows
    dfdf <- cfdf
    names(cfdf)[4:5] <- c("bond", "country")
    # now get the outstanding amounts
    bondtickers <- paste(unique(cfdf$bond), "Corp")
    outstanding <- bdp(conn, bondtickers, "amt_outstanding") # get outstanding by bond
    rownames(outstanding) <- sapply(strsplit(rownames(outstanding), " "), "[[", 1) # take the Corp out again
    alloutstanding <- sapply(cfdf$bond, function(x) outstanding[x, 1])
    cfdf$value <- ((alloutstanding / 1000000) * cfdf$value) / 1000000000 # adjust value of cashflow by amount outstanding
    cfdf$Date <- as.Date(cfdf$Date) # 
    cfdf <- cfdf[order(cfdf$Date), ] # order 
    return(cfdf)
}


# produces a chart of all the upcoming cashflows


    

    
# DEBUG ####################################################################################################################################
############################################################################################################################################
watchcurves <- function(ccode, daysback = 260 * 3) {
    cdata <- get(paste(ccode, "data", sep = ""))      # get the country data
    windows(7, 10)
    par(mfrow = c(2, 1))
    lapply(last(names(cdata$actives), daysback), function(x) {
        ally <- rbind(cdata$ns[[x]]$yerrors, cdata$nsOff[[x]]$yerrors, cdata$nsBoth[[x]]$yerrors)
        limy <- c(min(ally[, 2]), max(ally[, 2]))
        limx <- c(min(ally[, 1]), max(ally[, 1]))
        plot.default(cdata$nsBoth[[x]]$yerrors, col = "darkgreen", type = "o", pch = 19)
        points.default(cdata$nsOff[[x]]$yerrors, col = "red", pch = 19)
        lines.default(cdata$nsOff[[x]]$yerrors, col = "red")
        scan()
        points.default(cdata$ns[[x]]$yerrors, col = "black", pch = 19)
        lines.default(cdata$ns[[x]]$yerrors, col = "black")
        abline(h = 0)
        plot(cdata$nsBoth[[x]]$yhat, col = "darkgreen", type = "o", pch = 19, log = "y")
        points(cdata$nsOff[[x]]$yhat, col = addAlpha("red", 0.5), pch = 19)
        lines(cdata$nsOff[[x]]$yhat, col = addAlpha("red", 0.5))
        points(cdata$ns[[x]]$yhat, col = addAlpha("black", 0.4), pch = 19)
        lines(cdata$ns[[x]]$yhat, col = addAlpha("black", 0.4))
        title(paste(ccode, x), outer = TRUE, line = -1)
        legend("bottomright", legend = c("ns", "nsOff", "nsBoth"), col = c("black", "red", "darkgreen"), lwd = 2)
        scan()
    })
}


get4 <- function(ccodes) {
    for(x in ccodes) bbGetCountry(x)
}


# problem list !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# DEBUG
daybydaycurve <- function(cCode, model = keymodel[[cCode]], numyears = 3) {
    cData <- get(paste(cCode, "data", sep = ""))
    mod <- last(cData[[model]], numyears * 260)
    for(x in names(mod)) {
        plot(1:30, as.numeric(mod[[x]]$ycoupon)[1:30], col = "chartreuse", type = "l")
        points(mod[[x]]$y)
        lines(mod[[x]]$y)
        lines(mod[[x]]$yhat, col = "red")
        title(paste(cCode, x))
        scan()
    }
}


# DEBUG
#!! check bond yields against bloomberg
checkBondYields <- function(cCode, getbb = FALSE) {
    cData <- get(paste(cCode, "data", sep = ""))
    #dates <- last(names(cData$actives), 10 * 260)
    dates <- names(cData$actives)
    par(mar = c(10, 3, 5, 2)   , mfrow = c(2, 1))
    lapply(dates, function(x) {
        dyn <- cData$dynOff[[x]][[1]]
        cf <- create_cashflows_matrix(dyn, include_price = TRUE)
        mats <- create_maturities_matrix(dyn, include_price = TRUE)
        yields <- ann_yields(cf, mats)
        yields <- yields[order(yields[, 1]), ]
        yields[, 2] <- yields[, 2] * 100
        isins <- rownames(yields) # get the isins from here rather than dyn$ISIN as otherwise wrong order
        prices <- dyn$PRICE[match(rownames(yields), dyn$ISIN)]
        bbcodes <- isin2bb(cCode, isins)
        bbyields <- as.numeric(cData$historicData$YLD_YTM_MID[x, bbcodes])
        ydiff <- (bbyields - yields[, 2]) * 100
        ydiffsd <- sd(ydiff)
        ydiffmean <- mean(ydiff)
        sdOutliers <- sapply(ydiff, function(x) (x - ydiffmean) / ydiffsd)
        ydiffmad <- mad(ydiff)
        ydiffmedian <- median(ydiff)
        madOutliers <- sapply(ydiff, function(x) (x - ydiffmedian) / ydiffmad)
        if(getbb == TRUE) {
            fdate <- format(dyn$TODAY, "%Y%m%d")
            getyields <- unlist(sapply(1:length(isins), function(x) bdp(conn, paste(isins[x], "Corp"), "yld_ytm_bid",
                c("px_disc_bid", "settle_dt"), c(prices[x], fdate))))
        }
        barplot(t(cbind(yields[, 2], bbyields, (if(getbb) getyields else NULL))), beside = TRUE, las = 2, 
            cex.names = 0.7)
        barplot(ydiff, beside = TRUE, las = 2, cex.names = 0.7)
        title(paste(x, cCode))
        if((max(madOutliers) > 10)) {
            flushprint(madOutliers)
            flushprint(sdOutliers)
            scd/an()
        }
    })
}


# DEBUG ####################################################################################################################################
oneCouponBond <- function(dynObj, bond, isin = TRUE) {
    cCode <- names(dynObj)
    if(isin == FALSE) bond <- bb2isin(cCode, bond)
    dynDo <- dynObj
    lapply(dynObj[[1]]$ISIN, function(x) {
        if (x != bond) dynDo <<- rm_bond(dynDo, cCode, x)
    })
    return(dynDo[[1]])
}



# DEBUG ####################################################################################################################################
ccc <- function(dynObj, browse = FALSE, cData = NULL) {
# check coupon consistency
# check accrued interest date fall from bberg data is the same as my calculation
    changeGood <- ""
    changeBad <- ""
    if(is.null(cData)) cData <- get(paste(names(dynObj[[1]]), "data", sep = ""))
    accrued <- cData$historicData$PX_DIRTY_BID - cData$historicData$PX_BID
    lapply(2:length(dynObj), function(x) {
        #flushprint(x)
        #if(x == 2698) browser()
        dyn <- dynObj[[x]][[1]]
        dynBack <- dynObj[[x - 1]][[1]]
        if(x < length(dynObj)) dynFwd <- dynObj[[x + 1]][[1]]
        consider <- intersect(dyn$ISIN, dynBack$ISIN) # which bonds are in both dyns
        accrued <- as.numeric(sapply(consider, function(x) dyn$ACCRUED[dyn$ISIN == x]))
        accruedBack <- as.numeric(sapply(consider, function(x) dynBack$ACCRUED[dynBack$ISIN == x]))
        accruedBack[accruedBack == 0] <- 0.00001 # avoid nans in next line
        accrued[accrued == 0] <- 0.00001 # avoid nans in next line
        accruedRatio <- accrued / accruedBack
        accruedDrops <- accruedRatio < 0.2 # if today's is less than 0.5 * yesterday
        couponsLength <- sapply(consider, function(x) sum(dyn$CASHFLOWS$ISIN == x))
        couponsLengthBack <- sapply(consider, function(x) sum(dynBack$CASHFLOWS$ISIN == x))
        couponsDrops <- couponsLength != couponsLengthBack
        if(any(accruedDrops)) {
            if((all.equal(as.logical(couponsDrops), accruedDrops)) != TRUE) {
                bad <- names(couponsDrops)[couponsDrops != accruedDrops]
                if (browse == TRUE) browser()
                changeBad <<- c(changeBad, dynBack$TODAY)
            } else changeGood <<- c(changeGood, dynBack$TODAY)
        }
    })
    return(list(good = as.Date(as.numeric(changeGood[-1])), bad = as.Date(as.numeric(changeBad[-1]))))
}


cccBadList <- function(cCodes) {
    badlist <- lapply(cCodes, function(x) {
        cData <- get(paste(x, "data", sep = ""))
        bad <- ccc(cData$dynBoth)$bad
        return(bad)
    })
    names(badlist) <- cCodes
    save(badlist, file = "data/badlist.dat")
}


    
# DEBUG ####################################################################################################################################
plotDatedCurve <- function(nsObj, startDate) {
# plot all the curves from a certain day
    nsObj <- nsObj[as.Date(names(nsObj)) >= as.Date(startDate)]
    lapply(names(nsObj), function(x) {
        plotObj <- nsObj[[x]]
        plot(plotObj$y)
        lines(plotObj$y)
        points(plotObj$yhat, col = "red")
        lines(plotObj$yhat, col = "red")
        abline(v=plotObj$startparam[4], col = "green")
        title(x)
        scan()
    })
}


# DEBUG ####################################################################################################################################
minDL <- function(dynObj) {
# this will find the optimum yerror minimizing lambda for a dynobj
    cCode <- names(last(dynObj)[[1]])
    counter <- 0
    mindls <- sapply(dynObj, function(x) {
        counter <<- counter + 1
        f <- function(dd) sum(estim_nss(x, cCode, method = "dl", lambda = dd)$yerrors[[cCode]][, 2]^2)
        xx <- tryCatch(optimize(f, interval = c(0, 3))$minimum, error = function() NA)
        flushprint(xx)
        flushprint(counter)
        return(xx)
    })
    return(mindls)
}

allMinDL <- function(cCodes, obj = "dynOn") {
# will do minDL for multiple countries
    allmin <- lapply(cCodes, function(cCode) {
        dynObj <- get(paste(cCode, "data", sep = ""))[[obj]]
        cmin <- minDL(dynObj)
        save(cmin, file = paste(cCode, obj, "dlmin.dat", sep = ""))
        return(cmin)
    })
    names(allmin) <- cCodes
    return(allmin)
}




testDL <- function(lambda, betas = c(0, 0, 1)) {
# see how the lambda paramater affects diebold li
    windows(6, 10)
    par(oma = c(3, 3, 5, 2))
    par(mfrow = c(3, 1))
    x <- seq(0.1, 30, by = 0.1)
    y <- spr_dl(betas, x, lambda)
    spl <- smooth.spline(x, y)
    p1 <- predict(spl, deriv = 1)
    p2 <- predict(spl, deriv = 2)
    plot(spl, main = "third term contribution")
    plot(p1, main = "first derivative")
    plot(p2, main = "second derivative")
    title(paste("curve for beta = (0, 0, 1) and lambda =", lambda), outer = TRUE)
    return(x[y == max(y)])
}

optDL <- function(dynObj, days = 260 * 3) {
# find the optimim lambda
# this will put peaking of lambda at years 1 to 30 and check which one minimizes the bond errors over days
    dlList <- lapply(1:30, function(x) {flushprint(x); lightns(dons(dynObj, days, meth = "dl", lambdayrs = x))})
    errs <- (sapply(1:length(dlList), function(y) sum(sapply(dlList[[y]], function(x) 
        sum((rev(decay(nrow(x$y), nrow(x$y)/6)) * (x$y[, 2] - x$yhat[, 2]))^2)))))
    barplot(errs)
    return(list(dlList, errs))
}
    

# DEBUG Diebold Li#########################################################################################################################
doDL <- function(cCode, DLs = 0.0609 * c(12, 6, 3, 2)) {
# do a whole bunch of different lambda DLs for FRdata$dynOn
    cData <- get(paste(cCode, "data", sep = ""))
    lapply(DLs, function(x) {
        lapply(last(cData$dynOn, 260 * 3), function(y) {
            flushprint(x)
            flushprint(y)
            tryCatch(estim_nss(y, "FR", method = "dl", lambda = x), error = function(ee) NA)
        })
    })
}


plotDL <- function(dls) {
# takes a dl object from the function above and plots the curves
    lapply(1:(length(dls[[1]])), function(x) {
        plot(dls[[1]][[x]]$y[[1]], lwd = 2)
        lines(dls[[1]][[x]]$yhat[[1]], col = addAlpha("darkgreen", 0.5))
        lines(dls[[2]][[x]]$yhat[[1]], col = addAlpha("darkred", 0.5))
        lines(dls[[3]][[x]]$yhat[[1]], col = addAlpha("darkgoldenrod", 0.5))
        lines(dls[[4]][[x]]$yhat[[1]], col = addAlpha("blue", 0.5))
        scan()
    })
}

rmseDL <- function(dls) {
# root mean squared error of the DLs
    lapply(dls, function(x) {
        sapply(x, function(y) {
            diffs <- y$yhat[[1]][, 2] - y$y[[1]][, 2]
            sqrt(sum((diffs ^ 2)) / length(diffs))
        })
    })
}



# DEBUG ####################################################################################################################################
pcCorrels <- function(cCodes) {
# will plot the correls of each factor
    pc <- getBetas(cCodes)
    for(x in 1:3) {
        windows(12, 12)
        par(mar = c(2, 2, 1, 1), oma = c(2, 2, 3, 2))
        par(mfcol = c(length(cCodes), length(cCodes)))
        for(y in cCodes) for (z in cCodes) 
            regress(pc[[y]][, x], pc[[z]][, x], xlab = cCodes[y], ylab = cCodes[z])
        title(paste("Beta", x), outer = TRUE)
    }
}

            
# DEBUG ####################################################################################################################################
extractBondCD <- function(nsObj, bond, plot = TRUE) {
    cheap <- sapply(names(nsObj), function(x) {
        obj <- nsObj[[x]]
        if(bond %in% rownames(obj$yhat)) (obj$yhat[bond, 2] - obj$y[bond, 2]) * 10000 else NA
    })
    cheap <- xts(cheap, order.by = as.Date(names(nsObj)))
    cheap <- last(cheap, 3 * 260)
    if(all(is.na(cheap))) plot(rnorm(100), col = "white") else
    if(plot) plot(na.omit(cheap), main = bond, minor.ticks = FALSE)
    return(cheap)
}
    
    
# MAINT ####################################################################################################################################
addAllCouponYields <- function(cCode) {
    flushprint(cCode)
    cData <- get(paste(cCode, "data", sep = ""))
    #cData$dl <- addCouponYields(cData$dl)
    #cData$ns <- addCouponYields(cData$ns)
    #cData$sv <- addCouponYields(cData$sv)
    #cData$dlBoth <- addCouponYields(cData$dlBoth)
    #cData$nsBoth <- addCouponYields(cData$nsBoth)
    #cData$svBoth <- addCouponYields(cData$svBoth)
    cData$csBoth <- addCouponYields(cData$csBoth)
    assign(paste(cCode, "data", sep = ""), cData, pos = 1) # put it into the global environment interim for couponBonds
    saveCountry(cCode)
}


fixBadDyn <- function(cCode, dateOverride = NA) {
# will see which dyn objects are bad from the ccc function and move their index back
    nsObjs <- c("dl", "ns", "sv", "dlOff", "nsOff", "svOff", "dlBoth", "nsBoth", "svBoth", "asvBoth", "csBoth", "dynOn", "dynOff", "dynBoth")
    cData <- get(paste(cCode, "data", sep = ""))
    if(is.na(dateOverride)) bad <- as.character(ccc(cData$dynBoth)$bad) else bad <- as.character(dateOverride)
    flushprint("bad:")
    flushprint(bad)
    for(x in nsObjs) {
        flushprint(x)
        idx <- index(cData[[x]])[names(cData[[x]]) %in% bad]
        flushprint(idx)
        idxg <- sapply(idx, function(i) {z <- i; while(names(cData[[x]])[z] %in% bad) z <- z - 1; z}) # good idx
        for(y in index(idx)) {
            cData[[x]][idx[y]] <- cData[[x]][idxg[y]]
        }
    }
    assign(paste(cCode, "data", sep = ""), cData, pos = 1) # put it into the global environment
}

fixBadStatic <- function(cCode, badDates) {
# rows in all the data frames of historicData that correspond to badDates, are replaced with the first date after badDates
    cData <- get(paste(cCode, "data", sep = ""))
    dframenames <- names(cData$historicData) # PX_BID, PX_ASK etc
    gd <- index(cData$historicData[[dframenames[1]]][paste(max(badDates), "/", sep = "")])[2] # get the first good date
    for(dfn in dframenames) {
        for (bd in badDates) {
            if(dfn == "YLD_YTM_MID") browser()
            cData$historicData[[dfn]][as.Date(bd), ] <- cData$historicData[[dfn]][gd, ]
        }
    }
    assign(paste(cCode, "data", sep = ""), cData, pos = 1) # put it into the global environment
}

killsome <- function(cCode, numkill) {
# kill a certain number of dates from the back
    nsObjs <- c("actives", "offruns", "dl", "ns", "sv", "dlOff", "nsOff", "svOff", 
                "dlBoth", "nsBoth", "asvBoth", "svBoth", "csBoth", "dynOn", "dynOff", "dynBoth")
    cData <- get(paste(cCode, "data", sep = ""))
    for(x in nsObjs) {
        flushprint(x)
        cData[[x]] <- cData[[x]][1:(length(cData[[x]]) - numkill)]
    }
    cData$historicData <- lapply(cData$historicData, function(x) x[1:(nrow(x) - numkill), ])
    assign(paste(cCode, "data", sep = ""), cData, pos = 1) # put it into the global environment
}

# PRESENTATION #############################################################################################################################
# PHIL #####################################################################################################################################


plotNSpars <- function(cCode, howmany = 1500) {
# plots the NS parameters separately for a country
    mat <- 2:30
    cData <- get(paste(cCode, "data", sep = ""))
    nspars <- t(sapply(last(cData$nsBoth, howmany), function(x) x$startparam))
    dates <- last(names(cData$actives), howmany)
    plot.new()
    par(mfrow = c(2, 2))
    par(oma = c(1, 1, 1, 1))
    par(mar = c(1, 1, 4, 2))
    lapply(1:length(nspars), function(y) {
        x <- nspars[y, ]
        c1 <- spr_ns(c(x[1], 0, 0, x[4]), mat)
        c2 <- spr_ns(c(0, x[2], 0, x[4]), mat)
        c3 <- spr_ns(c(0, 0, x[3], x[4]), mat)
        c4 <- c1 + c3 + c3
        ylims <- c(min(c(c1, c2, c3, c4)), max(c(c1, c2, c3, c4)))
        plot(mat, c1, type = "l", col = "red")
        title("level", line = 1)
        plot(mat, c2, type = "l", col = "blue")
        title("slope", line = 1)
        plot(mat, c3, type = "l", col = "chartreuse2")
        title("curvature", line = 1)
        plot(mat, c4, ylim = ylims, type = "l", lwd = 2)
        lines(mat, c1, col = "red")
        lines(mat, c2, col = "blue")
        lines(mat, c3, col = "chartreuse2")
        title("combined", line = 1)
        title(paste(cCode, ": ", dates[y], sep = ""), outer = TRUE)
        scan()
    })
}
        

yieldFit <- function(onensObj) {
    setwd("../sagb")
    source("sagb.r")
    bNStau <- seq(48, 120, by = 12)
    maturities <- as.numeric(onensObj$y[, 1]) * 12
    yields <- as.numeric(onensObj$y[, 2]) * 100
    ss <- Nelson.Siegel(yields, maturities, bNStau)
    plot(maturities / 12, yields, xlab = "maturity", ylab = "yield")
    lines(1:35, NSrates(as.numeric(ss[1:3]), as.numeric(ss[4]), 1:35 * 12), col = "red")
    setwd("../gyields")
    return(ss)
}


videoCurves <- function(cCodes = c("DE", "IT", "AT", "SP", "FR"), howmany = 1500) {
    countryColours <- list(IT = "green2", SP = "orange2", AT = "red2", DE = "grey25", FR = "blue2")
    lims <- lapply(cCodes, function(x) {
        cData <- get(paste(x, "data", sep = ""))
        minn <- min(unlist(lapply(cData$svBoth, function(x) x$yhat[, 2])))
        maxx <- max(unlist(lapply(cData$svBoth, function(x) x$yhat[, 2])))
        return(c(minn, maxx))
    })
    minn <- min(sapply(lims, function(x) x[1]))
    maxx <- max(sapply(lims, function(x) x[2]))
    lims <- c(minn, maxx)
    cDatas <- lapply(cCodes, function(x) return(get(paste(x, "data", sep = ""))))
    names(cDatas) <- cCodes
    lapply((length(cDatas[[1]]$actives) - howmany):length(cDatas[[1]]$actives), function(x) {
        plot(cDatas[[1]]$svBoth[[x]]$y, ylim = lims)
        lapply(cCodes, function(y) {
            lines(cDatas[[y]]$svBoth[[x]]$yhat, lwd = 3, col = countryColours[[y]])
            points(cDatas[[y]]$svBoth[[x]]$y, cex = 1.5, pch = 21, col = countryColours[[y]], bg = addAlpha("white", 0.5))
        })
        title(names(cDatas[[1]]$actives)[x])
        legend("bottomright", cCodes, col = sapply(cCodes, function(i) countryColours[[i]]), lwd = 2)
    })
}


videoPCAs <- function(cCodes = ac, rollperiod = 15, usegdp = TRUE, years = 3) {
#videos the PCA
    p <- getPCAs(cCodes, 260 * years)[[1]][, 1:length(cCodes)] # first principal component
    if(usegdp) colweight <- unlist(gdp2012[cCodes]) else colweight <- rep(1, length(cCodes))
    cols <- unlist(cColors[cCodes])
    rollapply(p, rollperiod, function(x) {
        pc <- PCA(x, graph = FALSE, col.w = colweight)
        plot.PCA(pc, choix = "var", title = last(index(x)), col.var = cols)
    }, by.column = FALSE)
}

        
######################### MESS AROUND #############################

getcs <- function() {
    xx <- lapply(ac, function(x) {
        flushprint(x)
        cc <- get(paste(x, "data", sep = ""))
        cs <- lapply(cc$dynBoth, function(y) tryCatch(estim_cs(y, x), error = function(ee) NA))
        return(cs)
    })
    return(xx)
}

frontcurves <- function(years = 5, model = "auto", type = c("coupon", "zero", "fwd"), tenors = 2:7) {
#draw curve curvatures in front end
    ac <- ac[1:4]
    ay <- allYields(ac, days = years * 260, mats = tenors, model = model)
    flats <- lapply(ac, function(z) t(apply(ay[[z]], 1, function(x) approx(c(min(tenors), max(tenors)), 
             c(x[1], x[length(tenors)]), tenors)$y))) # interpolate a traight line
    names(flats) <- ac
    means <- lapply(ac, function(x) apply(ay[[x]], 1, mean))
    names(means) <- ac
    diffs <- lapply(ac, function(x) (ay[[x]] - flats[[x]]) * 10000)
    mins <- min(sapply(diffs, min))
    maxs <- max(sapply(diffs, max))
    names(diffs) <- ac
    windows(10, 12)
    par(mfrow = c(2, 2))
    lapply(ac, function(x) {
        plot(tenors, as.numeric(last(diffs[[x]])), col = "white", ylim = c(mins, maxs), xlab = "", ylab = "")
        apply(diffs[[x]], 1, function(y) lines(tenors, y, col = addAlpha("grey", 0.15)))
        apply(last(diffs[[x]], 260), 1, function(y) lines(tenors, y, col = addAlpha("steelblue2", 0.3/years)))
        apply(last(diffs[[x]], 65), 1, function(y) lines(tenors, y, col = addAlpha("goldenrod2", 0.15)))
        lines(tenors, last(diffs[[x]]), lwd = 2)
        title(x)
        abline(h = 0, lty = "dashed", col = addAlpha("black", 0.5))
        if(x == "DE") {
            legend("topright", c("now", "past 3 months", "past year", paste("past", years, "years")), col = 
                   c("black", colours()[149], colours()[617], "grey"), lwd = 2)
        }
    })
    windows(8, 5)
    par(mfrow = c(1, 2))
    plot(tenors, ay[[1]][300, ] * 100, xlab = "", ylab = "", type = "l")
    points(tenors, ay[[1]][300, ] * 100)
    lines(tenors, flats[[1]][300, ] * 100, lty = "dashed")
    title("Typical curve is convex in the front end \nwhen upward sloping\n(DE 4 years ago)", cex.main = 0.8, font.main = 3)
    plot(tenors, last(ay[[1]]) * 100, xlab = "", ylab = "", type = "l")
    points(tenors, last(ay[[1]]) * 100)
    lines(tenors, flats[[1]][nrow(flats[[1]]), ] * 100, lty = "dashed")
    title("Current curves are concave due to \nLTRO and carry/slide trades\n(DE today)", cex.main = 0.8, font.main = 3)
}

fwdVcoup <- function(cCode, model = keymodel[[cCode]], withlegend = FALSE) {
# draw a hair chart of the fwd curve, compare it to spot curve max, see how much yield we can get
    nsobj <- get(paste(cCode, "data", sep = ""))[[model]]
    dcoup <- couponYields(nsobj) * 100
    dfwd <- fwdYields(nsobj) * 100
    dzero <- zeroYields(nsobj) * 100
    years = as.numeric(colnames(dfwd))
    hair(dfwd, withlegend = withlegend, title = paste(cCode, "forward rates"))
    maxlastfwd <- max(last(dfwd))
    maxlastmat <- match(maxlastfwd, last(dfwd)) # maturity of max
    abline(h = maxlastfwd, lty = "dashed", col = "red")
    text(mean(years), maxlastfwd, paste("highest forward rate ", round(maxlastfwd, 2), " at ", round(maxlastmat), "y", sep = ""), 
         pos = 3, cex = 0.8, col = "red")
    # now plot coupon curve
    lines(as.numeric(colnames(dcoup)), as.numeric(last(dcoup)), lwd = 2)
    maxlastcoup <- max(last(dcoup))
    maxlastmat <- match(maxlastcoup, last(dcoup)) # maturity of max
    abline(h = maxlastcoup, lty = "dashed", col = "black")
    text(mean(years), maxlastcoup, paste("highest coupon rate ", round(maxlastcoup, 2), " at ", 
                                        round(maxlastmat), "y", sep = ""), 
         pos = 3, cex = 0.8, col = "black")
}


fwdVcoupRegress <- function(cCode, model = keymodel[[cCode]], usepca = FALSE) {
# draw a hair chart of the fwd curve, compare it to spot curve max, see how much yield we can get
    nsobj <- get(paste(cCode, "data", sep = ""))[[model]]
    dcoup <- couponYields(nsobj) * 100
    dfwd <- fwdYields(nsobj) * 100
    dzero <- zeroYields(nsobj) * 100
    eigvecs <- eigen(cor(dcoup[, keymats]))$vectors
    for(i in 1:ncol(eigvecs)) { # orient the pcs correctly using the last loading must be positive
        if(last(eigvecs[, i]) < 0) eigvecs[, i] <- -eigvecs[, i]
    }
    if(last(eigvecs[, 3]) > 0) eigvecs[, 3] <- -eigvecs[, 3] # but PC3 must have belly positive
    pcs <- (dcoup[, keymats] %*% eigvecs)
    for(i in 1:ncol(eigvecs)) pcs[, i] <- pcs[, i] / sum(abs(eigvecs[, i])) # normalise to yields
    years = as.numeric(colnames(dfwd))
    topfwd <- apply(dfwd, 1, max)
    topcoup <- apply(dcoup, 1, max)
    if(usepca) {
        regress(pcs[, 1], topfwd, main = paste(cCode), xlab = "Market yield (first principal component)", 
                ylab = "maximum forward yield")#
    } else {
        regress(topcoup, topfwd, main = paste(cCode), xlab = "maximum coupon yield", ylab = "maximum forward yield")
    }
}

runningYield <- function(cCode, model = keymodel[[cCode]], ddate = NA, priod = 1/12, plotit = TRUE)  {
# duration, modified duration, and DV01 of each bond for a country
    cData <- get(paste(cCode, "data", sep = ""))
    dfwd <- fwdYields(cData[[model]]) 
    dzero <- zeroYields(cData[[model]]) 
    if(is.na(ddate)) obj <- last(cData$dynBoth)[[1]] else obj <- cData$dynBoth[[as.character(ddate)]]
    cf <- create_cashflows_matrix(obj[[1]], include_price = FALSE)   
    cfp <- create_cashflows_matrix(obj[[1]], include_price = TRUE)   
    m <- create_maturities_matrix(obj[[1]], include_price = FALSE)
    mp <- create_maturities_matrix(obj[[1]], include_price = TRUE)
    # now we must kill bonds that pay a coupon within priod
    shortcoups <- m[1, ] <= priod
    m <- m[, !shortcoups]
    cf <- cf[, !shortcoups]
    fwdatm <- apply(m, 2, function(x) 
        approx(as.numeric(colnames(dfwd)), as.numeric(last(dfwd)), x, rule = 2)$y)
    weightedfwd <- sapply(1:ncol(fwdatm), function(i) sum(cf[, i] * fwdatm[, i]) / sum(cf[, i])) * 100
    modelcode <- strsplit(model, "B")[[1]][1] # get the model code
    beta <- last(cData[[model]])[[1]]$startparam
    pricesnow <- bond_prices(method = modelcode, beta = beta, m = m, cf = cf)$bond_prices
    priceslater <- bond_prices(method = modelcode, beta = beta, m = m - priod, cf = cf)$bond_prices
    pricediff <- priceslater - pricesnow
    priceret <- (((pricediff / pricesnow) + 1) ^ (1 / priod) - 1) * 100
    priodret <- (pricediff / pricesnow) * 10000 # monthlybps
    coupons <- sapply(names(pricesnow), function(x) cData$staticData[cData$staticData$ID_ISIN == x, "COUPON"])
    runningyield <- (coupons / pricesnow) * 100
    durations <- bdur(cCode)[names(priceret), ]
    priceret <- priceret[order(durations[, "mat"])]
    priodret <- priodret[order(durations[, "mat"])]
    runningyield <- runningyield[order(durations[, "mat"])]
    weightedfwd <- weightedfwd[order(durations[, "mat"])]
    # only after everything is sorted by maturity, then we can sort the matations matrix itself
    durations <- durations[order(durations[, "mat"]), ]
    labels <- isinLabel(cCode,names(priceret))
    par(mar = c(6, 4, 4, 3))
    if(plotit) {
        bfwds <- approx(as.numeric(colnames(dfwd)), as.numeric(last(dfwd)), durations[, "mat"])$y * 100
        b <- barplot(priceret, names.arg = labels, las = 2, cex.names = 0.8, 
                main = paste(cCode, "returns next", round(365 * priod), "days, assuming curve unchanged"), 
                cex.main = 1, ylim = c(min(c(0, bfwds)), max(bfwds)), ylab = "annualised return")
        abline(h = axTicks(2), col = "lightgrey", lty = "dashed")
        points(b, bfwds, cex = 1, pch = 19, col = "red")
        bondYields <- bond_yields(cfp, mp)
        bondYields <- bondYields[names(priceret), ] * 100
        points(b, bondYields[, "Yield"], cex = 1, pch = 19, col = "green")
        text(x = b, y = sapply(priceret, function(x) max(0, x)) + par("cxy")[2] / 2, labels = round(priceret, 2), cex = 0.6)
        #points(b, runningyield, cex = 1, pch = 19, col = "dodgerblue") # uncomment to see cashflow maturity weigthed fwd yield
        #legend("topleft", fill = c("grey", "red", "green", "dodgerblue"), 
        #    legend = c("expected return", "forwards", "YTM", "running yield"))
        legend("topleft", fill = c("grey", "red", "green"), 
            legend = c("expected return", "forwards", "YTM"))
        #points(b, weightedfwd, cex = 1, pch = 19, col = "blue") # uncomment to see cashflow maturity weigthed fwd yield
    } else {
        return (priceret/durations[, "mdur"])
    }
}


runningYieldMap <- function(cCode, priceret = runningYield(cCode, plotit = FALSE, priod = 1/12)) {
    names(priceret) <- isinLabel(cCode, names(priceret))
    retmat <- sapply(priceret, function(x) x - priceret) * 100
    retmelt <- melt(retmat)
    colnames(retmelt) <- c("Sell", "Buy", "value")
    retmelt$Buy <- factor(retmelt$Buy, levels = rownames(retmat))
    retmelt$Sell <- factor(retmelt$Sell, levels = rownames(retmat))
    heat <- ggplot(retmelt, aes(x = Buy, y = Sell)) + geom_tile(aes(fill = value))
    #heat <- heat + scale_fill_gradientn(colours = brewer.pal(7, "PiYG"))
    heat <- heat + scale_fill_gradientn(colours = c(rev(brewer.pal(7, "Reds"))[-1], brewer.pal(7, "Blues")[-7]))
    heat <- heat + geom_text(data = retmelt, aes(x = Buy, y = Sell, label = round(value)), size = 2.5, colour = "black")
    heat <- heat + theme(legend.position = "none")
    heat <- heat + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))    
    heat <- heat + ggtitle(paste(cCode, "\nBasis point pickup per unit of duration"))
   return(heat)
}


bondlowess <- function(cCode, years = 6, bonds, mdur = TRUE, fnum = 0.2, plotem = TRUE) {
# will do lowess on the curve for years, and see what the exact mdur or maturity point equivalents did historically
    factors <- get(paste(cCode, "factors", sep = ""))
    factbydate <- split(factors, factors$date) # split so each date has its own list
    dates <- wdaylist(Sys.Date() - years * 260)
    cydurs <- lapply(dates, function(x) cydur(cCode, x)) # get a list of all the cyDurs
    bd <- bdur(cCode)
    bdurs <- bd[bonds, "mdur"] # mduration of bonds in question
    bmats <- bd[bonds, "mat"] # maturity of bonds in question
    duryields <- t(sapply(cydurs, function(x) approx(x[, "mdur"], x[, "annyield"], bdurs)$y))
    duryields <- xts(duryields, order.by = dates)
    cy <- couponYields(cCode)
    cy <- last(cy, length(duryields))
    matyields <- t(sapply(cydurs, function(x) approx(x[, "mat"], x[, "annyield"], bmats)$y))
    matyields <- xts(matyields, order.by = dates)
    db <- duryields[, 1] - 2* duryields[, 2] + duryields[, 3]
    mb <- matyields[, 1] - 2* matyields[, 2] + matyields[, 3]
    for (ff in 1:10) {
        dev.new()
        ylowess <- lapply(cydurs, function(x) lowess(x[, "mdur"], x[, "annyield"], f = 1/ff))
        equiyields <- t(sapply(ylowess, function(x) approx(x$x, x$y, bdurs)$y))
        plot((equiyields[, 1] - 2* equiyields[, 2] + equiyields[, 3]) * 10000, main = ff)
    }
}

frontloadings <- function(cCode = "DE", scaleunit = FALSE, peiod = "quarters", whichloading = 1, userets = TRUE, 
                          startyear = "2005") {
    if(cCode == "IRS") cy <- eurirs[, 2:10] else if(cCode %in% em) cy <- emRates(cCode, 260 * 9)[, -1]
        else cy <- couponYields(cCode)[, 2:10]
    if(userets) cyr <- diffret(cy) else cyr <- cy
    cyr <- cyr[paste(startyear, "/", sep = "")]
    pcs <- period.apply(cyr, endpoints(cyr, peiod), function(x) princomp(x, cor = scaleunit)$loadings[, whichloading])
    pcs <- fixEigen(pcs)
    bb <- barplot(as.matrix(pcs), beside = TRUE, space = c(0, 3), col = c(rep("grey", nrow(pcs) - 1), "red"), border = "white", 
            width = c(rep(1, nrow(pcs) - 1), 2))
    title(main = paste(cCode, " evolution of loadings by maturity in PC", whichloading, " (", 
                       ifelse(scaleunit, "scaled", "unscaled"), 
                       ifelse(userets, " PCA using arithmetic return, last ", " PCA using outrights levels, last "), 
                       paste(nrow(pcs) - 1, "quarters"), ")", 
                       sep = ""), 
                       font.main = 3, col.main = "grey30")
}


fixEigen <- function(PCAweights) {
# this makes sure that the axis of the eigenvector is correctly oriented in rollapply PCA
    weightsMatrix <- as.matrix(PCAweights)
    n <- dim(weightsMatrix)[1]
    delta1 <- apply(abs(weightsMatrix[-1, ] - weightsMatrix[-n, ]), 1, sum)
    delta2 <- apply(abs(weightsMatrix[-1, ] + weightsMatrix[-n, ]), 1, sum)
    signs <- c(1, cumprod(rep(-1, n-1) ^ (delta2 <= delta1)))
    newWeights <- PCAweights * signs
    if(mean(last(newWeights)) < 0) newWeights <- -newWeights # reverse polarity if negative last weights (sgd)
    return(newWeights)
}


allbonds <- function(cCodes = ac, live = TRUE, isin = TRUE) {
    bonds <- sapply(cCodes, function(x) {
        cData <- get(paste(x, "data", sep = ""))
        if(live) bb <- c(last(cData$actives)[[1]], last(cData$offruns)[[1]]) else bb <- unique(unlist(cData$actives))
        if(isin) bb <- bb2isin(x, bb)
        return(bb)
    })
    return(bonds)
}


allBondYields <- function(cCodes = ac, separate = TRUE, mindata = 130, maxdata = 260, field = "y") {
# returns all yields of all bonds in countries, 
# mindata specifies whether or not to exclude bonds with less data than mindata
# maxdata cuts off past maxdata days
# separateCountries 
    bondData <- lapply(cCodes, function(cCode) {
        cfactors <- get(paste(cCode, "factors", sep = ""))
        splitbybond <- split(cfactors, cfactors$bond)
        bondlist <- lapply(splitbybond, function(xx) xts(xx[, field], order.by = as.Date(xx[, "date"])))
        if(!is.na(maxdata)) bondlist <- lapply(bondlist, last, maxdata)
        numofeach <- sapply(bondlist, nrow) # number of each bond
        if(!is.na(mindata)) bondlist <- bondlist[numofeach >= mindata]
        return(bondlist)
    })
    # now have a list of lists of bonds
    if(is.na(maxdata)) {
        maxdata <- max(unlist(sapply(bondData, function(x) sapply(x, function(y) length(y))))) # maximum number of rows
    }
    #now find the best xts index, that is, the one with a full maxdata length because we'll need it later to reconvert to xts
    xtsindex <- index(bondData[[1]][sapply(bondData[[1]], nrow) == maxdata][[1]])
    # now we format them with NAs and cut them if necessary
    filteredbondData <- lapply(bondData, function(x) {
        filterCountry <- sapply(x, function(y) c(rep(NA, maxdata - length(y)), y))
        filterCountry <- xts(filterCountry, order.by = xtsindex)
        return(filterCountry)
    })
    if(!separate) {
        filteredbondData <- do.call(cbind, filteredbondData)
    } else names(filteredbondData) <- cCodes
    if(("list" %in% class(filteredbondData)) & (length(filteredbondData) == 1)) filteredbondData <- filteredbondData[[1]]
    return(filteredbondData)
}


bondsForSpread <- function(thespread, thebonds, numinstr, maxconsider = 20) {
# finds the optimal vectors in thebonds matrix to match thespread
    thebonds <- na.omit(thebonds)
    thebonds <- last(thebonds, length(thespread)) # all same length
    thespread <- last(thespread, nrow(thebonds)) # and reciprocal
    regsub <- regsubsets(thespread ~ ., data = thebonds, nvmax = numinstr, nbest = maxconsider, 
                really.big = ifelse(ncol(thebonds) > 50, TRUE, FALSE))
    whicher <- summary(regsub)$which[, -1]
    whicher <- whicher[rownames(whicher) == numinstr, ] 
    linmods <- apply(whicher, 1, function(x) lm(thespread ~ ., data = thebonds[, x]))
    weighter <- sapply(linmods, function(x) coef(x)[-1])
    # now we return the trade objects
    return(sapply(1:maxconsider, function(x) {
        weightedspread <- thebonds[, whicher[x, ]] %*% weighter[, x]
        integerweights <- round(weighter[, x] / min(abs(weighter[, x])))
        iweightedspread <- thebonds[, whicher[x, ]] %*% integerweights
        individualyields <- thebonds[, whicher[x, ]]
        roundedweights <- round(weighter[, x] / min(abs(weighter[, x])), 2)
        labels <- sapply(colnames(whicher)[whicher[x, ]], function(ww) makelabels(NA, ww, fromisin = TRUE))
        thelm <- linmods[[x]]
        testlm <- lm(thespread ~ weightedspread)
        theilm <- lm(thespread ~ iweightedspread)
        thersq <- summary(thelm)$r.squared
        theirsq <- summary(theilm)$r.squared
        invlm <- lm(as.numeric(weightedspread) ~ as.numeric(thespread))
        invse <- last(invlm$residuals) / sd(invlm$residuals)
        these <- last(thelm$residuals) / sd(thelm$residuals)
        theise <- last(theilm$residuals) / sd(theilm$residuals)
        isins <- colnames(whicher)[whicher[x, ]]
        return(list(isins = isins, targetspread = thespread, 
                inputspread = thespread, weightedspread = weightedspread, integerspread = iweightedspread, 
                weights = weighter[, x], integerweights = integerweights, roundedweights = roundedweights, labels = labels, 
                thelm = thelm, theilm = theilm, testlm = testlm, rsq = thersq,
                irsq = theirsq, se = these, ise = theise, invse = invse))
    }))
}

doBoxForSpread <- function(bfsobj, years = 1) {
    if("matrix" %in% class(bfsobj)) {       # more than one sent
        apply(bfsobj, 2, function(x) {
            dobox(x$isins, x$roundedweights, regressions = FALSE, years = years)
            dev.new()
            regress(genseries(x$weightedspread, FALSE), genseries(x$inputspread, FALSE))
            readline()


        })
    } else {
        dobox(bfsobj$isins, bfsobj$roundedweights, regressions = FALSE, years = years)
    }
}


mongotest <- function(cCodes = ac, matured = FALSE) {
    allbonds <- sapply(cCodes, function(x) {
        cData <- get(paste(x, "data", sep = ""))
        return(bb2isin(x, c(last(cData$actives)[[1]], last(cData$offruns)[[1]])))
    })
    allbonds <- na.omit(unlist(allbonds))
    bdps <- plot(bdp(conn, paste(allbonds, "Corp"), "LAST_PRICE"))
    ms <- bbmongolatest()[allbonds, ]
    browser()
}



philkick <- function(countries = ac, save = TRUE) {
# start off every day function. loads, updates, saves each country
    if(!exists("conn")) gobb()
    getCalendars(countries)
    lapply(countries, function(x) {
        flushprint(paste("Doing", x))
        loadCountry(x)
        upCountry(x, minmat = ifelse(x == "IT", 1.90, 2))
        checkIntegrity(x)
        if(save) saveCountry(x)
    })
    getAllBondFactors(cl)
    load("trades.dat", envir = globalenv())
    if(useBB) {
        eurirs <<- bbdh(eurIRStickers, 10)
    } else {
        eurirs <<- mmdh(eurIRStickers, 10)
    }
}

plotSven <- function(nsobj, mats = 1:30) {
# this will plot the yield curve and its component functions
# for now only implented for svensson and adjusted svensson as otherwise must change beta vector parameters
    svens <- getSven(nsobj)
    meth <- nsobj[[1]]$method
    svenfun <- switch(meth,
                      "dl" = spr_dl,
                      "ns" = spr_ns,
                      "sv" = spr_sv,
                      "asv" = spr_asv)
    numbetas <- switch(meth,
                      "dl" = 3,
                      "ns" = 3,
                      "sv" = 4, 
                      "asv" = 4)
    cb0 <- t(apply(svens, 1, function(x) svenfun(c(x[1], 0, 0, x[4], 0, x[6]), mats)))
    cb1 <- t(apply(svens, 1, function(x) svenfun(c(0, x[2], 0, x[4], 0, x[6]), mats)))
    cb2 <- t(apply(svens, 1, function(x) svenfun(c(0, 0, x[3], x[4], 0, x[6]), mats)))
    cb3 <- t(apply(svens, 1, function(x) svenfun(c(0, 0, 0, x[4], x[5], x[6]), mats)))
    cball <- t(apply(svens, 1, function(x) svenfun(x, mats)))
    cblist <- list(cb0 = cb0, cb1 = cb1, cb2 = cb2, cb3 = cb3, cball = cball)
    cbmelt = melt(cblist)
    cblist <- lapply(cblist, function(x) as.xts(x, order.by = as.Date(rownames(x))))
    days <- as.Date(names(nsobj))
    windows(12, 10)
    par(mfrow = c(5, 5))
    par(mar = c(2, 2, 2, 2))
    while(length(days) > 0) {
        nowdays <- last(days, 25)
        trainingset <- sapply(cblist, function(x) x[nowdays, ])
        ylims <- c(min(trainingset), max(trainingset))  
        for (d in nowdays) {
            dd <- as.Date(d)
            plot(mats, cblist$cball[dd, ], ylim = ylims)
            lines(mats, cblist$cb0[dd, ], col = "blue")
            lines(mats, cblist$cb1[dd, ], col = "red")
            lines(mats, cblist$cb2[dd, ], col = "green")
            lines(mats, cblist$cb3[dd, ], col = "darkgoldenrod3")
            abline(v = svens[as.character(dd), "tau1"], col = "green", lty = "dashed")
            abline(v = svens[as.character(dd), "tau2"], col = "darkgoldenrod3", lty = "dashed")
            title(paste(meth, format(dd, "%d %b %Y")))
        }
        readLines(n = 1) # wait for keyboard input
        days <- as.Date(setdiff(days, nowdays))
    }
}


quantilesOfPCs <- function(vecseries) {
# will take a matrix of vectors. Will find first 3 pcs. Then will regress each vector against the PCs to find it's cheap dear
# then will take the quantiles of these and polot them
      period.apply(vecseries, endpoints(vecseries, "years"), function(x) {
      eigen(cor(x))$vectors[, 1:3] -> eigenvecs
      apply(eigenvecs, 2, function(y) y %*% t(x)) -> pc13
      apply(x, 2, function(y) lm(y ~ pc13)$residuals) -> resids
      apply(resids, 2, function(y) quantile(y, seq(0, 1, 0.05))) -> quantiles
      dev.new()
      plot(quantiles[1, ], ylim = c(min(quantiles), max(quantiles)))
      apply(quantiles, 1, lines)
    })
}


makeJoris <- function(cCode, settleplus = c(1, 5, 7, 10, 20, 31, 91, 182, 365),
                      pricepercent = c(-25, -10, -5, -2, -1, 0, 1, 2, 5, 10, 25)) {
# this will make a list for Joris 
    isins <- allbonds(cCode)
    cData <- get(paste(cCode, "data", sep = ""))
    allscenes <- lapply(isins, function(isin) {
        label <- makelabels(cCode, isin, TRUE)$labels
        bbcode <- isin2bb(cCode, isin)
        cashflows <- cData$cfData[[bbcode]]
        # now get starting price
        currentprice <- bdp(conn, paste(isin, "Corp"), "px_bid")
        currentyield <- bdp(conn, paste(isin, "Corp"), "yld_ytm_bid")
        pricerange <- round(as.numeric(currentprice) * (1 + pricepercent / 100), 3)
        currentsettle <- as.Date(as.character(bdp(conn, paste(isin, "Corp"), "settle_dt")))
        settlerange <- currentsettle + settleplus
        scenario <- lapply(pricerange, function(p) {
            ss <- sapply(settlerange, function(d) {
                bdp(conn, paste(isin, "Corp"), "yld_ytm_bid", c("settle_dt", "px_bid"), c(format(d, "%Y%m%d"), as.character(p)))
            })
            names(ss) <- settlerange
            return(ss)
        })
        names(scenario) <- pricerange
        return(list(label = label, isin = isin, cashflows = cashflows, currentprice = currentprice, currentyieldtomat = currentyield, 
                    currentsettle = currentsettle, scen_pricerange = pricerange, scen_settlerange = settlerange, scenario = scenario))
    })
    names(allscenes) <- isins
    return(allscenes)
}


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!/ BETA /################################################
eigenHedge <- function(eigenvecs, hedgerows, mainrow, numpcs = length(hedgerows)) {
# this will give the hedge ratios of the hedgerows assuming 1 in the main row
    aa <- eigenvecs[hedgerows, 1:numpcs]
    bb <- eigenvecs[mainrow, 1:numpcs]
    hedgeratios <- solve(t(aa), bb) # linear equation solving
    return(hedgeratios)
}


eigenBell <- function(cCode, bonds, usereturns = FALSE, years = 3, model = keymodel[[cCode]], usemats = keymats, actualbonds = FALSE, 
                      scale.unit = FALSE) {
    if(length(bonds) != 3) {
        flushprint ("must have 3 bonds")
        return(-1)
    }
    cData <- get(paste(cCode, "data", sep = ""))
    genyields <- couponYields(cData[[model]], mats = 1:35)
    genyields <- last(genyields, years * 260)
    dur <- bdur(cCode) # get the durations
    bondmats <- dur[bonds, "mat"]
    bondgenyields <- t(apply(genyields, 1, function(x) approx(1:35, x, bondmats)$y))
    bondgenyields <- xts(bondgenyields, order.by = index(genyields))
    bondactyields <- last(allBondYields(cCode, field = "y")[, bonds], years * 260)
    if(actualbonds) {
        genyields <- last(genyields, nrow(na.omit(bondactyields)))
        bondgenyields <- last(bondgenyields, nrow(na.omit(bondactyields)))
        bondactyields <- na.omit(bondactyields)
        completeyields <- xts(cbind(as.matrix(genyields[, usemats]), as.matrix(bondactyields)), order.by = index(genyields))
    } else {
        completeyields <- xts(cbind(as.matrix(genyields[, usemats]), as.matrix(bondgenyields)), order.by = index(genyields))
    }
    completeyields <- last(completeyields, years * 260) # whatever using bonds or not, limit to 260
    if(usereturns) completeyields <- diffret(completeyields)
    if(scale.unit) corfun <- cor else corfun <- cov
    lenmats <- length(usemats)
    eigs <- eigen(corfun(completeyields))$vectors
    eigs[, 2] <- - eigs[, 2]
    #hedges <- eigenHedge(eigs, hedgerows = lenmats + 3, mainrow = lenmats + 2, numpcs = 1)
    hedges <- eigenHedge(eigs, hedgerows = c(lenmats + 1, lenmats + 3), mainrow = lenmats + 2)
    if(hedges[1] * hedges[2] < 0) {
        browser()
        flushprint("ERROR: opposite sign hedges")
        #return(-1)
    }
    hedges <- - abs(hedges) # convert to negative
    hedges <- c(hedges[1], 1, hedges[2])
    genfly <- bondgenyields %*% hedges
    actfly <- bondactyields %*% hedges
    intgenfly <- bondgenyields %*% c(-0.5, 1, -0.5)
    intactfly <- bondactyields %*% c(-0.5, 1, -0.5)
    pcs <- completeyields %*% eigs
    return(list(hedges = hedges, genfly = genfly, actfly = actfly, intgenfly = intgenfly, intactfly = intactfly, 
                pcs = pcs, eigs = eigs, genyields = genyields))
}

triprep <- function(isins = as.character(unlist(sapply(ac, allBonds))),
                    startdate = as.Date("2013-01-01"), calendar = "TE") {
# Will get all the necessary bond data for creation of total return indices by the tri function
# Get bond clean and dirty prices, get coupon payment dates, then return dirty price and coupon payment matrices
    localmin <- function(xtsin, guessdate, checkbounds = 5, whichcol = 1)  {
    # this will find accrual date minimum. It will check for minima up to checbounds dates around it. 
    # this is used to reconcile accrued zero-drop dates with coupon cashflow dates, where the calc disagrees with bloomberg
        if(!("xts" %in% class(xtsin))) {
            flushprint("localmin: must be an xts as input")
            return(NA)
        } else {
            xtsin <- na.omit(xtsin) 
            if(length(xtsin) > 0) {
                guessdate <- as.Date(guessdate) # just in case
                currentmin <- as.numeric(xtsin[guessdate, whichcol])
                currentindex <- match(guessdate, index(xtsin))
                bounder <- 1
                mover <- 0
                while(bounder < checkbounds) {
                    if(nrow(xtsin) >= currentindex + bounder) {
                        if(as.numeric(xtsin[currentindex + bounder, whichcol]) < as.numeric(currentmin)) {
                            currentmin <- xtsin[currentindex + bounder, whichcol]
                            mover <- bounder
                        }
                    }
                    if(1 <= currentindex - bounder) {
                        if(as.numeric(xtsin[currentindex - bounder, whichcol]) < as.numeric(currentmin)) {
                            currentmin <- xtsin[currentindex - bounder, whichcol]
                            mover <- -bounder
                        }
                    }
                    bounder <- bounder + 1
                }
                if(abs(mover) == checkbounds) {
                    flushprint("Severe warning! looking for local minimum reached bounds")
                }
                return(index(xtsin)[currentindex + mover])

            } else {
                return(NA)
            }
        }
    }
    isins <- as.character(isins) # just in case they have been passed in vector format, which doesn't play nice here
    bondlists <- lapply(ac, allBonds)
    names(bondlists) <- ac
    cCodes <- sapply(isins, function(isin) { # get all the country codes
        ac[sapply(bondlists, function(x) isin %in% x)]
    })
    # get bloomberg codes
    bbcodes <- apply(cbind(cCodes, isins), 1, function(x) isin2bb(x[1], x[2]))
    # now get country data objects into a list
    cDatas <- lapply(unique(cCodes), function(cCode) get(paste(cCode, "data", sep = "")))
    names(cDatas) <- unique(cCodes)
    # now get prices and accrueds
    dates <- index(cDatas[[cCodes[1]]]$historicData$PX_BID)
    dates <- dates[dates >= startdate]
    coupons <- apply(cbind(bbcodes, cCodes), 1, function(x) cDatas[[x[2]]]$staticData[x[1], "COUPON"])
    couponfreqs <- apply(cbind(bbcodes, cCodes), 1, function(x) cDatas[[x[2]]]$staticData[x[1], "CPN_FREQ"])
    maturities <- as.Date(apply(cbind(bbcodes, cCodes), 1, function(x) cDatas[[x[2]]]$staticData[x[1], "MATURITY"]))
    issuedates <- as.Date(apply(cbind(bbcodes, cCodes), 1, function(x) cDatas[[x[2]]]$staticData[x[1], "ISSUE_DT"]))
    maturitytradedates <- tradeDate(maturities)
    names(maturitytradedates) <- names(maturities) <- names(issuedates) <- isins
    daystosettle <- apply(cbind(bbcodes, cCodes), 1, function(x) cDatas[[x[2]]]$staticData[x[1], "DAYS_TO_SETTLE"])
    cashflows <- apply(cbind(bbcodes, cCodes), 1, function(x) cDatas[[x[2]]]$cfData[[x[1]]])
    pxbids <- sapply(1:length(bbcodes), function(x) { 
        px <- as.numeric(cDatas[[cCodes[x]]]$historicData$PX_BID[, bbcodes[x]][paste(startdate, "/", sep = "")])
        px[dates > maturities[isins[x]]] <- NA
        return(px)
    })
    pxdbids <- sapply(1:length(bbcodes), function(x) { 
        px <- as.numeric(cDatas[[cCodes[x]]]$historicData$PX_DIRTY_BID[, bbcodes[x]][paste(startdate, "/", sep = "")])
        px[dates > maturities[isins[x]]] <- NA
        return(px)
    })
    if(!(all(dim(pxbids) == dim(pxdbids)))) { # error correct if dimensions returned not the same. Just in case. 
        flushprint("returned dimension of clean and dirty prices is different")
        return(-1)
    }
    pxbids <- xts(pxbids, order.by = dates)
    pxdbids <- xts(pxdbids, order.by = dates)
    accrueds <- pxdbids - pxbids
    colnames(pxbids) <- colnames(accrueds) <- isins
    # now get coupons and cashflow data
    principals <- sapply(cashflows, function(x) x[nrow(x), "Principal"])
    actualcouponpay <- lapply(isins, function(x) {
        considerdates <- dates[dates >= issuedates[x]]
        if(length(considerdates) > 0) {
            if(calendar == "auto") { # then use specific country calendar
                settledates <- settleDate(considerdates, calendar = as.character(cCodes[isins == x]), 
                tplus = daystosettle[x])
            } else { # use target
                settledates <- settleDate(considerdates, calendar = "TE", tplus = daystosettle[x])
            }
            coupondates <- as.Date(cashflows[[x]]$Date)
            coupondates <- coupondates[coupondates >= settledates[1]] # only from first settle date
            coupondates <- coupondates[coupondates <= settledates[length(settledates)]] # 
        } else {
            coupondates <- list() # empty
        }
        if(length(coupondates) > 0) {
            couponpaydates <- sapply(coupondates, function(cdt) settledates[settledates >= cdt][1]) #when is coupon paid
            couponpaydates <- as.Date(couponpaydates) 
            # now we have to deal with the duplicate settle dates problem
            settleselect <- settledates[settledates %in% couponpaydates]
            tradeselect <- considerdates[settledates %in% couponpaydates]
            runlength <- rle(as.numeric(settleselect))$lengths
            selector <- cumsum(runlength) - (runlength - 1)
            tradepaydates <- tradeselect[selector]
            paydf <- cbind(cashflows[[x]][as.Date(cashflows[[x]]$Date) %in% coupondates, ], couponpaydates, tradepaydates)
            return(paydf)
        } else {
            return(NA)
        }
    })
    names(actualcouponpay) <- isins
    dims <- dim(accrueds) # get the dimensions
    dividends <- matrix(rep(0, dims[1] * dims[2]), dims[1], dims[2]) # dividends matrix. Equals coupon / annual frequency
    dividends <- xts(dividends, order.by = dates)
    colnames(dividends) <- isins
    prinpay <- dividends
    for(isin in isins) {
        ap <- actualcouponpay[[isin]]
        if(!is.na(ap)) {
            prinpay[ap$tradepaydates, isin] <- 100 * ap$Principal / principals[isin]
            dividends[ap$tradepaydates, isin] <- 100 * ap$Interest / principals[isin]
        }
    }
    # now create returns
    #now fix the dates where they don't agree
    for(isin in isins) {
        ap <- actualcouponpay[[isin]] # get the cashflows and dates df out
        if(!is.null(ap)) { 
            if(!is.na(ap)) {
                checkdates <- ap$tradepaydates[ap$Principal == 0] # only coupon dates
                newdates <- sapply(checkdates, function(dt) localmin(accrueds[, isin], dt, checkbounds = 10))
                if(length(newdates) > 0) actualcouponpay[[isin]][ap$Principal == 0, "tradepaydates"] <- as.Date(newdates)
                # now all accrueds after maturity are zero
                prindate <- ap[ap$Principal > 0, "tradepaydates"]
                if(length(ap) > 0) { # if principal is in the life of the bond
                    accrueds[index(accrueds) >= prindate, isin] <- 0
                }
            }
        }
    }
    allidx <- lapply(isins, function(isin) {
        #if(isin == "DE0001141562") browser()
        bonddata <- na.omit(cbind(pxbids[, isin], accrueds[, isin], dividends[, isin]))
        if(is.null(bonddata)) {
            return(NA)
        } else {
            if((nrow(bonddata) == 0) || (is.na(bonddata))) {
                return(NA)
            } else {
                if(nrow(bonddata) == 1) { #just one line of data - just launched bond for example, or about to mature
                    flushprint("only one row in triprep")
                    browser()
                }
                idx <- cumsum(bonddata[, 3]) + bonddata[, 1] + bonddata[, 2]
                colnames(idx) <- isin
                return(idx)
            }
        }
    })
    names(allidx) <- isins
    # now we are going to build the returns
    return(list(accrueds = accrueds, pxbids = pxbids, dividends = dividends, prinpay = prinpay, 
                actualcouponpay = actualcouponpay, 
                maturities = maturities, issuedates = issuedates, cCodes = cCodes,
                maturitytradedate = maturitytradedates, allidx = allidx))
}


tri <- function(preplist, inweights = NA) {
# will create total return indices out of the triprep object
# if weights are passed, they must be a data frame with columns bond, date, weight. Weights will be carried down columns until changed
# the first row of weights defaults to 1
    dims <- dim(preplist$accrueds)
    isins <- colnames(preplist$accrueds)
    numbonds <- length(isins)
    dates <- index(preplist$accrueds)
    numdates <- length(dates)
    issuedates <- preplist$issuedates
    maturities <- preplist$maturities
    cleanprices <- preplist$pxbids
    accrueds <- preplist$accrueds
    dividends <- preplist$dividends
    # initialize in weights: 1 for bonds already in existence, else when they exist
    startdates <- as.Date(issuedates)
    startdates[issuedates <= dates[1]] <- as.Date(dates[1])
    weights <- data.frame(isins = isins, startdates = startdates, startweights = rep(1, numbonds), stringsAsFactors = FALSE)
    maturedonstart <- maturities <= dates[1]
    weights[maturedonstart, "startweights"] <- 0 # bonds already matured on the first date are zero weight
    # now create the daily weights matrix
    dailyweights <- matrix(rep(NA, dims[1] * dims[2]), dims[1], dims[2]) # create the empty matrix with NAs as will use na.locf later
    dailyweights <- xts(dailyweights, order.by = dates)
    colnames(dailyweights) <- isins
    for(x in 1:nrow(weights)) {   # this will now put the weights in
        isin <- weights[x, "isins"]
        date <- weights[x, "startdates"]
        weight <- weights[x, "startweights"]
        dailyweights[date, isin] <- weight
    }
    # now clear maturities
    for(isin in isins) {
        if (maturities[isin] %in% dates) {
            dailyweights[maturities[isin], isin] <- 0
        }
    }
    # now handle issue dates
    dailyweights <- na.locf(dailyweights)
    dailyweights[is.na(dailyweights)] <- 0
    # now we build the returns of each bond
}



triplots <- function(tris, since = c("year", "quarter", "month", "week")) {
# plots the total return index changes over the periods. Last one gets "special treatment"
    if(nrow(tris$pxbids) < 2) {
        flushprint("Not enough data in tris index")
        return(-1)
    }
    dates <- index(tris$pxbids) # get the dates that we have
    today <- as.Date(last(dates))
    sinceidxs <- lapply(since, function(x) {
        ss <- endpoints(tris$pxbids, paste(x, "s", sep = "")) # all sincedates
        return(ss[length(ss) - 1])
    })
    tribind <- do.call(cbind, tris$allidx) # all the return indices in one xts matrix
    rets <- sapply(sinceidxs, function(x) {
                   dd <- diff(log(tribind[c(x, nrow(tribind)), ]))
                   return(dd[2, ])
            })
    colnames(rets) <- since
    rets <- as.data.frame(rets)
    rets <- cbind(rownames(rets), rets)
    colnames(rets)[1] <- "isin"
    rets <- cbind(tris$cCodes, rets)
    colnames(rets)[1] <- "country"
    labels <- sapply(1:length(tris$cCodes), function(x) {
        isinlab <- isinLabel(tris$cCodes[x], colnames(tris$pxbids)[x], shortlab = TRUE, withcCode = TRUE)
        return(substr(isinlab, 3, 6))
    })
    rets <- cbind(as.character(labels), rets)
    colnames(rets)[1] <- "label"
    rets <- rets[tris$maturities > today, ]
    tbuckets <- apply(rets, 1, function(x) {
        cFactors <- get(paste(x[2], "factors", sep = ""))
        cFactors[(as.character(cFactors$date) == as.character(today)) & (cFactors$bond == x[3]), "bucket"]
    })
    tbuckets[is.na(tbuckets == 0)] <- NA
    rets <- cbind(as.character(unlist(tbuckets)), rets)
    colnames(rets)[1] <- "bucket"
    rets$bucket <- factor(rets$bucket, levels = as.character(names(buckets)))
    rets$country <- factor(rets$country, levels = cl)
    rets <- na.omit(rets)
    retsmelt <- melt(rets)
    retsmelt <- cbind(retsmelt, tris$maturities[retsmelt$isin])
    colnames(retsmelt)[ncol(retsmelt)] <- "maturity"
    lastfridaystring = format(dates[as.numeric(last(sinceidxs))], "%d %b") # last friday string
    ggret <- ggplot(data = retsmelt, aes(x = label, y = value, fill = country, colour = country)) 
    ggret <- ggret + geom_bar(stat = "identity", position = "dodge")
    ggret <- ggret + facet_grid(variable ~ bucket, scale = "free_x")
    ggret <- ggret + scale_fill_manual(values = muted(unlist(cColors[cl])))
    ggret <- ggret + scale_colour_manual(values = (unlist(cColors[cl])))
    ggret <- ggret + theme(axis.text.x = element_blank())
    ggret <- ggret + labs(y = "total return")
    ggret <- ggret + ggtitle("Total return to date by country")
    ggret <- ggret + scale_y_continuous(labels = percent)
    retsmeltsingle <- retsmelt[as.character(retsmelt$variable) == last(since), ]
    retsmeltsingle$label <- as.character(retsmeltsingle$label)
    ggret2 <- ggplot(data = retsmeltsingle, 
                     aes(x = maturity, 
                         y = value, fill = country, colour = country)) 
    ggret2 <- ggret2 + geom_bar(stat = "identity", position = "dodge")
    ggret2 <- ggret2 + facet_grid(~ country, scale = "free")
    ggret2 <- ggret2 + scale_fill_manual(values = muted(unlist(cColors[cl])))
    ggret2 <- ggret2 + scale_colour_manual(values = (unlist(cColors[cl])))
    ggret2 <- ggret2 + theme(axis.text.x = element_blank())
    ggret2 <- ggret2 + labs(y = "total return")
    ggret2 <- ggret2 + ggtitle(paste("Total return", last(since), "to date by country"))
    ggret2 <- ggret2 + scale_y_continuous(labels = percent)
    ggret2 <- ggret2 + theme(axis.text.x = element_text(angle = 90, size = 9)) 
    ggret2 <- ggret2 + geom_point(aes(x = maturity, y = value, colour = country))
    # now plot bucket returns
    buckret <- aggregate(retsmeltsingle$value, by = list(retsmeltsingle$bucket, retsmeltsingle$country), FUN = mean)
    colnames(buckret) <- c("bucket", "country", "value")
    ggbuck <- ggplot(buckret, aes(x = bucket, y = value, colour = country))
    ggbuck <- ggbuck + geom_bar(stat = "identity", position = "dodge", fill = "grey")
    ggbuck <- ggbuck + facet_grid( ~ country, scale = "free_x")
    ggbuck <- ggbuck + scale_fill_grey(start = 0.8, end = 0.3)
    #ggbuck <- ggbuck + scale_fill_manual(values = (unlist(cColors[cl])))
    ggbuck <- ggbuck + scale_colour_manual(values = unlist(cColors[cl]))
    #ggbuck <- ggbuck + ggtitle(paste("Total return", last(since), "to date by country"))
    ggbuck <- ggbuck + ggtitle(paste("Total return since Friday (", lastfridaystring, 
                                     ") close by country", sep = ""))
    ggbuck <- ggbuck + labs(y = "total return")
    ggbuck <- ggbuck + scale_y_continuous(labels = percent)
    ggbuck <- ggbuck + theme(legend.position = "none", plot.title = element_text(size = 9), 
                             axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7), 
                             axis.title.x = element_blank(), axis.title.y = element_blank())
    # now let's do yield change
    ay <- allYields()
    aysinceidxs <- lapply(since, function(x) {
        ss <- endpoints(ay[[1]], paste(x, "s", sep = "")) # all sincedates
        return(ss[length(ss) - 1])
    })
    lastidx <- last(aysinceidxs)[[1]] # week returns - can change if want other periods
    countryyieldchanges <- lapply(ay, function(x) {
        diff(x[c(lastidx, nrow(x)), ])[2, ] * 10000
    })
    yieldret <- buckret # then we'll change the bucket total returns to yield changes
    for (x in 1:nrow(yieldret)) {
        yieldret[x, 3] <- round(countryyieldchanges[[as.character(yieldret[x, 2])]][, 
                                as.numeric(as.character(yieldret[x, 1]))], 1)
    }

    ggyield <- ggplot(yieldret, aes(x = bucket, y = value, colour = country))
    ggyield <- ggyield + geom_bar(stat = "identity", position = "dodge", fill = "darkgrey")
    ggyield <- ggyield + facet_grid( ~ country, scale = "free_x")
    ggyield <- ggyield + scale_fill_grey(start = 0.8, end = 0.3)
    #ggyield <- ggyield + scale_fill_manual(values = (unlist(cColors[cl])))
    ggyield <- ggyield + scale_colour_manual(values = unlist(cColors[cl]))
    #ggyield <- ggyield + ggtitle(paste("Yield change", last(since), "to date by country"))
    ggyield <- ggyield + ggtitle(paste("Yield change since Friday (", lastfridaystring, 
                                       ") close by country", sep = ""))
    ggyield <- ggyield + labs(y = "yield change")
    ggyield <- ggyield + scale_y_continuous()
    ggyield <- ggyield + theme(legend.position = "none", plot.title = element_text(size = 9), 
                             axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7), 
                             axis.title.x = element_blank(), axis.title.y = element_blank())
    return(list(ggret = ggret, ggret2 = ggret2, ggbuck = ggbuck, ggyield = ggyield))

    
}

creditFly <- function(cCodes = ac, days = 260 * 3, mats = keymats, numhedges = 2, eitherside = 0, model = "auto") {
# will perform credit flies for each mat in each cCode using hedges from all other cCodes up to eitherside of the matching mat
    ay <- allYields(cCodes, days = days, model = model)
    l1 <- lapply(cCodes, function(cCode) {
        l2 <- lapply(mats, function(mat) {
            counterCodes <- cCodes[-match(cCode, cCodes)] # all countries excluding current one
            counterCodes <- combn(counterCodes, numhedges)
            counterMats <- mat
            if(eitherside > 0) {
                for (es in 1:eitherside) {                    # build the counter maturities we will consider
                    currentMatIndex <- match(mat, mats)
                    if((currentMatIndex + es) <= length(mats)) {
                        counterMats <- append(counterMats, mats[currentMatIndex + es])
                    }
                    if((currentMatIndex - es) >= 1) {
                        counterMats <- append(counterMats, mats[currentMatIndex - es])
                    }
                }
            }
            l3 <- lapply(counterMats, function(counterMat) {
                l4 <- apply(counterCodes, 2, function (counterCodeVec) {
                      counterdf <- do.call(cbind, lapply(counterCodeVec, function(ccvec) ay[[ccvec]][, counterMat]))
                      colnames(counterdf) <- counterCodeVec
                      dependent <- ay[[cCode]][, mat]
                      linmod <- lm(ay[[cCode]][, mat] ~ counterdf)
                      resids <- linmod$residuals
                      coeffs <- coef(linmod)
                      score <- last(resids) / sd(resids)
                      return(list(score = score, dependent = dependent, independent = counterdf, 
                                  resids = resids, coeffs = coeffs, linmod = linmod))
                })
                names(l4) <- apply(counterCodes, 2, function(x) do.call(paste, as.list(x)))
                return(l4)
            })
            names(l3) <- counterMats
            return(l3)
        })
        names(l2) <- mats
        return(l2)
    })
    names(l1) <- cCodes
    return(l1)
}





            

exportJSON <- function(cCodes = ac) {
    sapply(cCodes, function(x) {
        cData <- get(paste(x, "data", sep = ""))
        writeLines(toJSON(cData$cfData), paste(dataplace, x, "cf.JSON", sep = ""))
        writeLines(toJSON(cData$staticData), paste(dataplace, x, "static.JSON", sep = ""))
        writeLines(toJSON(cData$actives), paste(dataplace, x, "actives.JSON", sep = ""))
    })
}

        






    













    
    




        


    
####################### PLAY AROUND ZONE #############################

eigenplay <- function(cCodes = ac, model = "auto", days = 7 * 260, rolldays = 130, rets = TRUE, 
                      mats = keymats, scale.unit = TRUE ) {
# this will do eigen analysis of the allyields matrices involved and plot stuff out from them
    ay <- allYields(cCodes, days = days, model = model, mats = mats)
    days <- nrow(ay[[1]])
    mats <- colnames(ay[[1]])
    dates <- index(ay[[1]])
    if(scale.unit == TRUE) corfun <- cor else corfun <- cov
    if(rets == TRUE) {
        ay <- lapply(ay, diffret)
    }
    rolleigvals <- lapply(ay, function(x) {
        ev <- na.omit(rollapply(x, rolldays, function(x) eigen(corfun(x))$values, by.column = FALSE))
        colnames(ev) <- mats
        return(ev)
    })
    names(rolleigvals) <- cCodes
    eigpercentages <- lapply(rolleigvals, function(x) {
        ep <- t(apply(x, 1, function(y) round(100 * y / sum(y), 2)))
        ep <- xts(ep, order.by = last(dates, nrow(ep)))
    })
    names(eigpercentages) <- cCodes
    rolleigvecs <- lapply(1:length(mats), function(pcnum) {
        rolleigcunt <- lapply(ay, function(x) {
            rv <- fixEigen(na.omit(rollapply(x, rolldays, function(x) 
                eigen(corfun(x))$vectors[, pcnum], by.column = FALSE, align = "right")))
            colnames(rv) <- mats 
            return(rv)
        })
        names(rolleigcunt) <- cCodes
        return(rolleigcunt)
    })
    names(rolleigvecs) <- 1:length(mats)
    return(list(eigvals = rolleigvals, eigpercent = eigpercentages, eigvecs = rolleigvecs, data = ay))
}






################ MONGO FUNCTIONS ########################


testmongo <- function() {
# check comparability of mongo versus bb interfaces
    ab <- paste(unlist(sapply(ac, function(x) allBonds(x, TRUE))), "Corp")
    timerlist <<- list()
    while(TRUE) {
        mongo.remove(mongo, "bb.bblatest")
        timer <- Sys.time()
        repeat {
            Sys.sleep(1)
            mm <- bbmongolatest()[ab, ]
            currentnrow <- nrow(na.omit(bbmongolatest()[ab, ]))
            flushprint(currentnrow)
            if(!is.null(currentnrow)) {
                if(currentnrow == length(ab)) break
            }
        }
        expended <- Sys.time() - timer
        flushprint(paste("---------------> ", expended))
        timerlist <<- append(timerlist, expended)
    }
}

###################################### TEST ACCRUED INTEREST DATA ##############################

plotaccrued <- function(cCodes = ac, pdfname = "accruedplots.pdf") {
# Will check out the accrued interest on each of the bonds, plot them to a PDF for visual inspection
    pdf(file = pdfname, width = 6, height = 9, onefile = TRUE)
    bondlist <- lapply(cCodes, allBonds)
    names(bondlist) <- cCodes
    for(cCode in cCodes) {
        bonds <- bondlist[[cCode]]
        cData <- get(paste(cCode, "data", sep = ""))
        for(bond in bonds) {
            bbcode <- isin2bb(cCode, bond)
            dbid <- cData$historicData$PX_DIRTY_BID[, bbcode]
            cbid <- cData$historicData$PX_BID[, bbcode]
            dask <- cData$historicData$PX_DIRTY_ASK[, bbcode]
            cask <- cData$historicData$PX_ASK[, bbcode]
            par(mfrow = c(3, 1), oma = c(2, 2, 4, 2))
            plot(dbid)
            lines(cbid, col = "red")
            abline(h = 100, col = "green")
            legend("topright", legend = c("dirty bid", "clean bid"), fill = c("black", "red"))
            plot(dask)
            lines(cask, col = "red")
            abline(h = 100, col = "green")
            legend("topright", legend = c("dirty ask", "clean ask"), fill = c("black", "red"))
            plot(dask - cask)
            lines(dbid - cbid, col = "red")
            abline(h = 0, col = "purple")
            legend("topright", legend = c("accrued ask", "accrued bid"), fill = c("black", "red"))
            title(isinLabel(cCode, bond)[[1]], outer = TRUE)
        }
    }
    dev.off()
    shell(pdfname, wait = FALSE)
}














    





        




