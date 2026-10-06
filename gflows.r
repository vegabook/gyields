######################################################
#     COUPON AND PRINCIPLE FLOWS FOR CRVM PRODUCT    #
######################################################


flowprep <- function(cCodes = ac, startDate = Sys.Date(), fwdDays = 45) {
# all the flows principal and coupon due over the coming month
    endDate <- startDate + fwdDays
    allflows <- lapply(cCodes, function(cCode) {
        cData <- get(paste(cCode, "data", sep = ""))
        cFlows <- do.call(rbind, cData$cfData) # all the cashflows
        cFlows[, "Date"] <- as.Date(cFlows[, "Date"])
        cFlows <- cFlows[cFlows[, "Date"] >= startDate, ] 
        cFlows <- cFlows[cFlows[, "Date"] <= endDate, ]
        if(nrow(cFlows) > 0) {
            bbCodes <- sapply(strsplit(rownames(cFlows), ".", fixed = TRUE), "[[", 1)
            outstandings <- sapply(bbCodes, function(x) {
               as.numeric(bdp(conn, paste(x, "Govt"), "AMT_OUTSTANDING"))
            })
            denominators <- sapply(bbCodes, function(x) {
                last(cData$cfData[[x]][, "Principal"])
            })
            multiplier <- outstandings / denominators
            cFlows[, "Interest"] <- cFlows[, "Interest"] * multiplier / 1e9
            cFlows[, "Principal"] <- cFlows[, "Principal"] * multiplier / 1e9
            labels <- isinLabel(cCode, bb2isin(cCode, bbCodes))
            cFlows <- cbind(cFlows, rep(cCode, nrow(cFlows)), labels) # add country code
            colnames(cFlows)[4:5] <- c("Country", "Label")
        } else {
            cFlows <- NULL
        }
        return(cFlows)
    })
    result <- do.call(rbind, allflows)
    result <- melt(result, id.vars = c("Date", "Country", "Label"), value.vars = c("Interest", "Principal"))
    result[, "Date"] <- nextbd(result[, "Date"])
    return(result)
}

nextbd <- function(dates) {
    dd <- sapply(dates, function(d) {
        thisdate <- d
        while(weekdays(as.Date(thisdate)) %in% c("Saturday", "Sunday")) {
            thisdate <- thisdate + 1
        }
        return(as.Date(thisdate))
    })
    return(as.Date(dd))
}



flowplot <- function(flows = flowprep()) {
    flows$datelab <- apply(flows, 1, function(x) {
        if((x[4] == "Interest") | (as.numeric(x[5]) == 0)) {
            ""
        } else {
            paste(format(as.Date(x[1]), "%d %b"), ": ", round(as.numeric(x[5]), 1), "bn  ", sep = "")
        }
    })
    gg <- ggplot(flows, aes(x = Date, y = value)) 
    gg <- gg + geom_bar(aes(fill = Country), stat = "identity")
    gg <- gg + geom_text(aes(label = datelab), size = 3, colour = "white", angle = "90", hjust = 1, vjust = 0.3)
    gg <- gg + facet_grid(variable ~ .) 
    gg <- gg + labs(y = "EUR billion")
    gg <- gg + ggtitle("Upcoming interest and principal payments")
    return(gg)
}

    

    




    




