library("moments")
library("xts")
if(!exists("ct")) load("~/Copy/data/gyields/constraint_test/ct.dat")


dotest <- function() {
    h5createFile(fl)
    sapply(d1, function(dd1) {
        h5createGroup(fl, dd1)
        sapply(d2, function(dd2) {
            h5createGroup(fl, paste(dd1, dd2, sep = "/"))
            sapply(d3, function(dd3) {
                h5createGroup(fl, paste(dd1, dd2, dd3, sep = "/"))
                sapply(d4, function(dd4) {
                    h5createGroup(fl, paste(dd1, dd2, dd3, dd4, sep = "/"))
                    sapply(d5, function(dd5) {
                        if((dd4 > dd3) & ((dd4 - dd3) > dd5)) {
                            h5createGroup(fl, paste(dd1, dd2, dd3, dd4, dd5, sep = "/"))
                            nowtime <- Sys.time()
                            flushprint(nowtime)
                            flushprint(dd1)
                            flushprint(dd2)
                            flushprint(dd3)
                            flushprint(dd4)
                            flushprint(dd5)
                            thistest <- dons(ll[[dd1]], pcores = 7, meth = dd2, 
                                             taucon = list(c(dd3, dd4, dd5, 0.5)))
                            optresults <- lapply(thistest, function(x) {
                                                 if(is.na(x)) {
                                                     NA 
                                                 } else {
                                                     list("opt_result" = x$opt_result[[dd1]]$par,
														  "yhat" = x$yhat[[dd1]],
														  "y" = x$y[[dd1]],
                                                          "time" = as.numeric(difftime(Sys.time(), nowtime, units = "secs")))
                                                 }
                            })
                            names(optresults) <- names(ll[[dd1]])
                            testname <- paste(dd1, dd2, dd3, dd4, dd5, sep = "-")
                            h5write(optresults, fl,
                                    paste(dd1, dd2, dd3, dd4, dd5, testname, sep = "/"))
                            flushprint(Sys.time() - nowtime)
                        }
                    })
                })
            })
        })
    })
}

dfprint <- function(a1 = c("AT", "BE", "DE", "FR", "IT", "NE"),
                 a2 = c("asv", "sv"),
                 a3 = c("1", "2", "3"),
                 a4 = c("10", "15", "2", "20", "3", "30", "5", "7"), 
                 a5 = c("1", "2", "3", "5")) {
    badcount <<- 0
    goodcount <<- 0
    pdf("dfprint.pdf")
    titles <- c("Beta1", "Beta2", "Beta3", "Tau1", "Beta4", "Tau2")
    sapply(a1, function(aa1) {
        sapply(a2, function(aa2) {
            sapply(a3, function(aa3) {
                sapply(a4, function(aa4) {
                    sapply(a5, function(aa5) {
                        node = ct[[aa1]][[aa2]][[aa3]][[aa4]][[aa5]][[1]]
                        if(is.null(node)) {
                            badcount <<- badcount + 1
                            return(NULL)
                        } else {
                            a7 <- names(node) # dates
                            optres <- t(sapply(a7, function(aa7) {
                                node[[aa7]]$opt_result
                            }))
                            optres <- try(xts(optres, order.by = as.Date(rownames(optres))))
                            if("try-error" %in% class(optres)) {
                                browser()
                            }
                            goodcount <<- goodcount + 1
                            par(mfrow = c(3,2), oma = c(4, 4, 4, 4), mar = c(2.5, 2.5, 2.5, 2.5))
                            for(x in 1:6) {
                                loessidx <- as.numeric(as.POSIXct(index(optres)))
                                thisloess <- loess(optres[, x] ~ loessidx, span = 0.1)
                                plot(optres[, x], main = "", major.format = "%b%Y")
                                lines(loessidx, thisloess$fitted, col = cColors[[aa1]], lwd = 2)
                                resids <- xts(as.numeric(thisloess$residuals), order.by = as.Date(a7))
                                rollmean <- rollapply(resids, 52, function(x) mean(abs(x)))
                                par(new = TRUE)
                                plot(rollmean, col = "dodgerblue", main = "",
                                     axes = FALSE, xlab = "", ylab = "")
                                title(paste(titles[x], aa1, aa2, aa3, aa4, aa5, 
                                            round(sd(na.omit(rollmean)) / mean(na.omit(rollmean)), 3)))
                            }
                            return(optres)
                        }
                    })
                })
            })
        })
    })
    dev.off()
    print(paste("badcount:", badcount))
    print(paste("goodcount:", goodcount))
}


                            


                         





                        
