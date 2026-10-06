# meant to be loaded AFTER gyields.r has been loaded
can_provs = list(Alberta = "BVIS0676 Index", 
                 Manitoba = "BVIS0678 Index",
                 New_Brunswick = "BVIS0675 Index",
                 Newfoundland = "BVIS0679 Index",
                 Ontario = "BVIS0670 Index",
                 Quebec = "BVIS0671 Index",
                 Saskatchewan = "BVIS0677 Index",
                 Canada_Govt = "BVIS0587 Index")


get_rates <- function(curvelist = can_provs, years = 3) {
    tickers <- lapply(curvelist, function(x) bds(conn, x, "indx members"))
    tickers <- lapply(tickers, function(x) x[3:nrow(x), 1])
    rates <- lapply(tickers, function(x) bbdh(x, years = years))
    mats <- c(1, 2, 3, 4, 5, 7, 8, 9, 10, 15, 20, 25, 30)
    for(x in names(rates)) colnames(rates[[x]]) <- mats
    return(rates)
}

get_spreads <- function(rates) {
    spreads <- lapply(1:(length(rates) -1), function(x) rates[[x]] - rates[[length(rates)]])
    names(spreads) <- names(rates)[-length(rates)]
    return(spreads)
}


get_pcs <- function(rates) {
    allrates <- do.call(cbind, rates)
    pcs <- makePCs(allrates)
    return(pcs$pcs)
}


get_resids <- function(rates, pcs) {
    ll <- lapply(rates, function(x) apply(x, 2, function(y) lm(y ~ pcs)$residuals))
    lapply(ll, function(x) xts(x, order.by = as.Date(as.character(rownames(x)))))
}


plot_grid <- function(rates, lastcol = NA, ylab = "yield%") {
    windows(12, 12)
    par(mfrow = c(3, 3))
    for(n in names(rates)) {
        thiscol <- "red"
        if((n == (last(names(rates)))) && !is.na(lastcol)) thiscol <- "green3"
        hair(rates[[n]], ylab = ylab, nowcol = thiscol,title = n)
        abline(h = 0, lty = "dashed")
    }
}

canada <- function() {
    rates <- get_rates()
    spreads <- get_spreads(rates)
    pcs <- get_pcs(spreads)
    resids <- get_resids(spreads, pcs[, 1])
    plot_grid(c(spreads, last(rates)), lastcol = "green3")
    plot_grid(lapply(resids, function(x) x * 100))
}


boxheat <- function(rates, frontmat, backmat) {
    spreads <- lapply(rates, function(x) {
        x[, as.character(backmat)] - x[, as.character(frontmat)]
    })
    spreads <- do.call(cbind, spreads)
    colnames(spreads) <- names(rates)
    # now create z matrix
    windows(8, 8)
    zs <- apply(spreads, 2, function(x) {
        apply(spreads, 2, function(y) {
            res <- lm(y ~ x)$residuals
            (last(res) - mean(res)) / sd(res)
        })
    })
    hothot(t(zs), title = paste(as.character(backmat), "-", as.character(frontmat), "    sample days =", nrow(spreads)),
                             xlab = "sell", ylab = "buy")
    return(spreads)
}


    
