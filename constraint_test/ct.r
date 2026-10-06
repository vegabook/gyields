plotday = function(cCode, model, starttau, endtau, taugap, mats = 1:33) {
    par(mfrow = c(8, 7), mar = c(1, 1, 3, 1))
    dates <- last(names(ct[[cCode]][[model]][[starttau]][[endtau]][[taugap]][[1]]), 49)
    # masks for plotting different curves
    masks <- c(c(1, 1, 1, 1, 1, 1), c(1,0,0,1,0,0), c(0, 1, 0, 1, 0, 0), c(0, 0, 1, 1, 0, 0), c(0, 0, 0, 0, 1, 1))
    for (d in dates) {
        data <- ct[[cCode]][[model]][[starttau]][[endtau]][[taugap]][[1]][[d]]
        if(model == "asv") {
            zeroyields = spr_asv(data$opt_result, mats) / 100
        } else {
            zeroyields = spr_sv(data$opt_result, mats) / 100
        }
        matsize = length(mats)
        couponyields <- sapply(1:matsize, function(x) {
                optimize(function(y) abs(100 - sum(sapply(1:x, function(i) 
                    ifelse(i == x, 100 + y * 100, y * 100) / (1 + zeroyields[i]) ^ i))), interval = c(-1, 1))$minimum
        })
        plot(mats, couponyields)
        lines(mats, couponyields)
        points(data$y[, 1], data$y[ ,2], col = "red")
        points(data$yhat[, 1], data$yhat[, 2], col = "chartreuse3", pch = 19)
        title(paste(cCode, model, starttau, endtau, taugap, d))
    }
    coeffs <- t(sapply(dates, function(d) {
        ct[[cCode]][[model]][[starttau]][[endtau]][[taugap]][[1]][[d]][["opt_result"]]
    }))
    for(s in 1:ncol(coeffs)) {
        plot(coeffs[, s], col = "red", type = "l")
    }
}


