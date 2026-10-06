# quick generic yields regressions

library(FactoMineR)
library(leaps)
source("../common.r")
source("../tools.r")

yg <- function() source("ygeneric.r") # for reloading

gen10y <- list(FR = "GFRN10 Index", 
               DE = "GDBR10 Index", 
               IT = "GBTPGR10 Index", 
               SP = "GSPG10YR Index", 
               PO = "GSPT10YR Index",
#               GR = "GGGB10YR Index",
               US = "USGG10YR Index", 
               UK = "GUKG10 Index",
               NE = "GNTH10YR Index",
               AT = "GAGB10YR Index", 
               PL = "POGB10YR Index")


regMove <- function(years = 2) {
# move in bps adjusted by regressions
    y10 <<- bbdh(unlist(gen10y), years) # get data
    y10r <<- diffret(y10)
    rl <- sapply(1:ncol(y10r), function(x) {
        lm(y10r[, x] ~ ., data = y10r[, -x], weights = decay(nrow(y10r), nrow(y10r) / 4))$residuals
    })
    print(colnames(rl))
    colnames(rl) <- names(gen10y)[match(paste(colnames(y10r), "Index"), gen10y)]
    colnames(y10r) <<- colnames(rl)
    colnames(rl) <- names(gen10y)[match(paste(colnames(y10r), "Index"), gen10y)]
    return(rl)
}


pcaMove <- function(whatpcs = 1, years = 2) {
# move in bps adusted by principal components analysis
    y10 <<- bbdh(unlist(gen10y), 2) # get data
    y10r <<- diffret(y10)
    pca <- PCA(y10r, scale.unit = TRUE, graph = FALSE, row.w = decay(nrow(y10r), nrow(y10r) / 4))
    pcs <- sapply(1:5, function(x) pca$var$coord[, x] / sqrt(pca$eig[x, 1]))
    pc1 <- apply(data.frame(pcs[, whatpcs]), 1, sum)
    rets <- y10r %*% pc1
    rl <- sapply(1:ncol(y10r), function(x) {
        lm(y10r[, x] ~ rets, weights = decay(nrow(y10r), nrow(y10r) / 4))$residuals
    })
    colnames(rl) <- names(gen10y)[match(paste(colnames(y10r), "Index"), gen10y)]
    colnames(y10r) <<- colnames(rl)
    colnames(y10) <<- colnames(rl)
    return(list(rets = rets, regs = rl, pca = pca))
}


dogen <- function(whatpcs = 1, years = 2, days = 1) {
# plot the graphics
    pm <- pcaMove(whatpcs, years)
    lastMove <- last(y10r, days) * 100
    lastBeta <- last(pm$regs, days) * 100
    if(days > 1) {
        lastMove <- apply(lastMove, 2, sum)
        lastBeta <- apply(lastBeta, 2, sum)
    } else {
        lastMove <- as.numeric(lastMove)

        lastBeta <- as.numeric(lastBeta)
    }
    names <- colnames(y10r)
    windows(5, 6)
    par(mfrow = c(2, 1), mar = c(4, 3, 3, 1))
    barplot(lastMove, names.arg = names, cex.axis = 0.7, cex = 0.7,
        main = paste("10y benchmark move", if(days == 1) "today" else paste("past", days, "days")),
        cex.main = 1, col.main = "grey")
    barplot(lastBeta, names.arg = names, cex.axis = 0.7, cex = 0.7,
        main = paste("Beta-adjusted 10y benchmark move", if(days == 1) "today" else paste("past", days, "days")),
        cex.main = 1, col.main = "grey", col = "skyblue")
    windows(9, 12)
    par(mfrow = c(4, 3), mar = c(3, 3, 3, 1))
    lapply(colnames(pm$regs), function(x) regress(y10r[, x], pm$rets, main = x))
}

    
        
    
