#####################################################
#        CREATES an A4 PDF DAILY FOR CRVM           #
#####################################################

pacman::p_load(png) # for importing the titlebar
pacman::p_load(gridBase) # for viewports

dailyplace <- paste(startdir, "/daily/", sep = "")
bannerheight <- 0.1 # how high is the banner as a proportion of the page
bannertextcol <- "#bec3c8" # must match the banner headline colour
backcol <- "#6c7a89" 

dualpage <- c("DE", "FR", "IT", "GB") # too many bonds for single page

a4pdf <- function(filename = paste(dailyplace, "crvmdaily", format(Sys.Date(), "%d-%b-%Y"), ".pdf", sep = "")) {
# creates a pdf to disk in the dailyplace location 
    errcatch <- try(pdf(filename, width = 11.692, height = 8.267, onefile = TRUE))
    counter = 1
    while("try-error" %in% class(errcatch)) {
        errcatch <- try(filename = paste(dailyplace, "crvmdaily", format(Sys.Date(), "%d-%b-%Y"), "-", as.character(counter), ".pdf", sep = ""))
        if(counter < 9) {
            counter <- counter + 1
        } else {
            flushprint("COULD NOT CREATE THE PDF FILE")
            errcatch <- NA
        }
    }
}

a4win <- function() {
# simulation of pdf with windows window
    windows(width = 11.692, height = 8.267)
}

#------------------------ viewport definitions ------------------------#
vpbanner <- function() {
# returns the banner viewport
    bannervp <- viewport(x = 0.5, y = 1 - bannerheight / 2, width = 1, height = bannerheight)
    return(bannervp)
}

vpheading <- function() {
# returns a heading vp. banner vp must already be pushed
    headingvp <- viewport(x = 0.615, y = 0.8, width = 0.7, height = 0.3)
    return(headingvp)
}

vpexplanation <- function() {
# returns a viewport for the explanation text of a page
    explainvp <- viewport(x = 0.615, y = 0.35, width = 0.7, height = 0.6)
    return(explainvp)
}

vpdate <- function() {
    datevp <- viewport(x = 0.9, y = 0.035, width = 0.13, height = 0.07)
    return(datevp)
}

vp2Cols <- function() {
# adds a two column viewport to the page
    col1 <- viewport(x = 0.25, y = 0.5 - bannerheight / 2, width = 0.45, height = 0.9 - bannerheight)
    col2 <- viewport(x = 0.75, y = 0.5 - bannerheight / 2, width = 0.45, height = 0.9 - bannerheight)
    return(list(col1 = col1, col2 = col2))
}

vpFullpage <- function() {
# adds a two column viewport to the page
    col1 <- viewport(x = 0.5, y = 0.5 - bannerheight / 2, width = 0.9, height = 0.9 - bannerheight)
    return(list(col1 = col1))
}

vp4then1 <- function() {
# 4 small charts on the left, then 2 big charts for rest of page
    col1 <- viewport(x = 0.125, y = 0.05 + 0.5 * (0.9 - bannerheight) / 4, width = 0.2, height = (0.9 - bannerheight) / 4)
    col2 <- viewport(x = 0.125, y = 0.05 + 1.5 * (0.9 - bannerheight) / 4, width = 0.2, height = (0.9 - bannerheight) / 4)
    col3 <- viewport(x = 0.125, y = 0.05 + 2.5 * (0.9 - bannerheight) / 4, width = 0.2, height = (0.9 - bannerheight) / 4)
    col4 <- viewport(x = 0.125, y = 0.05 + 3.5 * (0.9 - bannerheight) / 4, width = 0.2, height = (0.9 - bannerheight) / 4)
    maincol <- viewport(x = 0.3625 + 0.25, y = 0.5 - bannerheight / 2, width = 0.725, height = 0.9 - bannerheight)
    return(list(col1 = col1, col2 = col2, col3 = col3, col4 = col4, maincol = maincol))
}

vp3by2 <- function() {
    heights <- (1 - bannerheight) / 3
    col1row1 <- viewport(x = 0.25, y = heights / 2, width = 0.45, height = heights)
    col1row2 <- viewport(x = 0.25, y = heights + heights / 2, width = 0.45, height = heights)
    col1row3 <- viewport(x = 0.25, y = 2 * heights + heights / 2, width = 0.45, height = heights)
    col2row1 <- viewport(x = 0.75, y = heights / 2, width = 0.45, height = heights)
    col2row2 <- viewport(x = 0.75, y = heights + heights / 2, width = 0.45, height = heights)
    col2row3 <- viewport(x = 0.75, y = 2 * heights + heights / 2, width = 0.45, height = heights)
    viewports = list(col1row1 = col1row1, col1row2 = col1row2, col1row3 = col1row3,
                     col2row1 = col2row1, col2row2 = col2row2, col2row3 = col2row3)
    return(viewports)
}

addBanner <- function() {
# adds banner to viewport
    pushViewport(vpbanner())
    banner <- readPNG(paste(dailyplace, "dailybanner.png", sep = ""))
    grid.raster(banner)
    popViewport()
}

addHeading <- function(heading, explaintext) {
    # add a heading
    pushViewport(vpbanner())
    pushViewport(vpheading())
    try(grid.text(heading, gp = gpar(col = bannertextcol), x = 0, y = 0.5, just = c("left", "centre")))
    popViewport()
    pushViewport(vpexplanation())
    try(grid.text(explaintext, gp = gpar(col = bannertextcol, cex = 0.7), x = 0, y = 0.5, 
                  just = c("left", "centre")))
    popViewport()
    popViewport()
}

addDate <- function(alpha = 1) {
    pushViewport(vpdate())
    grid.rect(gp = gpar(fill = addAlpha(backcol, alpha), col = NA))
    grid.text(cettime(), gp = gpar(col = bannertextcol, font = 3, cex = 0.7), y = 0.75)
    popViewport()
}

ggpages <- function(ggobj, daysback = 1) {
    pcobj <- ggPCs(ggobj$ggcdobj[[1]])
    lapply(names(ggobj$ggcdobj), function(cCode) {
        flushprint(paste("ggpages", cCode))
        thisgg <- ggobj$ggcdobj[[cCode]]
        if(cCode %in% dualpage) {
            plot.new()
            vps <- vpFullpage()
            pushViewport(vps[[1]])
            lightplotgrid(cCode, win = FALSE, daysback = daysback, smooth = F)
            popViewport()
            addBanner()
            addHeading("Country yield curve history", "A 3 year graphical historic chart of the \npar yield curve")
            addDate()
            plot.new()
            vps <- vpFullpage()
            pushViewport(vps[[1]])
            lightboxgrid(cCode, win = FALSE, daysback = daysback)
            popViewport()
            addBanner()
            addHeading("Cheap dear by instrument", "Background violin plot of up to three years history\n and foreground boxplot of past 6 months")
            addDate()
        } else {
            plot.new()
            vps <- vp2Cols()
            pushViewport(vps[[1]])
            lightplotgrid(cCode, win = FALSE, daysback = daysback, smooth = F)
            popViewport()
            pushViewport(vps[[2]])
            lightboxgrid(cCode, win = FALSE, daysback = daysback)
            popViewport()
            addBanner()
            addHeading("Country yield curve, and cheap dear by instrument", "A three year history of the yield curve \ncoupled with the residual historical cheap dear of each instrument")
            addDate()
        }
        plot.new()
        vps <- vp4then1()
        if(length(pcobj$pcshortplots) > 0) {
            pushViewport(vps[[4]])
            print(pcobj$pcshortplots[[1]], newpage = FALSE)
            popViewport()
        }
        if(length(pcobj$pcshortplots) > 1) {
            pushViewport(vps[[3]])
            print(pcobj$pcshortplots[[2]], newpage = FALSE)
            popViewport()
        }
        if(length(pcobj$pcshortplots) > 2) {
            pushViewport(vps[[2]])
            print(pcobj$pcshortplots[[3]], newpage = FALSE)
            popViewport()
        }
        if(length(pcobj$pcshortplots) > 3) {
            pushViewport(vps[[1]])
            print(pcobj$pcshortplots[[4]], newpage = FALSE)
            popViewport()
        }
        pushViewport(vps[[5]])
        print(thisgg$cdChart, newpage = FALSE)
        popViewport()
        addBanner()
        addHeading("Curve Sector cheap/dear", paste("Curves are analysed for which sectors are cheap or dear within ",
                                                    "the context of the past 2 years of history\n",
                                                    "The analysis takes into account the major dynamics of the market, ",
                                                    "namely levels, slopes, ", 
                                                    "spreads, and curvatures.\n",
                                                    "Individual bond idiosyncratic cheap dear is overlayed ",
                                                    "on the curve cheap dear with boxplots. White bars dotted and solid represent 1z and 2z respectively.", sep = ""))
        addDate()
    })
}


spreadpages <- function(spreads = list(c(2, 5), c(5, 10), c(7, 10), c(5, 20), c(2, 5, 10), c(2, 10), c(10, 15), 
                    c(10, 20), c(10, 30), c(5, 10, 15), c(5, 10, 30), c(10, 15, 30))) {
    plot.new()
    vps <- vp3by2()
    for(x in 1:6) {
        pushViewport(vps[[x]])
        par(new = TRUE, fig = gridFIG(), mar = c(1.25,1.5,1.25,0.5), oma = c(0, 0, 0, 0))
        spreadout(spreadget(ac, spreads[[x]]), plotfirst = FALSE, win = FALSE)
        upViewport()
    }
    addBanner()
    addHeading("Spread regressions", paste("The indicated spread in each country is multiple-regressed against the same spread for all other \n",
                                           "countries, of which the 3 best independent variables are found. The regression standard error z score\n",
                                           "is plotted, for the current day and for 21 days of history", sep = ""))
    plot.new()
    vps <- vp3by2()
    for(x in 7:12) {
        pushViewport(vps[[x - 6]])
        par(new = TRUE, fig = gridFIG(), mar = c(1.25,1.5,1.25,0.5), oma = c(0, 0, 0, 0))
        spreadout(spreadget(ac, spreads[[x]]), plotfirst = FALSE, win = FALSE)
        popViewport()
    }
    addBanner()
    addHeading("Spread regressions", paste("The indicated spread in each country is multiple-regressed against the same spread for all other \n",
                                           "countries, of which the 3 best independent variables are found. The regression standard error z score\n",
                                           "is plotted, for the current day and for 21 days of history", sep = ""))
}

        
hmpage <- function(ggobj) {
    plot.new()
    vps <- vp2Cols()
    pushViewport(vps[[1]])
    print(ggobj$heatmain, newpage = FALSE)
    popViewport()
    pushViewport(vps[[2]])
    print(ggobj$heatevol, newpage = FALSE)
    popViewport()
    addBanner()
    addHeading("Curve sector heatmaps", paste("Current cheap/dear of curve sectors by country, plus a 6 day evolution pattern\n", 
                                              "Note that these sector signals are optimized for use in a portfolio context; ", 
                                              "usage for single country vs country RV must be the subject of further analysis", sep = ""))
    addDate()
}


dailyPrePCs <- function(cCodes = cl, daysback = 15) {
    # get full PCAs first
    pp <- getPCAs(cCodes, 260 * 2, usedecay = FALSE, series = FALSE) # these parameters match quandata pcs. 
    pivotzs <- lapply(cCodes, function(cCode) {
        cpcs <- cbind(pp$pc1[, cCode], pp$pc2[, cCode], pp$pc3[, cCode]) # extract the pcs for this country
        cpcs <- apply(cpcs, 2, function(x) last(x / sd(x), daysback)) # z scores
        sdhours <- min(24, as.numeric(difftime(Sys.time(), as.POSIXct(paste(Sys.Date() - 1, "17:00:00")), units = "hours")))
        sdfactor <- sqrt(24/sdhours)
        cpcs[nrow(cpcs), ] <- cpcs[nrow(cpcs), ] * sdfactor # adjust by what time of day it is today
        colnames(cpcs) <- c("level", "slope", "curve") 
        return(cpcs)
    })
    names(pivotzs) <- cCodes
    meltzs <- melt(pivotzs)
    colnames(meltzs) <- c("hist", "pc", "z", "country")
    meltzs[, "hist"] <- as.numeric(as.Date(meltzs[, "hist"]) - Sys.Date()) # turn into number of days
    threshup <- meltzs[, "z"] >= 2
    threshdown <- meltzs[, "z"] <= -2
    meltzs["thresh"] <- rep(0, nrow(meltzs))
    meltzs[threshup, "thresh"] <- 2
    meltzs[threshdown, "thresh"] <- -2
    meltzs[, "thresh"] <- factor(meltzs[, "thresh"])
    meltzs[nrow(meltzs), "thresh"] <- 2
    maxz <- trunc(max(3, max(meltzs[, "z"])) + 1)
    g <- ggplot(meltzs, aes(x = hist, y = z, colour = thresh))
    g <- g + geom_hline(yintercept = c(-2, 0, 2), linetype = "dotted")
    g <- g + geom_bar(stat = "identity", fill = "white")
    g <- g + facet_grid(pc ~ country)    
    g <- g + coord_cartesian(ylim = c(-maxz, +maxz))
    g <- g + scale_colour_manual(values = c("limegreen", "grey", "tomato1"))
    #g <- g + scale_y_continuous(breaks = -maxz:maxz)
    g <- g + theme(axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   axis.ticks = element_blank(),
                   panel.grid.major.x = element_blank(),
                   panel.grid.major.y = element_blank(),
                   panel.grid.minor = element_blank(), 
                   legend.position = "none")
    plot(g)
}



dodo <- function(ggobj = ggHeat(plotheat = FALSE, usepcmat = FALSE, daysback = 5), daysback = 1) {
# do the whole daily
    a4pdf()
    hmpage(ggobj)
    spreadpages()
    ggpages(ggobj, daysback = daysback)
    dev.off()
}



    


