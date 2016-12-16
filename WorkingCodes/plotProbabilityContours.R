plotProbabilityContours <- function(TurbineData, gridPointsData, numPoints, classValue)
{
    # produces 3 different contour plots

    # set color function to plot heat map
    # red means high value : yellow means low value
    colfunc <- colorRampPalette(c("yellow", "red"))

    # set levels for contours
    nLevels = 10


    # poiData = data.frame(lon = rbind(transpose(TurbineData$lon), transpose(strikeLocData$lon)),
    #                      lat = rbind(transpose(TurbineData$lat), transpose(strikeLocData$lat)),
    #                      color = rbind(transpose(TurbineData$color), transpose(rep('black', length(strikeLoc[,1])))))


    ## Plot first type of contour plot
    filled.contour(lonGrid, latGrid, matrix(gridPointsData$strikeProb, ncol = numPoints),
                   plot.axes = points(TurbineData$lon, TurbineData$lat, col = TurbineData$color, pch = 16),
                   # plot.axes = points(poiData$lon, poiData$lat, col = poiData$color, pch = 16),
                   nlevels = nLevels, col = colfunc(nLevels),
                   plot.title = title(main = 'Contour Plot of Light Strike Probability scaled by Amplitude', xlab = 'Longitude',
                                      ylab = 'Latitude'))

    # # hfigContour = contour(xGrid, yGrid, matrix(gridPointsData$strikeProbLoc, ncol = numPoints))

    ## Plot 2nd type of contour plot

    # https://www.r-bloggers.com/using-2d-contour-plots-within-ggplot2-to-visualize-relationships-between-three-variables/
    # contour plots with region colored using continous variable strikeProbLoc
    hfigStikeProbRegion = ggplot(gridPointsData, aes(lon, lat, z = strikeProb)) +
        geom_tile(aes(fill = strikeProb)) +
        stat_contour(geom="polygon", aes(fill = ..level.., alpha = ..level..)) + # , bins = nLevels)) +
        # scale_fill_gradientn(colours = terrain.colors(nLevels)) +
        scale_fill_continuous(low="yellow", high = "red") +
        guides(alpha = "none") +
        coord_cartesian(xlim=range(gridPointsData$lon), ylim = range(gridPointsData$lat)) +
        # stat_contour(bins = 21) +
        xlab('Longitude') + ylab('Latitude') + guides(fill = guide_colorbar(title = 'strike probability')) +
        geom_point(inherit.aes = F, data = TurbineData, aes(lon, lat, col = classValue), pch = 16, size = 3)  +
        scale_color_manual(values = c('red', 'green'), name = 'Turbine Conditions',
                           labels = c('Damaged', 'Not Damaged'))

    print(hfigStikeProbRegion)

    ## Plot 3rd type of contour plot

    # contour plots with region colored using discrete levels of strikeProb
    gridPointsData$strikeProbLevels = cut(gridPointsData$strikeProb,
                                          # breaks = seq(min(gridPointsData$strikeProb), max(gridPointsData$strikeProb), length.out = nLevels + 1))
                                          breaks = seq(0, 1, 0.1))

    # Sort the segments in ascending order
    breaks <- levels(unique(gridPointsData$strikeProbLevels))
    hfigStikeProbRegion2 = ggplot() + geom_tile(data = gridPointsData, aes(lon, lat, strikeProb, fill = strikeProbLevels)) +
        geom_contour(color = 'white', alpha = 0.5) + theme_bw() + xlab('Longitude') +
        ylab('Latitude') + scale_fill_manual(values = colfunc(length(breaks)),
                                             # c('#35978f', '#80cdc1', '#c7eae5', '#f5f5f5',
                                             #                               '#f6e8c3', '#dfc27d', '#bf812d', '#8c510a',
                                             #                               '#543005', '#330000', '#325485'),
                                             name = 'Binomial Strike Probability', breaks = breaks, labels = breaks) +
        geom_point(inherit.aes = F, data = TurbineData, aes(lon, lat, col = classValue), pch = 16, size = 3)  +
        scale_color_manual(values = c('red', 'green'), name = 'Turbine Conditions', labels = c('Damaged', 'Good')) +
        ggtitle('Contour Plot of Lightning Strike Probability') +
        theme(legend.title = element_text(size = 20), legend.text = element_text(size = 20),
              plot.title = element_text(size = 20, hjust = 0.5), axis.title = element_text(size = 20),
              axis.text = element_text(size = 20))

    print(hfigStikeProbRegion2)
}

# ggsave('./Figures/binomHeatMap.png', width = 16, height = 9, type = 'cairo-png')