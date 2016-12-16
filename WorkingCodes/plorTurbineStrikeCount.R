# this code is not exactly correct : but useful to present the idea to audience
# only checks if the turbine location is inside any light strike ellipse
# does not check if there is overlap between ellipse and circle, even center of circle is not inside of the ellipse
# correct code is implemented in turbineStrikeOverLapArea.R --> this used for actual classification
# this strikeCounter value is only for presentation

# plots lightning strike counts for each turbine

plotTurbineStrikeCount <- function(LightningData,
                                   # Lightning Strike Data set with columns : x, y, majorRad, minorRad, stormIdx (this column only for old data set)
                                   TurbineData, # Turbine Data set has columns : x, y
                                   plotEllipseFlag = T, # flag to plot lightning strike ellipses
                                   newDataFlag = T # flag for indicating if Lightning Data is for new data set or old data set
)
{


    lightStrikeCenters <- cbind(LightningData$x, LightningData$y)
    lightStrikeRadius <- cbind(LightningData$majorRad, LightningData$minorRad)

    turbineCenters <- cbind(TurbineData$x, TurbineData$y)

    indxPoints = 1:10 # only plot the first 10 lightning strike

    if (!newDataFlag)
    {
        lightStrikeIndx = which(LightningData$stormIdx == 3)[indxPoints] # for old data set
    } else {
        lightStrikeIndx = which(LightningData$peakAmpAbs >= 25)[indxPoints] # for lighting data set generated using new data
    }

    numLightStrikes <- sum(!is.na(lightStrikeIndx))

    numTurbines <- nrow(turbineCenters)

    TurbineStrikeIdx <- matrix(0, numLightStrikes, numTurbines)
    iLightStrikeCount = 0
    for (iLightStrike in lightStrikeIndx)
    {
        iLightStrikeCount = iLightStrikeCount + 1
        # indicator for which ellipse contain each turbine
        TurbineStrikeIdx[iLightStrikeCount, ] = as.numeric(isPointInEllipse(turbineCenters,
                                                                       lightStrikeCenters[iLightStrike,], lightStrikeRadius[iLightStrike,],
                                                                       LightningData$angle[iLightStrike]));
    }

    # count the number of ellipses each turbine belongs to
    strikeCounter = colSums(TurbineStrikeIdx);


    # plot the strikeCounter values for each turbines
    hfigTurbineStrikeCount <- plotTurbineAttributes(TurbineData, strikeCounter,
                                                   'Plot of lightning strike counts for all turbine locations',
                                                   'Lightning Strike Count', showGndTruth = F, nLevels = max(strikeCounter)*2)

    if (plotEllipseFlag)
    {
        # plot the ellipses

        ellipseData <- getEllipseData(cbind(LightningData$x[lightStrikeIndx], LightningData$y[lightStrikeIndx]),
                                      cbind(LightningData$majorRad[lightStrikeIndx], LightningData$minorRad[lightStrikeIndx]),
                                      LightningData$angle[lightStrikeIndx]);
        tempPos <- cartesian2LatLon(ellipseData$x*1000, ellipseData$y*1000); # input to this function has to be in meters
        ellipseData$lon <- tempPos[,1];
        ellipseData$lat <- tempPos[,2];

        hfigTurbineStrikeCount <- hfigTurbineStrikeCount + geom_path(data = ellipseData,
                                                                     mapping = aes(lon, lat, group = grp), color = 'blue', inherit.aes = F)
    }

    print(hfigTurbineStrikeCount)


    # ggsave(file = "Figures/TurbineStrikeCount.png", width = 16, height = 9, type = "cairo-png");


}