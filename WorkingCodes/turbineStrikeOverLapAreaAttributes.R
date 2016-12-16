# computes attributes for Turbines based on area of over lap between the light strike ellipse and attractive region of the turbines

turbineStrikeOverLapAreaAttributes <- function(lightStrikeCenters,
                                               lightStrikeCentersLonLat,
                                               lightStrikeRadius,
                                               lightStrikeArea,
                                               lightStrikeAngle,
                                               lightStrikeAmplitude,
                                               turbineCenters,
                                               turbineCentersLonLat,
                                               turbineRadius,
                                               turbineClass,
                                               peakAmpThresh)
{
    numPoints = 200;
    # number of points generated in each dimension as probable strike locations
    # total points generated is numPoints^2 since 2D plane

    numTurbines = nrow(turbineCenters)

    numLightStrikes <- length(lightStrikeAmplitude)

    # find lightning strikes in dataset with high amplitude
    highPeakAmpIndx = which(lightStrikeAmplitude >= peakAmpThresh)
    # highPeakAmpIndx = which(lightStrikeAmplitude >= peakAmpThres)
    numHighPeakAmps = length(highPeakAmpIndx)


    # find range in longitude and latitude to create a 2D grid
    lonRange = range(rbind(transpose(turbineCentersLonLat[,1]), transpose(lightStrikeCentersLonLat[,1]))) + c(-0.05, 0.05)
    latRange = range(rbind(transpose(turbineCentersLonLat[,2]), transpose(lightStrikeCentersLonLat[,2]))) + c(-0.05, 0.05)

    # create the 2D grid
    lonGrid = seq(lonRange[1], lonRange[2], length.out = numPoints)
    latGrid = seq(latRange[1], latRange[2], length.out = numPoints)

    # get the points
    lonlatPoints = data.matrix(expand.grid(lonGrid, latGrid))
    xyPoints <- latLon2Cartesian(lonlatPoints[,1], lonlatPoints[,2])

    ## Initialize variables for storage

    # strikeOverLapAreaMax = rep(0, numTurbines)
    # strikeOverLapAreaAmpMax = rep(0, numTurbines)
    strikeOverLapAreaSum = rep(0, numTurbines)
    strikeOverLapAreaAmpSum = rep(0, numTurbines)

    turbineAreaIndx = matrix(0, numTurbines, numPoints^2)

    # estimate turbine area by finding how many points are inside the attractive region of a turbine
    for (iTurbine in 1:numTurbines)
    {
        turbineAreaIndx[iTurbine, ] = isPointInEllipse(xyPoints, turbineCenters[iTurbine,], turbineRadius, 0)
    }
    turbineArea = rowSums(turbineAreaIndx)

    # estimate ellipse area by finding how many points are inside the ellipse
    lightStrikeAreaCount = rep(0, numLightStrikes)
    for (iLightStrike in 1:numLightStrikes)
    {
        strikeAreaIndx = isPointInEllipse(xyPoints, lightStrikeCenters[iLightStrike,],
                                          lightStrikeRadius[iLightStrike,], lightStrikeAngle[iLightStrike])
        lightStrikeAreaCount[iLightStrike] = sum(strikeAreaIndx);
    }
    # check if estimation is good
    cat("Correlation between actual and estimated area for light strike ellipses is",
        cor(lightStrikeArea, lightStrikeAreaCount), '\n')

    closeLightStrikeIndx = matrix(F, numHighPeakAmps, numTurbines)
    iLightStrikeTmp = 0

    sumLightStrikeAmplitude <- sum(lightStrikeAmplitude)
    for (iLightStrike in highPeakAmpIndx)
    {
        iLightStrikeTmp = iLightStrikeTmp + 1
        peakAmpVal = lightStrikeAmplitude[iLightStrike] # /sumLightStrikeAmplitude

        # find points which are inside the ellipse
        strikeAreaIndx = isPointInEllipse(xyPoints, lightStrikeCenters[iLightStrike,],
                                          lightStrikeRadius[iLightStrike,], lightStrikeAngle[iLightStrike])

        for (iTurbine in 1:numTurbines)
        {
            # find points inside both ellipse and attractive region --> gives area of overlap
            overlapArea = sum(strikeAreaIndx & turbineAreaIndx[iTurbine, ])

            closeLightStrikeIndx[iLightStrikeTmp, iTurbine] = overlapArea > 0

            # strikeOverLapAreaAmpMax[iTurbine] = max(strikeOverLapAreaAmpMax[iTurbine],
            #                                         peakAmpVal * overlapArea/lightStrikeAreaCount[iLightStrike])
            #
            # strikeOverLapAreaMax[iTurbine] = max(strikeOverLapAreaMax[iTurbine],
            #                                      overlapArea/lightStrikeAreaCount[iLightStrike])
            #
            # strikeOverLapAreaAmpSum[iTurbine] = strikeOverLapAreaAmpSum[iTurbine] +
            #     peakAmpVal * overlapArea/lightStrikeAreaCount[iLightStrike]
            #
            # strikeOverLapAreaSum[iTurbine] = strikeOverLapAreaSum[iTurbine] +
            #     overlapArea/lightStrikeAreaCount[iLightStrike]

            strikeOverLapAreaAmpSum[iTurbine] = strikeOverLapAreaAmpSum[iTurbine] +
                peakAmpVal * overlapArea/turbineArea[iTurbine] # (lightStrikeAreaCount[iLightStrike] * turbineArea[iTurbine])

            strikeOverLapAreaSum[iTurbine] = strikeOverLapAreaSum[iTurbine] +
                overlapArea/turbineArea[iTurbine] # (lightStrikeAreaCount[iLightStrike] * turbineArea[iTurbine])
        }
    }


    # strikeOverLapAreaSum = strikeOverLapAreaSum/turbineArea
    # strikeOverLapAreaMax = strikeOverLapAreaMax/turbineArea
    # strikeOverLapAreaAmpSum = strikeOverLapAreaAmpSum/turbineArea
    # strikeOverLapAreaAmpMax = strikeOverLapAreaAmpMax/turbineArea


    #######################################################################################################################
    #######################################################################################################################

    strikeCounter = colSums(closeLightStrikeIndx)
    # find strike counter by evaluating if overlap exist or not


    #######################################################################################################################
    #######################################################################################################################

    # Aditi's Idea (?)
    # find distance between all pairs of light strike ellipse center and turbine locations
    distTurbineLightStrike = rdist(lightStrikeCenters[highPeakAmpIndx, ], turbineCenters)

    strikeProximity = rep(0, numTurbines)
    highPeakAmpVals = lightStrikeAmplitude[highPeakAmpIndx]

    for (iTurbine in 1:numTurbines)
    {
        # strikeProximity[iTurbine] = sum(distTurbineLightStrike[closeLightStrikeIndx[, iTurbine], iTurbine] /
        #                                     highPeakAmpVals[closeLightStrikeIndx[, iTurbine]])
        strikeProximity[iTurbine] = sum(highPeakAmpVals[closeLightStrikeIndx[, iTurbine]] /
                                            distTurbineLightStrike[closeLightStrikeIndx[, iTurbine]])
        # strikeProximity[iTurbine] = sum(highPeakAmpVals[closeLightStrikeIndx[, iTurbine]] *
        #                                     distTurbineLightStrike[closeLightStrikeIndx[, iTurbine]])
    }


    #######################################################################################################################
    #######################################################################################################################

    # find the ROC values
    overlapAreaAttrData <- cbind(
        strikeOverLapAreaSum
        # , strikeOverLapAreaMax
        , strikeOverLapAreaAmpSum
        # , strikeOverLapAreaAmpMax
        , strikeCounter
        , strikeProximity
        )

    overlapAreaAttrPerf <- colAUC(overlapAreaAttrData, turbineClass, plotROC = F)
    print(overlapAreaAttrPerf)

    # maxROCIdx <- which(overlapAreaAttrPerf == max(overlapAreaAttrPerf))
    #
    # strikeOverLapArea <- overlapAreaAttrData[, maxROCIdx]

    # return(list(
    #     strikeOverLapArea = strikeOverLapArea,
    #     strikeCounter = strikeCounter,
    #     strikeProximity = strikeProximity
    # ))


    # return the generated attribute values
    return(list(
        strikeOverLapAreaSum = strikeOverLapAreaSum,
        # strikeOverLapAreaMax = strikeOverLapAreaMax,
        strikeOverLapAreaAmpSum = strikeOverLapAreaAmpSum,
        # strikeOverLapAreaAmpMax = strikeOverLapAreaAmpMax,
        strikeCounter = strikeCounter,
        strikeProximity = strikeProximity
    ))
}