turbineStrikeProbabilityAttributes <- function(lightStrikeCenters,
                                               lightStrikeRadius,
                                               lightStrikeAngle,
                                               lightStrikeAmplitude,
                                               lightStrikeCovMat,
                                               turbineCenters,
                                               turbineCentersLonLat,
                                               turbineRadius,
                                               turbineClass,
                                               peakAmpThresh,
                                               plotHeatMapFlag = F)
{
# computes attributes for Turbines based on strike location probability

    numPoints <- 200
    # number of points generated in each dimension as probable strike locations
    # total points generated is numPoints^2 since 2D plane

    numTurbines <- nrow(turbineCenters) # number of turbines in data set

    # find range in longitude and latitude to create a 2D grid consisting of probable strike locations
    lonRange = range(turbineCentersLonLat[,1]) + c(-0.005, 0.005)
    latRange = range(turbineCentersLonLat[,2]) + c(-0.005, 0.005)

    # create the 2D grid
    lonGrid = seq(lonRange[1], lonRange[2], length.out = numPoints)
    latGrid = seq(latRange[1], latRange[2], length.out = numPoints)

    # get the points
    lonlatPoints = data.matrix(expand.grid(lonGrid, latGrid))
    xyPoints <- latLon2Cartesian(lonlatPoints[,1], lonlatPoints[,2]) # convert to cartesian co-ordinate from latitude/longitude


    # initialize probability values for strike locations
    strikeProbLocMax = rep(0, numPoints^2)
    strikeProbLocSum = rep(0, numPoints^2)
    strikeProbLocAmpMax = rep(0, numPoints^2)
    strikeProbLocAmpSum = rep(0, numPoints^2)
    strikeBinomProbLoc = rep(1, numPoints^2)
    # strikeBinomProbLocAmp = rep(1, numPoints^2)

    # find lightning strikes in dataset with high amplitude
    highPeakAmpIndx = which(lightStrikeAmplitude >= peakAmpThresh)

    for (iLightStrike in highPeakAmpIndx)
    {
        # only consider the light strikes with high amplitude
        peakAmpVal = lightStrikeAmplitude[iLightStrike]

        # find strike locations in the 2D grid inside the current lightning ellipse
        pointsInStrikeEllipseIdx = which(isPointInEllipse(xyPoints, lightStrikeCenters[iLightStrike,],
                                                          lightStrikeRadius[iLightStrike,], lightStrikeAngle[iLightStrike]))

        # get inverse of the covariance matrix
        covMat = matrix(lightStrikeCovMat[iLightStrike, ], ncol = 2)
        covMatInv = ginv(covMat)
        scaleConst = (1/(2*pi*sqrt(det(covMat))))

        for (iPoint in pointsInStrikeEllipseIdx)
        {
            distFromCenter = xyPoints[iPoint, ] - lightStrikeCenters[iLightStrike,]

            # get the probability of lightning happening at a certain point
            strikeProbLocCur = scaleConst * exp(-0.5 * distFromCenter %*% covMatInv %*% transpose(distFromCenter))

            # find the probability values following different heuristics
            strikeProbLocMax[iPoint] = max(strikeProbLocMax[iPoint], strikeProbLocCur)
            strikeProbLocSum[iPoint] = strikeProbLocSum[iPoint] + strikeProbLocCur

            strikeProbLocAmpMax[iPoint] = max(strikeProbLocAmpMax[iPoint], strikeProbLocCur * peakAmpVal)
            strikeProbLocAmpSum[iPoint] = strikeProbLocAmpSum[iPoint] + strikeProbLocCur * peakAmpVal

            # this one assumes strike happens following a binomial distribution
            strikeBinomProbLoc[iPoint] = strikeBinomProbLoc[iPoint] * (1 - strikeProbLocCur)
            # strikeBinomProbLocAmp[iPoint] = strikeBinomProbLocAmp[iPoint] * (1 - strikeProbLocCur * peakAmpVal)
        }
    }

    # strikeProbLocNorm = strikeProbLoc/max(strikeProbLoc)
    # strikeProbLoc = strikeProbLoc/sum(strikeProbLoc)

    strikeBinomProbLoc = 1 - strikeBinomProbLoc
    # strikeBinomProbLocAmp = 1 - strikeBinomProbLocAmp


    # initialize attributes for turbines
    turbineStrikeProbMax = rep(0, numTurbines)
    turbineStrikeProbSum = rep(0, numTurbines)
    turbineStrikeProbAmpMax = rep(0, numTurbines)
    turbineStrikeProbAmpSum = rep(0, numTurbines)
    turbineStrikeBinomProb = rep(0, numTurbines)
    # turbineStrikeBinomProbAmp = rep(0, numTurbines)

    for (iTurbine in 1:numTurbines)
    {
        # find the points in the 2D grid which are inside the attractive region of a turbine
        pointsInTurbineRadius = isPointInEllipse(xyPoints, turbineCenters[iTurbine,], turbineRadius, 0)

        # assign probability value which is sum of probabilities at all those points inside attactive region
        # very crude way of integration actually
        turbineStrikeProbMax[iTurbine] = sum(strikeProbLocMax[pointsInTurbineRadius])
        turbineStrikeProbSum[iTurbine] = sum(strikeProbLocSum[pointsInTurbineRadius])
        turbineStrikeProbAmpMax[iTurbine] = sum(strikeProbLocAmpMax[pointsInTurbineRadius])
        turbineStrikeProbAmpSum[iTurbine] = sum(strikeProbLocAmpSum[pointsInTurbineRadius])

        turbineStrikeBinomProb[iTurbine] = max(strikeBinomProbLoc[pointsInTurbineRadius])
        # turbineStrikeBinomProbAmp[iTurbine] = max(strikeBinomProbLocAmp[pointsInTurbineRadius])

    }

    # get ROC values for each attribute generated
    strikeProbAttrData <- cbind(turbineStrikeProbMax, turbineStrikeProbSum, turbineStrikeProbAmpSum,
                                turbineStrikeProbAmpMax, turbineStrikeBinomProb)
    strikeProbAttrPerf <- colAUC(strikeProbAttrData, turbineClass, plotROC = F)
    print(strikeProbAttrPerf)

    # strikeBinomProbAttrData <- cbind(turbineStrikeBinomProbAmp, turbineStrikeBinomProb)
    # strikeBinomProbAttrPerf <- colAUC(strikeBinomProbAttrData, turbineClass, plotROC = F)
    # print(strikeBinomProbAttrPerf)

    # strikeProbLoc = strikeProbLocAmpSum

    # maxROCIdx <- which(strikeProbAttrPerf == max(strikeProbAttrPerf))
    # strikeProb <- strikeProbAttrData[, maxROCIdx]
    # strikeProbLoc <- strikeProbAttrData[, maxROCIdx]
    #
    # maxROCIdx <- which(strikeBinomProbAttrPerf == max(strikeBinomProbAttrPerf))
    # strikeBinomProb <- strikeBinomProbAttrData[, maxROCIdx]

    if (plotHeatMapFlag)
    {
        # make data frame to use ggplot
        gridPointsData = data.frame(x = xyPoints[,1], y = xyPoints[,2], lon = lonlatPoints[,1], lat = lonlatPoints[,2],
                                    strikeProb = strikeBinomProbLoc)
        # strikeProb = strikeBinomProbLoc)

        # plot heatmap
        plotProbabilityContours(TurbineData, gridPointsData, numPoints, turbineClass)
    }


    # return the generated attributes
    return(list(
        strikeProbSum = turbineStrikeProbSum,
        strikeProbMax = turbineStrikeProbMax,
        strikeProbAmpSum = turbineStrikeProbAmpSum,
        strikeProbAmpMax = turbineStrikeProbAmpMax,
        strikeBinomProb = turbineStrikeBinomProb
        # strikeBinomProbAmp = turbineStrikeBinomProbAmp
    ))
}