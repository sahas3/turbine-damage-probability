# simulates ground truth for turbine damage

simulateTurbineDamage <- function(lightStrikeCenters, lightStrikeCentersLonLat, lightStrikeRadius, lightStrikeCovMat,
                                  lightStrikeAngle, lightStrikePeakAmp,
                                  turbineCenters, turbineCentersLonLat,
                                  peakAmpThres, distThresh,
                                  plotFlag = F)
{
    set.seed(3)
    # if amplitude is higher than the threshold then it can damage turbines
    highPeakAmpIndx = which(lightStrikePeakAmp >= peakAmpThres)

    numHighPeakAmps = length(highPeakAmpIndx)
    strikeLoc = matrix(0, numHighPeakAmps, 2)


    for (iLightStrike in 1:numHighPeakAmps)
    {

        # generate a strike location inside the ellipse based on the bivariate gaussian distribution of the ellipse

        covMat = matrix(lightStrikeCovMat[highPeakAmpIndx[iLightStrike], ], ncol = 2)
        validStrikeLoc = F
        while(!validStrikeLoc)
        {
            strikeLoc[iLightStrike, ] = mvrnorm(1, lightStrikeCenters[highPeakAmpIndx[iLightStrike], ], covMat)
            validStrikeLoc = isPointInEllipse(strikeLoc[iLightStrike, ], lightStrikeCenters[highPeakAmpIndx[iLightStrike],],
                                              lightStrikeRadius[highPeakAmpIndx[iLightStrike],],
                                              lightStrikeAngle[highPeakAmpIndx[iLightStrike]])
        }
    }

    tempPos <- cartesian2LatLon(strikeLoc[, 1]*1000, strikeLoc[, 2]*1000); # input to this function has to be in meters
    strikeLocData = data.frame(lon = tempPos[,1], lat = tempPos[,2], x = strikeLoc[,1], y = strikeLoc[,2])


    # compute euclidean distance between every pair of turbine and light strikes based on their positions in cartesian coordinate
    distTurbineLightStrike = rdist(strikeLoc, turbineCenters)
    # if distance is less than threshold then it can damage turbine
    closeStrikeIndx = distTurbineLightStrike <= distThresh

    badIndx = apply(closeStrikeIndx, 2, function(x) sum(x)>0)

    classData = ifelse(badIndx == T, 'B', 'G')
    # label the turbine as damaged if there is a light strike close to it and as well as has high amplitude
    classData = as.factor(classData)


    if (plotFlag)
    {
        hfigLightStrikeTurbineSim <-
            ggplot() +
            geom_point(data = NULL, aes(lightStrikeCentersLonLat[,1], lightStrikeCentersLonLat[,2], color = 'blue'),
                       pch = 21, show.legend = T) + # light strikes in blue
            geom_point(data = NULL, aes(turbineCentersLonLat[,1], turbineCentersLonLat[,2], color = classData), size = 3) +
            scale_color_manual(values = c('red', 'blue', 'green'), name = 'Locations',
                               labels = c('Damaged Turbines', 'Light Strike Ellipse Centers', 'Good Turbines')) +
            labs(x = 'Longitude', y = 'Latitude', # size = 'Probability of Damage', fill = 'Probability of Damage',
                 colour = 'Ground Truth') +
            ggtitle('Simulated Ground Truth Data') +
            theme(legend.position = 'bottom', legend.title = element_text(size = 20), legend.text = element_text(size = 20),
                  plot.title = element_text(size = 20, hjust = 0.5), axis.title = element_text(size = 20),
                  axis.text = element_text(size = 20), legend.box = 'vertical', legend.key.size = unit(0.5, 'cm'))

        print(hfigLightStrikeTurbineSim)

        # plotLightStrikeDamageFlag = F
        # if (plotLightStrikeDamageFlag)
        # {
        #     # get datapoints for drawing ellipses around Lightning Strike center points
        #     ellipseData <- getEllipseData(lightStrikeCenters[highPeakAmpIndx, ], lightStrikeRadius[highPeakAmpIndx, ],
        #                                   StormData$angle[highPeakAmpIndx])
        #     tempPos <- cartesian2LatLon(ellipseData$x*1000, ellipseData$y*1000); # input to this function has to be in meters
        #     ellipseData$lon <- tempPos[,1];
        #     ellipseData$lat <- tempPos[,2];
        #
        #     hfigLightStrikeDamage = ggplot(TurbineData, aes(lon, lat, color = Class)) + geom_point() +
        #         # turbine locations colored based on damaged or not
        #         scale_color_manual(values = c('red', 'green'), name = 'Turbine Conditions', labels = c('Damaged', 'Not Damaged')) +
        #         geom_point(inherit.aes = F, data = strikeLocData, aes(lon, lat), col = 'black') +
        #         geom_path(inherit.aes = F, data = ellipseData, aes(lon, lat, group = grp))
        #
        #     print(hfigLightStrikeDamage)
        # }
    }

    # ggplot() +
    #     geom_point(data = NULL, aes(xyPoints[,1], xyPoints[,2])) +
    #     geom_point(data = NULL, aes(strikeLoc[,1], strikeLoc[,2]), col = 'green') +
    #     geom_point(data = NULL, aes(turbineCenters[,1], turbineCenters[,2]), col = 'blue') +
    #     geom_point(data = NULL, aes(lightStrikeCenters[,1], lightStrikeCenters[,2]), col = 'red')

    cat(sum(classData == 'B')/nrow(turbineCenters)*100, '% bad turbines simulated.\n', sep = '')

    return(classData)
}