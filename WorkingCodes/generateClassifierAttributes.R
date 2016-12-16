# generate all attributes for the turbines

generateClassifierAttributes <- function(StormData, # lightning data set : data-frame with columns
                                         #
                                         TurbineData, # turbine data set : data-frame with columns lot, lat, x, y
                                         groundTruthDataClass = NULL, # ground truth data for Turbine Damage
                                         # in absence of ground truth simulate the ground truth
                                         peakAmpThresh = 20, # peak amplitude threshold for generating attributes
                                         distThresh = 0.45, # distance threshold for generating attributes
                                         peakAmpThreshGnd = runif(1, 30, 50), # peak amplitude threshold for simulating ground truth
                                         distThreshGnd = runif(1, 0.3, 0.6)# distance threshold for simulating ground truth
                                         )
{

    # create variables for easy access to data
    # numLightStrikes = length(StormData$x); # number of light strikes
    lightStrikeCenters <- cbind(StormData$x, StormData$y) # cartesian co-ordinates
    lightStrikeCentersLonLat <- cbind(StormData$lon, StormData$lat) # longitude-latitude coordinates
    lightStrikeRadius <- cbind(StormData$majorRad, StormData$minorRad)


    turbineCenters <- cbind(TurbineData$x, TurbineData$y)
    turbineCentersLonLat <- cbind(TurbineData$lon, TurbineData$lat)
    turbineRadius = rep(distThresh, 2) # defines attractive region of the turbine


    if (is.null(groundTruthDataClass))
    {
    #### Simulate turbine damage if ground truth is not provided
    turbineClass <- simulateTurbineDamage(lightStrikeCenters, lightStrikeCentersLonLat, lightStrikeRadius,
                                          StormData$covMat, StormData$angle, StormData$peakAmp,
                                          turbineCenters, turbineCentersLonLat,
                                          peakAmpThreshGnd, distThreshGnd,
                                          plotFlag = F)
    } else {turbineClass <- groundTruthDataClass}

    #### Generate attribute values based on overlap of circle around turbines and light strike ellipses
    overlapAreaData <- turbineStrikeOverLapAreaAttributes(lightStrikeCenters,
                                                          lightStrikeCentersLonLat,
                                                          lightStrikeRadius,
                                                          StormData$area,
                                                          StormData$angle,
                                                          StormData$peakAmp,
                                                          turbineCenters,
                                                          turbineCentersLonLat,
                                                          turbineRadius,
                                                          turbineClass,
                                                          peakAmpThresh)

    #### Generate attribute values based on Bivariate Gaussian Probability Distribution
    probabilityData <- turbineStrikeProbabilityAttributes(lightStrikeCenters,
                                                          lightStrikeRadius,
                                                          StormData$angle,
                                                          StormData$peakAmp,
                                                          StormData$covMat,
                                                          turbineCenters,
                                                          turbineCentersLonLat,
                                                          turbineRadius,
                                                          turbineClass,
                                                          peakAmpThresh,
                                                          plotHeatMapFlag = F)

    # combine the attributes
    turbineDamageData <- cbind(data.frame(overlapAreaData), data.frame(probabilityData), turbineClass)
    return(turbineDamageData)
}
