# generates simulated data sets to be used later

# set up parallel processing
clusters <- makeCluster(2) # makeCluster(detectCores()/2)
registerDoParallel(clusters)

set.seed(2007)

# randomly pick threshold for peak amplitude between 80% and 90% of the max peak amplitude
# peakAmpThresh = runif(1, min(LightningData$peakAmpAbs), 0.8*max(LightningData$peakAmpAbs))
peakAmpThreshGnd = runif(1, 30, 50)
# print(peakAmpThres)

# randomly pick threshold for distance between the turbine and light strike centers
distThreshGnd = runif(1, 0.3, 0.6)
# print(distThresh)
# attractive radius of turbines are 3*(height of turbine + radius of blades)
# for GE turbines 3*(100 + 50) = 450 ---> so pick some value randomly in the [2 4]*(height of turbine + radius of blades) range

foreach(iDataSet = 11:12, .packages = packageNames) %dopar% {
    LightSimData <- preProcessLightningData(
        lightStrikeSimulator(TurbineData, LightningData, numStrikes = 500, generateNewSimulation = T))
    # simulate lightning strike data

    # generate attributes for the tubrine data set based on the generated lightning strike
    dataValues <- generateClassifierAttributes(LightSimData, TurbineData, NULL, peakAmpThreshGnd = peakAmpThreshGnd,
                                               distThreshGnd = distThreshGnd)

    # store the data set
    saveRDS(dataValues, paste('./DataSets/dataValues', iDataSet, '.RData', sep = ''))
}


for (iDataSet in 11:12)
{
    # read the data sets generated and plot the ROC values for attributes along with % of damaged turbines simulated
    dataValues <- readRDS(paste('./DataSets/dataValues', iDataSet, '.RData', sep = ''))
    cat('Bad turbines simulated', sum(dataValues['turbineClass'] == 'B')/166*100, '%')
    predictorAttributes <- names(dataValues)
    predictorAttributes <- predictorAttributes[predictorAttributes != 'turbineClass']
    predictorAttributesPerf <- predictorAttributeROC(dataValues, dataValues[['turbineClass']], predictorAttributes, dataStr = '')
    plotROCs(predictorAttributesPerf, legendStr = 'Predictor Attributes', titleStr1 = '')
}

stopCluster(clusters)

