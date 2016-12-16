lightStrikeSimulator <- function(turbineData, lightningData, 
                                 numStrikes = 200, # number of lightning strikes to simulate
                                 seedValue = 3,
                                 generateNewSimulation = F # set to true to generate a new lightning strike Data
) 
{
    # Simulator designed from second data set acquired from GE 
    # Courtesy of GE UGrad Team
 
    if (!generateNewSimulation)
    {
        set.seed(seedValue)
    }
    
    columnNames <- c('lat','lon','peakAmpAbs','majorRadOrig','minorRadOrig','angle','numSensors')
    simulationData <- matrix(nrow=numStrikes,ncol=length(columnNames))
    simulationData <- as.data.frame(simulationData)
    colnames(simulationData) <- columnNames
    
    for (variable in columnNames) {
        if (variable == 'lat') {
            simulationData['lat'] <- runif(numStrikes,min(turbineData['lat']),max(turbineData['lat']))
        } else if (variable == 'lon') {
            simulationData['lon'] <- runif(numStrikes,min(turbineData['lon']),max(turbineData['lon']))
        } else if (variable == 'peakAmpAbs') {
            amp.fit.lnorm <- fitdist(lightningData$peakAmpAbs,'lnorm')
            estimate <- amp.fit.lnorm$estimate
            simulationData['peakAmpAbs'] <- rlnorm(numStrikes,estimate[1],estimate[2])
        } else if (variable == 'majorRadOrig') {
            simulationData['majorRadOrig'] <- sample(round(min(lightningData['majorRadOrig'])*1000):
                                        round(max(lightningData['majorRadOrig'])*1000),numStrikes,replace=TRUE)/1000
        } else if (variable == 'minorRadOrig') {
            simulationData['minorRadOrig'] <- sample(round(min(lightningData['minorRadOrig'])*1000):
                                        round(max(lightningData['minorRadOrig'])*1000),numStrikes,replace=TRUE)/1000
        } else if (variable == 'angle') {
            simulationData['angle'] <- sample(0:180,numStrikes,replace=TRUE)
        } else if (variable == 'numSensors') {
            sensors.fit.nbinom <- fitdist(lightningData$numSensors,'nbinom')
            estimate <- sensors.fit.nbinom$estimate
            simulationData['numSensors'] <- rnbinom(numStrikes,estimate[1],mu=estimate[2])
        } 
    }
    return(simulationData)
}