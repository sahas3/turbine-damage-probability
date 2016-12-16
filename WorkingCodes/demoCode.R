# demo code

set.seed(2007)

LightSimDataOld <- preProcessLightningData(
    lightStrikeSimulator(TurbineData, LightningData, numStrikes = 200, generateNewSimulation = T))
# training data set

LightSimDataNew <- preProcessLightningData(
    lightStrikeSimulator(TurbineData, LightningData, numStrikes = 200, generateNewSimulation = T))
# prediction to be done for this data

peakAmpThresh <- 20 # anything above 20kA peak amplitude can potentially damage turbines

distThresh <- 0.45 # radius of turbine attractive region in KMs


# UI backend will call lightStrikeOracle only
# user inputs are TurbineData, LightningDataOld (to be used for Training),
# groundTruthDataClass (if available or else will be simulated internally),
# LightningDataNew (for which prediction of damage is to be done),
# peakAmpThres and distThresh (to generate turbine attributes),
# modelFit (if a classifier is already available then just get predictions using that classifier model)

results <- lightStrikeOracle(TurbineData = TurbineData,
                             LightningDataOld = LightSimDataOld,
                             LightningDataNew = LightSimDataNew,
                             peakAmpThresh = peakAmpThresh,
                             distThresh = distThresh,
                             modelFit = NULL)

modelFit <- results$modelFit # best classifier model
predProbs <- results$predProbs # corresponding prediction probabilities
originalClassLabels <- results$originalClassLabels # ground truth data simulated (or provided) for the new Lightning Data

plotTurbineDamageProbability(TurbineData, predProbs, originalClassLabels)