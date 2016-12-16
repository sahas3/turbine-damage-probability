# pre-processes LightingData provided by GE in a form usable by the codes written
# this is based on the excel sheet provided for the new Lightning Data Set
# 1. ensures major radius > mnor radius
# 2. estimates covariance matrix
# 3. computes area of ellipses
# 4. adds cartesian coordinates for ellipse centers

preProcessLightningData <- function(LightningData)
{

    colNames = c("lat", "lon", "peakAmp", "majorRadOrig", "minorRadOrig",
                 "angle", "numSensors");

    # rename the column names
    colnames(LightningData) <- colNames

    LightningData$majorRad = LightningData$majorRadOrig;
    LightningData$minorRad = LightningData$minorRadOrig;


    # ensure major radius > minor radius
    properRadius = LightningData$majorRad >= LightningData$minorRad
    tmpVal1 = LightningData$minorRad[!properRadius]
    LightningData$minorRad[!properRadius] = LightningData$majorRad[!properRadius]
    LightningData$majorRad[!properRadius] = tmpVal1
    LightningData$angle[!properRadius] = wrapToPi(LightningData$angle[!properRadius] + pi/2, pi)

    # get covariance matrix
    LightningData$ellipseParams = estimateBivariateGaussianParams(cbind(LightningData$majorRad,
                                                                        LightningData$minorRad), LightningData$angle, 0.99)

    LightningData$covMat = LightningData$ellipseParams[, 1:4]

    # Area of each ellipse
    LightningData$area = pi*LightningData$majorRad*LightningData$minorRad;
    LightningData$areaOrig = pi*LightningData$majorRadOrig*LightningData$minorRadOrig;

    # convert to Cartesian coordiantes from Latitude/Longitude
    tempPos <- latLon2Cartesian(LightningData$lon, LightningData$lat);
    LightningData$x <- tempPos[,1];
    LightningData$y <- tempPos[,2];

    return(LightningData)
}