# reads Excel Dataset into R workspace

dataSetNew = T # flag for reading old or new lightning data set

# change this to your path

if (dataSetNew)
{
    pathToData <- './ProjectData/GE_Lightning/GE Renewables - Lightning Report 12-2015 thru 3-14-2016.xlsx';
    colNames = c("date", "time", "lat", "lon", "peakAmp", "peakAmpAbs", "chiSqVal", "majorRadOrig", "minorRadOrig",
                 "angle", "numSensors");
    # read the lightning data
    LightningData <- read.xlsx(pathToData, sheetName = 'Data1');
} else {
    source('./Codes/standardizeConfidenceLevel.R')

    pathToData <- './ProjectData/GE_Lightning/INCITE Lab GE Lightning Strike Data 2016-09-13.xlsx';
    colNames = c("date", "time", "peakAmp", "lat", "lon", "confInt", "majorRadOrig", "minorRadOrig", "angle");
    # read the lightning data
    LightningData <- read.xlsx(pathToData, sheetName = 'Data');
}


# rename the column names
colnames(LightningData) <- colNames

# list types for each attribute
sapply(LightningData, class)

# Express radius in KMs for old Data Set, new data set is in KMS already
if (!dataSetNew)
{

    LightningData$majorRadOrig <- LightningData$majorRadOrig/1000;
    LightningData$minorRadOrig <- LightningData$minorRadOrig/1000;

    # Obtain ellipses for standardized confidence interval of 99%
    tempRad = standardizeConfidenceLevel(cbind(LightningData$majorRadOrig, LightningData$minorRadOrig),
                                         LightningData$angle, LightningData$confInt/100, 0.99);
    LightningData$majorRad = tempRad[,1];
    LightningData$minorRad = tempRad[,2];

    mainAttributes = c('peakAmp', 'majorRad', 'minorRad', 'area', 'angle')

} else {

    LightningData$majorRad = LightningData$majorRadOrig;
    LightningData$minorRad = LightningData$minorRadOrig;

    mainAttributes = c('peakAmp', 'majorRad', 'minorRad', 'area', 'angle', 'chiSqVal', 'numSensors')

}

# ensure major radius > minor radius
properRadius = LightningData$majorRad >= LightningData$minorRad
tmpVal1 = LightningData$minorRad[!properRadius]
LightningData$minorRad[!properRadius] = LightningData$majorRad[!properRadius]
LightningData$majorRad[!properRadius] = tmpVal1
LightningData$angle[!properRadius] = wrapToPi(LightningData$angle[!properRadius] + pi/2, pi)



LightningData$ellipseParams = estimateBivariateGaussianParams(cbind(LightningData$majorRad,
                                                                    LightningData$minorRad), LightningData$angle, LightningData$confInt/100)

LightningData$covMat = LightningData$ellipseParams[, 1:4]
# Area of each ellipse
LightningData$area = pi*LightningData$majorRad*LightningData$minorRad;
LightningData$areaOrig = pi*LightningData$majorRadOrig*LightningData$minorRadOrig;

# removing timestamps from Date Column
LightningData$date <- as.Date(LightningData$date);
LightningData$date <- as.factor(LightningData$date);


# removing days from timestamps
tempTime = unlist(strsplit(as.character(LightningData$time), ' '));
LightningData$time = tempTime[seq(2, length(tempTime), 2)];

if (dataSetNew)
{
    goodChiSqVal = qchisq(0.99, LightningData$numSensors)
    goodDataVal = which(goodChiSqVal > LightningData$chiSqVal)
    LightningData = LightningData[goodDataVal, ]
}

# read the turbine locations data
pathToData <- './ProjectData/GE_Lightning/INCITE Lab GE Lightning Strike Data 2016-09-13.xlsx';
TurbineData <- read.xlsx(pathToData, sheetName = 'Turbine Locations');
colnames(TurbineData) <- c("serial", "lon", "lat");

# list types for each attribute
sapply(TurbineData, class)

# convert to Cartesian coordiantes from Latitude/Longitude
tempPos <- latLon2Cartesian(LightningData$lon, LightningData$lat);
LightningData$x <- tempPos[,1];
LightningData$y <- tempPos[,2];

tempPos <- latLon2Cartesian(TurbineData$lon, TurbineData$lat);
TurbineData$x <- tempPos[,1];
TurbineData$y <- tempPos[,2];


if (!dataSetNew)
{
    # grouping based on times
    datestormIdx = levels(LightningData$date);
    dayIndx = matrix(0, length(datestormIdx), length(LightningData$date));
    for (iLevel in 1:length(datestormIdx))
    {
        dayIndx[iLevel, ] = LightningData$date == datestormIdx[iLevel]; # indices for the nth day
    }
    LightningData$stormIdx = rep(0, length(LightningData$date));
    LightningData$stormIdx[dayIndx[1,] | dayIndx[2,]] = 1;
    LightningData$stormIdx[dayIndx[3,] | dayIndx[4,]] = 2;
    LightningData$stormIdx[as.logical(dayIndx[5,])] = 3;
    LightningData$stormIdx[as.logical(dayIndx[6,])] = 4;
    LightningData$stormIdx = as.factor(LightningData$stormIdx);
}