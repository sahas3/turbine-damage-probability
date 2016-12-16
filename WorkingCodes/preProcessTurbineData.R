# pre-processes TurbineData provided by GE in a form usable by the codes written
# adds cartesian coordinates for the turbine locations
preProcessTurbineData <- function(TurbineData)
{
    colnames(TurbineData) <- c("serial", "lon", "lat");

    # convert to Cartesian coordiantes from Latitude/Longitude
    tempPos <- latLon2Cartesian(TurbineData$lon, TurbineData$lat);
    TurbineData$x <- tempPos[,1];
    TurbineData$y <- tempPos[,2];

    return(TurbineData)
}