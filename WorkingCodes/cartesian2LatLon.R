# converts cartesian data for utm zone 14 to latitude/longitude form

cartesian2LatLon <- function(x, y)
{
    # x and y has to be in meter units

    # refer : https://stackoverflow.com/questions/36488866/latitude-and-longitude-x-and-y-and-differences-when-plotting-them-geosphere-a?rq=1
    # http://spatialreference.org/ref/?search=texas
    pos <- SpatialPoints(data.frame(x, y), proj4string = CRS("+proj=utm +zone=14 +datum=WGS84")); # +init=epsg:4326"));

    # pos <- spTransform(pos, CRS("+init=epsg:3663"));
    # zoneIdx = long2UTM(longitude);
    # pos <- spTransform(pos, CRS(paste("+proj=utm +zone=", as.character(zoneIdx), " +datum=WGS84", sep='')));

    pos <- spTransform(pos, CRS("+proj=longlat"));
    # https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf

    pos@coords <- pos@coords;
    return(pos@coords);
}