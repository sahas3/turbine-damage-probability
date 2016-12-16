# converts latitude-longitude data to cartesian

latLon2Cartesian <- function(longitude, latitude)
{
    # refer : https://stackoverflow.com/questions/36488866/latitude-and-longitude-x-and-y-and-differences-when-plotting-them-geosphere-a?rq=1
    # http://spatialreference.org/ref/?search=texas
    pos <- SpatialPoints(data.frame(longitude, latitude), proj4string = CRS("+proj=longlat")); # +init=epsg:4326"));


    # pos <- spTransform(pos, CRS("+init=epsg:3663"));
    # zoneIdx = long2UTM(longitude);
    # pos <- spTransform(pos, CRS(paste("+proj=utm +zone=", as.character(zoneIdx), " +datum=WGS84", sep='')));

    pos <- spTransform(pos, CRS("+proj=utm +zone=14 +datum=WGS84"));
    # https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf

    pos@coords <- pos@coords/1000; # *0.0003048; # /1000; # in KMs?
    return(pos@coords);
}