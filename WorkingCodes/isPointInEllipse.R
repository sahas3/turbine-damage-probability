# check if a point is inside an ellipse
# vectorized code
# can check if multiple points are inside a given ellipse

isPointInEllipse <- function(point, center, radius, angle)
{
    point = matrix(point, ncol = 2)
    pointEllipseCenterVec = sweep(point, 2, center, '-');

    # https://bioinfomagician.wordpress.com/2014/08/12/my-favorite-commands-part3-sweep-function-in-r/
    return(((cos(angle)*pointEllipseCenterVec[,1] + sin(angle)*pointEllipseCenterVec[,2])/radius[1])^2 +
        ((sin(angle)*pointEllipseCenterVec[,1] - cos(angle)*pointEllipseCenterVec[,2])/radius[2])^2 <= 1)
}