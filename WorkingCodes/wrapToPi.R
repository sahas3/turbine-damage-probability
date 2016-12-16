# wraps angle between +pi to -pi
# can be used to wrap within any limits : pass center to be mean(limits)

wrapToPi <- function(a,a_center=0)
{
    a <- a %% (2*pi);
    j <- a > pi - a_center;
    a[j] <- a[j] - (2*pi);
    j <- a < a_center - pi;
    a[j] <- a[j] + 2*pi;
    return(a)
}