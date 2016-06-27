# Paul GOUJON & Stéphane LOUIS
# UTC - SY09 - TP4

# Error rate calculation function

# Functions used to calculate the error rate of a discrimination and to estimate
# its confidence interval

error.rate = function (ztst, zpred) {
    # browser();
    ztst = as.vector(ztst);
    zpred = as.vector(zpred);
    rate = length(zpred[which(zpred != ztst)]) / length(zpred);
    return (rate);
}

confidence.interval = function(rate_mean, n) {
    u = 1.9600;
    sup = rate_mean + u * sqrt(rate_mean * (1 - rate_mean) / n);
    inf = rate_mean - u * sqrt(rate_mean * (1 - rate_mean) / n);
    l = NULL;
    l$inf = inf;
    l$sup = sup;
    return (l);
}

