# Paul GOUJON & Stéphane LOUIS
# UTC - SY09 - TP4

# Post probability calculation (logistic regression case)

# A small function used to calculate the posteriori probability of an
# individuals matrix, depending the beta parameter

post.pr = function (beta, X) {
    p = (exp(t(beta) %*% t(X))) / (exp(t(beta) %*% t(X)) + 1);
    return (p);
}

