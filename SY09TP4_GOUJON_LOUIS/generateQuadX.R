# Paul GOUJON & Stéphane LOUIS
# UTC - SY09 - TP4

# This function generates a quadratic regression dataset

generateQuadX = function(X) {
    final_X = as.matrix(X);
    for (i in 1:ncol(X)) {
    for (j in 1:ncol(X)) {
        if (j <= i) {
            final_X = cbind(final_X, (X[,i] * X[,j]));
        }
    }}
    return (final_X);
}

