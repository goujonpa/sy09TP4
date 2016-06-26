# Paul GOUJON & Stéphane LOUIS
# UTC - SY09 - TP4

# Quadratic Logistic Regression

# The aim of that function is to generate the quadratic individuals ensemble
# before applying the binary logistic regression to it

source("logb.R");
source("generateQuadX.R");

quad_log.app = function(Xapp, zapp, intercept, threshold) {

    # type checking
    Xapp = as.matrix(Xapp); # n x p matrix
    zapp = as.matrix(zapp); # n x 1 matrix
    intercept = as.logical(intercept); # T/F
    threshold = as.numeric(threshold); # numeric

    final_Xapp = generateQuadX(Xapp);

    # logistic regression
    l = bin_log.app(final_Xapp, zapp, intercept, threshold);
    return (l);
}

