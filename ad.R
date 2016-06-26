# Paul GOUJON & Stéphane LOUIS
# UTC - SY09 - TP4

# Discriminant analysis - validation function

# Considered case : class number g = 2

# The aim of the following function is to use the discriminant analysis model
# from the parameters we determined previously, to make the measurements we need
# and validate it

# verbose option
verbose = F;

library(MASS); # necessary for ginv
source("nba.R");
source("mvdnorm.R");

ad.val = function(Xapp, zapp, Xtst, method) {
    # get parameters
    Xapp = as.matrix(Xapp);
    zapp = as.matrix(zapp);
    Xtst = as.matrix(Xtst);
    method = as.character(method);
    l = nba.app(Xapp, zapp);
    if (verbose) {
        print(l);
    }

    props = l[[1]];
    means = l[[2]];
    vk = l[[3]];
    vke = l[[4]];
    vwe = l[[5]];
    vnba = l[[6]];
    method = as.character(method);
    classes = as.character(as.vector(rownames(props)));
    fk = matrix(nrow=nrow(Xtst), ncol=length(classes));
    #browser();
    colnames(fk) = classes;
    covariances = array(1, dim=c(ncol(Xtst), ncol(Xtst), length(classes)), dimnames=list(NULL, NULL, classes));
    P = 0;

    if (method == "adq") {
        covariances = vke;
    } else if (method == "adl") {
        covariances[,,1] = vwe;
        covariances[,,2] = vwe;
    } else if (method == "nba") {
        covariances[,,1] = vnba;
        covariances[,,2] = vnba;
    }

    l = NULL
    for (i in 1:length(classes)) {
        # conditionnal probs calculations
        fk[,i] = props[i] * mvdnorm(Xtst, means[i,], covariances[,,i]);
    }
    # a priori prob calcultation
    P = apply(fk, 1, sum);
    l$prob = fk / P;
    l$prob[is.na(l$prob)] = 0;
    l$zpred = apply(l$prob, 1, which.max)
    return (l);
}

