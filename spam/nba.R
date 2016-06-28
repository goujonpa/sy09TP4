# Paul GOUJON & Stéphane LOUIS
# UTC - SY09 - TP4

# Naive Bayesian Classifier training function

# The aim of that function is to estimate the mandatory parameters to execute
# a discriminant analysis with the Naive Bayesian Classifier assumptions

source("adl.R");


nba.app = function (Xapp, zapp) {
    l = adl.app(Xapp, zapp);
    if (verbose) {
        print(l);
    }

    proportions = l[[1]];
    means_matrix = l[[2]];
    vk_matrix = l[[3]];
    vke_matrix = l[[4]];
    vwe_matrix = l[[5]];
    vnba_matrix = vwe_matrix;

    for (i in 1:nrow(vnba_matrix)) {
        for (j in 1:ncol(vnba_matrix)) {
            if (i != j) {
                vnba_matrix[i, j] = 0;
            }
        }
    }
    l = list(proportions, means_matrix, vk_matrix, vke_matrix, vwe_matrix, vnba_matrix);
    names(l) = c("props", "means", "vk", "vke", "vwe", "vnba");
    return (l);
}

