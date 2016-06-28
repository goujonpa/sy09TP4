# Paul GOUJON & Stéphane LOUIS
# UTC - SY09 - TP4

# Linear Discriminant analysis - Training function

# The aim of the following function is to get the parameters used by our LDA
# model to discriminate individuals. It takes as input Xapp, the matrix of
# training individuals and zapp, the vector of training individuals label and
# returns the proportions vector, the means matrix and the covariance matrix.

source("adq.R");

adl.app = function(Xapp, zapp) {
    l = adq.app(Xapp, zapp);

    proportions = l[[1]];
    means_matrix = l[[2]];
    vk_matrix = l[[3]];
    vke_matrix = l[[4]];
    vwe_matrix = matrix(nrow = dim(Xapp)[2], ncol = dim(Xapp)[2]);

    classes_labels = as.vector(dimnames(vk_matrix)[3])[[1]];

    for (i in 1:dim(vk_matrix)[3]) {
        current_class = as.character(classes_labels)[i];
        if (i == 1) {
            vwe_matrix = (length(zapp[which(as.character(zapp) == current_class)]) - 1) * vke_matrix[,,i];
        } else {
            vwe_matrix = vwe_matrix + ((length(zapp[which(as.character(zapp) == current_class)]) - 1) * vke_matrix[,,i]);
        }
    }

    vwe_matrix = vwe_matrix / (length(zapp) - 2);

    l = list(proportions, means_matrix, vk_matrix, vke_matrix, vwe_matrix);
    return (l);
}
