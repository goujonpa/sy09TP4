# Paul GOUJON & Stéphane LOUIS
# UTC - SY09 - TP4

# Quadratic Discriminant analysis - Training function

# Considered case : class number g = 2

# The aim of the following function is to take as input the Xapp matrix of
# individuals, and the zapp vector of individuals labels and to return the
# parameters of the QDA model : proportions, means vectors and covariances
# matrix
verbose = F;


adq.app = function(Xapp, zapp) {
    classes = sort(unique(as.character(zapp)));

    proportions_vector = matrix(nrow = length(classes), ncol = 1);
    rownames(proportions_vector) = classes;

    means_matrix = matrix(nrow = length(classes), ncol = dim(Xapp)[2]);
    rownames(means_matrix) = classes;

    vk_matrix = array(1, dim=c(dim(Xapp)[2], dim(Xapp)[2], length(classes)), dimnames = list(NULL, NULL, as.character(classes)));
    vke_matrix = array(1, dim=c(dim(Xapp)[2], dim(Xapp)[2], length(classes)), dimnames = list(NULL, NULL, as.character(classes)));

    if (verbose) {
        print(paste("Classes : ", classes));
    }

    for (i in 1:length(classes)) {
        # for each class, the proportions of the class is the number of
        # individuals with its label, divided by the total number of individuals
        individuals = Xapp[which(as.character(zapp) == classes[i]),];
        proportions_vector[i,] = dim(individuals)[1] / dim(Xapp)[1];
        # mean for this class is mean by column of the individuals matrix
        means_matrix[i,] = apply(individuals, 2, mean);
        # we use the corrected estimator of Sigma
        vk_matrix[,,i] = cov(individuals);
        vke_matrix[,,i] = (dim(individuals)[1] / (dim(individuals)[1] - 1)) * cov(individuals);
    }

    if (verbose) {
        print(paste("Means Matrix : ", means_matrix));
        print(paste("Covariances : ", vk_matrix));
    }

    return_list = list(proportions_vector, means_matrix, vk_matrix, vke_matrix);
    return (return_list);
}

