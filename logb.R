# Paul GOUJON & Stéphane LOUIS
# UTC - SY09 - TP4

# Binary Logistic regression

# The aim of that is to implement the training of the logistic regression such
# as it takes as input Xapp, the individual matrix, zapp the labels vector,
# intercept, a boolean intercept, saying if yes or no we should an ordinate at
# the origin and threshold, the threshold above which it's considered the algorithm
# converged. The output of the function is beta, corresponding to the likelyhood
# maximum maximum of the logistic regression maximum (size p x 1)

library(MASS); # For the ginv function
source("postPr.R");

# verbose option
verbose = F;

bin_log.app = function(Xapp, zapp, intercept, threshold) {
    niter = 0; # iteration counter

    # type checking
    Xapp = as.matrix(Xapp); # n x p matrix
    zapp = as.matrix(zapp); # n x 1 matrix
    intercept = as.logical(intercept); # T/F
    threshold = as.numeric(threshold); # numeric
    classes_labels = as.character(sort(unique(zapp)));

    # defining beta
    if (intercept) {
        individuals = cbind(Xapp, matrix(1, nrow=dim(Xapp)[1], ncol=1));
        beta = matrix(0, nrow=(dim(Xapp)[2] + 1), ncol=1);
    } else {
        individuals = Xapp;
        beta = matrix(0, nrow=dim(Xapp)[2], ncol=1);
    }

    # t is n x 1 with 1 if classe(xi) == w1, else 0
    ti_m = zapp;
    ti_m[which(ti_m == classes_labels[1]),] = 1;
    ti_m[which(ti_m == classes_labels[2]),] = 0;

    # while ||beta(q+1) - beta(q)|| > threshold, repeat Newton Raphson algorithm
    repeat {
        niter = niter + 1;
        # p(q) calculation
        # beta is p x 1, individuals is n x p
        # t(beta) is 1 x p, t(individuals) is p x n, t(beta) %*% t(individuals) is 1 x n
        # p is 1 x n
        pi_m = post.pr(beta, individuals);


        # gradient calculation
        # grad is the sum of the xi * (ti - p(xi; beta))
        # p(xi; beta) is 1 x 1
        # xi is p x 1
        # grad should be p x 1
        # individuals is n x p
        # t(individuals is p x n)
        # grad is p x 1
        grad = t(individuals) %*% (ti_m - t(pi_m));

        # hessian matrix calculation
        # t(individuals) is p x n
        # t((pi_m * (1 - pi_m))) is n x n (diagonal matrix of beta)
        # individuals is n x p
        # H is p x p
        H = -1 * t(individuals) %*% diag(as.vector(pi_m * (1 - pi_m))) %*% individuals;

        # new beta calculation
        # H %*% grad is p x 1
        # new_beta is p x 1
        new_beta = beta - ginv(H) %*% grad;


        # if norm lower than threshold then stop
        if (dist(rbind(t(beta), t(new_beta))) <  threshold) {
            beta = new_beta;
            break;
        }
        beta = new_beta;
    }

    # Log L calculation
    # t is n x 1
    # beta is p x 1
    # log L is p x 1 as well

    logL = as.vector(ti_m) * (t(beta) %*% t(individuals)) - log(1 + exp(t(beta) %*% t(individuals)));
    logL = sum(logL);

    l = list(new_beta, niter, logL, individuals);
    names(l) = c("beta", "n_iter", "logL", "X");
    return (l);
}

