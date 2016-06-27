# Paul GOUJON & Stephane LOUIS
# UTC - SY09 - TP4

# logistic regression validation

# The aim of that function is to test the logistic model we learned from
# training on a test ensemble. It takes as input a test individuals matrix, a
# beta vector (corresponding to the beta we learned before) and returns a prob
# matrix containing the posteriori probabilities and a ztst vector containing
# the labels it gave to each individual

source("postPr.R");


log.val = function (beta, Xtst) {
    # check beta dimension, add coordinate at the origin if not the same as Xapp
    if (nrow(beta) != ncol(Xtst)) {
        individuals = cbind(Xtst, matrix(1, nrow=nrow(Xtst), ncol=1));
    } else {
        individuals = Xtst;
    }
    prob = post.pr(beta, individuals);
    ztst = matrix(1, nrow=nrow(Xtst), ncol=1);
    ztst[which(prob < 0.5),1] = 2;

    l = list(prob, ztst);
    names(l) = c("prob", "zpred");
    return (l);
}





