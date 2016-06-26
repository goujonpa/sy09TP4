# Paul GOUJON & Stéphane LOUIS
# UTC - SY09 - TP4

# Tree script : learns a full_tree classifier, prunes it at its best depth,
# makes a prediction on the test ensemble and returns the predicted labels for
# the test individuals

source("errorRate.R")
library(tree);

rate = matrix(nrow=20, ncol=1);

for (i in 1:20) {
    zapp <- as.factor(zapp);
    formula <- zapp ~ npreg + glu + bp + skin + bmi + ped + age;
    full_tree = tree(formula, data=Xapp, control=tree.control(nobs=nrow(Xapp), mindev = 0.0001));

    best_k = cv.tree(full_tree)$size[which.min(cv.tree(full_tree)$dev)];
    pruned_tree = prune.misclass(full_tree, best=best_k);
    prediction = predict(pruned_tree, Xtst);
    zpred = max.col(prediction);
    rate[i,] = error.rate(zpred, ztst);
}

mean_rate = apply(rate, 2, mean);
m = matrix(nrow=3, ncol=1);
m[1,] = mean_rate;
ic = confidence.interval(mean_rate, length(ztst));
m[2,] = ic$sup;
m[3,] = ic$inf;
rownames(m) = c("Mean", "Sup", "Inf");
write.csv(m, paste("./results/tree/", title, sep=""));

