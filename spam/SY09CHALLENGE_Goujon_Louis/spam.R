# Paul GOUJON & Stéphane LOUIS
# UTC - SY09 - TP4

# Challenge : spam data

# the aim of that script is to study the spam data, and in order to propose a
# strategy to discriminate spam individuals
library(devtools);
install_github("vqv/ggbiplot");
library(ggbiplot);

# data load
data = read.csv("donnees-tp4/spam.csv", header=T);
X = data[,2:58];
z = data$z;

# correlation study : are the variables correlated ?
correlation = cor(X);
pdf("./component_cor.pdf");
image(x=1:ncol(correlation), y=1:nrow(correlation), z=t(as.matrix(1-correlation)), axes=F, xlab="", ylab="", main="Spam Components Correlation");
axis(2, at=1:nrow(correlation), labels=colnames(correlation), col="white", las=1, cex.axis=0.8);
axis(1, at=1:ncol(correlation), labels=colnames(correlation), col="white", las=2, cex.axis=0.8);
dev.off();

# Use the PCA to see if we can guess anything from the data distribution
pdf("./pca_spam.pdf");
ggbiplot(prcomp(X, scale.=T),obs.scale=1, var.scale=1, groups=z);
dev.off();

# Test our classifier's performances
source("ad.R");
source("log.R");
source("logb.R");
source("errorRate.R");
library(tree);
m = matrix(nrow=3, ncol=1);
rownames(m) = c("mean", "inf", "sup");

l = ad.val(X, z, X, "adq");
m[1,] = error.rate(z, l$zpred);
ic = confidence.interval(m[1,], length(z));
m[2,] = ic$inf;
m[3,] = ic$sup;
write.csv(m, "./adq_spam_error.csv");


l = ad.val(X, z, X, "adl");
m[1,] = error.rate(z, l$zpred);
ic = confidence.interval(m[1,], length(z));
m[2,] = ic$inf;
m[3,] = ic$sup;
write.csv(m, "./adl_spam_error.csv");


l = ad.val(X, z, X, "nba");
m[1,] = error.rate(z, l$zpred);
ic = confidence.interval(m[1,], length(z));
m[2,] = ic$inf;
m[3,] = ic$sup;
write.csv(m, "./nba_spam_error.csv");

l = bin_log.app(X, z, intercept=F, 0.00001);
l = log.val(l$beta, X);
m[1,] = error.rate(z, l$zpred);
ic = confidence.interval(m[1,], length(z));
m[2,] = ic$inf;
m[3,] = ic$sup;
write.csv(m, "./logbf_spam_error.csv");

l = bin_log.app(X, z, intercept=T, 0.00001);
l = log.val(l$beta, X);
m[1,] = error.rate(z, l$zpred);
ic = confidence.interval(m[1,], length(z));
m[2,] = ic$inf;
m[3,] = ic$sup;
write.csv(m, "./logbt_spam_error.csv");

z = as.factor(z);
formula <- z ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + V28 + V29 + V30 + V31 + V32 + V33 + V34 + V35 + V36 + V37 + V38 + V39 + V40 + V41 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V50 + V51 + V52 + V53 + V54 + V55 + V56 + V57
full_tree = tree(formula, data=X, control=tree.control(nobs=nrow(X), mindev = 0.0001));
best_k = cv.tree(full_tree)$size[which.min(cv.tree(full_tree)$dev)];
pruned_tree = prune.misclass(full_tree, best=best_k);
prediction = predict(pruned_tree, X);
zpred = max.col(prediction);
m[1,] = error.rate(z, zpred);
ic = confidence.interval(m[1,], length(z));
m[2,] = ic$inf;
m[3,] = ic$sup;
write.csv(m, "./tree_spam_error.csv");





