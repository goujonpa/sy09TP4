# Paul GOUJON & Stéphane LOUIS
# SY09 - UTC - TP4

# Sources
source("mvdnorm.r");
source("adq.R");
source("adl.R");
source("nba.R");
source("ad.R");
source("logb.R");
source("logq.R");
source("log.R");
source("prob.ad.R");
source("prob.log.R");
source("prob.log2.R");
source("dataload.R");
source("separ1.R");
source("separ2.R");
source("errorRate.R");


for (i in 1:length(data)) {
    print(paste("i=", i));
    l = NULL;
    # select dataset
    current_dataset = as.matrix(data[[i]]);
    dataset_names = names(data);
    X = current_dataset[,1:(ncol(current_dataset)-1)];
    z = current_dataset[,ncol(current_dataset)];
    rate = matrix(nrow=20, ncol=7);
    method_names = c("adq", "adl", "nba", "binlrT", "binlrF", "quadlrT", "quadlrF");
    colnames(rate) = method_names;
    n_m = matrix(nrow=21, ncol=1);

    for (j in 1:21) {
        d = separ1(X, z);
        n_m[j,] = length(d$ztst);

        for (k in 1:3) {
            l = prob.ad(d$Xapp, d$zapp, d$Xtst, d$ztst, method_names[k], 0.5, names(data)[i], (j==21 && i<4));
            if (j != 21) {
                rate[j,k] = error.rate(d$ztst, l$zpred);
            }
        }

        # Binary Log Reg
        for (k in 4:5) {
            if (k == 4) {
                intercept = T;
            } else {
                intercept = F;
            }
            l = prob.log(d$Xapp, d$zapp, d$Xtst, d$ztst, 0.5, method_names[k], names(data)[i], (j==21 && i<4), intercept, 0.00001);
            if (j != 21) {
                rate[j,k] = error.rate(d$ztst, l$zpred);
            }
        }

        # Quadratic Log Reg
        if (i < 5) {
            for (k in 6:7) {
                if (k == 6) {
                    intercept = T;
                } else {
                    intercept = F;
                }
                l = prob.log2(d$Xapp, d$zapp, d$Xtst, d$ztst, 0.5, method_names[k], names(data)[i], (j==21 && i<4), intercept, 0.00001);
                if (j != 21) {
                    rate[j,k] = error.rate(d$ztst, l$zpred);
                }
            }
        }
    }
    mean_rates = apply(rate, 2, mean);
    for (k in 1:7) {
        m = matrix(nrow=3, ncol=1);
        rownames(m) = c("Error mean", "Sup", "Inf");
        m[1,] = mean_rates[method_names[k]];
        nindiv = apply(n_m, 2, mean);
        ic = confidence.interval(mean_rates[method_names[k]], nindiv);
        m[2,] = ic$sup;
        m[3,] = ic$inf;
        write.csv(m, paste("./results/", method_names[k], "/", method_names[k], "-", dataset_names[i], "-errors.csv", sep=""));
    }

}




