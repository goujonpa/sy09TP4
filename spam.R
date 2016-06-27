> data = read.csv("donnees-tp4/spam.csv", header=T)
> X = data[,2:58]
> z = data[,59]
> length(z)
> X1 = X[which(z==1),]
> X2 = X[which(z==2),]
> nrow(X2)
[1] 2788

> nrow(X1)
[1] 1813

l=NULL
> l$Xtst = X1[1:1800/3,]
> l$Xtst = rbind(l$Xtst, X2[1:2700/3,])
>

> l$Xapp = X1[601:nrow(X1),]
> l$Xapp = rbind(l$Xapp, X2[901:nrow(X2),])
>
l$zapp = rep(1, nrow(l$Xapp)),

l$zapp[nrow(X1[601:nrow(X1),]): nrow(X1[601:nrow(X1),])+nrow(X2[901:nrow(X2),])] = 2

1. Correlation
2. Differents classifieurs on it
3. Litterature


