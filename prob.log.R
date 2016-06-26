source("logb.R");

prob.log <- function(Xapp, zapp, X, z, niveaux, method, title, draw, intercept, threshold)
{
    discretisation=50
    deltaX <- (max(X[,1]) -min(X[,1]))/discretisation
    deltaY <- (max(X[,2]) -min(X[,2]))/discretisation
    minX <- min(X[,1])-deltaX
    maxX <- max(X[,1])+deltaX
    minY <- min(X[,2])-deltaY
    maxY <- max(X[,2])+deltaY

    # grille d'affichage
    grilleX <- seq(from=minX,to=maxX,by=deltaX)
    naffX <- length(grilleX)
    grilleY <- seq(from=minY,to=maxY,by=deltaY)
    naffY <- length(grilleY)
    grille <- cbind(rep.int(grilleX,times=rep(naffY,naffX)),rep(grilleY,naffX))

    grille <- as.matrix(grille)

    l = bin_log.app(Xapp, zapp, intercept, threshold);

    # calcul des valeurs de la fonction
    if(draw) {
        pdf(paste("./results/", method, "/", method, "-", title, ".pdf", sep=""));
        valf <- log.val(l$beta, grille)$prob
        plot(X, col=c("red","green","blue","magenta","orange")[z], main=paste(method, title));
        contour(grilleX, grilleY, matrix(valf,nrow=naffX,byrow=T), add=T, drawlabels=FALSE, levels=niveaux)
        dev.off();
    } else {
        l = log.val(l$beta, X);
    }
    return (l);
}
