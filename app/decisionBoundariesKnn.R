# bibliography:
# https://stackoverflow.com/questions/32449280/how-to-create-a-decision-boundary-graph-for-knn-models-in-the-caret-package
# http://michael.hahsler.net/SMU/EMIS7332/R/viz_classifier.html
decisionBoundariesKnn <- function(train, k, xlim=NULL, ylim=NULL) {
  # xmin[1] <= xlim[2] and ylim[1] <= ylim[2]
  # train has 3 columns: first 2 - x1 and x2; the third - class
  if(is.null(xlim)) {
    xlim <- range(train[,1])
  }
  if(is.null(ylim)) {
    ylim <- range(train[,2])
  }
  xs <- seq(xlim[1],xlim[2],length.out = 500)
  ys <- seq(ylim[1],ylim[2],length.out = 500)
  knn.grid.points <- expand.grid(xs,ys)
  knn.vector.predicted <- knn(train = train[,c(1,2)],
                              test = knn.grid.points,
                              cl = as.integer(train[,3]),
                              k = k)
  knn.grid.predicted <- matrix(knn.vector.predicted, 
                               length(xs), 
                               length(ys))
  plot(knn.grid.points, col = as.integer(knn.vector.predicted)+1, pch = ".", main=paste0(k,"-NN"), asp = 1)
  
  contour(xs, ys, knn.grid.predicted, add = TRUE, drawlabels = FALSE)
  
  points(train[,c(1,2)], pch=as.integer(train[,3]) - 1, cex=2, lwd=3)
}