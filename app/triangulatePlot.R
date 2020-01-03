triangulatePlot <- function(train) {
  p <- pslg(as.matrix(train[,1:2]))
  t <- triangulate(p)
  plot(t,asp=1,pch=".",main="Helping plot for 1-NN")
  points(train[,c(1,2)], pch=as.integer(train[,3]) - 1, cex=2, lwd=3)
}