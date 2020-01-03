if(!require(shiny)) {
  install.packages("shiny")
}

if(!require(class)) {
  install.packages("class")
}

if(!require(shinyWidgets)) {
  install.packages("shinyWidgets")
}

if(!require(shinyjs)) {
  install.packages("shinyjs")
}

if(!require(RTriangle)) {
  install.packages("RTriangle")
}

library(shiny)
library(class)
library(shinyWidgets)
library(shinyjs)
library(RTriangle)

# got this file from: 
#   https://github.com/daattali/advanced-shiny/blob/master/busy-indicator/helpers.R
source("helpers.R")

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

source("decisionBoundariesKnn.R")
source("triangulatePlot.R")