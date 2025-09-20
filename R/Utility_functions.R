#'SSOSVM: A package for online training of soft-margin support vector machines (SVMs) using the Stochastic majorization–minimization (SMM) algorithm.
#'
#'The SSOSVM package allows for the online training of Soft-margin support vector machines (SVMs) using the Stochastic majorization–minimization (SMM) algorithm.
#'\code{SquareHinge},\code{Hinge} and \code{Logistic} 
#'The function \code{generateSim} can also be used to generate simple test sets.
#'
#'@author Andrew T. Jones,  Hien D. Nguyen, Geoffrey J. McLachlan
#'@references  Hien D. Nguyen, Andrew T. Jones and Geoffrey J. McLachlan. (2018). Stream-suitable optimization algorithms for some soft-margin support vector machine variants, Japanese Journal of Statistics and Data Science, vol. 1, Issue 1, pp. 81-108. 
#'@name SSOSVM
"_PACKAGE"
NULL

#'Generate Simulations
#'@description Generate simple simulations for testing of the algorithms.
#'@param NN Number of observations. Default is 10^4
#'@param DELTA Separation of three groups in standard errors. Default is 2.
#'@param DIM Number of dimensions in data. Default is 2.
#'@param seed Random seed if desired.
#'@return A list containing:
#'\item{XX}{Coordinates of the simulated points.}
#'\item{YY}{Cluster membership of the simulated points.}
#'\item{YMAT}{YY and XX Combined as a single matrix.}
#'@examples
#'#100 points of dimension 4.
#'generateSim(NN=100, DELTA=2, DIM=4)
#'@export
generateSim <- function(NN = 10^4, DELTA = 2, DIM = 2, seed=NULL) {
  if(!is.null(seed)){set.seed(seed)}
  
  # Simulation
  XX <- mvtnorm::rmvnorm(round(NN/2),rep(DELTA,DIM))
  XX <- rbind(XX,mvtnorm::rmvnorm(NN-round(NN/2),rep(-DELTA,DIM)))
  # XX <- scale(XX)
  SAMPLE <- sample(1:NN)
  XX <- XX[SAMPLE,]
  YY <- c(rep(-1,round(NN/2)),rep(1,NN-round(NN/2)))
  YY <- YY[SAMPLE]
  YMAT <- cbind(YY,XX)
  
  return(list(XX=XX, YY=YY,YMAT=YMAT))
}

#'SSOSVM Fit function
#'@description This is the primary function for uses to fit SVMs using this package.
#'@param YMAT Data. First column is -1 or 1 indicating the class of each observation. The remaining columns are the coordinates of the data points.
#'@param EPSILON Small perturbation value needed in calculation. Default value is 0.00001.
#'@param method Choice of function used in SVM. Choices are 'logistic', 'hinge' and 'squareHinge'. Default value is 'logistic"
#'@param returnAll Return all of theta values? Boolean with default value FALSE.
#'@param rho Sensitivity factor to adjust the level of change in the SVM fit when a new observation is added. Default value 1.0
#'@return A list containing:
#'\item{THETA}{SVM fit parameters.}
#'\item{NN}{Number of observation points in YMAT.}
#'\item{DIM}{Dimension of data.}
#'\item{THETA_list}{THETA at each iteration (new point observed) as YMAT is fed into the algorithm one data point at a time.}
#'\item{PSI,OMEGA,CHI}{Intermediate value for PSI, OMEGA, or CHI (depending on method choice) at each iteration (new point observed).}
#'@examples
#'Sim<- generateSim(10^4)
#'m1<-SVMFit(Sim$YMAT)
#'@export
SVMFit<-function(YMAT, method = "logistic", EPSILON = 0.00001, returnAll = FALSE, rho=1.0) {
  DIM <- ncol(YMAT)-1
  results<-list()
  
  if(tolower(method)=="logistic"){
    results<-Logistic(YMAT, DIM, EPSILON, returnAll, rho)
  }
  
  if(tolower(method)=="hinge"){
    results<-Hinge(YMAT, DIM, EPSILON, returnAll, rho)
  }
  
  if(tolower(method)=="squarehinge"|tolower(method)=="square"){
    results<-SquareHinge(YMAT, DIM, EPSILON, returnAll, rho)
  }
  
  if(length(results)==0){
    stop("No method selected.")
  }
  
  return(results)
}
