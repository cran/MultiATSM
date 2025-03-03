#' Estimates a GVAR(1) and a VARX(1,1,1) models
#'
#
#'@param GVARinputs GVARinputs List. Inputs for GVAR model estimation:
#' \enumerate{
#'        \item  \code{Economies}:  A character vector containing the names of the economies included in the system.
#'        \item  \code{GVARFactors}: A list of all variables used in the estimation of the VARX model \cr
#'                (see e.g. \code{CM_Factors_GVAR} file for details);
#'        \item \code{VARXtype}: A character vector with three possible options:
#'  \itemize{
#'        \item \code{'unconstrained'}: model is estimated without constraints (each equation is estimated individually by ordinary least square);
#'        \item \code{'constrained: Spanned Factors'}: The model is estimated with the restriction that foreign pricing factors do NOT
#'                                affect (i) domestic economic variables and (ii) domestic pricing factors. \cr
#'                               (Equations are estimated using restricted least squares)
#'        \item \code{'constrained : [factor_name]'}: The model is estimated with the restriction that the specified risk factor
#'              is influenced only by its own lagged values and the lagged values of its corresponding star variables.
#'        (Equations are estimated using restricted least squares.)
#'          }
#'          \item \code{Wgvar}: The GVAR transition matrix (C x C) used in the model solution. \cr
#'                                (See the output from the \code{\link{Transition_Matrix}} function.).
#' }
#'@param N    Integer. Number of country-specific spanned factors.
#'@param CheckInputs A logical flag to indicate whether to perform a prior consistency check on the inputs provided in \code{GVARinputs}. The default is set to FALSE
#'
#'
#'
#'@return   A list containing
#'\enumerate{
#'\item parameters of the country-specific VARX(1,1,1)
#'\itemize{
#'\item intercept (M+Nx1);
#'\item phi_1   (M+N x M+N);
#'\item phi_1^star (M+N x M+N);
#'\item phi_g (M+N x M+N);
#'\item Sigma (M+N x G)
#'}
#'\item parameters of the GVAR.
#'\itemize{
#' \item F0 (F X 1);
#' \item F1 (F x F);
#' \item Sigma_y (F x F)
#'}
#'}
#'
#'
#'@examples
#' data(CM_Factors_GVAR)
#'
#' GVARinputs <- list( Economies = c("China", "Brazil", "Mexico", "Uruguay"),
#'                     GVARFactors = FactorsGVAR, VARXtype = "unconstrained")
#'
#' GVARinputs$Wgvar <- matrix( c(0, 0.83, 0.86, 0.38,
#'                               0.65, 0, 0.13, 0.55,
#'                               0.32, 0.12, 0, 0.07,
#'                               0.03, 0.05, 0.01, 0), nrow = 4, ncol = 4)
#' N <- 3
#'
#' GVARPara <- GVAR(GVARinputs, N)

#'@references
#' Chudik and Pesaran, (2016). "Theory and Practice of GVAR modelling" (Journal of Economic Surveys)
#'@export



GVAR <- function(GVARinputs, N, CheckInputs = F){

# 0) Check whether there are inconsistency in the specification of GVARinputs
  if (isTRUE(CheckInputs)){CheckInputsGVAR(GVARinputs, N)}

# 1) Preliminary work
# Labels of group of variables
DomAndStarLabels <- names(GVARinputs$GVARFactors[[GVARinputs$Economies[1]]]$Factors)
L <- length(DomAndStarLabels)
DomLabels <- DomAndStarLabels[1:(L/2)]
StarLabels <- DomAndStarLabels[(L/2+1):L]
GlobalLabels <- names(GVARinputs$GVARFactors$Global)

# 2) Prepare variables to be used in the estimation
Factors_Clean <- GVAR_PrepFactors(GVARinputs, DomLabels, StarLabels, GlobalLabels, N)

# 3) Estimate the country-specific VARX(1,1,1) models
ParaVARX <- VARX(GVARinputs, Factors_Clean, DomLabels, StarLabels, GlobalLabels, N)

# 4) Estimate the dynamics of the global variables: VAR(1)
ParaGlobal <- MarginalModelPara(GVARinputs)

# 5) Build a GVAR(1)
GVARoutputs <- BuildGVAR(ParaVARX, ParaGlobal, GVARinputs, DomLabels, GlobalLabels, N)

return(GVARoutputs)
}

#####################################################################################################################
#####################################################################################################################
#' Estimate numerically the variance-covariance matrix from the GVAR-based models
#'
#'@param SigmaUnres Unrestricted variance-covariance matrix (K x K)
#'@param res residuals from the VAR of a GVAR model (K x T)
#'@param IdxVarRest index of the variable that is selected as strictly exogenous
#'
#'
#'
#'@keywords internal
#'@return  restricted version of the variance-covariance matrix a GVAR model (K x K)


EstimationSigma_GVARrest <- function(SigmaUnres, res, IdxVarRest){

  # Choleski Factor
  Se <- t(chol(SigmaUnres))
  Se[IdxVarRest, -IdxVarRest] <- 0
  Se[-IdxVarRest, IdxVarRest] <- 0

  K <- nrow(SigmaUnres)


  # Set the constraints in the Sigma matrix
  IdxNONzeroGVAR <- which(Se!=0)
  x <- Se[IdxNONzeroGVAR] # vector containing the initial guesses


  MLfunction <- functional::Curry(llk_JLL_Sigma, res=res, IdxNONzero= IdxNONzeroGVAR, K=K)

  iter <- 'off' # hides the outputs of each iteration. If one wants to display these features then set 'iter'
  options200 <- neldermead::optimset(MaxFunEvals = 200000*length(x), Display = iter,
                                     MaxIter = 200000, GradObj='off', TolFun= 10^-2, TolX= 10^-2)


  Xmax <- neldermead::fminsearch(MLfunction, x, options200)$optbase$xopt
  SIGMA_Ye <- matrix(0, K,K)
  SIGMA_Ye[IdxNONzeroGVAR]<- Xmax # Cholesky term (orthogonalized factors)
  SIGMA_Res <- SIGMA_Ye%*%t(SIGMA_Ye)

  #Labels
  rownames(SIGMA_Res) <- rownames(SigmaUnres)
  colnames(SIGMA_Res) <- rownames(SigmaUnres)

  return(SIGMA_Res)
}


################################################################################################################
#' Prepare risk factors for the estimation of the GVAR model
#'
#'@param GVARinputs List of inputs for GVAR-based models
#'@param DomLabels string-based vector containing label of the domestic risk factors
#'@param StarLabels string-based vector containing label of the star domestic risk factors
#'@param GlobalLabels string-based vector containing label of the global risk factors
#'@param N number of country-specific spanned factors (scalar)
#'
#'
#'@keywords internal


GVAR_PrepFactors <- function(GVARinputs, DomLabels, StarLabels, GlobalLabels, N){
  T <- length(GVARinputs$GVARFactors[[GVARinputs$Economies[1]]]$Factors[[1]])
  C <- length(GVARinputs$Economies)
  G <- length(GlobalLabels)
  M <- length(DomLabels) - N

  # 1) Prepare variables to be used in the estimation
  # a) Z.t:
  Z.t <- list()
  for (i in 1:C){
    X <- matrix(NA, nrow= length(DomLabels), ncol=(T-1))
    for (j in 1:(M+N)){
      X[j,] <- GVARinputs$GVARFactors[[GVARinputs$Economies[i]]]$Factors[[j]][2:T]
    }
    Z.t[[i]] <- X
  }
  names(Z.t) <- GVARinputs$Economies

  # b) Z lagged (Z.Lt)
  Z.Lt <- list()
  for (i in 1:C){
    X <- matrix(NA, nrow= length(DomLabels), ncol=(T-1))
    for (j in 1:(M+N)){
      X[j,] <- GVARinputs$GVARFactors[[GVARinputs$Economies[i]]]$Factors[[j]][1:(T-1)]
    }
    Z.Lt[[i]] <- X
  }
  names(Z.Lt) <- GVARinputs$Economies

  # c) Z star lagged (Zstar.Lt)
  idx1 <- M + N
  Zstar.Lt <- list()
  for (i in 1:C){
    X <- matrix(NA, nrow= length(StarLabels), ncol=(T-1))
    for (j in 1:(M+N)){
      X[j,] <- GVARinputs$GVARFactors[[GVARinputs$Economies[i]]]$Factors[[idx1+j]][1:(T-1)]
    }
    Zstar.Lt[[i]] <- X
  }
  names(Zstar.Lt) <- GVARinputs$Economies

  # d) Global lagged (Global.Lt)
  Global.Lt <- list()
  X <- matrix(NA, nrow= G, ncol=(T-1))
  for (j in seq_len(G)){
    X[j,] <- GVARinputs$GVARFactors$Global[[j]][1:(T-1)]
  }
  Global.Lt <- X
  rownames(Global.Lt) <- GlobalLabels

  # Export outputs
  Outputs <- list(Z.t = Z.t, Z.Lt = Z.Lt, Zstar.Lt = Zstar.Lt, Global.Lt= Global.Lt)
  return(Outputs)
}

##############################################################################################################
#' Estimate a VARX(1,1,1)
#'
#'@param GVARinputs List of inputs for GVAR-based models
#'@param Factors_GVAR list containing the set of eisk factors used in the estimation of the VARX models
#'@param DomLabels string-based vector containing label of the domestic risk factors
#'@param StarLabels string-based vector containing label of the star domestic risk factors
#'@param GlobalLabels string-based vector containing label of the global risk factors
#'@param N number of country-specific spanned factors (scalar)
#'
#'@keywords internal


VARX <- function(GVARinputs, Factors_GVAR, DomLabels, StarLabels, GlobalLabels, N){

# Preliminary work
  Z.t <- Factors_GVAR$Z.t
  Z.Lt <- Factors_GVAR$Z.Lt
  Zstar.Lt <- Factors_GVAR$Zstar.Lt
  Global.Lt <- Factors_GVAR$Global.Lt

C <- length(GVARinputs$Economies)
T <- length(GVARinputs$GVARFactors[[GVARinputs$Economies[1]]]$Factors[[1]])
G <- length(GlobalLabels)
M <- length(DomLabels) - N

# Prepare regressor set in LHS and RHS
LHS <- vector(mode='list', length = C)
RHS <- vector(mode='list', length = C)
names(LHS) <- GVARinputs$Economies
names(RHS) <- GVARinputs$Economies
for (i in 1:C){
  LHS[[i]] <- Z.t[[i]]
  RHS[[i]] <- rbind(rep(1, times=T-1), Z.Lt[[i]], Zstar.Lt[[i]], Global.Lt)
  rownames(RHS[[i]]) <- c("Intercept", DomLabels, StarLabels, GlobalLabels)
}

# Set the foreign contemporaneous matrices to be equal to zero
phi0_star <- list()
X <- matrix(0, nrow= M + N, ncol= M + N)
for (i in 1:C){
  phi0_star[[i]]<- X
}
names(phi0_star) <- GVARinputs$Economies

# 1) Estimate the VARX(1,1,1)
# a) unconstrained system:
Coeff <- list()

if (GVARinputs$VARXtype == 'unconstrained'){
  ModelEstimates <- list()
  et <- list()
  Sigma <- list()
  for (i in 1:C){
    ModelEstimates[[i]] <- stats::lm( t(LHS[[i]]) ~ t(RHS[[i]])-1) # RHS and LHS are Tx(M+N)
    Coeff[[i]] <- t(ModelEstimates[[i]]$coefficients)
    et[[i]] <-t(ModelEstimates[[i]]$residual)
    Sigma[[i]] <- (et[[i]]%*%t(et[[i]]))/T
    rownames(Coeff[[i]]) <- DomLabels
  }
}
# b) constrained system: zero restrictions for the spanned factors in the feedback matrix
else  if (GVARinputs$VARXtype == 'constrained: Spanned Factors'){
  idxIntercept<- 1
  idxM <- idxIntercept + M
  idxP <- idxM + N
  idxM.star <- idxP + M
  idxP.star <- idxM.star + N
  idx.global <- idxP.star + G

  Bcon <- list()
  Coeff <- list()
  eT <- list()
  Sigma <- list()

  for (i in 1:C){
    Bcon[[i]] <- matrix(NaN, nrow= nrow(LHS[[i]]), ncol=nrow(RHS[[i]])) # (M+N)x (2*(M+N)+G+1)
    # b.1) if constrains is avoid the effect of P* on P and M:
    Bcon[[i]][,(idxM.star+1):idxP.star] <- 0

    Coeff[[i]] <- Reg__OLSconstrained(Y= LHS[[i]], X= RHS[[i]],Bcon[[i]], G=NULL)
    colnames(Coeff[[i]]) <- c("Intercept", DomLabels, StarLabels, GlobalLabels)
    rownames(Coeff[[i]]) <- DomLabels

    eT[[i]] <- LHS[[i]] - Coeff[[i]]%*%RHS[[i]]
    Sigma[[i]] <- (eT[[i]]%*%t(eT[[i]]))/T
  }
  names(Coeff) <- GVARinputs$Economies
}


# c) constrained system: one variable of the system is only affected by its own lags and the star counterparts
if (any(GVARinputs$VARXtype == paste("constrained:", DomLabels))){
  VARXLabs <- c("Intercept", DomLabels, StarLabels, GlobalLabels)
  zz <- stringr::str_length("constrained: ")
  VarInt <- substr(GVARinputs$VARXtype, start = zz+1, stop = stringr::str_length(GVARinputs$VARXtype) )

  idxIntercept<- 1
  idxCol <- which(grepl(VarInt, VARXLabs))
  idxRow <- which(grepl(VarInt, DomLabels))

  Bcon <- list()
  Coeff <- list()
  eT <- list()
  SigmaUnrest <- list()
  Sigma <- list()

  for (i in 1:C){

    Bcon[[i]] <- matrix(NaN, nrow= nrow(LHS[[i]]), ncol=nrow(RHS[[i]])) # (M+N)x (2*(M+N)+G+1)
    # c.1) Identify the zero-restrictions:
    rownames(Bcon[[i]]) <- DomLabels
    colnames(Bcon[[i]]) <- VARXLabs
    Bcon[[i]][idxRow, -c(idxIntercept, idxCol)] <- 0

    Coeff[[i]] <- Reg__OLSconstrained(Y= LHS[[i]], X= RHS[[i]],Bcon[[i]], G=NULL)
    colnames(Coeff[[i]]) <- VARXLabs
    rownames(Coeff[[i]]) <- DomLabels

    eT[[i]] <- LHS[[i]] - Coeff[[i]]%*%RHS[[i]]
    SigmaUnrest[[i]] <- (eT[[i]]%*%t(eT[[i]]))/T # Initial guess

    # Estimate sigma with restrictions
    Sigma[[i]] <- EstimationSigma_GVARrest(SigmaUnrest[[i]], eT[[i]], idxRow)

  }
  names(Coeff) <- GVARinputs$Economies
}


# 2) Prepare outputs:
ParaVARX <- list()

idxPhi0 <- 1
idxPhi1 <- idxPhi0 + (N+M)
idxPhi1.star <- idxPhi1 + (N+M)
idxPhi.global <- idxPhi1.star+G

if (G == 0){ Idx_G <- c()} else{Idx_G <- (idxPhi1.star+1):idxPhi.global}

for (i in 1:C){
  ParaVARX[[i]] <- vector(mode = 'list', length = 6)
  names(ParaVARX[[i]]) <- c('Phi0', 'Phi1', 'Phi1.star', 'Phi.global', 'Sigma', 'Phi0.star')

  ParaVARX[[i]]$Phi0 <- Coeff[[i]][,idxPhi0]
  ParaVARX[[i]]$Phi1 <- Coeff[[i]][,(idxPhi0+1):idxPhi1]
  ParaVARX[[i]]$Phi1.star <- Coeff[[i]][,(idxPhi1+1):idxPhi1.star]
  ParaVARX[[i]]$Phi.global <- Coeff[[i]][, Idx_G]
  ParaVARX[[i]]$Sigma <- Sigma[[i]]
  ParaVARX[[i]]$Phi0.star <- phi0_star[[i]]
}
names(ParaVARX) <- GVARinputs$Economies

return(ParaVARX)
}

##################################################################################################################
#'Build the GVAR(1) from the country-specific VARX(1,1,1)
#'
#'@param ParaVARX Set of VARX model parameters
#'@param GlobalPara Set of marginal model parameters
#'@param GVARinputs List of inputs for GVAR-based models
#'@param DomLabels string-based vector containing label of the domestic risk factors
#'@param GlobalLabels string-based vector containing label of the global risk factors
#'@param N number of country-specific spanned factors (scalar)
#'
#'@keywords internal


BuildGVAR <- function(ParaVARX, GlobalPara, GVARinputs, DomLabels, GlobalLabels, N){

C <- length(GVARinputs$Economies)
T <- nrow(GVARinputs$GVARFactors[[GVARinputs$Economies[1]]]$Factors[[1]])
G <- length(GlobalLabels)
M <- length(DomLabels) - N

#  1) Build a0, Ai0, Ai1 and Wi
# a) a0
a0 <- Get_a0(GVARinputs, ParaVARX)
# b) Ai0 and Ai1:
Ai0 <- list()
Ai1 <- list()
for (i in 1:C){
  Ai0[[i]] <- cbind(diag(N+M), ParaVARX[[GVARinputs$Economies[i]]]$Phi0.star)
  Ai1[[i]] <- cbind(ParaVARX[[GVARinputs$Economies[i]]]$Phi1, ParaVARX[[GVARinputs$Economies[i]]]$Phi1.star)
}
names(Ai0) <- GVARinputs$Economies
names(Ai1) <- GVARinputs$Economies
# c) Wi (link matrices)
Wi <- BuildLinkMat(GVARinputs, N, M)

# 2) Compute G0, G1 and Sigma
Gs_Sigma <- Get_G0G1Sigma(ParaVARX, GVARinputs, Ai0, Ai1, Wi)

# 3) Compute Gy.0,  Gy.1
# a) Gy.0:
Gy.0 <- magic::adiag(diag(G), Gs_Sigma$G0)
# b) Gy.1:
Gy.1 <- Get_Gy1(ParaVARX, GVARinputs,  Gs_Sigma$G1, GlobalPara$Phi.w1)

# 4) Build the GVAR(1): y_t = F0 + F1* y_{t-1} + (Gy.0)^(-1)ey_t ( equation 19)
# a) F0 and F1:
Phi0VARX <- list()
for (i in 1:C){  Phi0VARX[[i]] <-  ParaVARX[[GVARinputs$Economies[[i]]]]$Phi0}
Phi0VARX <- do.call(rbind,lapply(Phi0VARX,matrix,ncol=1))

F0 <- solve(Gy.0)%*%rbind(GlobalPara$Phi.w0, Phi0VARX)
F1 <- solve(Gy.0)%*%Gy.1

# b) Sigma_y:
Sigma_y <- magic::adiag(GlobalPara$Sigma_w, Gs_Sigma$Sigma)

# 5) Prepare labels of the tables
labelsDomVar <- c()
for (i in 1:C){
  labelsDomVarCS <- paste(DomLabels, GVARinputs$Economies[i])
  labelsDomVar <- append(labelsDomVar,labelsDomVarCS)
}

labelsTables <- c(GlobalLabels,labelsDomVar)

dimnames(F1) <- list(labelsTables, labelsTables)
dimnames(Sigma_y) <- list(labelsTables, labelsTables)
rownames(F0) <- labelsTables


GVARoutputs <- list(VARX = ParaVARX, Gy.0 = Gy.0, F0 = F0, F1 = F1, Sigma_y = Sigma_y)
return(GVARoutputs)
}
##########################################################################################################
#' Build country-specific link matrices
#'
#'@param GVARinputs  List of inputs for GVAR-based models
#'@param N number of country-specific spanned factors (scalar)
#'@param M number of country-specific unspanned factors (scalar)
#'
#'@keywords internal

BuildLinkMat<- function(GVARinputs, N, M){

  C <- length(GVARinputs$Economies)

# Bottom part of Wi
bottomWi <- list()
for(i in 1:C){
  b <- matrix(NA, nrow=N+M, ncol= C*(N+M))
  bottomWi[[i]] <- b
}

a <- matrix(NA, nrow=N+M, ncol= N+M)
for (j in 1:C){
  count0 <- 0
  for (i in 1:C){
    count1 <- count0 +  (N+M)
    a <- GVARinputs$Wgvar[j,i]*diag(N+M)
    bottomWi[[j]][,(count0+1):count1] <- a
    count0 <- count1
  }
}
names(bottomWi) <- GVARinputs$Economies

# Top part of Wi
topWi <- list()
for(i in 1:C){
  c <- matrix(NA, nrow= N+M, ncol= C*(N+M))
  topWi[[i]] <- c
}

d <- matrix(NA, nrow=N+M, ncol= N+M)
IndexPos <- diag(C)
for (j in 1:C){
  count0 <- 0
  for (i in 1:C){
    count1 <- count0 +  (N+M)
    d <- IndexPos[j,i]*diag(N+M)
    topWi[[j]][,(count0+1):count1] <- d
    count0 <- count1
  }
}
names(topWi) <- GVARinputs$Economies

# Concatenate TopWi and bottomWi in Wi
Wi <- list()
for (i in 1:C){ Wi[[i]] <- rbind(topWi[[i]], bottomWi[[i]])}
names(Wi) <- GVARinputs$Economies

return(Wi)
}
##############################################################################################################
#' Estimate the marginal model for the global factors
#'
#'@param GVARinputs List of inputs for GVAR-based models
#'
#'@keywords internal

MarginalModelPara <- function(GVARinputs){

  T <- nrow(GVARinputs$GVARFactors[[GVARinputs$Economies[1]]]$Factors[[1]])
  G <- length(GVARinputs$GVARFactors$Global)

X <- do.call(rbind,lapply(GVARinputs$GVARFactors$Global,matrix,ncol=T))
if (length(X) != 0 ){
  X <- t(X) # useful command for the case in which G <- 1
  RHS <- as.matrix(X[2:T,])
  LHS <- as.matrix(X[1:(T-1),])

  Phi.w1 <- matrix(NA, nrow=G, ncol= G)
  Phi.w0 <- matrix(NA, nrow=G, ncol= 1)
  for (i in seq_len(G)){
    Phi.w0[i,] <- stats::lm( LHS[,i] ~ RHS)$coefficients[1]
    Phi.w1[i,] <- stats::lm( LHS[,i] ~ RHS)$coefficients[seq_len(max(G, 0)) + 1]
  }

  eta.t <- matrix(NA, nrow= G, ncol=T-1)
  for (j in 1:(T-1)){
    eta.t[,j] <- LHS[j,] - Phi.w0 - Phi.w1%*%RHS[j,]
  }

  Sigma_w <- (eta.t%*%t(eta.t))/T
} else{
  Phi.w0 <- c()
  Phi.w1 <- matrix(,ncol = 0, nrow=0)
  Sigma_w <- matrix(,ncol = 0, nrow=0)
}

ParaExport <- list(Phi.w0 = Phi.w0, Phi.w1 = Phi.w1, Sigma_w = Sigma_w)

return(ParaExport)
}

##########################################################################################################
#' Get the intercept, feedback matrix and the variance-covariance matrix from GVAR without global factors
#'
#'@param ParaVARX Set of VARX model parameters
#'@param GVARinputs List of inputs for GVAR-based models
#'@param Ai0 list containing the country-specific intercepts
#'@param Ai1 list containing the country-specific feedback matrices
#'@param Wi list containing the country-specific link matrices
#'
#'@keywords internal


Get_G0G1Sigma <- function(ParaVARX, GVARinputs, Ai0, Ai1, Wi){

  C <- length(Ai1)
  MN <- nrow(Ai1[[1]])

# a) G0 and G1
G0prep <- list()
G1prep <- list()
for (i in 1:C){
  G0prep[[i]] <- Ai0[[i]]%*%Wi[[i]]
  G1prep[[i]] <- Ai1[[i]]%*%Wi[[i]]
}
G0 <- do.call(rbind,lapply(G0prep,matrix,ncol=C*(MN)))
G1 <- do.call(rbind,lapply(G1prep,matrix,ncol=C*(MN)))


# b) Sigma
Sigma <- matrix(0, ncol=C*(MN), nrow=C*(MN) )
count0 <- 0
for (i in 1:C){
  count1 <- count0+ (MN)
  Sigma[(count0+1):count1,(count0+1):count1] <- ParaVARX[[GVARinputs$Economies[i]]]$Sigma
  count0 <- count1
}

out <- list(G0=G0, G1=G1, Sigma = Sigma)

return(out)

}
############################################################################################################
#'Obtain the country-specific a0
#'
#'@param GVARinputs List of inputs for GVAR-based models
#'@param ParaVARX List containing the set of VARX model parameters
#'
#'@keywords internal


Get_a0<- function(GVARinputs, ParaVARX){

  C <- length(ParaVARX)
  MN <- length(ParaVARX[[1]]$Phi0)

  a0 <- matrix(NA, nrow=C*(MN), ncol=1)

  count0 <- 0
for (j in 1:C){
  count1 <- count0+ MN
  a0[(count0+1):count1] <- do.call(rbind,lapply(ParaVARX[[GVARinputs$Economies[j]]]$Phi0,matrix,ncol=1))
  count0  <- count1
}

return(a0)
}


########################################################################################################
#' Compute the feedback matrix from a GVAR model with global factors
#'
#'@param ParaVARX List containing the set of VARX model parameters
#'@param GVARinputs  List of inputs for GVAR-based models
#'@param G1 feedback matrix from a GVAR without global variables
#'@param Phi.w1 feedback matrix from a marginal model
#'
#'@keywords internal

Get_Gy1<- function(ParaVARX, GVARinputs, G1, Phi.w1){

  G <- length(GVARinputs$GVARFactors$Global)
  C <- length(GVARinputs$Economies)
  MN <- length(ParaVARX[[1]]$Phi0)

  if (G != 0 ){
    D1 <- list()
    for (i in 1:C){
      D1[[i]] <- ParaVARX[[GVARinputs$Economies[[i]]]]$Phi.global
    }
    D1 <- do.call(rbind,lapply(D1,matrix,ncol=G))
  } else { D1 <- c()}

  topGy.1 <- matrix(0, nrow =G, ncol= C*(MN) +G)
  topGy.1[seq_len(G),seq_len(G)] <- Phi.w1

  bottomGy.1 <- cbind(D1,G1)
  Gy.1 <- rbind(topGy.1, bottomGy.1)

  return(Gy.1)
}
#############################################################################################################
#' Check consistency of the inputs provided in GVARinputs
#'
#'@param GVARinputs List of inputs for GVAR-based models
#'@param N number of country-specific spanned factors (scalar)
#'
#'@keywords internal


CheckInputsGVAR <- function(GVARinputs, N){

  # CHECK 1: correctly specified number of spanned factors
  if (!(N %in% 1:8)){
    stop("N, the number of country-specific spanned factors, must be an integer between 1 and 8.")
  }

  # CHECK 2: Check the consistency of the names of the lists in GVARinputs
  if (!all(c("Economies", "GVARFactors", "VARXtype", "Wgvar") %in% names(GVARinputs))){
    stop("The list elements of GVARinputs must be named 'Economies', 'GVARFactors', 'VARXtype', 'Wgvar'")
  }

  # CHECK 3: Check whether country names are correctly specified
  if(!(all(sapply(GVARinputs$Economies, is.character)))){
    stop("All elements of the list 'Economies' must be exclusively country names")
  }

  # CHECK 4: Check for the consistency of VARXtype
  if(!(grepl("^constrained", GVARinputs$VARXtype) || GVARinputs$VARXtype == "unconstrained")){
    stop("GVARinputs$VARXtype must be 'unconstrained'or'constrained'.")
  }

}

###############################################################################################################
#' Generates the star variables necessary for the GVAR estimation
#'
#'@param RiskFactors time series of the risk factors (F x T)
#'@param Economies  string-vector containing the names of the economies which are part of the economic system
#'@param W GVAR transition matrix (C x C)
#'
#'@return List containg the star factors of each country of the economic system
#'
#'@examples
#'data(CM_Factors)

#' Economies <- c("China", "Brazil", "Mexico", "Uruguay")
#' Wgvar <- matrix( c(0, 0.83, 0.86, 0.38, 0.65, 0, 0.13, 0.55,
#'          0.32, 0.12, 0, 0.07, 0.03, 0.05, 0.01, 0), nrow = 4, ncol = 4,
#'          dimnames = list(Economies, Economies))
#' SF <- StarFactors(RiskFactors, Economies, Wgvar)
#'
#'@export


StarFactors <- function(RiskFactors, Economies, W){

  C <- length(Economies)
  T <- ncol(RiskFactors)

  StarLabel <- c()
  ListFactors <- list()


  # Re-arrange country-specific factors per country
  for (i in 1:C){
    IdxRF <- grepl(Economies[i], rownames(RiskFactors))
    ListFactors[[Economies[i]]]$Factors <- RiskFactors[IdxRF,] # Country-specific factors

    VarLabel <- rownames(RiskFactors)[IdxRF] # Label of the country-specific factors
    if (i==1){
      StarLabels <- paste(VarLabel, ".Star", sep="")
    }else{
      StarPrep<- paste(VarLabel, ".Star", sep="")
      StarLabels <- append(StarLabels, StarPrep) # Label of the star-variables
    }
  }


  NumDomFac <- length(IdxRF[IdxRF== TRUE])
  Z <- list()

  for (j in 1:NumDomFac){
    X <- matrix(NA, nrow= C, ncol=T)
    for (i in 1:C){
      X[i,] <- ListFactors[[Economies[i]]]$Factors[j,]
      Z[[j]] <- X # Each element of the list contains the same country-specific variable of all countries
    }
  }


  # Compute the star variables
  StarVariables <- matrix(NA, nrow = C*NumDomFac, ncol = T)
  for (i in 1:C){
    idxx <- (i-1)*NumDomFac
    for (j in 1:NumDomFac){
      StarVariables[idxx + j,]   <- t(W[i,]%*%Z[[j]])
    }
  }

  rownames(StarVariables) <- StarLabels
  colnames(StarVariables) <- colnames(RiskFactors)


  return(StarVariables)
}
