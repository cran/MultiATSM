#' Generates forecasts of bond yields for all model types
#'
#' @param ModelType A character vector indicating the model type to be estimated.
#' @param ModelPara A list containing the point estimates of the model parameters. For details, refer to the outputs from the \code{\link{Optimization}} function.
#' @param InputsForOutputs A list containing the necessary inputs for generating IRFs, GIRFs, FEVDs, GFEVDs and Term Premia.
#' @param FactorLabels A list of character vectors with labels for all variables in the model.
#' @param Economies A character vector containing the names of the economies included in the system.
#' @param JLLlist A list of necessary inputs for the estimation of JLL-based models (see the \code{\link{JLL}} function).
#' @param GVARlist A list containing the necessary inputs for the estimation of GVAR-based models (see the \code{\link{GVAR}} function).
#' @param WishBRW Whether to estimate the physical parameter model with bias correction, based on the method by Bauer, Rudebusch and Wu (2012) (see \code{\link{Bias_Correc_VAR}} function). Default is set to 0.
#' @param BRWlist List of necessary inputs for performing the bias-corrected estimation (see \code{\link{Bias_Correc_VAR}} function).
#'
#' @examples
#' # See an example of implementation in the vignette file of this package (Section 4).
#'
#' @return
#' An object of class 'ATSMModelForecast' containing the following elements:
#' \enumerate{
#' \item Out-of-sample forecasts of bond yields per forecast horizon
#' \item Out-of-sample forecast errors of bond yields per forecast horizon
#' \item Root mean square errors per forecast horizon
#' }
#'
#' @section Available Methods:
#' - `plot(object)`
#'
#'
#' @export

ForecastYields <- function(ModelType, ModelPara, InputsForOutputs, FactorLabels, Economies, JLLlist = NULL,
                           GVARlist = NULL, WishBRW, BRWlist = NULL) {

  cat("4) OUT-OF-SAMPLE FORECASTING ANALYSIS \n")
  forecast_info <- InputsForOutputs[[ModelType]]$Forecasting

  if (!forecast_info$WishForecast) {
    cat("No bond yields forecasts were generated \n")
    return(NULL)
  }

  start_time <- Sys.time()

  # 1) Redefine some general model outputs
  StatQ <- InputsForOutputs$StationaryQ
  UnitMatYields <- InputsForOutputs$UnitMatYields
  DataFreq <- InputsForOutputs$DataFreq

  ForecastType <- forecast_info$ForType
  t0Sample <- forecast_info$t0Sample
  t0Forecast <- forecast_info$t0Forecast
  H <- forecast_info$ForHoriz
  T <- ncol(if (any(ModelType %in% c("JPS original", "JPS global", "GVAR single"))) {
    ModelPara[[ModelType]][[Economies[1]]]$inputs$Y
  } else {
    ModelPara[[ModelType]]$inputs$Y
  })

  nForecasts <- T - t0Forecast - H + 1 # Number of times that the model will be re-estimated

  # 2) Perform preliminary consistency checks
  ChecksOOS(t0Forecast, t0Sample, nForecasts, ForecastType, T)

  # 3) Redefine full sample time series
  YieldsFull <- GetYields_AllCountries(ModelPara, Economies, ModelType)
  TimeSeries_Labels_Full <- colnames(YieldsFull)
  Facts <- Get_Unspanned(ModelPara, FactorLabels, Economies, ModelType)

  # 4) Model Estimation
  Forecast_AllDates <- list()
  t_last_forecast <- t0Forecast

  for (tt in 1:nForecasts) {
    # First observation of the information set
    if (ForecastType == "Rolling" & tt >= 2) {
      t0Sample <- t0Sample + 1
    }
    # Last observation of the information set
    if (tt > 1) t_last_forecast <- t_last_forecast + 1

    # Redefine the dataset used in the estimation
    T0_SubSample <- TimeSeries_Labels_Full[t0Sample]
    TF_SubSample <- TimeSeries_Labels_Full[t_last_forecast]

    # 4.1) Prepare the inputs of the likelihood function
    invisible(utils::capture.output(ATSMInputs <- InputsForOpt(T0_SubSample, TF_SubSample, ModelType, YieldsFull, Facts$Glob,
                                                               Facts$Dom, FactorLabels, Economies, DataFreq, GVARlist, JLLlist,
                                                               WishBRW, BRWlist, UnitMatYields, CheckInputs = FALSE)))

    # 4.2) Optimization of the ATSM
    invisible(utils::capture.output(FullModelParaList <- Optimization(ATSMInputs, StatQ, DataFreq, FactorLabels,
                                                                      Economies, ModelType, TimeCount = FALSE)))


    # 5) Forecasting bond yields
    Forecast_OneDate <- OOS_Forecast(H, t_last_forecast, FullModelParaList, FactorLabels, YieldsFull,
                                     Economies, ModelType)
    Forecast_AllDates <- Gather_Forecasts(Forecast_OneDate, Forecast_AllDates, Economies, ModelType)

    cat(sprintf("Out-of-sample forecast for the information set: %s || %s \n",
                T0_SubSample, TF_SubSample))
    saveRDS(Forecast_AllDates, paste(tempdir(), "/Forecast_", InputsForOutputs$'Label Outputs', '.rds', sep = ""))
  }

  # 6) RMSE
  OutofSampleForecast <- stats::setNames(list(Forecast_AllDates), ModelType)
  RMSEs <- RMSE(OutofSampleForecast)

  OutofSampleForecast <- append(OutofSampleForecast[[ModelType]], list(RMSE = RMSEs))

  saveRDS(OutofSampleForecast, paste(tempdir(), "/Forecast_", InputsForOutputs$'Label Outputs', '.rds', sep = ""))
  Optimization_Time(start_time)

  # Store metadata inside the class without explicitly exporting it
  attr(OutofSampleForecast, "ModelForecast") <- list(Economies = Economies, ModelType = ModelType, ForHoriz = H)

  return(structure(OutofSampleForecast, class = "ATSMModelForecast"))
}
#############################################################################################################
#' Preliminary checks for inputs provided for the performing out-of-sample forecasting
#'
#' @param t0Forecast Index of the last set of observations in the information set at the first forecasting round
#' @param t0Sample Index of the first set of observations in the information set at the first forecasting round
#' @param nForecasts Number of forecasting sets generated
#' @param ForecastType Forecast type. Available options are "Rolling" and "Expanding".
#' @param TimeLength Time-series dimension of the model
#'
#' @keywords internal

ChecksOOS <- function(t0Forecast, t0Sample, nForecasts, ForecastType, TimeLength){
  # CHECK 1: consistency  of the initial forecasting date
  if (t0Forecast < t0Sample ){stop("The first forecast cut-off date is earlier than the start of the sample.")}
  # CHECK 2: consistency of the first forecasting date range
  if (t0Forecast > TimeLength){stop("The first forecast cut-off date is longer than the sample length.")}
  # CHECK 3: check availability of the forecast method
  if(!any(ForecastType ==c("Rolling","Expanding"))){stop(paste("Forecasts method", ForecastType, "is not available. Available options are 'Expanding' or 'Rolling'."))}
  # CHECK 4: consistency of the number of forecasts
  if (nForecasts <= 0){stop("Impossible to generate forecast errors: sample period is extrapolated.")}
}

##############################################################################################################
#' Gather all country-specific yields in a single matrix of dimension CJ x T
#'
#' @param ModelPara List of model parameter estimates
#' @param Economies String-vector containing the names of the economies which are part of the economic system
#' @param ModelType A string-vector containing the label of the model to be estimated
#'
#' @keywords internal

GetYields_AllCountries <- function(ModelPara, Economies, ModelType){

  C <- length(Economies)

  if ( any(ModelType ==c("JPS original", "JPS global", "GVAR single"))){
    YieldsFull <- do.call(rbind, lapply(1:C, function(i) {
      ModelPara[[ModelType]][[Economies[i]]]$inputs$Y
    }))
  }else{
    YieldsFull <- ModelPara[[ModelType]]$inputs$Y
  }

  return(YieldsFull)
}

#############################################################################################################
#' Obtain the indexes of both the domestic and global unspanned factors
#'
#' @param RiskFactors_TS Time series of risk factors for the jointly estimated models (CJ x T)
#' @param FactorLabels A string-list based which contains all the labels of all the variables present in the model
#' @param Economies String-vector containing the names of the economies which are part of the economic system
#'
#' @keywords internal

Idx_UnspanFact <- function(RiskFactors_TS, FactorLabels, Economies){

  N <- length(FactorLabels$Spanned)
  M <- length(FactorLabels$Domestic) - N
  G <- length(FactorLabels$Global)
  C <- length(Economies)

  IdxGlobal <- seq_len(G)
  IdxSpa  <- IdxSpanned(G, M, N, C)
  IdxAll <- 1:nrow(RiskFactors_TS)
  IdxunSpa <- IdxAll[-c(IdxGlobal,IdxSpa)]

  return(list(IdxunSpa = IdxunSpa, IdxGlobal= IdxGlobal))
}
########################################################################################################
#' Collect both the domestic and global unspanned factors of all countries in single matrices
#'
#' @param ModelPara List of model parameter estimates
#' @param FactorLabels A string-list based which contains all the labels of all the variables present in the model
#' @param Economies String-vector containing the names of the economies which are part of the economic system
#' @param ModelType A string-vector containing the label of the model to be estimated
#'
#' @keywords internal

Get_Unspanned <- function(ModelPara, FactorLabels, Economies, ModelType){

  if (any(ModelType %in% c("JPS original", "JPS global", "GVAR single"))){
    G <- length(FactorLabels$Global)
    N <- length(FactorLabels$Spanned)
    C <- length(Economies)

    UnspannedFactors_CS <- function(Economy, G, N) {
      AllFactors <- ModelPara[[ModelType]][[Economy]]$inputs$AllFactors
      AllFactors[(G + 1):(nrow(AllFactors) - N), ]
    }

    DomesticMacroVar <- do.call(rbind, lapply(Economies, UnspannedFactors_CS, G, N))
    GlobalMacroVar <- ModelPara[[ModelType]][[Economies[1]]]$inputs$AllFactors[seq_len(G), , drop = FALSE]

} else{
  ZZfull <- ModelPara[[ModelType]]$inputs$AllFactors
  Idxs <- Idx_UnspanFact(ZZfull, FactorLabels, Economies) # indexes of the variable of interest
  GlobalMacroVar <- ZZfull[Idxs$IdxGlobal, , drop = FALSE]
  DomesticMacroVar <- ZZfull[Idxs$IdxunSpa, , drop = FALSE]
}
  return(list(Dom = DomesticMacroVar, Glob = GlobalMacroVar))
}

#############################################################################################################
#' Perform out-of-sample forecast of bond yields
#'
#' @param ForHoriz Forecast horizon-ahead (scalar)
#' @param t_Last Index of the last set of observations in the information set at a given forecasting round
#' @param ModelParaList List of model parameter estimates
#' @param FactorLabels A string-list based which contains all the labels of all the variables present in the model
#' @param Yields_FullSample Time-series of bond yields, complete set (J x T or CJ x T)
#' @param Economies String-vector containing the names of the economies which are part of the economic system
#' @param ModelType A string-vector containing the label of the model to be estimated
#'
#' @keywords internal

OOS_Forecast <- function(ForHoriz, t_Last, ModelParaList, FactorLabels, Yields_FullSample, Economies, ModelType){


  ForOut <- list()
  ForecastLabels <- colnames(Yields_FullSample[ ,(t_Last+1):(t_Last + ForHoriz)])
  ForecastDate <- ForecastLabels[length(ForecastLabels)]

  # 1) Forecast of yields
  ForecastYields <- YieldFor(ModelParaList, ForHoriz, Economies, FactorLabels, ForecastLabels, ModelType)


  # 2) Actual yields for the period of the forecasting and Forecast errors
  if ( any(ModelType ==c("GVAR multi", "JPS multi", "JLL original", "JLL No DomUnit", "JLL joint Sigma"))){

  YieldsObsForPer <- Yields_FullSample[ ,(t_Last+1):(t_Last+ForHoriz)]
  ForecastError <- YieldsObsForPer - ForecastYields
  # Outputs to export
  ForOut[[ForecastDate]]$Forcast <- ForecastYields
  ForOut[[ForecastDate]]$Error <- ForecastError

  } else {

    for (i in 1:length(Economies)){
    YieldsObsForPer <- Yields_FullSample[ grep(Economies[i], rownames(Yields_FullSample)) ,(t_Last+1):(t_Last+ForHoriz)]
    ForecastError <- YieldsObsForPer - ForecastYields[[Economies[i]]]
      # Outputs to export
    ForOut[[Economies[i]]][[ForecastDate]]$Forcast <- ForecastYields[[Economies[i]]]
    ForOut[[Economies[i]]][[ForecastDate]]$Error <- ForecastError
    }
  }

  return(ForOut)
}
##############################################################################################################
#' Compile the bond yield forecast for any model type
#'
#' @param ModelParaList List of model parameter estimates
#' @param ForHoriz Forecast horizon (scalar)
#' @param Economies String-vector containing the names of the economies which are part of the economic system
#' @param FactorLabels A string-list based which contains all the labels of all the variables present in the model
#' @param ForLabels Forecast labels (string-based vector)
#' @param ModelType A string-vector containing the label of the model to be estimated
#'
#' @keywords internal

YieldFor <- function(ModelParaList, ForHoriz, Economies, FactorLabels, ForLabels, ModelType){

  C <-  length(Economies)
  N <- length(FactorLabels$Spanned)
  M <- length(FactorLabels$Domestic) - N
  G <- length(FactorLabels$Global)


  # Define general inputs
  if ( any(ModelType ==c("GVAR multi", "JPS multi", "JLL original", "JLL No DomUnit", "JLL joint Sigma"))){

    J <- length(ModelParaList[[ModelType]]$inputs$mat)
    A <- ModelParaList[[ModelType]]$rot$P$A
    K0Z <- ModelParaList[[ModelType]]$ests$K0Z
    K1Z <- ModelParaList[[ModelType]]$ests$K1Z

    Bspanned <- ModelParaList[[ModelType]]$rot$P$B
    Bfull <- BUnspannedAdapJoint(G, M, N, C, J, Bspanned)
    ZZtemp <- ModelParaList[[ModelType]]$inputs$AllFactors

    YiedlsLab <- rownames(ModelParaList[[ModelType]]$inputs$Y)
    ForecastYields <- Gen_Forecast_Yields(K0Z, K1Z, A, Bfull, ZZtemp, C, J, YiedlsLab, ForLabels, ForHoriz, ModelType)
  } else {

    J <- length(ModelParaList[[ModelType]][[Economies[1]]]$inputs$mat)
    ForecastYields <- list()
    for (i in 1:length(Economies)){
    A <- ModelParaList[[ModelType]][[Economies[i]]]$rot$P$A
    Bfull <- BUnspannedAdapSep(G, M, ModelParaList[[ModelType]], Economies, Economy = Economies[i], ModelType)
    K0Z <- ModelParaList[[ModelType]][[Economies[i]]]$ests$K0Z
    K1Z <- ModelParaList[[ModelType]][[Economies[i]]]$ests$K1Z

    ZZtemp <- ModelParaList[[ModelType]][[Economies[i]]]$inputs$AllFactors
    YiedlsLab <- rownames(ModelParaList[[ModelType]][[Economies[i]]]$inputs$Y)
    ForecastYields[[Economies[i]]] <- Gen_Forecast_Yields(K0Z, K1Z, A, Bfull, ZZtemp, C, J, YiedlsLab,
                                                          ForLabels, ForHoriz, ModelType)
    }
    }

  return(ForecastYields)
}
###############################################################################################################
#' Compute the bond yield forecast for any model type
#'
#' @param K0Z Intercept from the P-dynamics (F x 1)
#' @param K1Z Feedback matrix from the P-dynamics (F x F)
#' @param A Intercept of model-implied yields model (J x 1)
#' @param Bfull Slope of model-implied yields model (J x N or CJ x CN)
#' @param ZZsubsample Sub-sample of risk factors (F x t)
#' @param C Number of countries in the economic cohort (scalar)
#' @param J Number of country-specific bond yields
#' @param YieldsLabels Labels of bond yields
#' @param ForLabels Forecast labels (string-based vector)
#' @param ForHoriz Forecast horizon (scalar)
#' @param ModelType A string-vector containing the label of the model to be estimated
#'
#' @keywords internal

Gen_Forecast_Yields <- function(K0Z, K1Z, A, Bfull, ZZsubsample, C, J, YieldsLabels, ForLabels, ForHoriz, ModelType){

  if ( any(ModelType ==c("GVAR multi", "JPS multi", "JLL original", "JLL No DomUnit", "JLL joint Sigma"))){
    ForecastYields <- matrix(NA, nrow = C*J, ncol = ForHoriz)
  }else{
    ForecastYields <- matrix(NA, nrow = J, ncol = ForHoriz)
  }
  dimnames(ForecastYields) <- list(YieldsLabels, ForLabels)

  ZZtt <- ZZsubsample[ , ncol(ZZsubsample)]
  K1ZsumOld <- 0

  for (hh in 1:ForHoriz){
    if(hh == 1) {
      K1Znew <- diag(nrow(K1Z))
      K1Zhh <- K1Z
    }else{
      K1Znew <- K1Znew%*%K1Z
      K1Zhh <- K1Zhh%*%K1Z
    }

    VARforecast <- (K1ZsumOld + K1Znew)%*%K0Z + K1Zhh%*%ZZtt
    ForecastYields[,hh] <- A + Bfull%*%(VARforecast)

    K1ZsumOld <- K1ZsumOld + K1Znew
  }

  return(ForecastYields)
}

###############################################################################################################
#' Compute the root mean square error for all models
#'
#' @param ForecastOutputs List of country-specific forecasts
#'
#' @keywords internal

RMSE <- function(ForecastOutputs){

  ModelType <- names(ForecastOutputs)
  SepQ_Labels <- c("JPS original", "JPS global", "GVAR single")

  # 1) SepQ models
  if (ModelType %in% SepQ_Labels) {

    nfor <- length(ForecastOutputs[[1]][[1]])
    Economies <- names(ForecastOutputs[[1]])
    H <- ncol(ForecastOutputs[[1]][[1]][[1]][[1]])
    C <- length(Economies)

    FElist <- vector("list", C)
    names(FElist) <- Economies

    ForecastData <- ForecastOutputs[[ModelType]]

    # Extract forecast errors
    for (i in seq_along(Economies)) {
      FElist[[Economies[i]]] <- lapply(seq_len(nfor), function(h) ForecastData[[Economies[i]]][[h]]$Error)
    }

    # Compute RMSE
    rmse <- lapply(FElist, function(errors) {
      result <- sqrt(Reduce("+", lapply(errors, function(x) x^2 / nfor)))
      colnames(result) <- seq_len(H)
      return(result)
    })

  } else {
    # 2) JointQ models
    nfor <- length(ForecastOutputs[[1]])
    Modeljoint <- ModelType
    H <- ncol(ForecastOutputs[[1]][[1]][[1]])

    FElist <- lapply(seq_len(nfor), function(h) ForecastOutputs[[Modeljoint]][[h]]$Error)

    rmse <- list()
    rmse[[Modeljoint]] <- sqrt(Reduce("+", lapply(FElist, function(x) x^2 / nfor)))
    colnames(rmse[[Modeljoint]]) <- seq_len(H)
  }

  return(rmse)
}

#################################################################################################################
#' Gather several forecast dates
#'
#' @param Forecast_OneDate Bond yield forecasts for a single date
#' @param Forecast_AllDates Bond yield forecasts for multiple dates
#' @param Economies String-vector containing the names of the economies which are part of the economic system
#' @param ModelType String-vector containing the label of the model to be estimated
#'
#' @keywords internal

Gather_Forecasts <- function(Forecast_OneDate, Forecast_AllDates, Economies, ModelType){

  if( ModelType %in% c("JPS original", "JPS global", "GVAR single")){
    Forecast_AllDates <- utils::modifyList(Forecast_AllDates, Forecast_OneDate)
  } else {
    Forecast_AllDates <- append(Forecast_AllDates, Forecast_OneDate)
  }

  return(Forecast_AllDates)
}
