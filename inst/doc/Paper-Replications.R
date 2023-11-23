## -----------------------------------------------------------------------------
library(MultiATSM)

## ---- eval=FALSE--------------------------------------------------------------
#  #########################################################################################################
#  #################################### USER INPUTS ########################################################
#  #########################################################################################################
#  # A) Load database data
#  data("BR_jps_gro_R3")
#  
#  # B) Decide on general model inputs
#  ModelType <- "JPS"
#  
#  StationarityUnderQ <- 0
#  BiasCorrection <- 0
#  WishForwardPremia <- 1
#  
#  
#  Economies <- c("U.S.") # Names of the economies from the economic system
#  GlobalVar <- c()
#  DomVar <- c("GRO", "INF") # Country-specific variables
#  
#  N <- 3 # Number of spanned factors per country
#  
#  DataFrequency <- "Monthly" # Frequency of the data
#  UnitMatYields <- "Month" # time-unit in which yields are expressed. Available options are "Month" or "Year"
#  
#  mat <- c(0.25, 0.5, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10) # maturities expressed in years
#  
#  #########################################################################################################
#  ############################### NO NEED TO MAKE CHANGES FROM HERE #######################################
#  #########################################################################################################
#  # 2) Minor preliminary work
#  C <- length(Economies)
#  M <- length(DomVar)
#  FactorLabels <- LabFac(N, DomVar, GlobalVar, Economies, ModelType) # Generate the set of labels
#  Yields <- t(BR_jps_out$Y)
#  W <- BR_jps_out$W[1:N,] # Use the Wpca matrix from BR (2017)
#  SpaFac <- W%*%Yields
#  rownames(SpaFac) <- FactorLabels$Tables$U.S.[-(1:M)]
#  ZZ <- rbind(t(BR_jps_out$M.o), SpaFac) # Complete set of risk factors
#  
#  
#  # 3) Prepare the inputs of the likelihood function
#  i <- length(Economies)
#    # 3.1) Compute the inputs that go directly into the log-likelihood function
#    ATSMInputs <- InputsForMLEdensity(ModelType, Yields, ZZ, FactorLabels, mat, Economies, DataFrequency)
#    # 3.2) Initial guesses for Variables that will be concentrared out of from the log-likelihood function
#    K1XQ <- ATSMInputs$K1XQ
#    SSZ <- ATSMInputs$SSZ
#    # 3.3) Adjust the inputs which are funtion of the W matrix
#    ATSMInputs$Wpca <- W
#    ATSMInputs$We <- t(pracma::null(W))
#    ATSMInputs$WpcaFull <- rbind(ATSMInputs$Wpca, ATSMInputs$We)
#    ATSMInputs$PP <- SpaFac
#  
#    # 4) Build the objective function
#    f <- Functionf(ATSMInputs, Economies, mat, DataFrequency, FactorLabels, ModelType)
#  
#    # 5) Set the optimization settings
#    VarLab <- ParaLabels(ModelType, StationarityUnderQ)
#  
#    varargin <- list()
#    varargin$K1XQ <-list(K1XQ, VarLab[[ModelType]][["K1XQ"]] , NULL , NULL)
#    varargin$SSZ <- list(SSZ, VarLab[[ModelType]][["SSZ"]], NULL, NULL)
#    varargin$r0 <- list(NULL, VarLab[[ModelType]][["r0"]], NULL, NULL)
#    varargin$se <- list(NULL, VarLab[[ModelType]][["se"]], 1e-6, NULL)
#    varargin$K0Z <- list(NULL, VarLab[[ModelType]][["K0Z"]], NULL, NULL)
#    varargin$K1Z <- list(NULL, VarLab[[ModelType]][["K1Z"]], NULL, NULL)
#    varargin$OptRun <-  c("iter off")
#  
#    LabelVar<- c('Value', 'Label', 'LB', 'UB') # Elements of each parameter
#    for (d in 1:(length(varargin)-1)){ names(varargin[[d]]) <-  LabelVar}
#  
#    tol <- 1e-4
#  
#    # 6) Optimization of the model
#    ModelPara <- Optimization(f, tol, varargin, FactorLabels, Economies, ModelType)$Summary
#  

## ---- echo= FALSE-------------------------------------------------------------
options(scipen = 100) # eliminate the scientific notation
data("BR_jps_gro_R3")
data("JPSrep")

RowsQ <- c("$r0$", "$\\lambda_1$", "$\\lambda_2$", "$\\lambda_3$" )
TableQ <- data.frame(matrix(NA, ncol = 0, nrow =length(RowsQ)))
row.names(TableQ) <- RowsQ

PackageQ<- c(ModelPara$ests$r0, diag(ModelPara$ests$K1XQ))
BRq <- c(BR_jps_out$est.llk$rho0.cP, diag(BR_jps_out$est.llk$KQ.XX))
TableQ$MultiATSM <- PackageQ
TableQ$'BR (2017)' <- BRq

TableQ <- round(TableQ, digits = 5)

suppressWarnings(library(magrittr))

kableExtra::kbl(TableQ, align = "c", caption = "$Q$-dynamics parameters") %>%
  kableExtra::kable_classic("striped", full_width = F)  %>%
  kableExtra::row_spec(0, font_size = 14) %>%
  kableExtra::footnote(general = " $\\lambda$'s are the eigenvalues from the risk-neutral feedback matrix and $r0$ is the long-run mean of the short rate under Q.")

## ---- echo= FALSE-------------------------------------------------------------
data("BR_jps_gro_R3")
data("JPSrep")

RowsP <- c("PC1", "PC2", "PC3", "GRO", "INF")
ColP <- c(" ", RowsP)

# 1) K0Z and K1Z
# Bauer and Rudebusch coefficients
TablePbr <- data.frame(matrix(NA, ncol = length(ColP), nrow =length(RowsP)))
row.names(TablePbr) <- RowsP
colnames(TablePbr) <- ColP

TablePbr[[ColP[1]]] <- BR_jps_out$est.llk$KP.0Z
for(j in 1:length(RowsP) ){TablePbr[[RowsP[j]]] <- BR_jps_out$est.llk$KP.ZZ[,j]}

TablePbr <- round(TablePbr, digits = 5)

# MultiATSM coefficients
TablePMultiATSM <- data.frame(matrix(NA, ncol = length(ColP), nrow =length(RowsP)))
row.names(TablePMultiATSM) <- RowsP
colnames(TablePMultiATSM) <- ColP

IdxVar <- c(3:5, 1:2) # indexes to flip the order of the spanned and unspanned factors
TablePMultiATSM[[ColP[1]]] <- ModelPara$ests$K0Z[IdxVar]
ModelPara$ests$K1Z <- ModelPara$ests$K1Z[IdxVar,IdxVar]
for(j in 1:length(RowsP) ){ TablePMultiATSM[[RowsP[j]]] <- ModelPara$ests$K1Z[,j]}


TablePMultiATSM <- round(TablePMultiATSM, digits = 5)
TableP <- rbind(TablePbr,TablePMultiATSM)
row.names(TableP) <- c(RowsP,paste(RowsP," ",sep="")) # trick to avoid rows to have the same name

kableExtra::kbl(TableP, align = "c", caption = "$P$-dynamics parameters") %>%
  kableExtra::kable_classic("striped", full_width = F)  %>%
  kableExtra::row_spec(0, font_size = 14) %>%
  kableExtra::add_header_above(c(" "= 1, "K0Z"=1, "K1Z" = 5), bold = T) %>%
  kableExtra::pack_rows("BR (2017)", 1, 5) %>%
  kableExtra::pack_rows("MultiATSM", 6, 10) %>%
  kableExtra::footnote(general = " $K0Z$ is the intercept and $K1Z$ is feedback matrix from the $P$-dynamics.")

## ---- echo= FALSE-------------------------------------------------------------
data("BR_jps_gro_R3")
data("JPSrep")

se <- data.frame(BR_jps_out$est.llk$sigma.e, ModelPara$ests$se )
rownames(se) <- "se"
colnames(se) <- c("MultiATSM","BR (2017)")
se <- round(se, digits = 7)

kableExtra::kbl(se, align = "c", caption ="Portfolio of yields with errors") %>%
  kableExtra::kable_classic("striped", full_width = F)  %>%
  kableExtra::row_spec(0, font_size = 14) %>%
  kableExtra::footnote(general = " $se$ is the standard deviation of the portfolio of yields observed with errors.")

## ---- eval=FALSE--------------------------------------------------------------
#  #########################################################################################################
#  #################################### USER INPUTS ########################################################
#  #########################################################################################################
#  # A) Load database data
#  data("CM_Factors")
#  data('CM_Factors_GVAR')
#  data('CM_Trade')
#  data('CM_Yields')
#  
#  # B) Decide on general model inputs
#  ModelType <- "GVAR jointQ" # Options: "GVAR jointQ", "JLL original"
#  
#  StationarityUnderQ <- 0
#  BiasCorrection <- 0
#  WishForwardPremia <- 0
#  FPmatLim <- c(47,48)
#  
#  Economies <- c("China","Brazil","Mexico", "Uruguay")
#  GlobalVar <- c("GBC", "CPI_OECD")
#  DomVar <- c("Eco_Act", "Inflation")
#  
#  N <- 3 # Number of spanned factors per country
#  
#  OutputLabel <- "CM_2021"
#  DataFrequency <- "Monthly"
#  UnitMatYields <- "Month"
#  
#  # B.1) Decide on specific model inputs
#  #################################### GVAR-based models ##################################################
#  t_First_Wgvar <- "2004"
#  t_Last_Wgvar <-  "2019"
#  W_type <- 'Sample Mean'
#  VARXtype <- "unconstrained"
#  #################################### JLL-based models ###################################################
#  DomUnit <- "China"
#  WishSigmas <- 0
#  SigmaNonOrtho <- NULL
#  JLLModelType <- ModelType
#  ###################################### BRW inputs  ######################################################
#  flag_mean <- TRUE
#  gamma <- 0.1
#  N_iter <- 500
#  N_burn <- N_iter*0.15
#  B <- 50
#  checkBRW <- 1
#  B_check <- 1000
#  #########################################################################################################
#  
#  # C) Decide on Settings for numerical outputs
#  Horiz <- 25
#  DesiredGraphs <- c("Fit", "GIRF", "GFEVD")
#  WishGraphRiskFac <- 0
#  WishGraphYields <- 1
#  WishOrthoJLLgraphs <- 1
#  
#  # D) Bootstrap settings
#  WishBootstrap <- 0
#  Bootlist <- list()
#  Bootlist$methodBS <- 'bs'
#  Bootlist$BlockLength <- 4
#  Bootlist$ndraws <-  1000
#  Bootlist$pctg   <-  95
#  
#  # E) Out-of-sample forecast
#  WishForecast <- 1
#  ForecastList <- list()
#  ForecastList$ForHoriz <- 12
#  ForecastList$t0Sample <- 1
#  ForecastList$t0Forecast <- 90
#  
#  #########################################################################################################
#  ############################### NO NEED TO MAKE CHANGES FROM HERE #######################################
#  #########################################################################################################
#  
#  # 2) Minor preliminary work
#  C <- length(Economies)
#  FactorLabels <- LabFac(N, DomVar,GlobalVar, Economies, ModelType)
#  ZZ <- RiskFactors
#  
#  Data <- list()
#  Data$GVARFactors <- FactorsGVAR
#  mat <- Maturities(Yields, Economies, UnitYields = UnitMatYields)
#  
#  # 2.1) Generate GVARinputs, JLLinputs and BRWinputs
#  ModInputs <- ListModelInputs(ModelType, Data, Economies, VARXtype, t_First_Wgvar, t_Last_Wgvar, W_type,
#                               DomUnit, WishSigmas, SigmaNonOrtho, BiasCorrection, flag_mean, gamma,
#                               N_iter, N_burn, B, checkBRW, B_check)
#  
#  GVARinputs <- ModInputs$GVARinputs
#  JLLinputs <- ModInputs$JLLinputs
#  BRWinputs <- ModInputs$BRWinputs
#  
#  # 3) Prepare the inputs of the likelihood function
#  ModelParaList <- list()
#  
#    # 3.1) Compute the inputs that go directly into the log-likelihood function
#    ATSMInputs <- InputsForMLEdensity(ModelType, Yields, ZZ, FactorLabels, mat, Economies, DataFrequency,
#                                      JLLinputs, GVARinputs, BRWinputs)
#  
#  
#    # 3.2) Initial guesses for Variables that will be concentrared out of from the log-likelihood function
#    K1XQ <- ATSMInputs$K1XQ
#    if (ModelType == "JLL original"){ SSZ <- NULL}else{SSZ <- ATSMInputs$SSZ}
#  
#    # 4) Build the objective function
#    f <- Functionf(ATSMInputs, Economies, mat, DataFrequency, FactorLabels, ModelType)
#  
#    # 5) Set the optimization settings
#    VarLab <- ParaLabels(ModelType, StationarityUnderQ)
#  
#    varargin <- list()
#    varargin$K1XQ <-list(K1XQ, VarLab[[ModelType]][["K1XQ"]] , NULL , NULL)
#    varargin$SSZ <- list(SSZ, VarLab[[ModelType]][["SSZ"]], NULL, NULL)
#    varargin$r0 <- list(NULL, VarLab[[ModelType]][["r0"]], NULL, NULL)
#    varargin$se <- list(NULL, VarLab[[ModelType]][["se"]], 1e-6, NULL)
#    varargin$K0Z <- list(NULL, VarLab[[ModelType]][["K0Z"]], NULL, NULL)
#    varargin$K1Z <- list(NULL, VarLab[[ModelType]][["K1Z"]], NULL, NULL)
#    varargin$OptRun <-  c("iter off")
#  
#    LabelVar<- c('Value', 'Label', 'LB', 'UB')
#    for (d in 1:(length(varargin)-1)){ names(varargin[[d]]) <-  LabelVar}
#  
#    tol <- 1e-4
#    # 6) Optimization of the model
#    ModelParaList[[ModelType]] <- Optimization(f, tol, varargin, FactorLabels, Economies, ModelType,
#                                               JLLinputs, GVARinputs)$Summary
#  
#  # 7) Numerical Outputs
#  InputsForOutputs <- InputsForOutputs(ModelType, Horiz, DesiredGraphs, OutputLabel, StationarityUnderQ,
#                                       UnitMatYields, WishGraphYields, WishGraphRiskFac, WishOrthoJLLgraphs,
#                                       WishForwardPremia, FPmatLim, WishBootstrap, Bootlist, WishForecast,
#                                       ForecastList)
#  # A) Fit, IRF, FEVD, GIRF, GFEVD and Risk premia
#  NumericalOutputs <- NumOutputs(ModelType, ModelParaList, InputsForOutputs, FactorLabels, Economies)
#  
#  # B) Bootstrap
#  Bootstrap <- Bootstrap(ModelType, ModelParaList, NumericalOutputs, mat, Economies, InputsForOutputs,
#                         FactorLabels, DataFrequency, varargin, JLLinputs, GVARinputs, BRWinputs)
#  
#  # C) Out-of-sample forecasting
#  Forecasts <- ForecastYields(ModelType, ModelParaList, InputsForOutputs, FactorLabels, Economies,
#                              DataFrequency, JLLinputs, GVARinputs, BRWinputs)
#  
#  
#  

## ---- eval=FALSE--------------------------------------------------------------
#  # A) Load database data
#  data("CM_Factors_2023")
#  data('CM_Factors_GVAR_2023')
#  data('CM_Trade_2023')
#  data('CM_Yields_2023')
#  
#  # B) Decide on general model inputs
#  ModelType <- "GVAR jointQ"
#  
#  StationarityUnderQ <- 0
#  BiasCorrection <- 0
#  
#  WishForwardPremia <- 1
#  FPmatLim <- c(47,48)
#  
#  Economies <- c("Brazil", "India", "Russia", "Mexico") # Names of the economies from the economic system
#  GlobalVar <- c("US_Output_growth", "China_Output_growth", "SP500") # Global Variables
#  DomVar <- c("Inflation","Output_growth", "CDS", "COVID") # Country-specific variables
#  
#  N <- 2 # Number of spanned factors per country
#  
#  OutputLabel <- "CM_2023"
#  DataFrequency <- "Weekly"
#  UnitMatYields <- "Month"
#  
#  # C.1) Decide on specific model inputs
#  #################################### GVAR-based models ##################################################
#  t_First_Wgvar <- "2015"
#  t_Last_Wgvar <-  "2020"
#  W_type <- 'Sample Mean'
#  VARXtype <- "constrained: COVID"
#  ###################################### BRW inputs  ######################################################
#  
#  # D) Decide on Settings for numerical outputs
#  Horiz <- 12
#  DesiredGraphs <- c("GIRF", "GFEVD", "TermPremia")
#  WishGraphRiskFac <- 0
#  WishGraphYields <- 1
#  WishOrthoJLLgraphs <- 0
#  
#  # E) Bootstrap settings
#  WishBootstrap <- 0 #  YES: 1; No = 0.
#  Bootlist <- list()
#  Bootlist$methodBS <- 'bs'
#  Bootlist$BlockLength <- 4 # necessary input if one chooses the block bootstrap
#  Bootlist$ndraws <-  100
#  Bootlist$pctg   <-  95 # confidence level
#  
#  # F) Out-of-sample forecast
#  WishForecast <- 0
#  ForecastList <- list()
#  ForecastList$ForHoriz <- 12 # forecast horizon
#  ForecastList$t0Sample <- 1 # initial sample date
#  ForecastList$t0Forecast <- 50 # last sample date for the first forecast
#  
#  ##########################################################################################################
#  ############################### NO NEED TO MAKE CHANGES FROM HERE ########################################
#  ##########################################################################################################
#  # 2) Minor preliminary work
#  C <- length(Economies)
#  FactorLabels <- LabFac(N, DomVar,GlobalVar, Economies, ModelType)
#  ZZ <- RiskFactors
#  
#  Data <- list()
#  Data$GVARFactors <- GVARFactors
#  Data$Wgvar <-  Trade_Flows
#  mat <- Maturities(Yields, Economies, UnitYields = UnitMatYields)
#  
#  # 2.1) Generate GVARinputs, JLLinputs and BRWinputs
#  ModInputs <- ListModelInputs(ModelType, Data, Economies, VARXtype, t_First_Wgvar, t_Last_Wgvar, W_type)
#  
#  GVARinputs <- ModInputs$GVARinputs
#  JLLinputs <- NULL
#  
#  # 3) Prepare the inputs of the likelihood function
#  # 3.1) Compute the inputs that go directly into the log-likelihood function
#  ATSMInputs <- InputsForMLEdensity(ModelType, Yields, ZZ, FactorLabels, mat, Economies, DataFrequency,
#                                    JLLinputs, GVARinputs)
#  
#  # 3.2) Initial guesses for Variables that will be concentrared out of from the log-likelihood function
#  K1XQ <- ATSMInputs$K1XQ
#  SSZ <- ATSMInputs$SSZ
#  
#  # 4) Build the objective function
#  f <- Functionf(ATSMInputs, Economies, mat, DataFrequency, FactorLabels, ModelType)
#  
#  # 5) Set the optimization settings
#  VarLab <- ParaLabels(ModelType, StationarityUnderQ)
#  
#  varargin <- list()
#  varargin$K1XQ <-list(K1XQ, VarLab[[ModelType]][["K1XQ"]] , NULL , NULL)
#  varargin$SSZ <- list(SSZ, VarLab[[ModelType]][["SSZ"]], NULL, NULL)
#  varargin$r0 <- list(NULL, VarLab[[ModelType]][["r0"]], NULL, NULL)
#  varargin$se <- list(NULL, VarLab[[ModelType]][["se"]], 1e-6, NULL)
#  varargin$K0Z <- list(NULL, VarLab[[ModelType]][["K0Z"]], NULL, NULL)
#  varargin$K1Z <- list(NULL, VarLab[[ModelType]][["K1Z"]], NULL, NULL)
#  varargin$OptRun <-  c("iter off")
#  
#  LabelVar<- c('Value', 'Label', 'LB', 'UB') # Elements of each parameter
#  for (d in 1:(length(varargin)-1)){ names(varargin[[d]]) <-  LabelVar}
#  
#  tol <- 1e-4
#  # 6) Optimization of the model
#  ModelParaList <- list()
#  ModelParaList[[ModelType]] <- Optimization(f, tol, varargin, FactorLabels, Economies, ModelType,
#                                             JLLinputs, GVARinputs)$Summary
#  
#  # 7) Numerical Outputs
#  InputsForOutputs <- InputsForOutputs(ModelType, Horiz, DesiredGraphs, OutputLabel, StationarityUnderQ,
#                                       UnitMatYields, WishGraphYields, WishGraphRiskFac, WishOrthoJLLgraphs,
#                                       WishForwardPremia, FPmatLim, WishBootstrap, Bootlist, WishForecast,
#                                       ForecastList)
#  # A) Fit, IRF, FEVD, GIRF, GFEVD, and Term Premia
#  NumericalOutputs <- NumOutputs(ModelType, ModelParaList, InputsForOutputs, FactorLabels, Economies)
#  
#  # B) Bootstrap
#  Bootstrap <- Bootstrap(ModelType, ModelParaList, NumericalOutputs, mat, Economies, InputsForOutputs,
#                         FactorLabels, DataFrequency, varargin, JLLinputs, GVARinputs)

