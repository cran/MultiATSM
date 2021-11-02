## ------------------------------------------------------------------------
library(MultiATSM)

## ----ModFea, message=FALSE, echo=FALSE-----------------------------------
ModelLabels <- c("JPS", "JPS jointP", "VAR jointQ", "GVAR sepQ", "GVAR jointQ", 
                 "JLL original", "JLL NoDomUnit", "JLL jointSigma")

# Rows
Tab <- data.frame(matrix(nrow = length(ModelLabels), ncol = 0)) 
rownames(Tab) <- ModelLabels

# Empty columns
EmptyCol <- c("", "", "", "", "", "", "", "") 
Tab$EmptyCol0 <- EmptyCol
# P-dynamics + 2 empty spaces
Tab$PdynIndUnco <- c("x", "x", "", "", "", "", "", "")
Tab$PdynIndCo <- c("", "", "", "", "", "", "", "")
Tab$PdynJointUnco <- c("", "", "x", "", "", "", "", "")
Tab$PdynJointJLL <- c("", "", "", "", "", "x", "x", "x")
Tab$PdynJointGVAR <- c("", "", "", "x", "x", "", "", "")
Tab$EmptyCol1 <- EmptyCol
Tab$EmptyCol2 <- EmptyCol
# Q-dynamics + 2 empty spaces
Tab$QdynInd <- c("x", "", "", "x", "", "", "", "")   
Tab$QdynJoint <- c("", "x", "x", "", "x", "x", "x", "x") 
Tab$EmptyCol3 <- EmptyCol
Tab$EmptyCol4 <- EmptyCol
# Sigma + 2 empty spaces
Tab$Ponly <-  c("", "", "", "", "", "x", "x", "")
Tab$PandQ <- c("x", "x", "x", "x", "x", "", "", "x")
Tab$EmptyCol5 <- EmptyCol
Tab$EmptyCol6 <- EmptyCol
# Dominant Unit
Tab$DomUnit <- c("", "", "", "", "", "x", "", "x")

# Adjust column names
ColNames <- c("","","","","JLL", "GVAR", "", "", "", "", "", "", "","", "", "","")
colnames(Tab) <- ColNames


# Generate the table
suppressWarnings(library(magrittr))


kableExtra::kbl(Tab, align = "c", caption = "Model Features") %>%
  kableExtra::kable_classic("striped", full_width = F)  %>%
  kableExtra::row_spec(0, font_size = 10) %>%
  kableExtra::add_header_above(c(" "=2, "Unrestricted" = 1, "Restricted" = 1, "Unrestricted" = 1, "Restricted" = 2, " " = 11)) %>%
 kableExtra::add_header_above(c(" "=2, "Individual" = 2, "Joint" = 3, " "=2, "Individual" = 1, "Joint" = 1, " "=2, "P only" = 1, "P and Q" = 1, " " = 3))  %>%
  kableExtra::add_header_above(c( " "=2, "P-dynamics"= 5, " "=2, "Q-dynamics"= 2, " "=2, "Sigma matrix estimation" = 2, " "=2, "Dominant Country"=1), bold = T) %>%
kableExtra::pack_rows("Unrestricted VAR", 1, 3 , label_row_css = "background-color: #666; color: #fff;")  %>%
kableExtra::pack_rows("Restricted VAR (GVAR)", 4, 5, label_row_css = "background-color: #666; color: #fff;") %>%
kableExtra::pack_rows("Restricted VAR (JLL)", 6, 8, label_row_css = "background-color: #666; color: #fff;")

## ------------------------------------------------------------------------
data('CM_Yields')

## ------------------------------------------------------------------------
data('CM_Factors')

## ------------------------------------------------------------------------
data('CM_Trade')

## ------------------------------------------------------------------------
data('CM_Factors_GVAR')

## ------------------------------------------------------------------------
Initial_Date <- "2006-09-01" # Format "yyyy-mm-dd"
Final_Date <- "2019-01-01" # Format "yyyy-mm-dd"
DataFrequency <- "Monthly"
GlobalVar <- c("GBC", "VIX") # Global Variables
DomVar <- c("Eco_Act", "Inflation", "Com_Prices", "Exc_Rates") #  Domestic variables
N <-  3 # Number of spanned factors per country
Economies <- c("China", "Mexico", "Uruguay", "Brazil", "Russia")
ModelType <- "JPS"

## ------------------------------------------------------------------------
FactorLabels <- LabFac(N, DomVar, GlobalVar, Economies, ModelType)
ZZfull <-DataForEstimation(Initial_Date, Final_Date, Economies, N, FactorLabels, ModelType, DataFrequency)

## ------------------------------------------------------------------------
ModelType <- "JPS"
DataFrequency <- "Monthly"
Economies <- c("China", "Brazil", "Mexico", "Uruguay")
N <- 3 # number of spanned factors per country
GlobalVar <- c("GBC", "CPI_OECD") # Global variables
DomVar <- c("Eco_Act", "Inflation") # Domestic variables 
mat <- c(0.25, 0.5, 1, 3, 5, 10) # vector of maturities
FactorLabels <- LabFac(N, DomVar,GlobalVar, Economies, ModelType) 
StationarityUnderQ <- 0 # 1 = set stationarity condition under  the Q; 0 = no stationarity condition
OutputLabel <- "Model_demo" # output label

## ------------------------------------------------------------------------
GVARinputs <- list()
GVARinputs$Economies <- Economies
GVARinputs$GVARFactors <- FactorsGVAR
GVARinputs$VARXtype <- "unconstrained"

## ------------------------------------------------------------------------
t_First <- "2000" # First year of the sample
t_Last <-  "2015" # Last year of the sample
W_type <- 'Sample Mean' 

## ------------------------------------------------------------------------
  JLLinputs <- list()
  ModelType <- "JLL original"  
  JLLinputs$Economies <- Economies
  JLLinputs$DomUnit <- "China"
  JLLinputs$WishSigmas <- 1 
  JLLinputs$SigmaNonOrtho <- NULL
  JLLinputs$JLLModelType <- ModelType

## ------------------------------------------------------------------------
Horiz <- 100

## ------------------------------------------------------------------------
DesiredGraphs <- c("Fit", "GIRF", "GFEVD") # Available options are: "Fit", "IRF", "FEVD", "GIRF", "GFEVD"

## ------------------------------------------------------------------------
WishGraphRiskFac <- 0 #   YES: 1; No = 0.
WishGraphYields <- 1 #    YES: 1; No = 0.
WishOrthoJLLgraphs <- 0 # YES: 1; No = 0.

## ------------------------------------------------------------------------
Bootlist <- list()
Bootlist$methodBS <- 'block' 
Bootlist$BlockLength <- 4 
Bootlist$ndraws <-  3  
Bootlist$pctg   <-  95 

## ------------------------------------------------------------------------
ForecastList <- list()
ForecastList$ForHoriz <- 12 # forecast horizon
ForecastList$t0Sample <- 1 # initial sample date
ForecastList$t0Forecast <- 70 # last sample date for the first forecast

## ------------------------------------------------------------------------
w <- pca_weights_one_country(Yields, Economy = "Uruguay") 

## ---- fig.cap = "Yield loading on the spanned factors", echo=FALSE-------
LabSpaFac <- c("Level", "Slope", "Curvature")
N <- length(LabSpaFac)
 
w_pca <- data.frame(t(w[1:N,]))
colnames(w_pca) <- LabSpaFac
w_pca$mat <- mat

# Prepare plots
colors <- c("Level" = "blue", "Slope" = "green", "Curvature" = "red")

g <-  ggplot2::ggplot(data = w_pca, ggplot2::aes(x=  mat)) +  
    ggplot2::geom_line(ggplot2::aes(y = Level, color = "Level"), size = 0.7) + 
    ggplot2::geom_line(ggplot2::aes(y = Slope, color = "Slope"), size = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = Curvature, color = "Curvature"),  size = 0.7) +
      ggplot2::labs(color = "Legend") + ggplot2::scale_color_manual(values = colors) + ggplot2::theme_classic() +
    ggplot2::theme(axis.title.y= ggplot2::element_blank(), legend.position="top", legend.title=ggplot2::element_blank(), legend.text= ggplot2::element_text(size=8) ) + 
   ggplot2::xlab("Maturity (Years)") + ggplot2::geom_hline(yintercept=0)

print(g)

## ------------------------------------------------------------------------
data('CM_Yields')
Economies <- c("China", "Brazil", "Mexico", "Uruguay")
N <- 2
SpaFact <- Spanned_Factors(Yields, Economies, N)

## ------------------------------------------------------------------------
data("CM_Factors")
PdynPara <- VAR(RiskFactors, VARtype= "unconstrained")

## ------------------------------------------------------------------------
FactorsChina <- RiskFactors[1:7,]
PdynPara <- VAR(FactorsChina, VARtype= "unconstrained")

## ------------------------------------------------------------------------
data("CM_Trade")
t_First <- "2006"
t_Last <-  "2019"
Economies <- c("China", "Brazil", "Mexico", "Uruguay")
type <- "Sample Mean"
W_gvar <- Transition_Matrix(t_First, t_Last, Economies, type, DataPath = NULL, TradeFlows)

## ------------------------------------------------------------------------
data("CM_Factors_GVAR")

GVARinputs <- list()
GVARinputs$Economies <- Economies
GVARinputs$GVARFactors <- FactorsGVAR
GVARinputs$VARXtype <- "unconstrained"
GVARinputs$Wgvar <- W_gvar

N <- 3

GVARpara <- GVAR(GVARinputs, N)

## ------------------------------------------------------------------------
data('CM_Factors')
StaFac <- StarFactors(RiskFactors, Economies, W_gvar)

## ---- eval=FALSE---------------------------------------------------------
#  data("CM_Factors")
#  N <- 3
#  JLLinputs <- list()
#  ModelType <- "JLL original"
#  JLLinputs$Economies <- Economies
#  JLLinputs$DomUnit <- "China"
#  JLLinputs$WishSigmas <- 1
#  JLLinputs$SigmaNonOrtho <- NULL
#  JLLinputs$JLLModelType <- ModelType
#  JLLpara <- JLL(RiskFactors, N, JLLinputs)

## ------------------------------------------------------------------------
# Inputs to be specified by the user
data("CM_Yields")
data("CM_Factors")
ModelType <- "VAR jointQ"
Yields <- Yields
ZZ <- RiskFactors
Economies <- c("China", "Brazil", "Mexico", "Uruguay") 
mat <-  Maturities(Yields, Economies, UnitYields = "Month")
DataFrequency <- "Monthly"
GlobalVar <- c("GBC", "CPI_OECD") # Global variables
DomVar <- c("Eco_Act", "Inflation") # Domestic variables 
N <- 3 # Number of country-specific spanned factors

# Generate the "Factor Labels" list (necessary preliminarily step) 
FactorLabels <- LabFac(N, DomVar, GlobalVar, Economies, ModelType) 

# Prepare the inputs for log-Likelihood function
ATSMInputs <- InputsForMLEdensity(ModelType, Yields, ZZ, FactorLabels, mat, Economies, DataFrequency)

# Log-Likelihood function
f <- Functionf(ATSMInputs, Economies, mat, DataFrequency, FactorLabels, ModelType)

## ------------------------------------------------------------------------
K1XQinputs <- list(NULL, "K1XQ: Jordan" , NULL , NULL)
SSZinputs <- list(NULL, "SSZ: psd", NULL, NULL)
r0inputs <- list(NULL, "@r0: bounded", NULL, NULL)
seinputs <- list(NULL, "@se: bounded", 1e-6, NULL)
K0Zinputs<- list(NULL, "@K0Z: bounded", NULL, NULL)
K1Zinputs<- list(NULL, "@K1Z: bounded", NULL, NULL)

## ---- eval=FALSE---------------------------------------------------------
#  ###############################################################################################################
#  #################################### USER INPUTS ##############################################################
#  ###############################################################################################################
#  # A) Load database data
#  data("CM_Factors")
#  data('CM_Factors_GVAR')
#  data('CM_Trade')
#  data('CM_Yields')
#  
#  # B) Decide on general model inputs
#  ModelType <- "JLL original" # available options are "JPS", "JPS jointP", "GVAR sepQ", "VAR jointQ", "GVAR jointQ", "JLL original", "JLL NoDomUnit", "JLL jointSigma".
#  
#  StationarityUnderQ <- 1 # Wish to impose stationary condition for the eigenvalues of each country: YES: 1, NO:0
#  
#  Economies <- c("China", "Brazil", "Mexico", "Uruguay") # Names of the economies from the economic system
#  GlobalVar <- c("GBC", "CPI_OECD") # Global Variables
#  DomVar <- c("Eco_Act", "Inflation") # Country-specific variables
#  
#  N <- 3 # Number of spanned factors per country
#  
#  OutputLabel <- "Test" # label of the model for saving the file
#  DataFrequency <- "Monthly" # Frequency of the data
#  UnitMatYields <- "Month" # time-unit in which yields are expressed. Available options are "Month" or "Year"
#  
#  # B.1) Decide on specific model inputs
#  #################################### GVAR-based models #########################################################
#  if (ModelType == 'GVAR sepQ' || ModelType == 'GVAR jointQ'){
#    # Transition matrix inputs:
#    t_First <- "2006" # First year of the sample
#    t_Last <-  "2019" # Last year of the sample
#    W_type <- 'Sample Mean' # Method to compute the transition matrix
#    # VARX input:
#    VARXtype <- "unconstrained"
#  }
#  
#  #################################### JLL-based models #########################################################
#  if (ModelType =="JLL original" || ModelType == "JLL NoDomUnit" || ModelType == "JLL jointSigma"){
#    DominantCountry <- "China" # (i) one of the countries of the system or (ii) "None"
#  }
#  ################################################################################################################
#  
#  # C) Decide on Settings for numerical outputs
#  Horiz <- 50
#  DesiredGraphs <- c("IRF", "FEVD") #c("Fit", "IRF", "FEVD", "GIRF", "GFEVD")
#  WishGraphRiskFac <- 0
#  WishGraphYields <- 1
#  WishOrthoJLLgraphs <- 0
#  
#  # D) Bootstrao settings
#  WishBootstrap <- 1 #  YES: 1; No = 0.
#  Bootlist <- list()
#  Bootlist$methodBS <- 'bs' # (i) 'bs' ; (ii) 'wild'; (iii) 'block'
#  Bootlist$BlockLength <- 4 # necessary input if one chooses the block bootstrap
#  Bootlist$ndraws <-  30
#  Bootlist$pctg   <-  95 # confidence level
#  
#  # E) Out-of-sample forecast
#  WishForecast <- 1 #  YES: 1; No = 0.
#  ForecastList <- list()
#  ForecastList$ForHoriz <- 12 # forecast horizon
#  ForecastList$t0Sample <- 1 # initial sample date
#  ForecastList$t0Forecast <- 145 # last sample date for the first forecast
#  
#  ###############################################################################################################
#  ############################### NO NEED TO MAKE CHANGES FROM HERE #############################################
#  ###############################################################################################################
#  
#  # 2) Minor preliminary work
#  C <- length(Economies)
#  FactorLabels <- LabFac(N, DomVar,GlobalVar, Economies, ModelType) # Generate the set of labels
#  mat <- Maturities(Yields, Economies, UnitYields = UnitMatYields) # Vector of maturities expressed in years
#  # (alternatively, it can be completed manually)
#  ZZ <- RiskFactors
#  # 2.1) Build the GVARinputs
#  if (ModelType == 'GVAR sepQ' || ModelType == 'GVAR jointQ'){
#    GVARinputs <- list()
#    GVARinputs$Economies <- Economies
#    GVARinputs$GVARFactors <- FactorsGVAR
#    GVARinputs$VARXtype <- VARXtype
#    GVARinputs$Wgvar <- Transition_Matrix(t_First, t_Last, Economies, W_type, DataPath = NULL, Data = TradeFlows)
#  } else { GVARinputs <- NULL }
#  
#  # 2.2) Build the JLLinputs
#  if (ModelType =="JLL original" || ModelType == "JLL NoDomUnit" || ModelType == "JLL jointSigma"){
#    JLLinputs <- list()
#    JLLinputs$Economies <- Economies
#    JLLinputs$DomUnit <- DominantCountry
#    JLLinputs$WishSigmas <- 1 # Sigma matrix is estimated within the "InputsForMLEdensity" function
#    JLLinputs$SigmaNonOrtho <- NULL
#    JLLinputs$JLLModelType <- ModelType
#  }else{
#    JLLinputs <- NULL
#  }
#  
#  # 3) Prepare the inputs of the likelihood function
#  ModelParaList <- list()
#  for (i in 1:C){
#    if (( ModelType == "GVAR jointQ" || ModelType == "VAR jointQ" || ModelType == "JLL original"
#          || ModelType == "JLL NoDomUnit" || ModelType == "JLL jointSigma" )   & i >1 ){break} # the break avoids the models with joint estimation under the Q to be estimated C times
#  
#    # 3.1) Compute the inputs that go directly into the log-likelihood function
#    ATSMInputs <- InputsForMLEdensity(ModelType, Yields, ZZ, FactorLabels, mat, Economies, DataFrequency, JLLinputs,   GVARinputs)
#  
#    # 3.2) Initial guesses for Variables that will be concentrared out of from the log-likelihood function
#    K1XQ <- ATSMInputs$K1XQ
#    if (ModelType == "JLL original" || ModelType == "JLL NoDomUnit" ){ SSZ <- NULL} else{SSZ <- ATSMInputs$SSZ}
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
#    varargin$OptRun <- c("iter off")
#  
#    LabelVar<- c('Value', 'Label', 'LB', 'UB') # Elements of each parameter
#    for (d in 1:(length(varargin)-1)){ names(varargin[[d]]) <-  LabelVar}
#  
#    tol <- 1e-4
#  
#    # 6) Optimization of the model
#    if (ModelType == 'JPS' || ModelType == 'JPS jointP' || ModelType == "GVAR sepQ"){
#      ModelParaList[[ModelType]][[Economies[i]]] <- Optimization(f, tol, varargin, FactorLabels, Economies, ModelType)$Summary
#    }else{ ModelParaList[[ModelType]] <- Optimization(f, tol, varargin, FactorLabels, Economies, ModelType, JLLinputs)$Summary}
#  
#  }
#  
#  # 7) Numerical and graphical outputs
#  InputsForOutputs <- InputsForOutputs(ModelType, Horiz, DesiredGraphs, OutputLabel, StationarityUnderQ,                  WishGraphYields, WishGraphRiskFac, WishOrthoJLLgraphs, WishBootstrap, Bootlist, WishForecast, ForecastList)
#  # A) Fit, IRF, FEVD, GIRF and GFEVD
#  NumericalOutputs <- NumOutputs(ModelType, ModelParaList, InputsForOutputs, FactorLabels, Economies)
#  
#  # B) Bootstrap
#  Bootstrap <- Bootstrap(ModelType, ModelParaList, NumericalOutputs, mat, Economies, InputsForOutputs, FactorLabels, DataFrequency, varargin, JLLinputs, GVARinputs)
#  
#  # C) Out-of-sample forecasting
#  Forecasts <- ForecastYields(ModelType, ModelParaList, InputsForOutputs, FactorLabels, Economies, DataFrequency, JLLinputs, GVARinputs)
#  

## ---- eval=FALSE---------------------------------------------------------
#  ###############################################################################################################
#  #################################### USER INPUTS ##############################################################
#  ###############################################################################################################
#  # A) Load database data
#  data("BR_jps_gro_R3")
#  
#  # B) Decide on general model inputs
#  ModelType <- "JPS"
#  
#  StationarityUnderQ <- 0 # Wish to impose stationary condition for the eigenvalues of each country: YES: 1, NO:0
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
#  ###############################################################################################################
#  ############################### NO NEED TO MAKE CHANGES FROM HERE #############################################
#  ###############################################################################################################
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
#  for (i in 1:C){
#  # 3.1) Compute the inputs that go directly into the log-likelihood function
#  ATSMInputs <- InputsForMLEdensity(ModelType, Yields, ZZ, FactorLabels, mat, Economies, DataFrequency)
#  # 3.2) Initial guesses for Variables that will be concentrared out of from the log-likelihood function
#  K1XQ <- ATSMInputs$K1XQ
#  SSZ <- ATSMInputs$SSZ
#  # 3.3) Adjust the inputs which are funtion of the W matrix
#  ATSMInputs$Wpca <- W
#  ATSMInputs$We <- t(pracma::null(W))
#  ATSMInputs$WpcaFull <- rbind(ATSMInputs$Wpca, ATSMInputs$We)
#  ATSMInputs$PP <- SpaFac
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
#  
#  # 6) Optimization of the model
#  ModelPara <- Optimization(f, tol, varargin, FactorLabels, Economies, ModelType)$Summary
#  }

## ---- echo= FALSE--------------------------------------------------------
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


kableExtra::kbl(TableQ, align = "c", caption = "$Q$-dynamics parameters") %>%
  kableExtra::kable_classic("striped", full_width = F)  %>%
  kableExtra::row_spec(0, font_size = 14) %>%
  kableExtra::footnote(general = " $\\lambda$'s are the eigenvalues from the risk-neutral feedback matrix and $r0$ is the long-run mean of the short rate under Q.")

## ---- echo= FALSE--------------------------------------------------------
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

## ---- echo= FALSE--------------------------------------------------------
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

## ---- eval=FALSE---------------------------------------------------------
#  ###############################################################################################################
#  #################################### USER INPUTS ##############################################################
#  ###############################################################################################################
#  # A) Load database data
#  data("CM_Factors")
#  data('CM_Factors_GVAR')
#  data('CM_Trade')
#  data('CM_Yields')
#  
#  # B) Decide on general model inputs
#  ModelType <- "GVAR jointQ" # Set "GVAR jointQ" for the GVAR-ATSM and "JLL original" for the JLL-ATSM
#  
#  StationarityUnderQ <- 0
#  
#  Economies <- c("China", "Brazil", "Mexico", "Uruguay")
#  GlobalVar <- c("GBC", "CPI_OECD") # Global Variables
#  DomVar <- c("Eco_Act", "Inflation") # Country-specific variables
#  
#  N <- 3 # Number of spanned factors per country
#  
#  OutputLabel <- "CM_2021" # label of the model for saving the file
#  DataFrequency <- "Monthly" # Frequency of the data
#  UnitMatYields <- "Month" # time-unit in which yields are expressed. Available options are "Month" or "Year"
#  
#  # B.1) Decide on specific model inputs
#  #################################### GVAR-based models #########################################################
#  if (ModelType == 'GVAR jointQ'){
#  # Transition matrix inputs:
#  t_First <- "2006" # First year of the sample
#  t_Last <-  "2019" # Last year of the sample
#  W_type <- 'Sample Mean' # Method to compute the transition matrix
#  # VARX input:
#  VARXtype <- "unconstrained"
#  }
#  
#  #################################### JLL-based models #########################################################
#  if (ModelType =="JLL original"){
#      DominantCountry <- "China"
#  }
#  ################################################################################################################
#  
#  # C) Decide on Settings for numerical outputs
#  Horiz <- 25
#  DesiredGraphs <- c("Fit", "GIRF", "GFEVD")
#  WishGraphRiskFac <- 0
#  WishGraphYields <- 1
#  WishOrthoJLLgraphs <- 1
#  
#  # D) Bootstrao settings
#  WishBootstrap <- 0 #  If bootstrap outputs are desired set this variable equal to 1 (it may take several hours/days).
#  Bootlist <- list()
#  Bootlist$methodBS <- 'bs'
#  Bootlist$BlockLength <- c()
#  Bootlist$ndraws <-  1000
#  Bootlist$pctg   <-  95
#  
#  # E) Out-of-sample forecast
#  WishForecast <- 0 #  If out-of-sample forecasts outputs are desired set this variable equal to 1
#  ForecastList <- list()
#  ForecastList$ForHoriz <- 12
#  ForecastList$t0Sample <- 1
#  ForecastList$t0Forecast <- 90
#  
#  ###############################################################################################################
#  ############################### NO NEED TO MAKE CHANGES FROM HERE #############################################
#  ###############################################################################################################
#  
#  # 2) Minor preliminary work
#  C <- length(Economies)
#  FactorLabels <- LabFac(N, DomVar,GlobalVar, Economies, ModelType) # Generate the set of labels
#  mat <- Maturities(Yields, Economies, UnitYields = UnitMatYields) # Vector of maturities expressed in years
#  
#  ZZ <- RiskFactors
#  # 2.1) Build the GVARinputs
#  if (ModelType == "GVAR jointQ"){
#    GVARinputs <- list()
#    GVARinputs$Economies <- Economies
#    GVARinputs$GVARFactors <- FactorsGVAR
#    GVARinputs$VARXtype <- VARXtype
#    GVARinputs$Wgvar <- Transition_Matrix(t_First, t_Last, Economies, W_type, DataPath = NULL, Data = TradeFlows)
#    } else { GVARinputs <- NULL }
#  
#  # 2.2) Build the JLLinputs
#  if (ModelType =="JLL original"){
#    JLLinputs <- list()
#    JLLinputs$Economies <- Economies
#    JLLinputs$DomUnit <- DominantCountry
#    JLLinputs$WishSigmas <- 1
#    JLLinputs$SigmaNonOrtho <- NULL
#    JLLinputs$JLLModelType <- ModelType
#  }else{  JLLinputs <- NULL }
#  
#  # 3) Prepare the inputs of the likelihood function
#  ModelParaList <- list()
#  
#  # 3.1) Compute the inputs that go directly into the log-likelihood function
#  ATSMInputs <- InputsForMLEdensity(ModelType, Yields, ZZ, FactorLabels, mat, Economies, DataFrequency, JLLinputs,
#  GVARinputs)
#  
#    # 3.2) Initial guesses for Variables that will be concentrared out of from the log-likelihood function
#  K1XQ <- ATSMInputs$K1XQ
#  if (ModelType == "JLL original"){ SSZ <- NULL} else{SSZ <- ATSMInputs$SSZ}
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
#  LabelVar<- c('Value', 'Label', 'LB', 'UB')
#  for (d in 1:(length(varargin)-1)){ names(varargin[[d]]) <-  LabelVar}
#  
#  tol <- 1e-4
#  
#  # 6) Optimization of the model
#  ModelParaList[[ModelType]] <- Optimization(f, tol, varargin, FactorLabels, Economies, ModelType, JLLinputs)$Summary
#  
#  
#  # 7) Numerical and graphical outputs
#  InputsForOutputs <- InputsForOutputs(ModelType, Horiz, DesiredGraphs, OutputLabel, StationarityUnderQ, WishGraphYields,  WishGraphRiskFac, WishOrthoJLLgraphs, WishBootstrap, Bootlist, WishForecast, ForecastList)
#  
#  # A) Fit, IRF, FEVD, GIRF and GFEVD
#  NumericalOutputs <- NumOutputs(ModelType, ModelParaList, InputsForOutputs, FactorLabels, Economies)
#  
#  # B) Bootstrap
#  Bootstrap <- Bootstrap(ModelType, ModelParaList, NumericalOutputs, mat, Economies, InputsForOutputs, FactorLabels,
#  DataFrequency, varargin, JLLinputs, GVARinputs)
#  
#  # C) Out-of-sample forecasting
#  Forecasts <- ForecastYields(ModelType, ModelParaList, InputsForOutputs, FactorLabels, Economies, DataFrequency, JLLinputs, GVARinputs)

