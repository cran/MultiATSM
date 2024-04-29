## -----------------------------------------------------------------------------
library(MultiATSM)

## ----ModFea, message=FALSE, echo=FALSE----------------------------------------
ModelLabels <- c("JPS", "JPS jointP", "VAR jointQ", "GVAR sepQ", "GVAR jointQ", 
                 "JLL original", "JLL NoDomUnit", "JLL jointSigma")

# Rows
Tab <- data.frame(matrix(nrow = length(ModelLabels), ncol = 0)) 
rownames(Tab) <- ModelLabels

# Empty columns
EmptyCol <- c("", "", "", "", "", "", "", "") 
Tab$EmptyCol0 <- EmptyCol
# P-dynamics + 2 empty spaces
Tab$PdynIndUnco <- c("x", "", "", "", "", "", "", "")
Tab$PdynIndCo <- c("", "", "", "", "", "", "", "")
Tab$PdynJointUnco <- c("", "x", "x", "", "", "", "", "")
Tab$PdynJointJLL <- c("", "", "", "", "", "x", "x", "x")
Tab$PdynJointGVAR <- c("", "", "", "x", "x", "", "", "")
Tab$EmptyCol1 <- EmptyCol
Tab$EmptyCol2 <- EmptyCol
# Q-dynamics + 2 empty spaces
Tab$QdynInd <- c("x", "x", "", "x", "", "", "", "")   
Tab$QdynJoint <- c("", "", "x", "", "x", "x", "x", "x") 
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

## -----------------------------------------------------------------------------
data('CM_Yields')

## -----------------------------------------------------------------------------
data('CM_Factors')

## -----------------------------------------------------------------------------
data('CM_Trade')

## -----------------------------------------------------------------------------
data('CM_Factors_GVAR')

## -----------------------------------------------------------------------------
Initial_Date <- "2006-09-01" # Format "yyyy-mm-dd"
Final_Date <- "2019-01-01" # Format "yyyy-mm-dd"
DataFrequency <- "Monthly"
GlobalVar <- c("GBC", "VIX") # Global Variables
DomVar <- c("Eco_Act", "Inflation", "Com_Prices", "Exc_Rates") #  Domestic variables
N <-  3 # Number of spanned factors per country
Economies <- c("China", "Mexico", "Uruguay", "Brazil", "Russia")
ModelType <- "JPS"

## -----------------------------------------------------------------------------
FactorLabels <- LabFac(N, DomVar, GlobalVar, Economies, ModelType)
ZZfull <-DataForEstimation(Initial_Date, Final_Date, Economies, N, FactorLabels, ModelType, DataFrequency)

## -----------------------------------------------------------------------------
ModelType <- "JPS"
BiasCorrection <- 1 # 1 = estimate model with bias correction; 0 = estimate model without bias correction
DataFrequency <- "Monthly"
UnitMatYields <- "Month" # Available options are: "Month" or "Year".
Economies <- c("China", "Brazil", "Mexico", "Uruguay")
N <- 3 # number of spanned factors per country
GlobalVar <- c("GBC", "CPI_OECD") # Global variables
DomVar <- c("Eco_Act", "Inflation") # Domestic variables 
mat <- c(0.25, 0.5, 1, 3, 5, 10) # vector of maturities
StationarityUnderQ <- 0 # 1 = set stationarity condition under  the Q; 0 = no stationarity condition
WishForwardPremia <- 1 # Wish to estimate the forward premia: 1= Yes; 0= No
FPmatLim <- c(60,120) # maturty of the loan starts in 60 months and ends in 120 months in the future
OutputLabel <- "Model_demo" # output label

## -----------------------------------------------------------------------------
GVARinputs <- list()
GVARinputs$Economies <- Economies
GVARinputs$GVARFactors <- FactorsGVAR
GVARinputs$VARXtype <- "constrained: Inflation"

## -----------------------------------------------------------------------------
t_First <- "2000" # First year of the sample
t_Last <-  "2015" # Last year of the sample
W_type <- 'Sample Mean' 

## -----------------------------------------------------------------------------
  JLLinputs <- list()
  ModelType <- "JLL original"  
  JLLinputs$Economies <- Economies
  JLLinputs$DomUnit <- "China"
  JLLinputs$WishSigmas <- 1 
  JLLinputs$SigmaNonOrtho <- NULL
  JLLinputs$JLLModelType <- ModelType

## -----------------------------------------------------------------------------
flag_mean <- TRUE # TRUE = compute the mean; FALSE = compute the median
gamma <- 0.2 # Adjustment parameter
N_iter <- 500 # Number of iteration to be conserved 
N_burn <- N_iter*0.15  # Number of iteration to be discarded 
B <- 50 # Number of bootstrap samples 
checkBRW <- 1 # Closeness check
B_check <- 1000 # Number of bootstrap samples used in the closeness check

## -----------------------------------------------------------------------------
Horiz <- 100

## -----------------------------------------------------------------------------
DesiredGraphs <- c("Fit", "GIRF", "GFEVD") # Available options are: "Fit", "IRF", "FEVD", "GIRF", 
                                          # "GFEVD", "TermPremia".

## -----------------------------------------------------------------------------
WishGraphRiskFac <- 0 #   YES: 1; No = 0.
WishGraphYields <- 1 #    YES: 1; No = 0.
WishOrthoJLLgraphs <- 0 # YES: 1; No = 0.

## -----------------------------------------------------------------------------
Bootlist <- list()
Bootlist$methodBS <- 'block' 
Bootlist$BlockLength <- 4 
Bootlist$ndraws <-  3  
Bootlist$pctg   <-  95 

## -----------------------------------------------------------------------------
ForecastList <- list()
ForecastList$ForHoriz <- 12 # forecast horizon
ForecastList$t0Sample <- 1 # initial sample date
ForecastList$t0Forecast <- 70 # last sample date for the first forecast
ForecastList$ForType <- "Rolling" # forecasting will be performed using rolling windows 

## -----------------------------------------------------------------------------
w <- pca_weights_one_country(Yields, Economy = "Uruguay") 

## ----fig.cap = "Yield loadings on the spanned factors", echo=FALSE------------
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

## -----------------------------------------------------------------------------
data('CM_Yields')
Economies <- c("China", "Brazil", "Mexico", "Uruguay")
N <- 2
SpaFact <- Spanned_Factors(Yields, Economies, N)

## -----------------------------------------------------------------------------
data("CM_Factors")
PdynPara <- VAR(RiskFactors, VARtype= "unconstrained")

## -----------------------------------------------------------------------------
FactorsChina <- RiskFactors[1:7,]
PdynPara <- VAR(FactorsChina, VARtype= "unconstrained")

## -----------------------------------------------------------------------------
data("CM_Trade")
t_First <- "2006"
t_Last <-  "2019"
Economies <- c("China", "Brazil", "Mexico", "Uruguay")
type <- "Sample Mean"
W_gvar <- Transition_Matrix(t_First, t_Last, Economies, type, DataPath = NULL, TradeFlows)
print(W_gvar)

## -----------------------------------------------------------------------------
t_First <- "2019"
t_Last <-  "2019"
type <- "Full Sample"

## -----------------------------------------------------------------------------
data("CM_Factors_GVAR")

GVARinputs <- list()
GVARinputs$Economies <- Economies
GVARinputs$GVARFactors <- FactorsGVAR
GVARinputs$VARXtype <- "unconstrained"
GVARinputs$Wgvar <- W_gvar

N <- 3

GVARpara <- GVAR(GVARinputs, N)

## -----------------------------------------------------------------------------
data('CM_Factors')
StaFac <- StarFactors(RiskFactors, Economies, W_gvar)

## ----eval=FALSE---------------------------------------------------------------
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

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
K1XQinputs <- list(NULL, "K1XQ: Jordan" , NULL , NULL)
SSZinputs <- list(NULL, "SSZ: psd", NULL, NULL)
r0inputs <- list(NULL, "@r0: bounded", NULL, NULL)
seinputs <- list(NULL, "@se: bounded", 1e-6, NULL)
K0Zinputs<- list(NULL, "@K0Z: bounded", NULL, NULL)
K1Zinputs<- list(NULL, "@K1Z: bounded", NULL, NULL)

## ----eval=FALSE---------------------------------------------------------------
#  ########################################################################################################
#  #################################### USER INPUTS #######################################################
#  ########################################################################################################
#  # A) Load database data
#  data("CM_Factors")
#  data('CM_Factors_GVAR')
#  data('CM_Trade')
#  data('CM_Yields')
#  
#  # B) Decide on general model inputs
#  ModelType <- "VAR jointQ" # available options are "JPS", "JPS jointP", "GVAR sepQ", "VAR jointQ",
#                            #"GVAR jointQ", "JLL original", "JLL NoDomUnit", "JLL jointSigma".
#  
#  StationarityUnderQ <- 0 # Wish to impose stationary condition for the eigenvalues of each country:
#                           #YES: 1,NO:0
#  BiasCorrection <- 1 # Wish to estimate the model with the bias correction method of BRW (2012):
#                      #YES: 1, NO:0
#  
#  WishForwardPremia <- 1 # Wish to estimate the forward premia: YES: 1, NO:0
#  FPmatLim <- c(60,120) #  If the forward premia is desired, then choose the Min and max maturities of the
#                          # forward premia. Otherwise set NA
#  
#  Economies <- c("China", "Brazil", "Mexico", "Uruguay") # Names of the economies from the economic system
#  GlobalVar <- c("GBC", "CPI_OECD") # Global Variables
#  DomVar <- c("Eco_Act", "Inflation") # Country-specific variables
#  
#  N <- 3 # Number of spanned factors per country
#  
#  OutputLabel <- "Test" # label of the model for saving the file
#  DataFrequency <- "Monthly" # Frequency of the data
#  UnitMatYields <- "Month" # bond yields time-unit. Available options are "Month" or "Year"
#  
#  # B.1) Decide on specific model inputs
#  #################################### GVAR-based models ##################################################
#  t_First_Wgvar <- "2004" # First year of the sample (transition matrix)
#  t_Last_Wgvar <-  "2019" # Last year of the sample (transition matrix)
#  W_type <- 'Sample Mean' # Method to compute the transition matrix
#  VARXtype <- "unconstrained" # (i) "unconstrained" or (ii) ""constrained" (VARX)
#  #################################### JLL-based models ###################################################
#  DomUnit <- "China"
#  WishSigmas <- 1 # Sigma matrix is estimated within the "InputsForMLEdensity" function
#  SigmaNonOrtho <- NULL
#  JLLModelType <- ModelType
#  ###################################### BRW inputs  ######################################################
#  flag_mean <- TRUE # TRUE = compute the mean; FALSE = compute the median
#  gamma <- 0.2 # Adjustment parameter
#  N_iter <- 500 # Number of iteration to be conserved
#  N_burn <- N_iter*0.15  # Number of iteration to be discarded
#  B <- 50 # Number of bootstrap samples
#  checkBRW <- 1
#  B_check <- 1000 #
#  #########################################################################################################
#  
#  # C) Decide on Settings for numerical outputs
#  Horiz <- 50
#  DesiredGraphs <- c("Fit", "IRF", "FEVD") # "Fit", "IRF", "FEVD", "GIRF", "GFEVD", "TermPremia"
#  WishGraphRiskFac <- 0
#  WishGraphYields <- 1
#  WishOrthoJLLgraphs <- 0
#  
#  # D) Bootstrao settings
#  WishBootstrap <- 1 #  YES: 1; No = 0.
#  Bootlist <- list()
#  Bootlist$methodBS <- 'bs' # (i) 'bs' ; (ii) 'wild'; (iii) 'block'
#  Bootlist$BlockLength <- 4 # necessary input if one chooses the block bootstrap
#  Bootlist$ndraws <-  5
#  Bootlist$pctg   <-  95 # confidence level
#  
#  # E) Out-of-sample forecast
#  WishForecast <- 1 #  YES: 1; No = 0.
#  ForecastList <- list()
#  ForecastList$ForHoriz <- 12 # forecast horizon
#  ForecastList$t0Sample <- 1 # initial sample date
#  ForecastList$t0Forecast <- 145 # last sample date for the first forecast
#  ForecastList$ForType <- c()
#  
#  #########################################################################################################
#  ############################### NO NEED TO MAKE CHANGES FROM HERE #######################################
#  #########################################################################################################
#  
#  # 2) Minor preliminary work
#  C <- length(Economies)
#  FactorLabels <- LabFac(N, DomVar,GlobalVar, Economies, ModelType) # Generate the set of labels
#  ZZ <- RiskFactors
#  if(any(ModelType == c("GVAR sepQ", "GVAR jointQ"))){
#  Data <- list()
#  Data$GVARFactors <- FactorsGVAR}
#  mat <- Maturities(Yields, Economies, UnitYields = UnitMatYields)
#  
#  
#  # 2.1) Generate GVARinputs, JLLinputs and BRWinputs
#  ModInputs <- ListModelInputs(ModelType, Data, Economies, VARXtype, t_First_Wgvar, t_Last_Wgvar, W_type,
#                               DomUnit, WishSigmas, SigmaNonOrtho, BiasCorrection, flag_mean, gamma, N_iter,
#                               N_burn, B, checkBRW, B_check)
#  
#  GVARinputs <- ModInputs$GVARinputs
#  JLLinputs <- ModInputs$JLLinputs
#  BRWinputs <- ModInputs$BRWinputs
#  
#  # 3) Prepare the inputs of the likelihood function
#  ModelParaList <- list()
#  for (i in 1:C){
#    if (( any(ModelType ==c("GVAR jointQ", "VAR jointQ","JLL original", "JLL NoDomUnit","JLL jointSigma")))
#        & i >1 ){break}
#  
#    # 3.1) Compute the inputs that go directly into the log-likelihood function
#    ATSMInputs <- InputsForMLEdensity(ModelType, Yields, ZZ, FactorLabels, mat, Economies, DataFrequency,
#                                      JLLinputs, GVARinputs, BRWinputs)
#  
#    # 3.2) Initial guesses for Variables that will be concentrared out of from the log-likelihood function
#    K1XQ <- ATSMInputs$K1XQ
#    if (any(ModelType == c("JLL original", "JLL NoDomUnit"))){ SSZ <- NULL} else{SSZ <- ATSMInputs$SSZ}
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
#  ModelParaList[[ModelType]][[Economies[i]]] <- Optimization(f, tol, varargin, FactorLabels,
#                                                             Economies, ModelType)$Summary
#    }else{
#  ModelParaList[[ModelType]] <- Optimization(f, tol, varargin, FactorLabels, Economies, ModelType,
#                                             JLLinputs, GVARinputs)$Summary}
#  }
#  
#  # 7) Numerical and graphical outputs
#  InputsForOutputs <- InputsForOutputs(ModelType, Horiz, DesiredGraphs, OutputLabel, StationarityUnderQ,
#                                       UnitMatYields, WishGraphYields, WishGraphRiskFac, WishOrthoJLLgraphs,
#                                       WishForwardPremia, FPmatLim, WishBootstrap, Bootlist, WishForecast,
#                                       ForecastList)
#  # A) Fit, IRF, FEVD, GIRF, GFEVD, and Term Premia
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

