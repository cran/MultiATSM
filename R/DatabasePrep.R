#' Gather data of several countries in a list. Particularly useful for GVAR-based setups (Compute "GVARFactors")
#'
#'@param t_First        Start date of the sample period in the format yyyy-mm-dd.
#'@param t_Last         End date of the sample period in the format yyyy-mm-dd.
#'@param Economies      A character vector containing the names of the economies included in the system.
#'@param N              Integer. Number of country-specific spanned factors.
#'@param FactorLabels   A list of character vectors with labels for all variables in the model.
#'@param ModelType      A character vector indicating the model type to be estimated.
#'@param Wgvar          GVAR transition matrix of size C x C, applicable if a GVAR-type model is selected. Default is NULL.
#'@param DataPathMacro  File path to the Excel file containing macroeconomic data, if provided. The default path points to the Excel file available within the package.
#'@param DataPathYields File path to the Excel file containing yields data, if provided. The default path points to the Excel file available within the package
#'
#'
#'
#'@examples
#' DomVar <- c("Eco_Act", "Inflation")
#' GlobalVar <- c("GBC", "CPI_OECD")
#' t0 <- "2006-09-01"
#' tF <-  "2019-01-01"
#' Economies <- c("China", "Brazil", "Mexico", "Uruguay", "Russia")
#' N <- 3
#' ModelType <- "GVAR multi"
#' FactorLabels <-  LabFac(N, DomVar, GlobalVar, Economies, ModelType)
#' Wgvar <- Transition_Matrix(t_First = "2006", t_Last= "2019", Economies, type = "Sample Mean")
#'
#'
#' GVARFactors <- DatabasePrep(t0, tF, Economies, N, FactorLabels, ModelType, Wgvar)
#'
#'
#'@returns
#' List containing the risk factor set used in the estimation of the GVAR-based models
#'
#'@export



DatabasePrep <- function(t_First, t_Last, Economies, N, FactorLabels, ModelType, Wgvar = NULL,
                         DataPathMacro = NULL, DataPathYields = NULL){


  if (length(DataPathMacro) ==0 ) { DataPathMacro <- system.file("extdata", "MacroData.xlsx", package = "MultiATSM") }
 if (length(DataPathYields) ==0 ){ DataPathYields <- system.file("extdata", "YieldsData.xlsx", package = "MultiATSM") }

  # 1) Retrieve Data
  tab_names_Macro <- readxl::excel_sheets(DataPathMacro)
  list_all_Macro <- lapply(tab_names_Macro, function(x) readxl::read_excel(path = DataPathMacro, sheet = x))
  names(list_all_Macro) <- tab_names_Macro

  tab_names_Yields <- readxl::excel_sheets(DataPathYields)
  list_all_Yields <- lapply(tab_names_Yields, function(x) readxl::read_excel(path = DataPathYields, sheet = x))
  names(list_all_Yields) <- tab_names_Yields

  t_First <- as.POSIXct(as.Date(t_First, "%Y-%m-%d"))
  t_Last <- as.POSIXct(as.Date(t_Last, "%Y-%m-%d"))
  Idx <- match(unclass(c(t_First, t_Last)), unclass(list_all_Macro[["Global"]]$Period)) # Find the indexes of the first and last observation of the desired sample

  SampleMacro <- lapply(list_all_Macro, "[", Idx[1]:Idx[2], )
  SampleYields <- lapply(list_all_Yields, "[", Idx[1]:Idx[2], )


  # 2) Pre-allocate list of factors
  T <- length(SampleMacro$Global$Period) # length of model's time dimension
  C <- length(Economies) # number of economies in the study
  M <- length(FactorLabels$Domestic) - N # Number of country-specific macro variables
  M.star <- length(FactorLabels$Star) - N # Number of foreign-country-specific macro variables
  G <- length(FactorLabels$Global) # Number of global variables

  ListFactors <- vector(mode='list', length = length(Economies)+1) # length = all countries + global factors
  names(ListFactors) <- c(Economies, 'Global')

  # Country-specific factors (CSF)
  CSF <- vector(mode='list', length = length(FactorLabels$Domestic))
  names(CSF) <- FactorLabels$Domestic
  for (i in 1:C){  ListFactors[[Economies[i]]] <- CSF }

  #  Star factors (SF)
  SF <- vector(mode='list', length = length(FactorLabels$Star))
  names(SF) <- FactorLabels$Star
  for (i in 1:length(Economies)){
    ListFactors[[Economies[i]]] <- list(append(CSF,SF))
    names(ListFactors[[Economies[i]]]) <- 'Factors'
  }

  # Global Factors (GF)
  GF <- vector(mode='list', length = length(FactorLabels$Global))
  names(GF) <- FactorLabels$Global
  ListFactors[[ length(Economies)+1 ]] <- GF

  # Yields
  YieldsSeries <- vector(mode='list', length = C)
  Wpca <- vector(mode='list', length = C)
  names(Wpca) <- rep("Wpca", times=C)
  YieldsList <- vector(mode='list', length = C)


  # 3) Remove series which are entirely filled with NAs
  AllFactorsClean <- RemoveNA(SampleYields, SampleMacro, Economies)
  # A) Select yields with common maturities across countries
  mats <- vector(mode='list', length = C)
  for (i in 1:C){
    mats[[i]] <- names(AllFactorsClean$Yields[[Economies[i]]])
  }
  CommonMats <- Reduce(intersect, mats)
  for (i in 1:C){
    AllFactorsClean$Yields[[Economies[i]]] <- AllFactorsClean$Yields[[Economies[i]]][CommonMats]
  }
  # B) Make macro series be expressed in percentage points
  for (i in 1:C){
    AllFactorsClean$DomMacro[[Economies[i]]] <- AllFactorsClean$DomMacro[[Economies[i]]]
  }
  AllFactorsClean$GlobalMacro <- AllFactorsClean$GlobalMacro


  # 4) Fill in list with the corresponding factors
  # A) Country-specific variables (economy-related variables)
  for (i in 1:C) {
    for (j in 1:M){
      ListFactors[[Economies[i]]]$Factors[[j]]<- AllFactorsClean$DomMacro[[Economies[i]]][[FactorLabels$Domestic[j]]]
    }
  }


   # B) Country-specific variables (pricing-related variables)
  idx0 <- M
  for (i in 1:C) {
 # Adjust format of the time series of yields
    matsCS <-  paste(CommonMats, "_", Economies[i],  sep="")
    AllFactorsClean[["Yields"]][[Economies[i]]] <- t(AllFactorsClean[["Yields"]][[Economies[i]]])
    rownames(AllFactorsClean[["Yields"]][[Economies[i]]]) <- matsCS
# Compute the pricing factors
    for (j in 1:N){
  ListFactors[[Economies[i]]]$Factors[[idx0+j]] <- Spanned_Factors(AllFactorsClean$Yields[[Economies[i]]],
                                                                       Economies[i],N)[j, ]
}
  }

  # C) Foreign country-specific variables (economy and pricing-related)
  idx1 <- M+N
  Z <- list()
  for (j in 1:(M+N)){
    X <- matrix(NA, nrow= C, ncol=T)

    for (i in 1:C){
      X[i,] <- ListFactors[[Economies[i]]]$Factors[[j]]
      Z[[j]] <- X # Each element of the list contains the same country-specific variable of all countries
    }
  }
  names(Z) <- FactorLabels$Domestic

  if (any(ModelType == c("GVAR single", "GVAR multi"))){

    if (length(Wgvar) == 0){ stop("The transition matrix needs to be specified!")}


    # c.1) If star variables are computed with time-varying weigths
    if (is.list(Wgvar)){
  # Use only the transition matrices that are included in the sample span
      t_First_Wgvar <- format(t_First, "%Y")
      t_Last_Wgvar <- format(t_Last, "%Y")

      Wgvar_subset <- Wgvar[names(Wgvar) >= t_First_Wgvar & names(Wgvar) <= t_Last_Wgvar]

      # Add common column label (i.e. the year of the observation) to all variables
      YearLabels <- substr(SampleMacro[[Economies[1]]]$Period, 1,4)
      Z <- lapply(Z, function(x) {  colnames(x) <- paste0(YearLabels, seq_along(colnames(x)))
        return(x)
      })


      # Compute the star variables with time-varying weigths
      for (i in 1:C){
       for (j in 1:(M+N)){

         StarTimeVarTemp <- matrix(NA, nrow = T, ncol = 1)

         for (k in 1:length(Wgvar_subset)) {
         YearRef <- names(Wgvar_subset)[k] # year of reference
        IdxYear <- grep(YearRef, colnames(Z[[j]]))
        WgvarYear <- Wgvar_subset[[k]]
        StarTimeVarTemp[IdxYear]  <- t(WgvarYear[i, ]%*% Z[[j]][, IdxYear])
         }
         # If the last year of the transition matrix happens earlier than the year of the last observation from the sample,
         # then use the last transition matrix for the remaining observations
         if (anyNA(StarTimeVarTemp)){
           LenlastYear <- length(IdxYear)
           IdxLastObs <- IdxYear[LenlastYear] + 1
           StarTimeVarTemp[IdxLastObs:T]  <- t(WgvarYear[i, ]%*% Z[[j]][, (IdxLastObs):T])
           }

             ListFactors[[Economies[i]]]$Factors[[idx1+j]] <- StarTimeVarTemp
        }
      }

      # c.2) If star variables are computed with time fixed weigths
    }else{


    for (i in 1:C){
      for (j in 1:(M+N)){
        ListFactors[[Economies[i]]]$Factors[[idx1+j]] <- t(Wgvar[i,]%*%Z[[j]])
      }
    }
    }
  }
  # D) Global Factors

  for (i in seq_len(G)){
    ListFactors[[length(Economies)+1]][[i]] <- AllFactorsClean$Global[[FactorLabels$Global[i]]]
  }
  # E) Yields + Wpca:
  for (i in 1:C) {
    Wpca[[i]] <- pca_weights_one_country(AllFactorsClean$Yields[[Economies[i]]], Economies[i])
    YieldsSeries[[i]] <- AllFactorsClean$Yields[[Economies[i]]]
    YieldsList[[i]] <- list(YieldsSeries[[i]])
    names(YieldsList[[i]]) <- "Yields"
    ListFactors[[Economies[i]]]  <- append(ListFactors[[Economies[i]]], YieldsList[[i]] )
    ListFactors[[Economies[i]]]  <- append(ListFactors[[Economies[i]]], Wpca[i])
  }


  return(ListFactors)
}



#################################################################################################################
#' Exclude series that contain NAs

#'@param YieldsData     List of country-specific bond yields
#'@param MacroData      List of country-specific and global economic variables
#'@param Economies      string-vector containing the names of the economies which are part of the economic system
#'
#'@keywords internal
#'
#'@return return the time series data that were not initially composed by NAs.
#'

RemoveNA <- function(YieldsData, MacroData, Economies){

C <- length(Economies)

Yields <- list()
DomesticMacro <- list()
LLL <- length(MacroData$Global)

for (i in 1:C){
  L <- length(YieldsData[[Economies[i]]])
  LL <- length(MacroData[[Economies[i]]])


    # Remove empty series from the series of yields
    Yields[[i]]<- YieldsData[[Economies[i]]][2:L] # We start at the second column because we want to remove the time series span
    IdxEmptyYields <- which(sapply(Yields[[i]], function(x)all(is.na(x)))==1)
    if (!identical(length(IdxEmptyYields),0)){
      IdxEmptyYields <- rev(IdxEmptyYields)
      for (j in c(IdxEmptyYields)){
        Yields[[i]][[j[[1]]]] <- NULL
      }
    }


  # Remove empty series from the domestic macro variables
    DomesticMacro[[i]]<- MacroData[[Economies[i]]][2:LL] # We start at the second column because we want to remove the time series span
    IdxEmptyMacroDom <- which(sapply(DomesticMacro[[i]], function(x)all(is.na(x)))==1)
    if (!identical(length(IdxEmptyMacroDom),0)){
      IdxEmptyMacroDom <- rev(IdxEmptyMacroDom)
      for (j in c(IdxEmptyMacroDom)){
        DomesticMacro[[i]][[j[[1]]]] <- NULL
      }
    }
  }


  # Remove empty series from the global macro variables
  GlobalMacro <- list()

  GlobalMacro <- MacroData$Global[2:LLL] # We start at the second column because we want to remove the time series span
  IdxEmptyMacroGlobal <- which(sapply(GlobalMacro, function(x)all(is.na(x)))==1)
  if (!identical(length(IdxEmptyMacroGlobal),0)){
    IdxEmptyMacroGlobal <- rev(IdxEmptyMacroGlobal)
    for (j in c(IdxEmptyMacroGlobal)){
      GlobalMacro[[j]] <- NULL
    }
  }


  names(Yields) <- Economies
  names(DomesticMacro) <- Economies


  Output <- list(Yields,DomesticMacro, GlobalMacro)
  names(Output) <- c("Yields","DomMacro","GlobalMacro")

  return(Output)
}
