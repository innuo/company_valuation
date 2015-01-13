# Load models and allow for manual plotting of performance charts
# run from R folder that is peer to data folder

makeErrorVector <- function(model) {
  # Expects columns of the same length between which a relative error normalized to y is calculated
  
  # Create calc matrix
  m <- cbind(model$train.y, model$train.y.cv)
  mClean <- na.omit(m)
  
  # Create normalized 
  errVectorNormalized <- (mClean[,1]-mClean[,2])/mClean[,1]
  abs(errVectorNormalized[is.finite(errVectorNormalized)])
}

myHist <- function(errorVector, nbreaks)  {
  hist(errorVector, probability = TRUE, 
       breaks = nbreaks,
       xlab = "Relative error (Actual-Est)/Actual", 
       main = "Estimation error distribution")
}

dv <- function(model) model$train.y - model$train.y.cv

models <- list(readRDS("../data/model_ebidta.Rdata"), readRDS("../data/model_revenues.Rdata"),
                readRDS("../data/model_rf.Rdata"), readRDS("../data/model_rf_industry.Rdata"))
model.choices <- c("Multiple of EBIDTA", "Multiple of Revenue", "Statistical (no industry group)",
                   "Statistical (with industry group)")

model.id <- "Multiple of EBIDTA"
ebitdaModel <- models[[which(model.choices == model.id)]]
ebitdaError <- makeErrorVector(ebitdaModel)

model.id <- "Multiple of Revenue"
revenueModel <- models[[which(model.choices == model.id)]]
revError <- makeErrorVector(revenueModel)

model.id <- "Statistical (no industry group)"
rfstatsModel <- models[[which(model.choices == model.id)]]
rfError <- makeErrorVector(rfstatsModel)

errorSummary <- lapply( list(revError, ebitdaError, rfError), summary)

errorMatrix <- data.frame("company"=ebitdaModel$train.ids,
                     "value"=ebitdaModel$train.y, 
                     "ebitdaPredError"=dv(ebitdaModel), 
                     "revPredError"=dv(revenueModel), 
                     "rfStatPredError"=dv(rfstatsModel),
                     "ebitdaNormErr"=dv(ebitdaModel)/ebitdaModel$train.y,
                     "revNormError"=dv(revenueModel)/ebitdaModel$train.y, 
                     "rfStatNormError"=dv(rfstatsModel)/ebitdaModel$train.y)

errorMatrixclean <- na.omit(errorMatrix)
  
make.model.error.df <- function(){
  
  df <- NULL
  for(i in 1:length(models)){
    m <- models[[i]]
    error <- abs(m$train.y-m$train.y.cv)/(m$train.y+0.01) * 100
    
    if(is.null(df)){
      df <- data.frame(Model=model.choices[i], PercentageError=error)
    }
    else{
      df <- rbind(df, data.frame(Model=model.choices[i], PercentageError=error))
    }
    
  }
  df
}


