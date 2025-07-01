#' evalSDM
#'
#' Evaluate SDM perfomance
#' @param observation vector containing the observed response 
#' @param predictions vector containing the predictions
#' @param thresh threshold to use for calculating threshold dependent performance measures. If NULL (the default) then threshold is optimised based on the observed presence/absence data provided and the thresh.method
#' @param thresh.method a string indicating which method to use for optimising the binarising threshold (see ?PresenceAbsence::optimal.thresholds. Defaults to "MaxSens+Spec" (the maximum of sensitivity+specificity). Will be ignored if thresh is provided.
#' @param req.sens additional argument to PresenceAbsence::optimal.thresholds(). Will be ignored if thresh is provided.
#' @param req.spec additional argument to PresenceAbsence::optimal.thresholds(). Will be ignored if thresh is provided.
#' @param FPC additional argument to PresenceAbsence::optimal.thresholds(). Will be ignored if thresh is provided.
#' @param FNC additional argument to PresenceAbsence::optimal.thresholds(). Will be ignored if thresh is provided.
#' @param weights an optional vector of prior weights used in the model
#' @return A dataframe with performance statistics.
#' @examples 
#' data(Anguilla_train)
#' m1 <- glm(Angaus ~ poly(SegSumT,2), data=Anguilla_train, family='binomial')
#' preds_cv <- crossvalSDM(m1, kfold=5, traindat=Anguilla_train, colname_species = 'Angaus', colname_pred = 'SegSumT')
#' evalSDM(Anguilla_train$Angaus, preds_cv)
#' @export
evalSDM <- function(observation, predictions, thresh=NULL, thresh.method='MaxSens+Spec', req.sens=0.85, req.spec = 0.85, FPC=1, FNC=1, weigths=rep(1, length(observation))){
  thresh.dat <- data.frame(ID=seq_len(length(observation)), 
                           obs = observation,
                           pred = predictions)
  
  if (is.null(thresh)) {
    thresh.mat <- PresenceAbsence::optimal.thresholds(DATA= thresh.dat, req.sens=req.sens, req.spec = req.spec, FPC=FPC, FNC=FNC)
    thresh <- thresh.mat[thresh.mat$Method==thresh.method,2]
  }
  
  cmx.opt <- PresenceAbsence::cmx(DATA= thresh.dat, threshold=thresh)
  
  data.frame(AUC = PresenceAbsence::auc(thresh.dat, st.dev=F),
             TSS = TSS(cmx.opt), 
             Kappa = PresenceAbsence::Kappa(cmx.opt, st.dev=F),
             Sens = PresenceAbsence::sensitivity(cmx.opt, st.dev=F),
             Spec = PresenceAbsence::specificity(cmx.opt, st.dev=F),
             PCC = PresenceAbsence::pcc(cmx.opt, st.dev=F),
             D2 = expl_deviance(observation, predictions, weights=weigths),
             thresh = thresh)
}

#' TSS
#'
#' Calculates the true skill statistic (sensitivity+specificity-1) \insertCite{allouche2006}{mecofun}.
#' 
#' @importFrom Rdpack reprompt
#' 
#' @param cmx a confusion matrix
#' 
#' @return A numeric value.
#' 
#' @examples  TSS()
#' 
#' @references
#' \insertAllCited{}
#' 
#' @export
TSS = function(cmx){
  PresenceAbsence::sensitivity(cmx, st.dev=F) + 
    PresenceAbsence::specificity(cmx, st.dev=F) - 1
}


#' expl_deviance
#'
#' Calculates the explained deviance based on the dismo package.
#' 
#' @param obs a numeric vector of observations
#' @param pred a numeric vector of predictions
#' @param family a description of the error distribution and link function to be used in the model.
#' @param weights an optional vector of prior weights used in the models
#' 
#' @return A numeric value.
#' 
#' @examples 
#' data(Anguilla_train)
#' m1 <- glm(Angaus ~ poly(SegSumT,2), data=Anguilla_train, family='binomial')
#' expl_deviance(Anguilla_train$Angaus, m1$fitted)
#' 
#' @seealso [calc.deviance()]
#' 
#' @export
expl_deviance <- function(obs, pred, family='binomial',weights=rep(1, length(obs))){
  if (family=='binomial') {pred <- ifelse(pred<.00001,.00001,ifelse(pred>.9999,.9999,pred))}
  
  null_pred <- rep(mean(obs), length(obs))
  
  1 - (dismo::calc.deviance(obs, pred, family=family, weights=weights) / 
         dismo::calc.deviance(obs, null_pred, family=family, weights=weights))
}
