# Red List criteria underestimate climate-related extinction risk of range-shifting species


#-------------------------------------------------------------------------------

# ---------------------------------------------------------------------- #
#                            00. Functions                               #
# ---------------------------------------------------------------------- #


#-------------------------------------------------------------------------------

# 1. Function for calculating extinction probability based on the outputs from the RangeShifter simulations ------
Calc_ExtProb <- function(pop_df,s) {
  require(dplyr)
  require(tidyr)
  
  pop_df %>%
    group_by(Rep,Year) %>%
    # Sum individuals over all cells per year and replicate
    summarise(sumPop = sum(NInd), .groups='keep') %>%
    group_by(Year) %>%
    # Average extinction probability (1 minus the proportion of replicates with surviving populations)
    summarise(extProb = 1-sum(sumPop>0, na.rm=T)/RepNb) %>%
    # Make sure that data frame is filled until last year of simulation
    right_join(tibble(Year = seq_len(s@simul@Years)), by='Year') %>% replace_na(list(extProb=1))
}

#-------------------------------------------------------------------------------

# 2. Function for evaluation of performance metrics ----------------------------

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

#-------------------------------------------------------------------------------

# 3. Function for response plot ------------------------------------------------

#' inflated_response
#'
#' plot inflated response curves - inflated partial dependence plots \insertCite{Zurell2012}{mecofun}. Plot effect of one variable on response variable over the range (min,mean,median,max and quartiles) of other predictors. As the number of combinations increases exponentially, the maximum number of combinations can be set with lhsample. Whenever lhsample is exceeded, candidate combinations are drawn by latin hypercube sampling.
#' 
#' @importFrom Rdpack reprompt
#' 
#' @param object model object.
#' @param predictors a data frame with predictor variables. 
#' @param select.columns optional character vector indicating subset of predictors to plot
#' @param label optional character vector indicating alternative names of predictors for labelling plots
#' @param len a numeric value indicating the number of intervals for drawing the environmental gradients
#' @param lhsample a numeric value indicating the number of latin hypercube samples to draw
#' @param lwd line width
#' @param method character indicating at which values the other predictors are held constant. Needs to take a value of "mean", "stat3" (default), or "stat6". "stat3" considers minimum, mean and maximum values of predictors. "stat6" considers min,mean,median,max and quartiles.
#' @param disp can take options "all" (default) or "eo.mask" - in the latter case, eo.mask() is used to distinguish between areas of the estimated environmental niche / plotting areas that are supported by data and those that require extrapolation.
#' @param overlay.mean logical value. If true, then the mean response curve is overlaid on the inflated plot
#' @param ylab y axis label
#' @param col.curves colour of response curves
#' @param col.novel colour of novel environments
#' @param col.mean colour of mean response
#' @param lwd.known line width for known environments
#' @param lwd.mean line width of mean response
#' @param ... further plotting parameters
#' 
#' @return A numeric vector with predictions.
#' 
#' @references
#' \insertAllCited{}
#' 
#' @examples 
#' data(Anguilla_train)
#' m1 <- glm(Angaus ~ poly(SegSumT,2) + poly(SegTSeas,2) + poly(DSDist,2), data=Anguilla_train, family='binomial')
#' par(mfrow=c(1,3))
#' inflated_response(m1,Anguilla_train[,c('SegSumT','SegTSeas','DSDist')])
#' 
#' @export
inflated_response=function(object,predictors,select.columns=NULL,label=NULL,
                           len=50,lhsample=100, lwd=1, ylab=NULL,
                           method="stat3",disp="all",overlay.mean=T,
                           col.curves='grey',col.novel='grey',col.mean='black',lwd.known=2,lwd.mean=2,ylim=c(0,1),...){
  
  if (is.null(select.columns)) select.columns=seq_len(ncol(predictors))
  
  for (i in select.columns)
  {
    summaries=data.frame(matrix(0,6,ncol(predictors)))
    for (iz in 1:ncol(predictors)) {
      summaries[,iz]=summary(predictors[,iz])
    }
    if (method=="stat3") {
      summaries.j=as.matrix(summaries[c(1,4,6),-i],ncol=(ncol(predictors)-1));comb=min(lhsample,3^(ncol(predictors)-1));nc=3
    } else
      if (method=="stat6") {
        summaries.j=as.matrix(summaries[,-i],ncol=(ncol(predictors)-1));comb=min(lhsample,6^(ncol(predictors)-1));nc=6
      } else
        if (method=="mean") {
          summaries.j=as.matrix(summaries[4,-i],ncol=(ncol(predictors)-1));comb=1;nc=1;overlay.mean=F
        }
    
    dummy.j=as.matrix(predictors[1:len,-i],ncol=(ncol(predictors)-1))
    
    if (comb<lhsample) {
      mat=vector("list",ncol(dummy.j))
      for (m in 1:ncol(dummy.j)) mat[[m]]=1:nc
      mat=expand.grid(mat)
    } else {
      mat=round(qunif(lhs::randomLHS(lhsample,ncol(dummy.j)),1,nrow(summaries.j)),0)
    }
    
    if (is.null(label)) {
      label=names(predictors)
    }
    
    for (r in 1:nrow(mat))
    {
      for (j in 1:ncol(dummy.j))
      {
        dummy.j[,j]=as.vector(rep(summaries.j[mat[r,j],j],len))
      }
      
      dummy=data.frame(seq(min(predictors[,i]),max(predictors[,i]),length=len),dummy.j)
      names(dummy)[-1]=names(predictors)[-i]
      names(dummy)[1]=names(predictors)[i]
      
      curves <- predictSDM(object, dummy)
      
      # display all lines in same type
      if (disp=='all')
      {
        if (r==1)
        {
          if (i==1) plot(dummy[,names(predictors)[i]],
                         curves,type="l",ylim=ylim,xlab=label[i],
                         lwd=lwd,col=col.curves,ylab=ylab, ...)
          else plot(dummy[,names(predictors)[i]],
                    curves,type="l",ylim=ylim,xlab=label[i],lwd=lwd,col=col.curves,
                    ylab='', ...)
        }
        else lines(dummy[,names(predictors)[i]],
                   curves,lwd=lwd,col=col.curves,...)
      }
      
      # highlight extrapolation to novel environmental conditions
      if (disp=='eo.mask')
      {
        novel=eo.mask(predictors,dummy)
        curves.known=curves
        curves.known[novel==1]=NA
        curves.novel=curves
        curves.novel[novel==0]=NA
        
        if (r==1)
        {
          if (i==1) {plot(dummy[,names(predictors)[i]],
                          curves.known,type="l",ylim=ylim,xlab=label[i],
                          lwd=lwd.known,col=col.curves,ylab=ylab,...)
            lines(dummy[,names(predictors)[i]],
                  curves.novel,lwd=lwd,col=col.novel,lty='dotted',...)}
          else {plot(dummy[,names(predictors)[i]],
                     curves.known,type="l",ylim=ylim,xlab=label[i],lwd=lwd.known,
                     col=col.curves,ylab='',...)
            lines(dummy[,names(predictors)[i]],
                  curves.novel,lwd=lwd,col=col.novel,lty='dotted',...)}
        }
        else {lines(dummy[,names(predictors)[i]],
                    curves.known,lwd=lwd.known,col=col.curves,...)
          lines(dummy[,names(predictors)[i]],
                curves.novel,lwd=lwd,col=col.novel,lty='dotted',...)}
      }
    }
    
    #-------------------------------------------------
    # now, this is for overlaying mean response curve
    if (overlay.mean==T)
    {
      dummy=predictors[1:len,]
      dummy[,i]=seq(min(predictors[,i]),max(predictors[,i]),length=len)
      for (j in 1:ncol(predictors))
      {
        if (j!=i) 
        {
          dummy[,j]=rep(mean(predictors[,j]),len)
        }
      }
      
      curves <- predictSDM(object, dummy)
      
      lines(dummy[,names(predictors)[i]],
            curves,lwd=lwd.mean,col=col.mean,...)
    }    
  }}


#' eo_mask
#'
#' Calculate environmental overlap mask as proposed in \insertCite{Zurell2012}{mecofun}
#' 
#' @importFrom Rdpack reprompt
#' 
#' @param traindata a dataframe containing the environmental data for the training data
#' @param newdata a dataframe containing the environmental data for the transfer
#' @param nbin the number of bins to split the environmental gradients in
#' @param type a character string taking the values "EO" or "ID". "EO" returns a vector of zeros and ones for analog(0) and novel(1) environments. "ID" returns a character vector defining the combination of bins each data entry belongs to - this may help finding the problematic parts of the prediction space.
#' 
#' @return A dataframe with performance statistics.
#' 
#' @references
#' \insertAllCited{}
#' 
#' @examples eo_mask()
#' 
#' @export
eo_mask=function(traindata,newdata,nbin=5,type="EO")
{
  train.minima=apply(traindata,2,min)
  train.maxima=apply(traindata,2,max)
  
  train.ids=apply(apply(ceiling(apply(round(
    sweep(sweep(traindata, 2, train.minima, "-"), 2, train.maxima - train.minima, "/")*nbin,4),
    c(1,2),FUN=function(x){if(x==0)x=1 else x=x})),
    c(1,2),FUN=function(x){if(x<1)x=0 else if(x>nbin)x=nbin+1 else x=x}),1,paste,collapse=".")
  
  new.ids=apply(apply(ceiling(apply(round(
    sweep(sweep(newdata[,names(train.minima)], 2, train.minima, "-"), 2, train.maxima - train.minima, "/")*nbin,4),
    c(1,2),FUN=function(x){if(x==0)x=1 else x=x})),
    c(1,2),FUN=function(x){if(x<1)x=0 else if(x>nbin)x=nbin+1 else x=x}),1,paste,collapse=".")
  
  if (type=="ID") return(new.ids)
  else if (type=="EO") return(sapply(new.ids%in%train.ids,FUN=function(x){if(x==T) x=0 else if(x==F)x=1}))    
}  



#' partial_response
#'
#' plot partial response curves. Plot effect of one variable on response variable while keeping the other predictors constant at their mean.
#' 
#' @param object model object
#' @param predictors a data frame with predictor variables. 
#' @param select.columns optional character vector indicating subset of predictors to plot
#' @param label optional character vector indicating alternative names of predictors for labelling plots
#' @param ylab y axis label
#' @param len a numeric value indicating the number of intervals for drawing the environmental gradients
#' @param lwd line width
#' @param col colour of response curves
#' @param lwd line width
#' @param ... further plotting parameters
#' 
#' @return A numeric vector with predictions.
#' 
#' @examples 
#' data(Anguilla_train)
#' m1 <- glm(Angaus ~ poly(SegSumT,2) + poly(SegTSeas,2) + poly(DSDist,2), data=Anguilla_train, family='binomial')
#' par(mfrow=c(1,3))
#' partial_response(m1,Anguilla_train[,c('SegSumT','SegTSeas','DSDist')])
#' 
#' @export
partial_response=function(object,predictors,select.columns=NULL, label=NULL, len=50,
                          col='black',ylab=NULL, ...){
  
  inflated_response(object,predictors,select.columns,label,len,method='mean',
                    col.curves=col, ylab=ylab,...)
}

#-------------------------------------------------------------------------------

# 4. Function for extracting legend from ggplot objects -------------------------------------------

extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

# 5. Function for a moving window to obtain classification time points for every single replicate run -------------------------------------------
MW_replicates <- function(data, new_data, timehorizon, threshold, category, metric){
  
  # select the right column depending on the selected metric
  if(metric == "Pop"){
    column_data <- "pop_sum"
  } else if(metric == "HS") {
    column_data <- "hs_change"
  } else {
    column_data <- "extProb"
  }
  
  # reduce timehorizon if it exceeds the maximum number of years in the data set
  if(timehorizon > max(data$Year) - 100){
    timehorizon <- max(data$Year) - 100
    #print(timehorizon)
  }
  
  # for every year calculated the relative size to the size x years into the future
  for (i in 1:nrow(data)) {
    if(column_data == "extProb"){
      rel_loss <- data[data$Year == unique(data$Year)[i+timehorizon] & data$Rep == rep_nr, column_data]
      #print(rel_loss)
    } else {
      rel_loss <- 1 - (data[data$Year == unique(data$Year)[i+timehorizon] & data$Rep == rep_nr, column_data]/data[data$Year == unique(data$Year)[i] & data$Rep == rep_nr, column_data])
      #print(rel_loss)
    }
    
    # controls if the relative loss exceeds the threshold
    if(rel_loss >= threshold){
      #print(unique(data$Year)[i])
      new_data[new_data$BatchNum == BatchNum & new_data$land_rep == land_rep & new_data$replicates == rep_nr, paste(category, metric, sep = "_")] <- (unique(data$Year)[i])-100
      return(new_data)
      break
    }
  }
}