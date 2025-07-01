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

