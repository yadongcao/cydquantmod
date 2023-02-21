"tradeModel" <- function(x,
                         signal.threshold=c(0,0),
                         leverage=1,
                         return.model=TRUE,
                         plot.model=FALSE,
                         trade.dates=NULL,
                         exclude.training=TRUE,
                         ret.type=c('weeks','months','quarters','years'),...)
{
  trade.offset = 0;
  cydquantmod <- getModelData(x);
  if(!inherits(cydquantmod, "cydquantmod")) stop("model must be of class cydquantmod");
  if(!is.null(trade.dates) && length(trade.dates) < 2) stop("trade.dates must be of length 2");
  model.data <- modelData(cydquantmod,trade.dates,exclude.training=exclude.training);
  fitted.zoo <- predictModel(cydquantmod@fitted.model,model.data,...)
  if(inherits(fitted.zoo, "zoo", which = TRUE) != 1) {
    # if(class(fitted.zoo) != "zoo") {
    # /\ this is only needed if 'fitted.zoo' is not exactly a zoo object (i.e. not xts)
    fitted.zoo <- zoo(as.vector(fitted.zoo),index(model.data));
  }
  # trade Rule section

  #on open
  signal.zoo <- ifelse(fitted.zoo < signal.threshold[1] |
                       fitted.zoo > signal.threshold[2],
                       ifelse(fitted.zoo > 0,1,-1), 0); 
  tmp.index <- index(signal.zoo)[-(1+trade.offset)];
  market.zoo <- model.data[-(nrow(model.data)+trade.offset),1]
  signal.zoo <- signal.zoo[-c(length(index(signal.zoo))-trade.offset,length(index(signal.zoo)))];
  signal.zoo = merge(market.zoo,signal.zoo)
  index(signal.zoo) <- tmp.index;

  #cydquantmodResults <- new("cydquantmodResults", model=cydquantmod, signal=signal.zoo);
  cydquantmodResults <- list(model=cydquantmod, signal=signal.zoo)

  model.returns <- modelReturn(cydquantmodResults,trade.dates=trade.dates,leverage=leverage,ret.type=ret.type);
  cydquantmodResults$return <- model.returns;

  # strip data to minimize memory consumption
  cydquantmodResults$model <- stripModelData(cydquantmodResults$model);
 return(structure(cydquantmodResults, class="cydquantmodResults"));
}

print.cydquantmodResults <- function(x, ...) {
    cat("\n  Model: ",x$model@model.id,"\n")
	cat("\n  C.A.G.R.: ",sprintf("%04.2f%%",x$return@CAGR*100),"\tH.P.R.: ",
        sprintf("%04.2f%%",x$return@HPR*100),"\n");
    to.date.ret <- sprintf("%04.2f%%",x$return@returnsBy[NROW(x$return@returnsBy),-1]*100)
    to.date.ret <- as.data.frame(t(to.date.ret),row.names="            ")

    colnames(to.date.ret) <- colnames(x$return@returnsBy[,-1])
    cat("\n  Returns by period summary:\n\n")
    print(as.data.frame(lapply(as.data.frame(x$return@returnsBy[,-1]), 
            function(x) sprintf("%04.2f%%",(rev(as.numeric(summary(x))[1:6]*100)))),
            row.names=c('    Max.','    3rd Qu.','    Mean','    Median','    2rd Qu.','    Min.')))
    cat("\n  Period to date returns:\n\n")
    print(to.date.ret)
}
