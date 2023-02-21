#".onLoad" <- function(lib,pkg) {
#  cat("cydquantmod: Quantitative Financial Modelling Framework\n\n")
#  cat("Version 0.3-7, Revision 461\n")
#  cat("http://www.cydquantmod.com\n\n")
#}
.plotEnv <- new.env()
.cydquantmodEnv <- new.env()
 
cydquantmodenv <- function() as.environment(".cydquantmodEnv")
print.cydquantmodEnv <- function(x, ...) {
  print("<environment: cydquantmodEnv>")
}

.onAttach <- function(libname,pkgname) {
  #msg <- "Version 0.4-0 included new data defaults. See ?getSymbols."
  #packageStartupMessage(msg)
  # --as-cran check is complaining of this, as a NOTE
  #attach(NULL, name='.cydquantmodEnv')  
}

# Loading cydquantmod produces the following message:
#
# Registered S3 method overwritten by 'cydquantmod':
#   method            from
#   as.zoo.data.frame zoo
#
# Message users that this method will be deprecated in a future release.
options(cydquantmod.deprecate.as.zoo.data.frame = TRUE)

setOldClass("zoo");
setOldClass("xts");
setOldClass("Date");
setClassUnion("xtsORzoo", c("xts","zoo"))
setClass("cydquantmod",representation(
                    model.id="character",
		            model.spec="formula",
                    model.formula="formula",
                    model.target="character",
                    model.inputs="character",
                    build.inputs="character",
                    symbols="character",
                    product="character",
                    price.levels="ANY",
                    training.data="ANY",
                    build.date="character",
                    fitted.model="ANY",
                    model.data="ANY",
                    cydquantmod.version="numeric"
                    )
        );
setClass("cydquantmodReturn",representation(
                    results="xtsORzoo",
                    returns="xtsORzoo",
                    CAGR="numeric",
                    HPR="numeric",
                    accuracy="xtsORzoo",
                    directional.accuracy="list",
                    dist.of.returns="list",
                    returnsBy="ANY"
                    )
        );
#setClass("cydquantmodResults",representation(
#                    model="cydquantmod",
#                    signal="zoo",
#                    return="cydquantmodReturn"
#                    )
#        );
#setClass("tradeLog",representation(
#                    action="character",
#                    quantity="numeric",
#                    underlying="character",
#                    price="numeric",
#                    currency="character",
#                    date="Date",
#                    trade.id="numeric"),
#                    prototype = list(action='',
#                                     quantity=0,
#                                     underlying='',
#                                     price=0,
#                                     currency='USD',
#                                     date=as.Date('2007-01-01'),
#                                     trade.id=1)
#        )
#setMethod("show","tradeLog", 
#         function(object)
#         {
#         tradeLog <- cbind(object@date,object@trade.id,object@price,object@quantity)
#         print(zoo(tradeLog,order.by=object@date))
#         })
setMethod("show", "chobTA",
          function(object) {
            plot.chobTA(object)
          }
)

setMethod("show","cydquantmod", function(object) {
	cat("\ncydquantmod object:  ",
        object@model.id,"\tBuild date: ",
        paste(object@build.date),"\n");
	cat("\nModel Specified: \n    ",
        gsub("[ ]+"," ",deparse(object@model.spec)),"\n");
        cat("\nModel Target: ",object@model.target,"\t\t",
            "Product: ",object@product,"\n");
	cat("Model Inputs: ",
        paste(object@model.inputs,collapse=", "),"\n\n");
	cat("Fitted Model: \n\n");
	if(class(object@fitted.model)[1]=="NULL") {
		cat("\tNone Fitted\n");
	} else {
		cat("\tModelling procedure: ",
        class(object@fitted.model),"\n");
	cat("\tTraining window: ",
        length(object@training.data)," observations from ",
        paste(object@training.data[c(1,length(object@training.data))],
        collapse=" to "));
	cat("\n")
    print(object@fitted.model) 
	}
}
)
setMethod("summary","cydquantmod", function(object) {
	cat("\ncydquantmod object:  ",
        object@model.id,"\tBuild date: ",
        paste(object@build.date),"\n");
	cat("\nModel Specified: \n    ",
        gsub("[ ]+"," ",deparse(object@model.spec)),"\n");
        cat("\nModel Target: ",object@model.target,"\t\t",
            "Product: ",object@product,"\n");
	cat("Model Inputs: ",
        paste(object@model.inputs,collapse=", "),"\n\n");
	cat("Fitted Model: \n\n");
	if(class(object@fitted.model)[1]=="NULL") {
		cat("\tNone Fitted\n");
	} else {
		cat("\tModelling procedure: ",
        class(object@fitted.model),"\n");
	cat("\tTraining window: ",
        length(object@training.data)," observations from ",
        paste(object@training.data[c(1,length(object@training.data))],
        collapse=" to "));
	cat("\n")
    summary(object@fitted.model) 
    }
})

#setMethod("show","cydquantmodResults", function(object) {
#    cat("\n  Model: ",object@model@model.id,"\n")
#	cat("\n  C.A.G.R.: ",sprintf("%04.2f%%",object@return@CAGR*100),"\tH.P.R.: ",
#        sprintf("%04.2f%%",object@return@HPR*100),"\n");
#    to.date.ret <- sprintf("%04.2f%%",object@return@returnsBy[NROW(object@return@returnsBy),-1]*100)
#    to.date.ret <- as.data.frame(t(to.date.ret),row.names="            ")
#
#    colnames(to.date.ret) <- colnames(object@return@returnsBy[,-1])
#    cat("\n  Returns by period summary:\n\n")
#    print(as.data.frame(lapply(as.data.frame(object@return@returnsBy[,-1]), 
#            function(x) sprintf("%04.2f%%",(rev(as.numeric(summary(x))[1:6]*100)))),
#            row.names=c('    Max.','    3rd Qu.','    Mean','    Median','    2rd Qu.','    Min.')))
#    cat("\n  Period to date returns:\n\n")
#    print(to.date.ret)
#}
#)

"fittedModel"<-function(object) {object@fitted.model}
#setGeneric("fittedModel<-", function(x,...,value) {standardGeneric("fittedModel<-")})
setGeneric("fittedModel<-", function(object,value) {standardGeneric("fittedModel<-")})
#setReplaceMethod("fittedModel","cydquantmod", function(x,...,value)
setReplaceMethod("fittedModel","cydquantmod", function(object,value)
{
    object@fitted.model <- value
    
}
)

## setGeneric('plot', function(x,y,...) { standardGeneric('plot') });
## setMethod("plot","tR.results", function(x,y,...) {
##     object <- x
##     ret.by <- object@return@returnsBy
##     plot(ret.by,type=c('l',rep('h',ncol(ret.by)-1)))
## }
## )
## setMethod("plot",signature("ANY","ANY"),function(x,y,...) { UseMethod('plot') } )
#####################################################
###
### Default S3 method and definition for predictModel
###
#####################################################

"predictModel" <-
function(object,data,...)
{
    UseMethod("predictModel");
}
"predictModel.default" <-
function(object,data,...)
{
    predict(object,data,...);
}
'plot.cydquantmodResults' <-
function(x,...)
{
    ret.by <- x@return@returnsBy
    plot(ret.by,type=c('l',rep('h',ncol(ret.by)-1)),...)
}

'formula.cydquantmod' <-
function(x,...)
{
    x@model.formula
}

'coef.cydquantmod' <-
function(object,...)
{
    if(!is.null(fittedModel(object)))
    coef(fittedModel(object),...)    
}

'coefficients.cydquantmod' <- coef.cydquantmod

'fitted.cydquantmod' <-
function(object,...)
{
    if(!is.null(fittedModel(object)))
    fitted(fittedModel(object),...)
}

'fitted.values.cydquantmod' <- fitted.cydquantmod

'residuals.cydquantmod' <-
function(object,...)
{
    if(!is.null(fittedModel(object)))
    residuals(fittedModel(object,...))
}

'resid.cydquantmod' <- residuals.cydquantmod

'vcov.cydquantmod' <-
function(object,...)
{
    if(!is.null(fittedModel(object)))
    vcov(fittedModel(object,...))
}

'logLik.cydquantmod' <-
function(object, ...)
{
    if(!is.null(fittedModel(object)))
    logLik(fittedModel(object),...)
}

'anova.cydquantmod' <-
function(object,...)
{
    if(!is.null(fittedModel(object)))
    anova(fittedModel(object),...)
}

'plot.cydquantmod' <-
function(x,...)
{
    if(!is.null(fittedModel(x)))
    plot(fittedModel(x),...)
}
