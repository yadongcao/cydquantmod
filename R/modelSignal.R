"modelSignal" <-
function(x)
{
  if(!is.cydquantmodResults(x)) stop(paste(dQuote("x"),"must be of class",
                                  dQuote("cydquantmodResults")))
  x@signal
}
