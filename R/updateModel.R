"updateModel" <-
function(fitted.model,cydquantmod) {
   cydquantmod@model.inputs <- fitted.model@model.inputs;
   cydquantmod@fitted.model <- fitted.model;
   return(cydquantmod);
}

