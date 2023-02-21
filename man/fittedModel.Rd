\name{fittedModel}
\alias{fittedModel}
\alias{fittedModel<-}
\alias{formula.cydquantmod}
\alias{plot.cydquantmod}
\alias{coefficients.cydquantmod}
\alias{coef.cydquantmod}
\alias{residuals.cydquantmod}
\alias{resid.cydquantmod}
\alias{fitted.values.cydquantmod}
\alias{fitted.cydquantmod}
\alias{anova.cydquantmod}
\alias{logLik.cydquantmod}
\alias{vcov.cydquantmod}
\title{ cydquantmod Fitted Objects }
\description{
Extract and replace fitted models from
\code{cydquantmod} objects built with
\code{buildModel}. All objects fitted
through methods specified in \code{buildModel}
calls can be extracted for further analysis.
}
\usage{
fittedModel(object)


\method{formula}{cydquantmod}(x, \dots)

\method{plot}{cydquantmod}(x, \dots)

\method{coefficients}{cydquantmod}(object, \dots)

\method{coef}{cydquantmod}(object, \dots)

\method{residuals}{cydquantmod}(object, \dots)

\method{resid}{cydquantmod}(object, \dots)

\method{fitted.values}{cydquantmod}(object, \dots)

\method{fitted}{cydquantmod}(object, \dots)

\method{anova}{cydquantmod}(object, \dots)

\method{logLik}{cydquantmod}(object, \dots)

\method{vcov}{cydquantmod}(object, \dots)
}
\arguments{
  \item{object}{ a \code{cydquantmod} object }
  \item{x}{ a suitable object }
  \item{\dots}{ additional arguments }
}
\details{
Most often used to extract the final fitted
object of the modelling process, usually for
further analysis with tools outside
the \pkg{cydquantmod} package.

Most common methods to apply to fitted objects
are available to the parent \code{cydquantmod}
object. At present, one can call directly the
following S3 methods on a built model as if
calling directly on the fitted object.
See *Usage* section.

It is also \emph{possible} to add
a fitted model to an object. This
may be of value when applying heuristic
rule sets for trading approaches, or when
fine tuning already fit models by hand.
}
\value{
Returns an object matching that returned
by a call to the method specified in
\code{buildModel}.
}
\author{ Jeffrey A. Ryan }
\note{ 
The replacement function \code{fittedModel<-}
is highly experimental, and may or may not
continue into further releases.

The fitted model added \emph{must} use the
same names as appear in the \code{cydquantmod}
object's dataset.
}
\seealso{ \code{\link{cydquantmod}},\code{\link{buildModel}} }
\examples{
\dontrun{
x <- specifyModel(Next(OpCl(DIA)) ~ OpCl(VIX))
x.lm <- buildModel(x,method='lm',training.per=c('2001-01-01','2001-04-01'))

fittedModel(x.lm)

coef(fittedModel(x.lm))
coef(x.lm)                  # same

vcov(fittedModel(x.lm))
vcov(x.lm)                  # same
}
}
\keyword{ models }
