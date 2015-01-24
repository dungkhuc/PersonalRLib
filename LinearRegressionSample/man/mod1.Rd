\name{mod1}
\alias{mod1}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("mod1")}
\format{
  The format is:
List of 7
 $ coefficients : Named num [1:2] -0.357 4.034
  ..- attr(*, "names")= chr [1:2] "Const" "Bwt"
 $ vcov         : num [1:2, 1:2] 0.4792 -0.1706 -0.1706 0.0626
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:2] "Const" "Bwt"
  .. ..$ : chr [1:2] "Const" "Bwt"
 $ sigma        : num 1.45
 $ df           : int 142
 $ fitted.values: num [1:144] 7.71 7.71 7.71 8.11 8.11 ...
 $ residuals    : num [1:144] -0.711 -0.311 1.789 -0.915 -0.815 ...
 $ call         : language linmod.default(x = x, y = y)
 - attr(*, "class")= chr "linmod"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(mod1)
## maybe str(mod1) ; plot(mod1) ...
}
\keyword{datasets}
