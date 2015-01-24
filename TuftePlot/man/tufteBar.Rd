\name{tufteBar}
\alias{tufteBar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tufteBar(dat, x, y = NULL, font = "Arial", font_size = 20, col = "black")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{font}{
%%     ~~Describe \code{font} here~~
}
  \item{font_size}{
%%     ~~Describe \code{font_size} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dat, x, y = NULL, font = "Arial", font_size = 20, col = "black") 
{
    if (is.null(y)) 
        gplot = ggplot(dat, aes(x = order)) + geom_bar(stat = "bin", 
            width = 0.6, fill = col)
    else gplot = ggplot(dat, aes_string(x = x, y = y)) + geom_bar(stat = "identity", 
        width = 0.6, fill = col)
    if (exists("gplot")) 
        tufteTheme(gplot, font, font_size)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
