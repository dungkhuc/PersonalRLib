\name{tufteTheme}
\alias{tufteTheme}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tufteTheme(tuftePlot, font, font_size)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tuftePlot}{
%%     ~~Describe \code{tuftePlot} here~~
}
  \item{font}{
%%     ~~Describe \code{font} here~~
}
  \item{font_size}{
%%     ~~Describe \code{font_size} here~~
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
function (tuftePlot, font, font_size) 
{
    tuftePlot = tuftePlot + theme_tufte(base_family = font, base_size = font_size, 
        ticks = F)
    y.axis.ticks = ggplot_build(tuftePlot)$panel$ranges[[1]]$y.major_source
    tuftePlot = tuftePlot + theme(axis.line = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_blank()) + 
        geom_hline(yintercept = y.axis.ticks, col = "white", 
            lwd = 1.2)
    tuftePlot
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
