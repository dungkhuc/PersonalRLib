\name{tufteBar}
\alias{tufteBar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a bar plot according to Tufte's principle.
}
\description{
This function makes it easy to create a minimalistic bar plot according to Tufte's principle. ggplot2 and ggthemes are used.
}
\usage{
tufteBar(dat, x, y = NULL, font = "Arial", font_size = 20, col = "black")
}

\arguments{
  \item{dat}{
  data.frame object
  }
  \item{x}{
  Name of column in data frame specified. Data in this column will be used for x axis.
  }
  \item{y}{
  Name of column in data frame specified. Data in this column will be used for y axis. If not specified, this function will count the occurences of each value in x.
  }
  \item{x.text.rotate} {
  Rotate text in x-axis. [0...360]. 
  }
  \item{y.text.rotate} {
  Rotate text in y-axis. [0...360].
  }
  \item{font}{
  Specify font. 
  }
  \item{font_size}{
  Specify font size.
  }
  \item{color}{
  Specify color of the bar.
  }
}
\details{
The function will sum up values in y that have same corresponding value in x.
If y is not specified, the function will count the occurences of each unique value in x.
}
\value{
Return a ggplot.
}
\references{

}
\author{
Dung Khuc.
}
\note{

}



\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }