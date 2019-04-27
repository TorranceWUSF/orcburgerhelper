\name{Roll Example}
\alias{Roll Example}
\docType{data}
\title{
        Table Top Information Helper
}
\description{
        These functions help keep track of information during a table top game Results will vary because of the nature of the function.
}
\usage{roll(a,b,c,d), character(x,y)}
\format{
  For the first two arguments any number. For the last two arguments alphabetic with qoutation marks.
  \describe{
    \item{\code{a}}{a numeric vector - how many times I wish to roll}
    \item{\code{b}}{a numeric vector - how many sides does the die you wish to roll have}
    \item{\code{c}}{an alphabetic vector - names the character that is rolling the die}
    \item{\code{d}}{an alphabetic vector - how you wish for the roll data to be displayed. Whether "combined" or "split".}
    \item{\code{x}}{an aplhabetic vector - name of character. Will also serve as the name of the list returned by the function}
    \item{\code{y}}{an aplhabetic vector - the action that you wish to do to the first argument. Whether to "create" a character or to "update" one}
}
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
playeroneroll <- roll(4,6,"Fred", "split")
playeroneroll
    "Fred" , 4,1,3,6


playerroll <- character(Fred,"create")

playerroll

  return a readline script to fill in the rest of the data

}

\keyword{datasets}
