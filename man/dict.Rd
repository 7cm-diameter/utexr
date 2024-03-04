\name{dict}
\alias{dict}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Information about `dict`
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dict(a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
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
function (a, b) 
{
    na = length(a)
    nb = length(b)
    nua = length(unique(a))
    nub = length(unique(b))
    if (!(na == nua && nb == nub)) {
        stop("Arguments, a and b, must be a vector of unique elements")
    }
    if (na != nb) {
        stop("a and b must be the same length")
    }
    inner <- function(x) {
        i <- which(a == x)
        if (length(i) == 0) {
            return(NA)
        }
        return(b[i])
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
