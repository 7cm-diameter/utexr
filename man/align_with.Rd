\name{align_with}
\alias{align_with}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Information about `align_with`
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
align_with(data, event, target, sequence, onset, offset)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{event}{
%%     ~~Describe \code{event} here~~
}
  \item{target}{
%%     ~~Describe \code{target} here~~
}
  \item{sequence}{
%%     ~~Describe \code{sequence} here~~
}
  \item{onset}{
%%     ~~Describe \code{onset} here~~
}
  \item{offset}{
%%     ~~Describe \code{offset} here~~
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
function (data, event, target, sequence, onset, offset) 
{
    refs <- data[data[event] == target, ][, sequence]
    onsets <- refs + onset
    offsets <- refs + offset
    in_ranges <- lapply(seq_len(length(refs)), function(i) {
        in_range <- data[onsets[i] <= data[sequence] & data[sequence] <= 
            offsets[i], ]
        in_range[sequence] <- in_range[sequence] - refs[i]
        in_range$serial <- rep(i, nrow(in_range))
        in_range
    })
    do.call(rbind, in_ranges)
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
