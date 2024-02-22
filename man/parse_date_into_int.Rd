\name{parse_date_into_int}
\alias{parse_date_into_int}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Information about `parse_date_into_int`
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
parse_date_into_int(date, year = T, month = T, day = T, hour = F, min = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{date}{
%%     ~~Describe \code{date} here~~
}
  \item{year}{
%%     ~~Describe \code{year} here~~
}
  \item{month}{
%%     ~~Describe \code{month} here~~
}
  \item{day}{
%%     ~~Describe \code{day} here~~
}
  \item{hour}{
%%     ~~Describe \code{hour} here~~
}
  \item{min}{
%%     ~~Describe \code{min} here~~
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
function (date, year = T, month = T, day = T, hour = F, min = F) 
{
    date_elements <- unlist(str_split(date, pattern = "-"))
    elements_used <- c(year, month, day, hour, min)
    parsed_date <- as.character()
    for (i in seq_len(length(elements_used))) {
        if (elements_used[i]) {
            parsed_date <- paste0(parsed_date, date_elements[i])
        }
    }
    return(parsed_date)
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
