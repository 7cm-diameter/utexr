#' @export
align_with <- function(data, event, target, sequence, onset, offset) {
  refs <- data[data[event] == target, ][, sequence]
  onsets <- refs + onset
  offsets <- refs  + offset
  in_ranges <- lapply(seq_len(length(refs)), function(i) {
      in_range <- data[onsets[i] <= data[sequence] & data[sequence] <= offsets[i], ]
      in_range[sequence] <- in_range[sequence] - refs[i]
      in_range$serial <- rep(i, nrow(in_range))
      in_range
    })
  do.call(rbind, in_ranges)
}

#' @importFrom stringr str_split
#' @export
get_metedata_from_filename <- function(filename) {
  metadata_elements <- unlist(str_split(basename(filename), pattern = "_"))
  subject <- metadata_elements[1]
  condition <- metadata_elements[2]
  date <- parse_date_into_int(metadata_elements[3])
  return(c(subject = subject, condition = condition, date = date))
}

#' @importFrom stringr str_split
parse_date_into_int <- function(date, year = T, month = T, day = T,
                                hour = F, min = F) {
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

#' @importFrom stringr str_split
#' @export
add_metadata_to_df <- function(data, filename) {
  metadata <- get_metedata_from_filename(filename)
  n <- nrow(data)
  metadf <- data.frame(subject = rep(metadata["subject"], n),
                       condition = rep(metadata["condition"], n),
                       date = rep(metadata["date"], n))
  return(cbind(metadf, data))
}

#' @export
dict <- function(a, b) {
  na = length(a)
  nb = length(b)

  nua = length(unique(a))
  nub = length(unique(b))

  if(!(na == nua && nb == nub)) {
    stop("Arguments, a and b, must be a vector of unique elements")
  }

  if(na != nb) {
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

#' @export
bining <- function(x, binwidth) {
  floor(x / binwidth) * binwidth
}

#' @export
read.csv.with.colname <- function(path, colnames) {
  d <- read.csv(path, header = FALSE)
  names(d) <- colnames
  return(d)
}

#' @export
read.and.combine <- function(path, newdata) {
  if(file.exists(path)) {
    olddata <- read.csv(path)
    data <- rbind(olddata, newdata)
    write.csv(data, path, row.names = FALSE)
  } else {
    data <- newdata
  }
  return(data)
}
