# @authors: a character-vector of author names
# @year: a numeric specifying the year
# @model: a character-string specigying model name (should be always empty)
# @text: a character-string
# @url: a character-string specifying the models source homepage
description <- function(authors=c("Kosuke Imai", "Gary King", "Olivia Lau"),
                        year=NULL, model="", text="", url="",
                        category = NULL) {
  # error-catching
  if (!is.character(authors))
    author <- "Kosuke Imai, Gary King, and Olivia Lau"

  else if (length(authors) > 1) {
    # collapse author names if it is a character-vector bigger than 1
    authors <- paste(paste(head(authors, -1), collapse=", "),
                     ", and ",
                     tail(authors, 1),
                     sep = ""
                     )
  }

  if (!is.numeric(year))
    year <- as.numeric(format(Sys.Date(), "%Y"))

  if (!is.character(model) || length(model) != 1)
    stop("model must be a character-string")

  if (length(text) > 1)
    stop("text must be a character-vector of length 1")

  if (is.null(url))
    url <- "http://gking.harvard.edu/zelig"

  if (!is.character(category))
    category <- ""

  else if (length(url) > 1 || !is.character(url))
    stop("url must be a character-vector of length 1")

  # double back-up, even though this should be impossible now
  authors <- ifelse(nchar(authors) > 0, authors, "NAMELESS AUTHOR")
  year <- ifelse(!is.null(year), year, "UNKNOWN YEAR")
  model <- ifelse(nchar(model) > 0, model, "UNNAMED MODEL")

  # construct object
  self <- list(authors = authors,
               year    = year,
               model   = model,
               text    = text,
               url     = url
               )
  class(self) <- "description"
  self
}


# @descr: an description object
# return: a character-string giving citation info
cite <- function(descr) {
  # catch error, and make due
  if (!inherits(descr, "description"))
    descr <- description()

  # 
  url <- "http://gking.harvard.edu/zelig"

  title <- if (is.null(descr$text))
    descr$model
  else
    paste(descr$model, ": ", descr$text, sep="")

  # quote
  title <- paste('"', title, '"', sep="")

  # construct string.  This should be done much more elegantly
  # and with localization
  str <- "How to cite this model in Zelig:\n"
  str <- paste(str, descr$authors, ". ", descr$year, ". ", title, sep="")
  str <- paste(str, " in Kosuke Imai, Gary King, and Olivia Lau, ", sep="")
  str <- paste(str, "\"Zelig: Everyone's Statistical Software,\"", sep="")
  str <- paste(str, url, "\n", sep="")
}


# declare factory-method generic
as.description <- function(...)
  UseMethod("as.description")

# @descr: a description object
# return: the same description object
as.description.description <- function(descr)
  descr

# @descr: a list
# return: a description object
as.description.list <- function(descr) {
  text <- if (!is.null(descr$text))
    descr$text
  else if (!is.null(descr$description))
    descr$description
  else
    NULL
  
  description(authors = descr$authors,
              year    = descr$year,
              model   = descr$model,
              text    = text,
              url     = descr$url,
              category= descr$category
              )
}
