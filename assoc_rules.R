library(SuperLearner)
library(dplyr)
library(caret)
library(e1071)
library(bnlearn)
library(arules)
library(rpart)

data <- read.table("hw8_data.csv", sep = ',', header = TRUE)

detectFactor <- function(x) {
  which(apply(x, 2, function(x) {length(unique(x))}) <= 10)
}

my_discretize <- function (x, method = "frequency", breaks = 3, labels = NULL, 
                           include.lowest = TRUE, right = FALSE, dig.lab = 3, ordered_result = FALSE, 
                           infinity = FALSE, onlycuts = FALSE, categories = NULL, ...) 
{
  if (!is.null(categories)) {
    warning("Parameter categories is deprecated. Use breaks instead! Also, the default method is now frequency!")
    breaks <- categories
  }
  methods = c("interval", "frequency", "cluster", "fixed")
  method <- methods[pmatch(tolower(method), methods)]
  if (is.na(method)) 
    stop("Unknown method!")
  if (method == "fixed" && length(breaks) < 2) 
    stop("fixed needs at least two values for breaks.")
  if (method != "fixed" && (length(breaks) != 1 || breaks < 
                            1)) 
    stop("breaks needs to be a single positive integer for this method.")
  breaks <- switch(method, interval = seq(from = min(x, na.rm = TRUE), 
                                          to = max(x, na.rm = TRUE), length.out = breaks + 1), 
                   frequency = unique(quantile(x, probs = seq(0, 1, length.out = breaks + 
                                                         1), na.rm = TRUE), cluster = {
                                                           cl <- stats::kmeans(stats::na.omit(x), breaks, ...)
                                                           centers <- sort(cl$centers[, 1])
                                                           as.numeric(c(min(x, na.rm = TRUE), head(centers, 
                                                                                                   length(centers) - 1) + diff(centers)/2, max(x, 
                                                                                                                                               na.rm = TRUE)))
                                                         }, fixed = breaks))
  if (any(duplicated(breaks))) {
    stop("The calculated breaks are: ", paste(breaks, collapse = ", "), 
         "\n  Some breaks are not unique. Change the number of breaks or consider using method 'fixed'.")
  }
  if (infinity) {
    breaks[1] <- -Inf
    breaks[length(breaks)] <- Inf
  }
  if (onlycuts) 
    return(as.vector(breaks))
  structure(cut(x, breaks = breaks, labels = labels, include.lowest = include.lowest, 
                right = right, ordered_result = ordered_result), `discretized:breaks` = as.vector(breaks), 
            `discretized:method` = method)
}

discretize.DF <-function (df, methods = NULL, default = NULL) 
{
  if (is.data.frame(methods)) 
    return(.rediscretizeDF(methods, df))
  for (i in colnames(df)) {
    if (!is.numeric(df[[i]])) 
      next
    args <- if (is.null(methods[[i]])) 
      default
    else methods[[i]]
    if (!is.null(args) && (is.null(args$method) || args$method == 
                           "none")) 
      next
    if (is(err <- try(df[[i]] <- do.call("my_discretize", c(list(x = df[[i]]), 
                                                         args)), silent = TRUE), "try-error")) 
      stop("Problem with column ", i, "\n", err)
  }
  df
}



data.numeric <- as.data.frame(sapply(data[-detectFactor(data)], as.numeric))
data.discrete <- discretize.DF(data.numeric, default=list(method = "frequency", breaks = 10))
data[, colnames(data.discrete)] <- data.discrete[, colnames(data.discrete)]


write.csv(data, file="discrete.csv", row.names = FALSE)


