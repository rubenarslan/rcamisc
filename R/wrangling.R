#' missingness patterns
#'
#' this function shows how common possible missingness patterns are. Emulates misschk in stata.
#' 1. excludes any variables that don't have any missings, so as not to clutter output. Disable using omit_complete
#' 2. sorts variables by number of missings, so that the usual suspects show up at the front.
#' 3. displays number of missings accounted for by each pattern
#'
#' @param df dataset
#' @param min_freq show only patterns that occur at least this often. Defaults to 1 observation.
#' @param long_pattern by default (FALSE) only shows column indices for space and legibility reasons.
#' @param print_legend prints a legend for the column indices, defaults to FALSE if long_pattern is set
#' @param show_culprit defaults to TRUE. In case a missingness pattern boils down to one variable, it will be shown here.
#' @param relative defaults to FALSE. If true, percentages are shown (relative to total before excluding minimum frequency).
#' @param omit_complete defaults to TRUE. Columns that don't have any missings are excluded.
#' @export
#' @examples
#' data(ChickWeight)
#' ChickWeight[1:2,c('weight','Chick')] = NA
#' ChickWeight[3:5,'Diet'] = NA
#' names(ChickWeight); nrow(ChickWeight)
#' missingness_patterns(ChickWeight)
missingness_patterns = function(df, min_freq = ifelse(relative,
                                                      1/nrow(df), 1), long_pattern = FALSE, print_legend = ifelse(long_pattern,
                                                                                                                  FALSE, TRUE), show_culprit = TRUE, relative = FALSE, omit_complete = TRUE) {
  missings_by_column = colSums(is.na(df))
  if (omit_complete) {
    takethese = missings_by_column != 0
  } else {
    takethese = TRUE
  }
  names(missings_by_column) = names(df)
  missings_by_column = sort(missings_by_column[takethese],
                            decreasing = T)
  any_missing_sorted = names(missings_by_column)
  df = subset(df, select = any_missing_sorted)
  cols = names(df)
  if (length(cols) == 0) {
    cat("No missings at all.\n")
    return(invisible(NULL))
  }
  df = !is.na(df)
  ddf = as.data.frame(df)
  if (min_freq > 0) {
    counted = dplyr::count_(ddf, vars = names(ddf))
    names(counted) = c(cols, "Freq")
  } else {
    counted = as.data.frame(stats::xtabs(data = df))
  }
  if (relative) {
    counted$Freq = counted$Freq/sum(counted$Freq)
  }
  counted = counted[counted$Freq >= min_freq, ]
  pattern = character(length = nrow(counted))
  if (show_culprit) {
    culprit = rep(x = "_", nrow(counted))
  }
  for (i in 1:length(cols)) {
    if (show_culprit) {
      culprit[counted[, i] == "FALSE"] = ifelse(culprit[counted[,
                                                                i] == "FALSE"] == "_", cols[i], "")  # if it's a _, set it, if it's set, set it to empty
    }
    nr = as.character(i)
    pattern = paste0(pattern, ifelse(i == 1, "", "_"), ifelse(counted[,
                                                                      i] == "TRUE", stringr::str_pad("", stringr::str_length(nr),
                                                                                                     pad = "_"), nr))
  }
  missingness = data.frame(Pattern = pattern, Freq = counted$Freq,
                           Culprit = culprit)

  if (long_pattern == TRUE) {
    long_pattern = character(length = nrow(counted))
    for (i in 1:length(cols)) {
      long_pattern = paste0(long_pattern, ifelse(counted[,
                                                         i] == "TRUE", "_", paste0(cols[i], ".")))
    }
    missingness$Pattern = long_pattern
  }
  if (print_legend) {
    print(data.frame(index = 1:length(cols), col = cols,
                     missings = missings_by_column), row.names = FALSE)
  }
  missingness = missingness[order(missingness$Freq, decreasing = T),
                            ]
  rownames(missingness) = NULL
  missingness
}



#' take only nonmissing
#'
#' this function takes a subset of a dataset, omitting all
#' cases with missings in variables specified in 'keep'
#' and omitting all variables that still have missings after that.
#' Good to see how large your dataset for a certain analysis
#' will be and which covariates are 'free' in terms of sample size.
#'
#' @param df dataset
#' @param keep defaults to empty vector
#' @export
#' @examples
#' data(ChickWeight)
#' ChickWeight[1:2,c('weight','Chick')] = NA
#' ChickWeight[3:4,'Diet'] = NA
#' names(ChickWeight); nrow(ChickWeight)
#' ChickWeight2 = take_nonmissing(ChickWeight, keep = c('weight'))
#' names(ChickWeight2); nrow(ChickWeight2)
take_nonmissing = function(df, keep = c()) {
  df = df[rowSums(is.na(subset(df, select = keep, drop = F))) ==
            0, ]  # omit all cases with missings in keep
  df = subset(df, select = names(which(colSums(is.na(df)) ==
                                         0)), drop = F)  # omit all variables with missings
}


#' repeat last non-NA value
#'
#' Will repeat the last non-NA value. This is also known as carrying the last observation forward/backward.
#' It's faster than zoo::na.locf http://rpubs.com/rubenarslan/repeat_last_na_locf and other alternatives.
#' By specifying maxgap, you can choose not to bridge overly long gaps.
#' By specifying forward = FALSE, you can carry the last observation backward.
#'
#'
#'
#' @param x vector to be repeated
#' @param forward carry last observation forward? or backward (FALSE)
#' @param maxgap bridge only up to x NAs (defaults to Inf)
#' @param na.rm whether to omit NAs at the beginning (defaults to FALSE)
#' @export
#' @examples
#' x = c(NA,NA,1,NA,NA,NA,NA,NA,NA,NA,NA,2,3,4,NA,NA,NA,NA,NA,5, NA)
#' data.frame(x,
#'    repeat_last(x),
#'    repeat_last(x, forward = FALSE),
#'    repeat_last(x, maxgap = 5),
#' check.names = FALSE)
#'
repeat_last = function(x, forward = TRUE, maxgap = Inf, na.rm = FALSE) {   # repeats the last non NA value.
  if (!forward) x = rev(x)           # reverse x twice if carrying backward
  ind = which(!is.na(x))             # get positions of nonmissing values
  if (is.na(x[1]) && !na.rm)         # if it begins with NA
    ind = c(1,ind)    					     # add first pos
  rep_times = diff(                  # diffing the indices + length yields how often
    c(ind, length(x) + 1) )          # they need to be repeated
  if (maxgap < Inf) {
    exceed = rep_times - 1 > maxgap  # exceeding maxgap
    if (any(exceed)) {               # any exceed?
      ind = sort(c(ind[exceed] + 1, ind))      # add NA following large gaps to indices
      rep_times = diff(c(ind, length(x) + 1) ) # diff again
    }
  }
  x = rep(x[ind], times = rep_times) # repeat the values at these indices
  if (!forward) x = rev(x)           # second reversion
  x
}


#' aggregates two variables from two sources into one
#'
#' Takes two variables with different missings
#' and gives one variable with values of the second
#' variable substituted where the first had missings.
#'
#' @param df data.frame or variable
#' @param new_var new variable name
#' @param var1 first source. Assumed to be new_var.x (default suffixes after merging)
#' @param var2 second source. Assumed to be new_var.y (default suffixes after merging)
#' @param remove_old_variables Defaults to not keeping var1 and var2 in the resulting df.
#' @export
#' @examples
#' cars$dist.x = cars$dist
#' cars$dist.y = cars$dist
#' cars$dist.y[2:5] = NA
#' cars$dist.x[10:15] = NA # sprinkle missings
#' cars$dist = NULL # remove old variable
#' cars = aggregate2sources(cars, 'dist')
aggregate2sources = function(df, new_var = NULL, var1 = NULL, var2 = NULL,
                             remove_old_variables = TRUE) {
  if (!is.null(new_var) && ncol(df) == 2) {
    new_var <- names(df)[1]
    var1 <- names(df)[1]
    var2 <- names(df)[2]
  }
  if (is.null(var1) && is.null(var2)) {
    var1 = paste0(new_var, ".x")
    var2 = paste0(new_var, ".y")
  }
  if (exists(new_var, where = df)) {
    warning(paste(new_var, "already exists. Maybe delete it or choose a different name, if you're saving over your original dataframe."))
  }
  df[, new_var] = df[, var1]
  oldmiss = sum(is.na(df[, new_var]))
  df[is.na(df[, var1]), new_var] = df[is.na(df[, var1]), var2]

  if (remove_old_variables) {
    df[, var1] = NULL
    df[, var2] = NULL
  }

  message(paste(oldmiss - sum(is.na(df[, new_var])), " fewer missings"))
  df
}