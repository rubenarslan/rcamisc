#' multi trait multi method matrix
#'
#' renders a MTMM using ggplot2. This function will split the variable names in a correlation matrix, or a data.frame. The first part will be used as the trait, the second as the method. Correlations are displayed as text, with the font size corresponding to absolute size.
#' You can optionally supply a data frame of reliabilites to show in the diagonal.
#'
#' @param variables data frame of variables that are supposed to be correlated
#' @param reliabilities data frame of reliabilties: column 1: scale, column 2: rel. coefficient
#' @param split_regex regular expression to separate construct and method from the variable name, splits on '.' by default
#' @param cors you can also supply a (named) correlation matrix
#'
#' @import ggplot2
#'
#' @export
#' @examples
#' data.mtmm = data.frame(
#' `Ach_self_report` = rnorm(200), `Pow_self_report` = rnorm(200), `Aff_self_report`= rnorm(200),
#' `Ach_peer_report` = rnorm(200),`Pow_peer_report`= rnorm(200),`Aff_peer_report` = rnorm(200),
#' `Ach_diary` = rnorm(200), `Pow_diary` = rnorm(200),`Aff_diary` = rnorm(200))
#' reliabilities = data.frame(scale = names(data.mtmm), rel = stats::runif(length(names(data.mtmm))))
#' mtmm(data.mtmm, reliabilities = reliabilities)
#'
mtmm = function(variables = NULL, reliabilities = NULL, split_regex = "_",
                cors = NULL) {
  if (is.null(cors) & is.null(variables)) {
    stop("You have to provide either cors or variables.")
  }
  if (is.null(cors) & !is.null(variables))
    cors = stats::cor(variables, use = "pairwise.complete.obs")  # select variables

  var.names = colnames(cors)

  corm = reshape2::melt(cors)
  names(corm) = c("Var1", "Var2", "value")
  # substitute the 1s with the scale reliabilities here
  corm = corm[corm[, "Var1"] != corm[, "Var2"], ]
  if (!is.null(reliabilities)) {
    rel = reliabilities
    names(rel) = c("Var1", "value")
    rel$Var2 = rel$Var1
    rel = rel[which(rel$Var1 %in% var.names), c("Var1", "Var2",
                                                "value")]
    corm = rbind(corm, rel)
  }

  if (any(is.na(stringr::str_split_fixed(corm$Var1, split_regex,
                                         n = 2)))) {
    print(unique(stringr::str_split_fixed(corm$Var1, split_regex,
                                          n = 2)))
    stop("regex broken")
  }
  # regex matching our column naming schema to extract trait
  # and method
  corm[, c("trait_X", "method_X")] = stringr::str_split_fixed(corm$Var1,
                                                              split_regex, n = 2)
  corm[, c("trait_Y", "method_Y")] = stringr::str_split_fixed(corm$Var2,
                                                              split_regex, n = 2)
  corm[, c("trait_Y", "method_Y", "trait_X", "method_X")] = sapply(corm[,
                                                                        c("trait_Y", "method_Y", "trait_X", "method_X")], FUN = function(x) stringr::str_replace_all(x,
                                                                                                                                                                     "(\\.|_)", " "))
  # sort pairs to find dupes
  corm[, c("var1.s", "var2.s")] <- t(apply(corm[, c("Var1",
                                                    "Var2")], 1, sort))
  corm[which(corm[, "trait_X"] == corm[, "trait_Y"] & corm[,
                                                           "method_X"] != corm[, "method_Y"]), "type"] = "validity"
  corm[which(corm[, "trait_X"] != corm[, "trait_Y"] & corm[,
                                                           "method_X"] == corm[, "method_Y"]), "type"] = "heterotrait-monomethod"
  corm[which(corm[, "trait_X"] != corm[, "trait_Y"] & corm[,
                                                           "method_X"] != corm[, "method_Y"]), "type"] = "heterotrait-heteromethod"
  corm[which(corm[, "trait_X"] == corm[, "trait_Y"] & corm[,
                                                           "method_X"] == corm[, "method_Y"]), "type"] = "reliability"

  # would be nice to have the facet_grid labels in the same
  # palce as the tick marks
  corm$trait_X = factor(corm$trait_X)
  corm$trait_Y = factor(corm$trait_Y, levels = rev(levels(corm$trait_X)))
  corm$method_X = factor(corm$method_X)
  corm$method_Y = factor(corm$method_Y, levels = levels(corm$method_X))
  corm = corm[order(corm$method_X, corm$trait_X), ]
  corm = corm[!duplicated(corm[, c("var1.s", "var2.s")]), ]  # remove dupe pairs
  corm$rvalue = stringr::str_replace(round(corm$value, 2),
                                     "0\\.", ".")
  corm$logvalue = log(corm$value^2)
  # building ggplot the melted correlation matrix
  mtmm_plot <- ggplot(data = corm) + geom_tile(aes_string(x = "trait_X",
                                                          y = "trait_Y", fill = "type")) + geom_text(aes_string(x = "trait_X",
                                                                                                                y = "trait_Y", label = "rvalue", size = "logvalue")) +
    facet_grid(method_Y ~ method_X) + ylab("") + xlab("") +
    theme_bw(base_size = 18) + theme(panel.background = element_rect(colour = NA),
                                     panel.grid.minor = element_blank(), axis.line = element_blank(),
                                     panel.border = element_blank(), strip.background = element_blank(),
                                     panel.grid = element_blank(), legend.position = c(1,
                                                                                       1), legend.justification = c(1, 1)) + scale_fill_brewer("Type") +
    scale_size("Absolute size", guide = F) + scale_colour_gradient(guide = F)

  mtmm_plot
}

#' iterate adding ribbons to a ggplot2 plot at varying confidence levels to shade by confidence. Horribly inefficient, because smooth stat is computed every time, but flexible.
#'
#' @param levels the confidence levels that are supposed to be displayed, defaults to 0.6, 0.8, 0.95
#' @param base_alpha divided by length(levels)
#' @param fill_gradient a vector of colors that has at least the same length as levels. Color each ribbon differently
#' @param fill a single color for the ribbon
#' @param stat defaults to smooth
#' @param ... everything else is passed to and documented in [ggplot2::geom_smooth()]
#' @inheritParams ggplot2::geom_smooth
#' @export
#' @examples
#' data(beavers)
#' plot = ggplot2::ggplot(beaver1, ggplot2::aes(time, temp))
#' plot + geom_shady_smooth() + ggplot2::facet_wrap(~ day)
#' plot + geom_shady_smooth(fill = 'blue', levels = seq(0.05,0.95,0.1))
#' plot + geom_shady_smooth(size = 0.1, fill = '#49afcd', levels = seq(0.1,0.8,0.01))
#' plot + geom_shady_smooth(fill_gradient = c('red', 'orange', 'yellow'), base_alpha = 3)
geom_shady_smooth <- function(mapping = NULL, data = NULL, stat = "smooth",
                              method = "auto", formula = y ~ x, se = TRUE, position = "identity",
                              na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, levels = c(0.6,
                                                                                              0.8, 0.95), base_alpha = 1, fill_gradient = NULL, fill = "black",
                              ...) {
  layers = list()
  ribbon_alpha = base_alpha/length(levels)
  if (ribbon_alpha > 1)
    ribbon_alpha = 1

  params <- list(na.rm = na.rm, fill = fill, ...)
  if (identical(stat, "smooth")) {
    params$method <- method
    params$formula <- formula
    params$se <- se
  }
  params_ribbon = params
  params_ribbon$color = NULL  # don't want the line color to be the stroke of the ribbon
  params_ribbon$alpha = ribbon_alpha  # alpha level for ribbon is automatically based on nr of levels and base_alpha
  levels = rev(levels)
  if (!is.null(fill_gradient)) {
    stopifnot(length(fill_gradient) == length(levels))
    fill_gradient = rev(fill_gradient)
  }
  for (i in seq_along(levels)) {
    params_ribbon$level = levels[[i]]
    if (!is.null(fill_gradient)) {
      params_ribbon$fill = fill_gradient[i]
    }
    layers[[i]] = ggplot2::layer(data = data, mapping = mapping,
                                 stat = stat, geom = ggplot2::GeomRibbon, position = position,
                                 show.legend = show.legend, inherit.aes = inherit.aes,
                                 params = params_ribbon)
  }
  params$fill = NULL  # line knows no fill aesthetic
  layers[[i + 1]] = ggplot2::layer(data = data, mapping = mapping,
                                   stat = stat, geom = ggplot2::GeomLine, position = position,
                                   show.legend = show.legend, inherit.aes = inherit.aes,
                                   params = params)
  layers
}



waffle_df = function(x, rows = NULL, cols = NULL) {
  # x = sort(x)
  total = length(x)
  # Specify unique x and y coord for each case
  if (is.null(rows) & is.null(cols)) {
    rows = cols = ceiling(sqrt(total))
    if ((rows * cols - total) == cols) {
      cols = cols - 1 # if we have an empty column, remove it
    }
  } else if (is.numeric(cols) & is.numeric(rows)) {
    if (total < rows * cols) {
      warning(paste0("Total ",total," smaller than number of cells (",rows,"x",cols,")"))
    }
  } else if (is.numeric(rows)) {
    cols = ceiling(total / rows)
  } else if (is.numeric(cols)) {
    rows = ceiling(total / cols)
  }
  x_pa = c(x, rep(NA, times = rows * cols - total))
  dim(x_pa) = c(cols, rows)
  xdf = reshape2::melt(x_pa)
  xdf$value = factor(xdf$value,exclude = NA)
  xdf$Var1 = xdf$Var1 - 1 # left shift all
  xdf$Var1 = xdf$Var1 + floor(xdf$Var1 / 5) * 2 / sqrt(total) # every fifth gets extra distance
  xdf$Var1 = xdf$Var1 + floor(xdf$Var1 / 10) * 1 / sqrt(total) # every tenth gets even more
  xdf$Var2 = xdf$Var2 - 1 # up shift all
  xdf$Var2 = xdf$Var2 + floor(xdf$Var2 / 5) * 2 / sqrt(total)
  xdf$Var2 = xdf$Var2 + floor(xdf$Var2 / 10) * 1 / sqrt(total)
  xdf
}

#' Waffle plot
#'
#' Pass in a a variable and get a waffle plot.
#' Useful to display simple counts or if the variable has different values,
#' a square pie chart. If the variable has a length that makes the individual squares
#' hard to see, consider showing hundreds, thousands etc.
#'
#' To avoid the Hermann grid illusion, don't use dark colours.
#'
#' @param x a variable with not too many unique values
#' @param shape defaults to a filled square
#' @param rows defaults to the rounded up square root of the number of values
#' @param cols defaults to the rounded down square root of the number of values
#' @param drop_shadow_h horizontal offset of the drop shadow, tinker with this to get a proper shadow effect
#' @param drop_shadow_v vertical offset of the drop shadow
#' @export
#' @import ggplot2
#' @examples
#' qplot_waffle(rep(1:2,each=5))
qplot_waffle = function(x, shape = 15, rows = NULL, cols = NULL, drop_shadow_h = -0.3, drop_shadow_v = 0.3) {
  xdf = waffle_df(x, rows, cols)
  total = length(x)
  types = length(unique(stats::na.omit(x)))

  miss_value = is.na(levels(xdf$value)[xdf$value])
  xdf$Var1_offset = xdf$Var1 + drop_shadow_h/sqrt(total)
  xdf$Var2_offset = xdf$Var2 + drop_shadow_v/sqrt(total)
  ggplot(xdf) +
    geom_point(aes_string(x = "Var1_offset", y = "Var2_offset"),
               colour = ifelse(miss_value, NA, "black"),
               shape = shape,
               size = round(140/sqrt(total)),
               show_legend = F) +
    geom_point(aes_string(x = "Var1", y = "Var2", colour = "value"),
               shape = shape,
               size = round(140/sqrt(total)),
               show_legend = ifelse(types>1,T,F)) +
    coord_fixed() +
    theme_minimal() +
    guides(colour = guide_legend(override.aes = list(shape = 15, size = 12) ) ) +
    ylab("")+ xlab("")+
    scale_x_continuous(expand = c(0.12, 0.12)) +
    scale_y_continuous(expand = c(0.12, 0.12), trans = "reverse") +
    theme(
      panel.background = element_rect(colour = NA),
      panel.border = element_blank(),
      strip.background = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()) +
    scale_colour_manual("",values = c("#aea96f","#a5c25c","#a3ccdc"))
}

fontawesome_square = '\uf0c8'

#' Waffle plot (text)
#'
#' Pass in a a variable and get a waffle plot.
#' Useful to display simple counts or if the variable has different values,
#' a square pie chart. If the variable has a length that makes the individual squares
#' hard to see, consider showing hundreds, thousands etc.
#'
#' This functions is like waffle_plot but it allows you to specify custom symbols
#' from FontAwesome. Copypaste them from here: http://fontawesome.io/cheatsheet
#'
#' To avoid the Hermann grid illusion, don't use dark colours.
#'
#' @param x a variable with not too many unique values
#' @param symbol pass a unicode symbol from FontAwesome here. Defaults to a square with rounded edges
#' @param rows defaults to the rounded up square root of the number of values
#' @param cols defaults to the rounded down square root of the number of values
#' @param drop_shadow_h horizontal offset of the drop shadow, tinker with this to get a proper shadow effect
#' @param drop_shadow_v vertical offset of the drop shadow
#' @param font_family defaults to FontAwesome
#' @param font_face defaults to Regular
#' @param font_size defaults to round(140/sqrt(length(x)))
#' @export
#' @import ggplot2
#' @encoding UTF-8
#' @examples
#' \dontrun{
#' qplot_waffle_text(rep(1:2,each=30), rows = 5)
#' }
qplot_waffle_text = function(x, symbol = fontawesome_square, rows = NULL, cols = NULL, drop_shadow_h = -0.9, drop_shadow_v = 0.9, font_family = "FontAwesome", font_face = "Regular",
                             font_size = round(140/sqrt(length(x)))) {
  xdf = waffle_df(x, rows, cols)
  types = length(unique(stats::na.omit(x)))

  miss_value = is.na(levels(xdf$value)[xdf$value])
  xdf$Var1_offset = xdf$Var1 + drop_shadow_h * font_size/140
  xdf$Var2_offset = xdf$Var2 + drop_shadow_v * font_size/140

  ggplot(xdf) +
    geom_point(aes_string(x = "Var1", y = "Var2", colour = "value"),size = 0,show_legend = ifelse(types>1,T,F)) +
    geom_text(aes_string(x = "Var1_offset", y = "Var2_offset"),
              colour = ifelse(miss_value, NA, "black"),
              size = font_size,label= symbol,
              show_legend = F, alpha = .3, family = font_family, face = font_face) +
    geom_text(aes_string(x = "Var1", y = "Var2", colour = "value"), show_legend = F,
              size = font_size, label= symbol, family = font_family, face = font_face) +
    coord_fixed() +
    theme_minimal() +
    guides(colour = guide_legend(override.aes = list(shape = 15, size = 12) ) ) +
    ylab("") + xlab("") +
    scale_x_continuous(expand = c(0.12, 0.12)) +
    scale_y_continuous(expand = c(0.12, 0.12), trans = "reverse") +
    theme(
      panel.background = element_rect(colour = NA),
      panel.border = element_blank(),
      strip.background = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()) +
    scale_colour_manual("",values = c("#aea96f","#a5c25c","#a3ccdc"))
}


#' Waffle plot (tile)
#'
#' Pass in a a variable and get a waffle plot.
#' Useful to display simple counts or if the variable has different values,
#' a square pie chart. If the variable has a length that makes the individual squares
#' hard to see, consider showing hundreds, thousands etc.
#'
#' This function allows and requires the least tinkering, but also does not
#' drop shadows.
#' To avoid the Hermann grid illusion, don't use dark colours.
#'
#' adapted from http://shinyapps.stat.ubc.ca/r-graph-catalog/
#' who adapted it from http://www.techques.com/question/17-17842/How-to-make-waffle-charts-in-R
#' who adapted it from http://ux.stackexchange.com/a/46543/56341
#'
#'
#' @param x a variable with not too many unique values
#' @param rows defaults to the rounded up square root of the number of values
#' @param cols defaults to the rounded down square root of the number of values
#' @export
#' @import ggplot2
#' @examples
#' qplot_waffle_tile(rep(1:2,each=500))
qplot_waffle_tile = function(x, rows = NULL, cols = NULL) {
  xdf = waffle_df(x, rows, cols)
  total = length(x)
  types = length(unique(stats::na.omit(x)))

  miss_value = is.na(levels(xdf$value)[xdf$value])
  xdf$color = "white"

  ggplot(xdf) +
    geom_tile(aes_string(x = "Var1", y = "Var2", fill = "value", color = "color"), size = 25/sqrt(total),show_legend = ifelse(types > 1, T, F)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), trans = "reverse") +
    scale_color_manual(values = "white",guide = F)+
    coord_fixed() +
    theme_minimal() +
    theme(panel.border = element_blank(),
          plot.title = element_text(size = rel(1.2), face = "bold"),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank())  +
    scale_fill_manual("",values = c("#aea96f","#a5c25c","#a3ccdc"))
}
