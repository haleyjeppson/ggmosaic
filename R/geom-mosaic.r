#' Mosaic plots.
#'
#' @export
#'
#' @description
#' A mosaic plot is a convenient graphical summary of the conditional ditributions
#' in a contingency table and is composed of spines in alternating directions.
#'
#' @inheritParams ggplot2::layer
#' @param divider Divider function. The default divider function is mosaic() which will use spines in alternating directions. The four options for partioning:
#' \itemize{
#' \item \code{vspine} Vertical spine partition: width constant, height varies.
#' \item \code{hspine}  Horizontal spine partition: height constant, width varies.
#' \item \code{vbar} Vertical bar partition: height constant, width varies.
#' \item \code{hbar}  Horizontal bar partition: width constant, height varies.
#' }
#' @param offset Set the space between the first spine
#' @param na.rm If \code{FALSE} (the default), removes missing values with a warning. If \code{TRUE} silently removes missing values.
#' @param ... other arguments passed on to \code{layer}. These are often aesthetics, used to set an aesthetic to a fixed value, like \code{color = 'red'} or \code{size = 3}. They may also be parameters to the paired geom/stat.
#' @examples
#' data(Titanic)
#' titanic <- as.data.frame(Titanic)
#' titanic$Survived <- factor(titanic$Survived, levels=c("Yes", "No"))
#' ggplot(data=titanic) + geom_mosaic(aes(weight=Freq, vars=product(Class, Survived)))
#' ggplot(data=titanic) + geom_mosaic(aes(weight=Freq, vars=interaction(Class, Survived), fill=Age))
#' gg <- ggplot(data=titanic) + geom_mosaic(aes(weight=Freq, vars=interaction(Survived, Class), fill=Age))
#' gg
#' gg + geom_text(aes(x = (xmin+xmax)/2, y = (ymin+ymax)/2,
#' label=paste(paste0("Survived: ",vars1),paste0("Class: ",vars2), sep="\n")), data=subset(ggplot_build(gg)$data[[1]], level==2))
#' # doing the right thing, but we need labelling to make it less confusing
#' ggplot(data=titanic) + geom_mosaic(aes(weight=Freq, x=interaction(Class, Survived), conds = Age))
#' ggplot(data=titanic) + geom_mosaic(aes(weight=Freq, vars=Class, conds=Age, fill=Survived))
#'
#' data(happy, package="productplots")
#' ggplot(data = happy) + geom_mosaic(aes(vars=c(happy)))
#' ggplot(data = happy) + geom_mosaic(aes(weight=wtssall, vars=c(happy)))
#' ggplot(data = happy) + geom_mosaic(aes(weight=wtssall, vars=c(health), fill=happy))
#' ggplot(data = happy) + geom_mosaic(aes(weight=wtssall, vars=c(health), fill=happy), na.rm=TRUE)
#' # ggplot(data = happy) + geom_mosaic(aes(weight=wtssall, vars=interaction(health, sex, degree), fill=happy), na.rm=TRUE)
#' ggplot(data = happy) + geom_mosaic(aes(weight=wtssall, vars=product(health, sex, degree), fill=happy), na.rm=TRUE)
#'
#' # here is where a bit more control over the spacing of the bars would be helpful:
#' ggplot(data = happy) + geom_mosaic(aes(weight=wtssall, vars=c(age), fill=happy), na.rm=TRUE)
#' ggplot(data = happy) + geom_mosaic(aes(weight=wtssall, vars=c(age), fill=happy, conds = sex), na.rm=TRUE)
#' # facetting works!!!!
#' ggplot(data = happy) + geom_mosaic(aes(weight=wtssall, vars=c(age), fill=happy), na.rm=TRUE) + facet_grid(sex~.)
#'
#'# set the offet
#'ggplot(data = happy) + geom_mosaic(aes(weight = wtssall, vars = product(happy, finrela, health), group = 1))
#'ggplot(data = happy) + geom_mosaic(aes(weight = wtssall, vars = product(happy, finrela, health), group = 1), offset=.005)
#'


geom_mosaic <- function(mapping = NULL, data = NULL, stat = "mosaic",
                        position = "identity", na.rm = FALSE,  divider = mosaic(), offset = 0.01,
                        show.legend = NA, inherit.aes = FALSE, ...)
{
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMosaic,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      divider = divider,
      offset = offset,
      ...
    )
  )
}

#' @importFrom grid grobTree
GeomMosaic <- ggplot2::ggproto(
  "GeomMosaic", ggplot2::Geom,
  setup_data = function(data, params) {
    cat("setup_data in GeomMosaic\n")
    #    data$x <- 1
    #    data$y <- 1

 #   browser()
    data
  },
  required_aes = c("xmin", "xmax", "ymin", "ymax"),
  default_aes = ggplot2::aes(width = 0.75, linetype = "solid", fontsize=5,
                             shape = 19, colour = NA,
                             size = .1, fill = "grey30", alpha = .8, stroke = 0.1,
                             linewidth=.1),

  draw_panel = function(data, panel_scales, coord) {
    cat("draw_panel in GeomMosaic\n")
# browser()
    if (all(is.na(data$colour)))
      data$colour <- alpha(data$fill, data$alpha) # regard alpha in colour determination

    GeomRect$draw_panel(subset(data, level==max(data$level)), panel_scales, coord)
  },

  check_aesthetics = function(x, n) {
    ns <- vapply(x, length, numeric(1))
    good <- ns == 1L | ns == n

    if (all(good)) {
      return()
    }
    # browser()
    stop(
      "Aesthetics must be either length 1 or the same as the data (", n, "): ",
      paste(names(!good), collapse = ", "),
      call. = FALSE
    )
  },

  draw_key = ggplot2::draw_key_rect
)
