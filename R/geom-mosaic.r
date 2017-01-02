#' Mosaic plots.
#'
#' @export
#'
#' @description
#' A mosaic plot is a convenient graphical summary of the conditional ditributions
#' in a contingency table and is composed of spines in alternating directions.
#'
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
#'
#' data(Titanic)
#' titanic <- as.data.frame(Titanic)
#' titanic$Survived <- factor(titanic$Survived, levels=c("Yes", "No"))
#'
#'
#' ggplot(data=titanic) +
#'   geom_mosaic(aes(weight=Freq, x=product(Class), fill=Survived))
#' # good practice: use the 'dependent' variable (or most important variable)
#' # as fill variable
#' ggplot(data=titanic) +
#'   geom_mosaic(aes(weight=Freq, x=product(Class, Age), fill=Survived))
#'
#' # we can change where we define variables
#' ggplot(data=titanic, aes(weight = Freq, fill=Survived, x=product(Class, Age))) +
#'   geom_mosaic()
#'
#' ggplot(data=titanic) +
#'   geom_mosaic(aes(weight=Freq, x=product(Class), conds=product(Age), fill=Survived))
#' ggplot(data=titanic) +
#'   geom_mosaic(aes(weight=Freq, x=product(Survived, Class), fill=Age))
#'
#' \dontrun{
#' data(happy, package="productplots")
#'
#' ggplot(data = happy) + geom_mosaic(aes(x=product(happy)), divider="hbar")
#' ggplot(data = happy) + geom_mosaic(aes(x=product(happy))) +
#'   coord_flip()
#' # weighting is important
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight=wtssall, x=product(happy)))
#' ggplot(data = happy) + geom_mosaic(aes(weight=wtssall, x=product(health), fill=happy)) +
#'   theme(axis.text.x=element_text(angle=35))
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight=wtssall, x=product(health), fill=happy), na.rm=TRUE)
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight=wtssall, x=product(health, sex, degree), fill=happy),
#'   na.rm=TRUE)
#'
#' # here is where a bit more control over the spacing of the bars is helpful:
#' # set labels manually:
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight=wtssall, x=product(age), fill=happy), na.rm=TRUE, offset=0) +
#'   scale_x_productlist("Age", labels=c(17+1:72))
#' # thin out labels manually:
#' labels <- c(17+1:72)
#' labels[labels %% 5 != 0] <- ""
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight=wtssall, x=product(age), fill=happy), na.rm=TRUE, offset=0) +
#'   scale_x_productlist("Age", labels=labels)
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight=wtssall, x=product(age), fill=happy, conds = sex),
#'   divider=mosaic("v"), na.rm=TRUE, offset=0.001) +
#'   scale_x_productlist("Age", labels=labels)
#' # facetting works!!!!
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight=wtssall, x=product(age), fill=happy), na.rm=TRUE, offset = 0) +
#'   facet_grid(sex~.) +
#'   scale_x_productlist("Age", labels=labels)
#'
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight = wtssall, x = product(happy, finrela, health)),
#'   divider=mosaic("h"))
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight = wtssall, x = product(happy, finrela, health)), offset=.005)
#'
#' # Spine example
#' ggplot(data = happy) +
#'  geom_mosaic(aes(weight = wtssall, x = product(health), fill = health)) +
#'  facet_grid(happy~.)
#' }


geom_mosaic <- function(mapping = NULL, data = NULL, stat = "mosaic",
                        position = "identity", na.rm = FALSE,  divider = mosaic(), offset = 0.01,
                        show.legend = NA, inherit.aes = TRUE, ...)
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
  #  cat("setup_data in GeomMosaic\n")

    #   browser()
    data
  },
  required_aes = c("xmin", "xmax", "ymin", "ymax"),
  default_aes = ggplot2::aes(width = 0.75, linetype = "solid", fontsize=5,
                             shape = 19, colour = NA,
                             size = .1, fill = "grey30", alpha = .8, stroke = 0.1,
                             linewidth=.1, weight = 1, x = NULL, y = NULL, conds = NULL),

  draw_panel = function(data, panel_scales, coord) {
  #  cat("draw_panel in GeomMosaic\n")
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
