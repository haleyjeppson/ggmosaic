#' Mosaic plots.
#'
#' @export
#'
#' @description
#' A mosaic plot is a convenient graphical summary of the conditional ditributions
#' in a contingency table and is composed of spines in alternating directions.
#'
#' XXX fill at the moment is always the last variable - even if the same variable shows up ... we should respect the order when the variable is explicitly listed in the formula
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
#' @param label If \code{TRUE} include labels for all rectangles
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
#' ggplot(data=titanic) +
#'   geom_mosaic(aes(weight=Freq, x=product(Class), conds=product(Age), fill=Survived))
#' gg <- ggplot(data=titanic) +
#'         geom_mosaic(aes(weight=Freq, x=product(Survived, Class), fill=Age))
#' gg
#'
#' # we should try to get something along these lines to work. Maybe write a labelling
#' # function?
#' gg + geom_text(aes(x = (xmin+xmax)/2, y = (ymin+ymax)/2,
#'                     label=paste(x1, x2, sep="\n")),
#'                data=subset(ggplot_build(gg)$data[[1]], level==2))
#' ggplot(data=titanic) +
#'         geom_mosaic(aes(weight=Freq, x=product(Survived, Class), fill=Age), label = TRUE)
#'
#'
#' # doing the right thing, but we need labelling to make it less confusing
#'
#'
#' ggplot(data=titanic) +
#'   geom_mosaic(aes(weight=Freq, x=product(Survived, Class),
#'                   conds = Age))
#' ggplot(data=titanic) + geom_mosaic(aes(weight=Freq, x=product(Class, Age),
#'                                        fill=Survived))
#'
#' data(happy, package="productplots")
#'
#' ggplot(data = happy) + geom_mosaic(aes(x=product(happy)))
#' ggplot(data = happy) + geom_mosaic(aes(x=product(happy)), divider=mosaic("h"))
#' ggplot(data = happy) + geom_mosaic(aes(x=product(happy)), divider=mosaic("h")) +
#'   coord_flip()
#' # weighting is important
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight=wtssall, x=product(happy)), divider=mosaic("h"))
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
#'   scale_x_product("Age", labels=c(17+1:72, "NA"))
#' # thin out labels manually:
#' labels <- c(17+1:72, NA)
#' labels[labels %% 5 != 0] <- ""
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight=wtssall, x=product(age), fill=happy), na.rm=TRUE, offset=0) +
#'   scale_x_product("Age", labels=labels)
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight=wtssall, x=product(age), fill=happy, conds = sex),
#'   divider=mosaic("v"), na.rm=TRUE, offset=0.001) +
#'   scale_x_product("Age", labels=labels)
#' # facetting works!!!!
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight=wtssall, x=product(age), fill=happy), na.rm=TRUE, offset = 0) +
#'   facet_grid(sex~.) +
#'   scale_x_product("Age", labels=labels)
#'
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight = wtssall, x = product(happy, finrela, health)),
#'   divider=mosaic("h"))
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight = wtssall, x = product(happy, finrela, health)), offset=.005)
#'
#' data(rochdale)
#' ggplot(data=rochdale) +
#'   geom_mosaic(aes(x=product(Husband.sEduc, Child), fill=Household),
#'               divider=ddecker(), na.rm=FALSE) + coord_flip()
#' ggplot(data=rochdale) +
#'   geom_mosaic(aes(x=product(Wife.sEduc, Child), fill=Household),
#'               divider=ddecker(), na.rm=FALSE) + coord_flip()
#' ggplot(data=rochdale) +
#'   geom_mosaic(aes(x=product(Wife.sEduc,Husband.sEduc, Child), fill=Household),
#'               divider=ddecker(), na.rm=FALSE) + coord_flip()
#'
#' # Spine example
#' ggplot(data = happy) +
#'  geom_mosaic(aes(weight = wtssall, x = product(health), fill = health)) +
#'  facet_grid(happy~.)
#'
#' # Working on using other characters as the separators:
#'    ## current issue is that it has to be changed in two different places
#'
#' employment <- as.data.frame(Employment)
#' ggplot(data = employment) +
#'  geom_mosaic(aes(weight=Freq, x=product(EmploymentLength, EmploymentStatus,
#'        separators = c(":", "_","."))), separators = c(":", "_","."))

geom_mosaic <- function(mapping = NULL, data = NULL, stat = "mosaic",
                        position = "identity", na.rm = FALSE,  divider = mosaic(), offset = 0.01,
                        show.legend = NA, inherit.aes = FALSE, label = FALSE,
                        separators = c(":", "-", "."), ...)
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
      separators = separators,
      label = label,
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
                             linewidth=.1),

  draw_panel = function(data, panel_scales, coord, label = FALSE) {
  #  cat("draw_panel in GeomMosaic\n")
 # browser()
    if (all(is.na(data$colour)))
      data$colour <- alpha(data$fill, data$alpha) # regard alpha in colour determination

    # label_grob <- NULL
    # if (label){
    #   text <- data
    #   text$x <- (text$xmin + text$xmax)/2
    #   text$y <- (text$ymin + text$ymax)/2
    #   text$label <- apply(data[grep("^[x]{1,}[0-9]", names(data))],1,paste, collapse = "\n") # need to come up with solution for having two lines of text
    #   text$colour = "black"
    #   text$size = 3.88
    #   text$angle = 0
    #   text$hjust = 0.5
    #   text$vjust = 0.5
    #   text$alpha = NA
    #   text$family = ""
    #   text$fontface = 1
    #   text$lineheight = 1.2
    #
    # text <- subset(text, level==max(text$level))
    # label_grob <- GeomText$draw_panel(text, panel_scales, coord, parse = FALSE, na.rm = FALSE, check_overlap = FALSE)
    # }




#    ggplot2:::ggname("geom_mosaic", grobTree(
      GeomRect$draw_panel(subset(data, level==max(data$level)), panel_scales, coord) #,
#      label_grob
#    ))

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
