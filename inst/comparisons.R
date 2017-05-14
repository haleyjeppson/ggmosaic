# compare productplots to ggmosaic

library(productplots)

a.1 <- productplots::prodplot(happy, ~ happy, "hbar")
b.1 <- ggplot(data = happy) + geom_mosaic(aes(weight = wtssall, x = product(happy)), divider="hbar")
multiplot(a.1, b.1,layout=matrix(c(1:2), nrow=1, byrow=FALSE))

a.2 <- productplots::prodplot(happy, ~ happy, "hspine")
b.2 <- ggplot(data = happy) + geom_mosaic(aes(weight = wtssall, x = product(happy)), divider="hspine")
multiplot(a2, b.2,layout=matrix(c(1:2), nrow=1, byrow=FALSE))



a.3 <- prodplot(happy, ~ sex + happy, c("vspine", "hbar"))
b.3 <- ggplot(data = happy) + geom_mosaic(aes(x = product(sex, happy)), divider=c("vspine", "hbar"))
multiplot(a.3, b.3,layout=matrix(c(1:2), nrow=1, byrow=FALSE))


a.4 <- prodplot(happy, ~ happy + sex, mosaic())
b.4 <- ggplot(data = happy) + geom_mosaic(aes(weight = wtssall, x = product(happy, sex)))
multiplot(a.4, b.4,layout=matrix(c(1:2), nrow=1, byrow=FALSE))


a.5 <- prodplot(happy, ~ happy + finrela + health, mosaic())
b.5 <- ggplot(data = happy) + geom_mosaic(aes(weight = wtssall, x = product(happy, finrela, health)))
multiplot(a.5, b.5, layout=matrix(c(1:2), nrow=1, byrow=FALSE))

a.6 <- prodplot(happy, ~ happy + marital + health, mosaic())
b.6 <- ggplot(data = happy) + geom_mosaic(aes(weight = wtssall, x = product(happy, marital, health)))
multiplot(a.6, b.6, layout=matrix(c(1:2), nrow=1, byrow=FALSE))







###################################################
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

