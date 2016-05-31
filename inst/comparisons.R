# compare productplots to ggmosaic

library(productplots)

one.a <- prodplot(happy, ~ happy, "hbar")
one.b <- ggplot(data = happy) + geom_mosaic(aes(weight = wtssall, vars = happy, group = 1), divider="hbar")
multiplot(one.a, one.b,layout=matrix(c(1:2), nrow=1, byrow=FALSE))


two.a <- prodplot(happy, ~ happy, "hspine")
two.b <- ggplot(data = happy) + geom_mosaic(aes(weight = wtssall, vars = happy, group = 1), divider="hspine")
multiplot(two.a, two.b,layout=matrix(c(1:2), nrow=1, byrow=FALSE))

a.3 <- prodplot(happy, ~ sex + happy, c("vspine", "hbar"))
b.3 <- ggplot(data = happy) + geom_mosaic(aes(weight = wtssall, vars = product(sex, happy), group = 1), divider=c("vspine", "hbar"))
multiplot(a.3, b.3,layout=matrix(c(1:2), nrow=1, byrow=FALSE))


a.4 <- prodplot(happy, ~ happy + sex, mosaic())
b.4 <- ggplot(data = happy) + geom_mosaic(aes(weight = wtssall, vars = product(happy, sex), group = 1))
multiplot(a.4, b.4,layout=matrix(c(1:2), nrow=1, byrow=FALSE))


a.5 <- prodplot(happy, ~ happy + finrela + health, mosaic())
b.5 <- ggplot(data = happy) + geom_mosaic(aes(weight = wtssall, vars = product(happy, finrela, sex), group = 1))
multiplot(a.5, b.5, layout=matrix(c(1:2), nrow=1, byrow=FALSE))

a.6 <- prodplot(happy, ~ happy + marital + health, mosaic())
b.6 <- ggplot(data = happy) + geom_mosaic(aes(weight = wtssall, vars = product(happy, marital, sex), group = 1))
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

