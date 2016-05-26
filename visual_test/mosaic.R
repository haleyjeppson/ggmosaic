vcontext("mosaic")

data(Titanic)
titanic <- as.data.frame(Titanic)


ggplot(data=titanic) + geom_mosaic(aes(weight=Freq, vars=interaction(Class, Survived), group=1))
save_vtest("titanic: class and survived")

ggplot(data=titanic) + geom_mosaic(aes(weight=Freq, vars=interaction(Class, Survived), group=1, fill=Age))
save_vtest("titanic: class and survived with age colored")


ggplot(data=titanic) + geom_mosaic(aes(weight=Freq, vars=interaction(Class, Survived), conds = Age, group=1))
save_vtest("titanic: class and survived conditioned on age")


ggplot(data=titanic) + geom_mosaic(aes(weight=Freq, vars=Class, group=1, conds=Age, fill=Survived))
save_vtest("titanic: class conditioned on age and survived colored")

data(happy, package="productplots")
ggplot(data = happy) + geom_mosaic(aes(vars=c(happy), group=1))
save_vtest("happy: happy with no weight provided")


ggplot(data = happy) + geom_mosaic(aes(weight=wtssall, vars=c(happy), group=1))
save_vtest("happy: happy")

ggplot(data = happy) + geom_mosaic(aes(weight=wtssall, vars=c(health), fill=happy, group=1))
save_vtest("happy: health with happiness colored")

ggplot(data = happy) + geom_mosaic(aes(weight=wtssall, vars=c(health), fill=happy, group=1), na.rm=TRUE)
save_vtest("happy: health with happiness colored and NAs removed")

# here is where a bit more control over the spacing of the bars would be helpful:
ggplot(data = happy) + geom_mosaic(aes(weight=wtssall, vars=c(age), fill=happy, group=1), na.rm=TRUE)
save_vtest("happy: age with happiness colored")


ggplot(data = happy) + geom_mosaic(aes(weight=wtssall, vars=c(age), fill=happy, conds = sex, group=1), na.rm=TRUE)
save_vtest("happy: age conditioned on sex with happiness colored")

# facetting works!!!!
ggplot(data = happy) + geom_mosaic(aes(weight=wtssall, vars=c(age), fill=happy, group=1), na.rm=TRUE) + facet_grid(sex~.)
save_vtest("happy: age with happiness colored and facetting for sex")



end_vcontext()
