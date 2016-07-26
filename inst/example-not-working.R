library(ggplot2)
library(ggmosaic)
library(plotly)
library(curl)
library(readr)

# works: read file with read.csv, not read_csv
# use plotly 3.6.0 (CRAN)
fly <- read.csv(curl("https://raw.githubusercontent.com/fivethirtyeight/data/master/flying-etiquette-survey/flying-etiquette.csv"), na.strings=c("", " ", NA))
ggplot(data=fly) + geom_mosaic(aes(x=product(Gender, Do.you.have.any.children.under.18.), fill=In.general..is.itrude.to.bring.a.baby.on.a.plane.), divider=ddecker()) +
  theme(axis.text.x=element_text(angle=90, vjust=.5, hjust=1))

ggplotly()

table(fly$Do.you.have.any.children.under.18.)
table(fly$In.general..is.itrude.to.bring.a.baby.on.a.plane.)

##########
# warning: file with read_csv, not sure why
# error: ggplotly duplicate data column x

fly <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/flying-etiquette-survey/flying-etiquette.csv")
ggplot(data=fly) + geom_mosaic(aes(x=product(Gender, `Do you have any children under 18?`), fill=`In general, is itrude to bring a baby on a plane?`), divider=ddecker()) +
  theme(axis.text.x=element_text(angle=90, vjust=.5, hjust=1))

ggplotly()

table(fly$`In general, is itrude to bring a baby on a plane?`)
table(fly$Gender)
table(fly$`Do you have any children under 18?`)


fly$baby <- fly$`In general, is itrude to bring a baby on a plane?`
ggplot(data=fly) + geom_mosaic(aes(x=product(Gender, `Do you have any children under 18?`), fill=baby), divider=ddecker()) +
  theme(axis.text.x=element_text(angle=90, vjust=.5, hjust=1))


fly$children <- fly$`Do you have any children under 18?`
ggplot(data=fly) + geom_mosaic(aes(x=product(Gender, children), fill=baby), divider=ddecker())
+
  theme(axis.text.x=element_text(angle=90, vjust=.5, hjust=1))

ggplotly()
get.separators()


ggplot(data=titanic) +
 geom_mosaic(aes(weight=Freq, x=product(Survived, Class), fill=Age))
