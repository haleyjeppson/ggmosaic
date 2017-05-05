library(tidyverse)
library(forcats)
library(curl)

fly <- read_csv( curl("https://raw.githubusercontent.com/fivethirtyeight/data/master/flying-etiquette-survey/flying-etiquette.csv"))

names(fly) <- c("ID", "FlightFreq", "DoYouRecline", "Height", "Child18",
                "Seats3_2Arms", "Seats2_1Arm", "WhoControlsWindowShade",
                "RudeToMoveToUnsoldSeat", "RudeToTalkToNeighbor",
                "6hrFlightRudeToLeaveSeat", "RecliningObligationToBehind",
                "RudeToRecline", "EliminateReclining", "RudeToSwitchSeatsForFriends",
                "RudeToSwitchSeatsForFamily", "RudeToWakeNeighborForBathroom",
                "RudeToWakeNeighborForWalk", "RudeToBringBaby",
                "RudeToBringUnrulyChild", "UseElectronicsDuringTakeoff",
                "HaveYouSmoked", "Gender", "Age", "HouseholdIncome", "Education", "Region")


fly$DoYouRecline <- fct_relevel(fly$DoYouRecline, c("Always", "Usually", "About half the time", "Once in a while"))
fly$RudeToRecline <- fct_relevel(fly$RudeToRecline, c("Yes, very rude", "Yes, somewhat rude"))
fly$FlightFreq <- fct_relevel(fly$FlightFreq, c("Every day", "A few times per week" , "A few times per month","Once a month or less",  "Once a year or less" ,"Never"  ) )
levels(fly$RecliningObligationToBehind) <- c("No", "Yes")

fly$HouseholdIncome <- fct_relevel(fly$HouseholdIncome,
                                   c( "$0 - $24,999" ,"$25,000 - $49,999",
                                      "$50,000 - $99,999" ,
                                      "$100,000 - $149,999", "150000"))
levels(fly$HouseholdIncome)[5]<- "150,000 or more"

library(ggplot2)
library(ggmosaic)
library(ggthemes)
library(ggsci)
head(fly)
str(fly)



ggplot(data = fly) + geom_mosaic(aes(x=product(RudeToRecline), fill=DoYouRecline), na.rm=T) +
  scale_fill_locuszoom()+theme_hc() + theme(axis.text.y = element_blank())+
  labs(title="Rude to recline? vs. Do you recline?", y="Do you recline?", x="Rude to recline?") #+coord_flip()

mosaicplot(~RudeToRecline + DoYouRecline, data= fly, color=DoYouRecline2)
getPalette <- colorRampPalette(pal_locuszoom("default")(5))
DoYouRecline2 <- getPalette(5)
names(DoYouRecline2) <- levels(fly$DoYouRecline)
mosaic(~RudeToRecline + DoYouRecline, data= fly, gp = gpar(fill = DoYouRecline2))



ggplot(data = fly) + geom_mosaic(aes(x=product(RudeToRecline), fill=DoYouRecline), na.rm=T) +
  scale_fill_locuszoom()+theme_hc() + theme(axis.text.y = element_blank())+
  labs(title="Rude to recline? vs. Do you recline?", y="Do you recline?", x="Rude to recline?")


ggplot(data = fly) + geom_mosaic(aes(x=product(DoYouRecline), fill=RudeToRecline), na.rm=T)+
  scale_fill_locuszoom()+theme_hc() +labs(title="Rude to recline? vs. Do you recline?", x="Do you recline?", y="Rude to recline?")


ggplot(data = fly) + geom_mosaic(aes(x=product(Child18), fill=RudeToBringBaby), na.rm=T, offset=0.005) +
  scale_fill_locuszoom()+theme_hc() +
  labs(title="Is it rude to bring a baby onboard?", x="Has child under age 18", y="Rude to bring baby?")



ggplot(data = fly) + geom_mosaic(aes(x=product(RudeToRecline), fill=DoYouRecline), na.rm=T)+coord_flip()+
  scale_fill_locuszoom()+theme_hc() +
  labs(title="Rude to recline? vs. Do you recline?", y="Do you recline?", x="Rude to recline?")


ggplot(data = fly) + geom_mosaic(aes(x=product(FlightFreq), fill=RudeToTalkToNeighbor), na.rm=T) +
  guides(fill=guide_legend( reverse = TRUE)) +
  scale_fill_locuszoom()+theme_hc()

ggplot(data = fly) + geom_mosaic(aes(x=product(RudeToRecline), fill=RecliningObligationToBehind), na.rm=T) +
  labs(x="Is it rude to recline?", title="If reclining, do you have an obligation to the person behind?")   +
  scale_fill_locuszoom()+theme_hc()

ggplot(data = fly) + geom_mosaic(aes(x=product(RudeToRecline), fill=EliminateReclining), na.rm=T) +
  labs(x="Is it rude to recline?", title="Should reclining be eliminated?")   +
  scale_fill_locuszoom()+theme_hc()

ggplot(data = fly) + geom_mosaic(aes(x=product(Age), fill=RudeToBringUnrulyChild),
                                 divider=ddecker(), na.rm=T) +
  scale_fill_locuszoom()+theme_hc()

ggplot(data = fly) +
  geom_mosaic(aes(x=product(FlightFreq), fill=DoYouRecline), na.rm=T) +
  scale_fill_locuszoom()+theme_hc()

ggplot(data = fly) +
  geom_mosaic(aes(x=product(Height), fill=RudeToRecline), na.rm=T) +
  scale_fill_locuszoom()+theme_hc()


ggplot(data = fly) +
  geom_mosaic(aes(x=product(HouseholdIncome), fill=FlightFreq), na.rm=T) +
  scale_fill_locuszoom()+theme_hc()

ggplot(data = fly) +
  geom_mosaic(aes(x=product(Seats3_2Arms), fill=WhoControlsWindowShade), na.rm=T) +
  scale_fill_locuszoom()+theme_hc()
