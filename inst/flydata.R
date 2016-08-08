library(curl)

fly <- read.csv( curl("https://raw.githubusercontent.com/fivethirtyeight/data/master/flying-etiquette-survey/flying-etiquette.csv") ,
                 na.strings=c(""," ","NA"))

names(fly) <- c("ID", "FlightFreq", "DoYouRecline", "Height", "Child18",
                "3SeatsWhoUses2Arms", "2SeatsWhoUses1Arm", "WhoControlsWindowShade",
                "RudeToMoveToUnsoldSeat", "RudeToTalkToNeighbor",
                "6hrFlightRudeToLeaveSeat", "RecliningObligationToBehind",
                "RudeToRecline", "EliminateReclining", "RudeToSwitchSeatsForFriends",
                "RudeToSwitchSeatsForFamily", "RudeToWakeNeighborForBathroom",
                "RudeToWakeNeighborForWalk", "RudeToBringBaby",
                "RudeToBringUnrulyChild", "UseElectronicsDuringTakeoff",
                "HaveYouSmoked", "Gender", "Age", "HouseholdIncome", "Education", "Region")


set.separators(c(":", ";","|"))
ggplot(data = fly) + geom_mosaic(aes(x=product(DoYouRecline)))
ggplot(data = fly) + geom_mosaic(aes(x=product(Height), fill=RudeToRecline), offset=0.005, na.rm=T)

ggplot(data = fly) + geom_mosaic(aes(x=product(Gender, Child18), fill=RudeToBringBaby), offset=0.005)

ggplot(data = fly) + geom_mosaic(aes(x=product(RudeToRecline), fill=DoYouRecline), na.rm=T)

ggplot(data = fly) + geom_mosaic(aes(x=product(DoYouRecline), fill=RudeToRecline), na.rm=T)
