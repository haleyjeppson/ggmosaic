library(NHANES)
library(ggplot2)
library(ggmosaic)

data(NHANES)
head(NHANES)



ggplot(data = NHANES) + geom_mosaic(aes(x=product(Race1), fill=factor(Age1stBaby)),
                                    offset=0.003, na.rm=T) + coord_flip()+ labs(x="Race", y="Age1stBaby")

ggplot(data = NHANES) + geom_mosaic(aes(x=product(Age1stBaby), fill=factor(Age1stBaby)),
                                    offset=0.003, na.rm=T) + labs(x="Race", y="Age1stBaby") + facet_grid(Race1~.)

ggplot(data = NHANES) + geom_mosaic(aes(x=product(Race1), fill=Education)) + labs(x="Race", y="Education") +
  theme(axis.text.x=element_text(angle=-25, hjust=-.1))

ggplot(data = NHANES) + geom_mosaic(aes(x=product(Race1), fill=Education, conds=product(Work))) + labs(x="Race", y="Education") +
  theme(axis.text.x=element_text(angle=-25, hjust=-.1))

ggplot(data = NHANES) + geom_mosaic(aes(x=product(Education), fill=MaritalStatus, conds=product(Gender)), divider= mosaic("v"), na.rm=TRUE) +
  labs(x="Race", y="Gender:Education") +
  theme(axis.text.x=element_text(angle=-25, hjust=-.1))


ggplot(data = NHANES) + geom_mosaic(aes(x=product(Education), fill=MaritalStatus, conds=product(Gender)), divider= mosaic("h"), na.rm=TRUE) +
  labs(x="Race", y="Gender:Education") +
  theme(axis.text.x=element_text(angle=-25, hjust=-.1))

ggplot(data = NHANES) + geom_mosaic(aes(x=product(Education), fill=factor(Weight)), na.rm=T)


set.separators(c(":", ";","|"))
ggplot(data = NHANES) + geom_mosaic(aes(x=product(BMI_WHO), fill=Diabetes))
ggplot(data = NHANES) + geom_mosaic(aes(x=product(BMI_WHO), fill=HealthGen), divider=ddecker(), na.rm=TRUE)

gg <- ggplot(data = NHANES) + geom_mosaic(aes(x=product(BMI_WHO), fill=HealthGen, conds=Diabetes), divider=ddecker(), na.rm=TRUE)
ggplotly()


ggplot(data = NHANES) + geom_mosaic(aes(x=product(nPregnancies), fill=Education), na.rm=TRUE)

ggplot(data = NHANES) + geom_mosaic(aes(x=product(SexOrientation), fill=LittleInterest))
ggplot(data = NHANES) + geom_mosaic(aes(x=product(SexNumPartnLife), fill = HardDrugs), offset=0, na.rm=T)

ggplot(data = NHANES) + geom_mosaic(aes(x=product(AgeFirstMarij), fill = RegularMarij), offset=0, na.rm=T)


ggplot(data = NHANES) + geom_mosaic(aes(x=product(SleepHrsNight), fill = HealthGen), na.rm=T)
ggplot(data = NHANES) + geom_mosaic(aes(x=product(SleepHrsNight), fill = CompHrsDay), na.rm=T)
ggplot(data = NHANES) + geom_mosaic(aes(x=product(Education), fill = CompHrsDay), na.rm=T, divider=mosaic("v"))
ggplot(data = NHANES) + geom_mosaic(aes(x=product(Education, Gender), fill = CompHrsDay), na.rm=T, divider=mosaic("v"))

ggplot(data = NHANES) + geom_mosaic(aes(x=product(CompHrsDay), fill = factor(SleepHrsNight)), na.rm=T)

ggplot(data = NHANES) + geom_mosaic(aes(x=product(SleepHrsNight), fill = AgeDecade), na.rm=T)
ggplot(data = NHANES) + geom_mosaic(aes(x=product(AgeDecade), fill = factor(SleepHrsNight)), na.rm=T)
ggplot(data = NHANES) + geom_mosaic(aes(x=product(SleepHrsNight, Gender, AgeDecade), fill = factor(SleepHrsNight)), na.rm=T, divider=ddecker())







ggplot(data = NHANES) + geom_mosaic(aes(x=product(PhysActive), fill=SleepTrouble))

ggplot(data = NHANES) + geom_mosaic(aes(x=product(PhysActive), fill=Diabetes))
ggplot(data = NHANES) + geom_mosaic(aes(x=product(Race3), fill=Diabetes))
