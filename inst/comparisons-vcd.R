library(vcd)
library(ggplot2)
library(ggmosaic)

#############################################
data("Arthritis")
art <- xtabs(~ Treatment + Improved, data = Arthritis, subset = Sex == "Female")
vcd::mosaic(art, gp = shading_Friendly)
vcd::mosaic(art, gp = shading_max)

ggplot(data = Arthritis) +
  geom_mosaic(aes(x=product(Treatment), y=product(Improved), fill=Improved))  +
  labs(x = "Treatment", y = "Improvement") +
  coord_flip()

### levels of factors show up differently

############################################
seats <- structure(c(226, 61, 54, 51, 222), .Names = c("CDU/CSU", "FDP", "Linke", "Gruene", "SPD"))
parties <- colorspace::rainbow_hcl(6, c = 60, l = 75)[c(5, 2, 6, 3, 1)]
names(parties) <- names(seats)
data("Bundestag2005")
votes <- Bundestag2005[c(1, 3:5, 9, 11, 13:16, 2, 6:8, 10, 12),
                       c("CDU/CSU", "FDP", "SPD", "Gruene", "Linke")]
vcd::mosaic(votes, gp = gpar(fill = parties[colnames(votes)]),
       spacing = spacing_highlighting, labeling = labeling_left,
       labeling_args = list(rot_labels = c(0, 90, 0, 0), pos_labels = "center",
                            just_labels = c("center","center","center","right"), varnames = FALSE),
       margins = unit(c(2.5, 1, 1, 12), "lines"),
       keep_aspect_ratio = FALSE)



vote <- as.data.frame(votes)
# ggplot(data=vote)+geom_mosaic(aes(x=product(Bundesland)))

ggplot(data=vote) +
  geom_mosaic(aes(weight=Freq, x=product(Bundesland, separators = c(":", "_",".")), fill=Fraktion), separators = c(":", "_","."))


### doesn't work initially because of hyphenated names.

vote$Bundesland <- gsub("-", "", vote$Bundesland)
ggplot(data=vote) +
  geom_mosaic(aes(weight=Freq, x=product(Bundesland), y=product(Fraktion), fill=Fraktion)) +
  labs(x="Bundesland", y="Fraktion") + coord_flip() +
  theme(axis.text.x=element_text(angle=45, hjust=1))


#############################################
data("Employment")
vcd::mosaic(Employment,
       expected = ~ LayoffCause * EmploymentLength + EmploymentStatus,
       main = "Layoff*EmployLength + EmployStatus")


employment <- as.data.frame(Employment)
ggplot(data=employment) +
  geom_mosaic(aes(weight=Freq, x=product(EmploymentLength, EmploymentStatus, separators = c(":", "_","."))),
              separators = c(":", "_","."))
## doesn't work because of hyphenated dates
employment$EmploymentLength <- gsub("-", "to", employment$EmploymentLength)

ggplot(data=employment)+
  geom_mosaic(aes(weight=Freq, x=product(EmploymentLength, EmploymentStatus), y=product(EmploymentStatus), fill=LayoffCause), offset=0.02) +
  labs(x="Employment Length", y="Employment Status")+
  theme(axis.text.x=element_text(angle=35, hjust=1))



## Closure
vcd::mosaic(Employment[,,1], main = "Layoff: Closure")
ggplot(data=employment[employment$LayoffCause=="Closure",]) +
  geom_mosaic(aes(weight=Freq, y=product(EmploymentStatus), x=product(EmploymentLength, EmploymentStatus)), offset=0.02) +
  labs(x="Employment Status", y="Employment Length") +
  coord_flip()


vcd::mosaic(Employment[,,2], main = "Layoff: Replaced")
ggplot(data=employment[employment$LayoffCause=="Replaced",]) +
  geom_mosaic(aes(weight=Freq, y=product(EmploymentStatus), x=product(EmploymentLength, EmploymentStatus)), offset=0.02) +
  labs(x="Employment Status", y="Employment Length") +
  coord_flip() +
  theme(axis.text.x=element_text(angle=35, hjust=1))

##################################################
data("Hospital")
vcd::mosaic(t(Hospital), shade = TRUE)


hospital <- as.data.frame(Hospital)
# doesn't work because of hyphenated dates
hospital$Length.of.stay <- gsub("-", "x", hospital$Length.of.stay)
ggplot(data=hospital) +
  geom_mosaic(aes(weight=Freq, y=product(Length.of.stay), x=product(Visit.frequency, Length.of.stay)), offset=0.015) +
  labs(x="Visit Frequency ", y="Length of stay")
### I think there is an issue here because the variable names have periods in them

hospital$Length.of.stay <- c(rep(c("2x9", "10x19", "20plus"), each=3))

vcd::mosaic(Hospital, shade = TRUE)
# something is going wrong here
# variable names have "." in them
names(hospital) <- c("VisitFrequency", "LengthOfStay", "Freq")
ggplot(data=hospital) +
  geom_mosaic(aes(weight=Freq, x=product(LengthOfStay, VisitFrequency), y=product(LengthOfStay))) +
  labs(x="Visit Frequency ", y="Length of stay") +
  theme(axis.text.x=element_text(angle=-10, hjust=.5, vjust = .3)) +
  coord_flip()


######################################################
data("Titanic")
vcd::mosaic(Titanic)

titanic <- as.data.frame(Titanic)
ggplot(data=titanic) +
  geom_mosaic(aes(weight=Freq, x=product(Sex, Age, Class), y=product(Age), fill=Survived), offset=.03) +
  labs(x="Class, Sex", y="Age") +
  coord_flip()

######################################################
data("PreSex")
vcd::mosaic(~ PremaritalSex + ExtramaritalSex | Gender + MaritalStatus,
       data = PreSex, labeling = labeling_conditional)

presex <- as.data.frame(PreSex)
ggplot(data = presex) +
  geom_mosaic(aes(weight=Freq, x=product(PremaritalSex, ExtramaritalSex), y=product(Gender),
                  conds=product(Gender, MaritalStatus)), offset = 0.03) +
  labs(x="Marital Status, Extramarital Sex", y="Gender, Premarital Sex") +
  theme(axis.text.x=element_text(angle=15, hjust=1))


########################################################
data("HairEyeColor")
vcd::mosaic(HairEyeColor, shade = TRUE)
## Independence model of hair and eye color and sex. Indicates that there are significantly more blue eyed blond females than expected
## in the case of independence (and too few brown eyed blond females).
hairEyeColor <- as.data.frame(HairEyeColor)
ggplot(data = hairEyeColor) +
  geom_mosaic(aes(weight=Freq, x=product(Eye, Hair), y=product(Hair), fill = Sex), offset = 0.03) +
  labs(x="Eye color", y="Hair color")


## Model of joint independence of sex from hair and eye color. Males
## are underrepresented among people with brown hair and eyes, and are
## overrepresented among people with brown hair and blue eyes, but not "significantly".
## Formula interface for raw data: visualize crosstabulation of numbers of gears and carburettors in Motor Trend car data.
data("mtcars")
vcd::mosaic(~ gear + carb, data = mtcars, shade = TRUE)

cars <- as.data.frame(mtcars)
ggplot(data = cars) +
  geom_mosaic(aes(x=product(gear, carb), y=product(gear))) +
  labs(x="Carb", y="Gear")



#################################################################
## Highlighting:
vcd::mosaic(Survived ~ ., data = Titanic)
ggplot(data=titanic) +
  geom_mosaic(aes(weight=Freq, x=product(Age, Sex, Class),y=product(Sex), fill=Survived), offset=.03) +
  labs(x="Class, Age", y="Sex") +
  coord_flip()


#################################################################
data("Arthritis")
vcd::mosaic(Improved ~ Treatment | Sex, data = Arthritis, zero_size = 0)
ggplot(data=Arthritis) +
  geom_mosaic(aes(x=product(Treatment, Sex),y=product(Sex), fill=Improved), offset=.03) +
  labs(x="Treatment", y="Sex")


vcd::mosaic(Improved ~ Treatment | Sex, data = Arthritis, zero_size = 0, highlighting_direction = "right")
ggplot(data=Arthritis) +
  geom_mosaic(aes(x=product(Treatment, Sex), y=product(Sex), fill=Improved), offset=.03,
              divider=c("hspine", "hspine", "vspine")) +
  labs(x="Treatment", y="Sex")   +
  theme(axis.text.x=element_text(angle=15, hjust=1))


#################################################################
data("OvaryCancer")

tab <- xtabs(Freq ~ xray + survival + stage + operation, data = OvaryCancer)
ftable(tab, col.vars = "survival", row.vars = c("stage", "operation", "xray"))
## model: ~ xray * operation * stage + survival * stage
## interpretation: treat xray, operation, stage as fixed margins,
## the survival depends on stage, but not xray and operation.
doubledecker(survival ~ stage + operation + xray, data = tab)
cancer <- as.data.frame(OvaryCancer)
ggplot(data = cancer) +
  geom_mosaic(aes(weight=Freq, x=product(xray, operation, stage), fill=survival),divider=ddecker(), offset=0.05) +
  theme(axis.text.x=element_text(angle=25, hjust = 1)) +
  labs(x="Stage, Operation, xray", y="Survival")


vcd::mosaic(~ stage + operation + xray + survival,
       split = c(FALSE, TRUE, TRUE, FALSE), data = tab, keep = FALSE,
       gp = gpar(fill = rev(grey.colors(2))))

ggplot(data = cancer) +
  geom_mosaic(aes(weight=Freq, x=product(xray, operation, stage), y=product(stage), fill=survival),
              divider=c("vspine", "hspine", "hspine", "vspine"), offset=0.05) +
  theme(axis.text.x=element_text(angle=15, hjust=1)) +
  labs(x="Operation, xray", y="Stage, Survival")



########################################################################
## 4 way tables
data(Punishment, package = "vcd")
vcd::mosaic(attitude ~ age + education + memory, data = Punishment,
       highlighting_direction="left", rep = c(attitude = FALSE))
punish <- as.data.frame(Punishment)
# doesn't work because of hyphenated dates
punish$age <- gsub("-", "to", punish$age)
ggplot(data = punish) +
  geom_mosaic(aes(weight=Freq, x=product(attitude, education, age), y=product(age), fill=memory), divider=mosaic("h"), offset=0.05)+
  theme(axis.text.x=element_text(angle=15, hjust=1)) +
  labs(x="Education, Memory", y="Age, Attitude")

##############################################################
## (Gender Pre)
vcd::mosaic(margin.table(PreSex, c(3,4)), main = "Gender and Premarital Sex")

ggplot(data = presex) +
  geom_mosaic(aes(weight=Freq, x=product(Gender, PremaritalSex), y=product(PremaritalSex)), divider=mosaic("h")) +
  labs(x="Gender", y="Premarital Sex", title="Gender and Premarital Sex")


## (Gender Pre)(Extra)
vcd::mosaic(margin.table(PreSex, c(2,3,4)),
       expected = ~Gender * PremaritalSex + ExtramaritalSex ,
       main = "PreMaritalSex*Gender +Sex")

ggplot(data = presex) +
  geom_mosaic(aes(weight=Freq, x=product(PremaritalSex, ExtramaritalSex), y=product(ExtramaritalSex), fill = Gender), offset=.02) +
  labs(x="Premarital Sex", y="Extramarital Sex") +
  theme(axis.text.x=element_text(angle=0, hjust=.5))


###################################################################

data("RepVict")
vcd::mosaic(RepVict[-c(4,7),-c(4,7)], gp = shading_max,
       main = "Repeat Victimization Data")
repvict <- as.data.frame(RepVict[-c(4,7),-c(4,7)])

####
## doesn't work initially
names(repvict) <- c("FirstVictimization", "SecondVictimization", "Freq")
punish$age <- gsub("-", "to", punish$age)


ggplot(data = repvict) +
  geom_mosaic(aes(weight=Freq, x=product(SecondVictimization, FirstVictimization), y=product(SecondVictimization))) +
  labs(x="First Victimization", y="Second Victimization") +
  theme(axis.text.x=element_text(angle=15, hjust=1)) +
  coord_flip()

#########################################################################

data("Rochdale")
vcd::mosaic(Rochdale)
data(rochdale)

 ggplot(data=rochdale) +
   geom_mosaic(aes(x=product(Husband.sEduc, Child), fill=Household),
               divider=ddecker(), na.rm=FALSE) + coord_flip()
 ggplot(data=rochdale) +
   geom_mosaic(aes(x=product(Wife.sEduc, Child), fill=Household),
               divider=ddecker(), na.rm=FALSE) + coord_flip()
 ggplot(data=rochdale) +
   geom_mosaic(aes(x=product(Wife.sEduc,Husband.sEduc, Child), fill=Household),
               divider=ddecker(), na.rm=FALSE) + coord_flip()
 ###########################################

 ## load Arthritis data
 data("Arthritis")
 art <- xtabs(~Treatment + Improved, data = Arthritis)
 ## plain mosaic display without shading
 vcd::mosaic(art)
 ggplot(data=Arthritis) +
   geom_mosaic(aes(x=product(Improved, Treatment),y=product(Improved))) +
   labs(x="Treatment", y="Improvement") +
   coord_flip()


  ## Marimekko Chart
hec <- margin.table(HairEyeColor, 1:2)
vcd::mosaic(hec, gp = shading_Marimekko(hec))

HEC <- as.data.frame(hec)
ggplot(data = HEC) +
  geom_mosaic(aes(weight=Freq, x=product(Hair), y=product(Eye), fill=Eye), offset=0.02, divider=mosaic("h")) +
  labs(x="Eye Color", y="Hair Color")


 vcd::mosaic(HairEyeColor, gp = shading_Marimekko(HairEyeColor))
 haireyecolor <- as.data.frame(HairEyeColor)

 ggplot(data = haireyecolor) +
   geom_mosaic(aes(weight=Freq, x=product(Sex, Eye, Hair), y=product(Eye), fill=Eye), offset=0.02) +
   theme(axis.text.x=element_text(angle=-25, hjust=-.1)) +
   labs(x="Eye color", y="Hair color, Sex") +
   coord_flip()
## labeling breaks here


 ## Diagonal cells shading
 ac <- xtabs(VisualAcuity)
 vcd::mosaic(ac, gp = shading_diagonal(ac))

 AC <- as.data.frame(ac)
 ggplot(data = AC) +
   geom_mosaic(aes(weight=Freq, x=product(gender, left, right), y=product(left), fill=left), offset=0.03) +
   theme(axis.text.x=element_text(angle=-25, hjust=-.1)) +
   labs(y="Left", x="Right, Sex") +
   coord_flip()

 ##############################################

 data("UKSoccer")
 vcd::mosaic(UKSoccer, gp = shading_max, main = "UK Soccer Scores")

uksoccer <- as.data.frame(UKSoccer)


ggplot(data = uksoccer) +
  geom_mosaic(aes(weight=Freq, x=product(Away, Home), y=product(Away)))+
  labs(x="Home", y="Away") +
  coord_flip()

