library(vcd)
library(ggplot2)
library(ggmosaic)

#############################################
data("Arthritis")
data
art <- xtabs(~ Treatment + Improved, data = Arthritis, subset = Sex == "Female")
vcd::mosaic(art, gp = shading_Friendly)
vcd::mosaic(art, gp = shading_max)

ggplot(data = Arthritis) + geom_mosaic(aes(x=product(Treatment), y=product(Improved), fill=Improved))+coord_flip()

############################################
seats <- structure(c(226, 61, 54, 51, 222), .Names = c("CDU/CSU", "FDP", "Linke", "Gruene", "SPD"))
parties <- rainbow_hcl(6, c = 60, l = 75)[c(5, 2, 6, 3, 1)]
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
ggplot(data=vote)+geom_mosaic(aes(x=product(Bundesland)))
# doesn't work because of hyphenated names
vote$Bundesland <- gsub("-", "", vote$Bundesland)
ggplot(data=vote)+geom_mosaic(aes(weight=Freq, x=product(Bundesland), y=product(Fraktion), fill=Fraktion))+coord_flip()


#############################################
data("Employment")
## Employment Status
vcd::mosaic(Employment,
       expected = ~ LayoffCause * EmploymentLength + EmploymentStatus,
       main = "Layoff*EmployLength + EmployStatus")


employment <- as.data.frame(Employment)
ggplot(data=employment)+geom_mosaic(aes(weight=Freq, x=product(EmploymentLength, EmploymentStatus)))
# doesn't work because of hyphenated dates
employment$EmploymentLength <- gsub("-", "x", employment$EmploymentLength)

ggplot(data=employment)+geom_mosaic(aes(weight=Freq, y=product(EmploymentStatus), x=product(EmploymentLength, EmploymentStatus), fill=LayoffCause), offset=0.02) +labs(x="Employment Length", y="Employment Status")


vcd::mosaic(Employment,
       expected = ~ LayoffCause * EmploymentLength + LayoffCause * EmploymentStatus,
       main = "Layoff*EmployLength + Layoff*EmployStatus")

## Closure
vcd::mosaic(Employment[,,1], main = "Layoff: Closure")
ggplot(data=employment[employment$LayoffCause=="Closure",])+geom_mosaic(aes(weight=Freq, y=product(EmploymentStatus), x=product(EmploymentLength, EmploymentStatus)), offset=0.02) + labs(x="Employment Status", y="Employment Length")+coord_flip()


vcd::mosaic(Employment[,,2], main = "Layoff: Replaced")
ggplot(data=employment[employment$LayoffCause=="Replaced",])+geom_mosaic(aes(weight=Freq, y=product(EmploymentStatus), x=product(EmploymentLength, EmploymentStatus)), offset=0.02) + labs(x="Employment Status", y="Employment Length")+coord_flip()

##################################################
data("Hospital")
vcd::mosaic(t(Hospital), shade = TRUE)


hospital <- as.data.frame(Hospital)
# doesn't work because of hyphenated dates
hospital$Length.of.stay <- gsub("-", "x", hospital$Length.of.stay)
ggplot(data=hospital)+geom_mosaic(aes(weight=Freq, y=product(Length.of.stay), x=product(Visit.frequency, Length.of.stay)), offset=0.015) +labs(x="Visit Frequency ", y="Length of stay")

hospital$Length.of.stay <- c(rep(c("2x9", "10x19", "20plus"), each=3))

vcd::mosaic(Hospital, shade = TRUE)
# something is going wrong here
ggplot(data=hospital)+geom_mosaic(aes(weight=Freq, x=product(Length.of.stay, Visit.frequency)))
+labs(x="Visit Frequency ", y="Length of stay")


######################################################
data("Titanic")
vcd::mosaic(Titanic)

titanic <- as.data.frame(Titanic)
ggplot(data=titanic)+geom_mosaic(aes(weight=Freq, x=product(Survived, Sex, Age, Class)), offset=.03)+coord_flip()

