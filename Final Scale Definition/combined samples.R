## Combining data sources for 20-to-18

######################################################################################################
######################################################################################################
######################################################################################################

## Cleaning Qualtrics construct validation data - 10/14/21

newdata.att <- read.csv("..//Engagement-Survey-Projects//Qualtrics//Engagement+(Attitudinal)_October+12,+2021_08.02.csv"); names <- newdata.att[1,c(1:92)] #[47:66]
newdata.sub <- read.csv("..//Engagement-Survey-Projects//Qualtrics//Engagement+(Substantive)_October+12,+2021_08.01.csv")#[47:66]

newdata.att <- newdata.att[-c(1:2),]                      ## peskies
newdata.sub <- newdata.sub[-c(1:2),]

write.csv(newdata.att, "changingnumerica.csv")
newdata.att <- read.csv("changingnumerica.csv")            ## that worked!! 
write.csv(newdata.sub, "changingnumerics.csv")
newdata.sub <- read.csv("changingnumerics.csv") 

library(careless)
newdata.att$careless_long <- longstring(newdata.att[20:83])
newdata.sub$careless_long <- longstring(newdata.sub[20:83])
newdata.att$irv <- irv(newdata.att[48:51])
newdata.sub$irv <- irv(newdata.sub[55:58])

newdata.att$irv2 <- irv(newdata.att[25:28])   ## another reverse-score
newdata.sub$irv2 <- irv(newdata.sub[25:28])

## newdata.att$missing <- rowSums(is.na(newdata.att[20:83])) 
## newdata.sub$missing <- rowSums(is.na(newdata.sub[20:83])) 
descr::freq(newdata.att$irv2)
descr::freq(newdata.sub$irv2)

## need filter for non-differentiating reverse-score people

newdata.att$flag <- "use"; newdata.sub$flag <- "use"         ## need to do because subset on lines 44/45 can't != "Flagged"
## upped time to 300 seconds as data still looked bad with 200

newdata.att$flag[newdata.att$careless_long > 12 | newdata.att$Duration..in.seconds. < 300 | newdata.att$irv == 0 | newdata.att$irv2 == 0]  <- "Flagged"
newdata.sub$flag[newdata.sub$careless_long > 12 | newdata.sub$Duration..in.seconds. < 300 | newdata.sub$irv == 0 | newdata.sub$irv2 == 0]  <- "Flagged"

write.csv(newdata.att, "attitudeset.csv")
write.csv(newdata.sub, "substantiveset.csv")

newdata.att$flag <- as.factor(newdata.att$flag)
newdata.sub$flag <- as.factor(newdata.sub$flag)

descr::freq(newdata.att$flag)
descr::freq(newdata.sub$flag)

useatt <- newdata.att[ which(newdata.att$flag == "use"), c(1:92)]
usesub <- newdata.sub[ which(newdata.sub$flag == "use"), c(1:92)]

useseb.sorted <- usesub[,c(1:47, 48:50,55:56,61:63,51:52,57:58,64:65,53:54,59:60,66:67,68:92)]
names(useseb.sorted) <- names(useatt)           ##### focal engagement is 48:67

qualtrics <- as.data.frame(rbind(useseb.sorted,useatt))      ## 377 out of 743 (51% retained)
                                                             ## engagement = 48-67
qualtrics.engage <- qualtrics[,c(48:67)]

qualtrics.engage <- data.frame(lapply(qualtrics.engage, function(x) as.numeric(as.character(x))))


psych::alpha(qualtrics.engage[1:8])
psych::alpha(qualtrics.engage[9:14])
psych::alpha(qualtrics.engage[15:20])

################################################################################
################################################################################
################################################################################
################################################################################

## Prolific:    

## European - no counterbalancing. Only included engagement scale as DV for bigger project looking at Job Demands and Resources - 8/2/22


temp <- read.csv("..//Engagement-Survey-Projects//prolific data//initial_data_screen.csv", header=FALSE, na.strings="")   ## NOTE: 404 vars (9/28/21) make sure to check indexing used throughout script

x <- paste("item", sep="",1:404)
y <- t(temp[2,])
## decluttering Qualtrics excess
data <- temp[-c(1:3),]                                           ## Getting rid of all 3 weird Qualtrics rows
colnames(data) <- x

incomplete <- read.csv("..//Engagement-Survey-Projects//prolific data//inprogress.csv", header=FALSE, na.strings="")   ## NOTE: 404 vars (9/28/21) make sure to check indexing used throughout script

data2 <- incomplete[-c(1:2),-c(10,12,16,113,199,285,371,379,400, 406:411)]                                          

data3 <- as.data.frame(cbind(data2$V8,data2$V9,data2$V1,data2$V1,data2$V1,data2$V1,data2$V1,data2$V1,data2))

colnames(data3) <- x

rm(x, y, temp)       

use <- rbind(data,data3)

## STILL NEED FILE WITH BAD RESPONDENTS IDENTIFIED (E.G., Item_18=2, na > 200, longstring > 20, CARELESS CHECKS > 1)

library(careless)
use$careless_long <- longstring(use[18:399])
use$missing <- rowSums(is.na(use[18:404])) 

use$flag <- NA

use$flag[use$item18 == 2 | use$careless_long > 20 | use$missing > 200] <- "Flagged"
use$flag[use$item61 == 5 & use$item145 == 5 & use$item248 == 2 & use$item308 == 3] <- "Good"

use2 <- use[which(use$flag == "Good"), ]
## descr::freq(use2$flag)                         ## no overlap - all "good" unflagged


prolific <- use2[,c(380:382,387:388,393:395,
                    383:384,389:390,396:397,
                    385:386,391:392,398:399)]     ## 568 out of 785 (72% retained)

names(prolific) <- names(qualtrics.engage)
prolific <- data.frame(lapply(prolific, function(x) as.numeric(as.character(x))))

######################################################################################################
######################################################################################################
######################################################################################################


## The sample was expanded via further snowball sampling with Eagle newbies on/about March 2022, with the additional sample (n=232; "Engagement(post-Qualtrics)April1920221118.csv")
## The current sample has 234 using choice text from Engagement_Pos Qual_2022-05-01_Choic Text.csv. Was pulled on May 1, 2022. 

snowball <- read.csv("..//Engagement-Survey-Projects//Snowball Data//Engagement_Pos Qual_2022-05-01_Numeric Text.csv")[-c(1:2),c(47:66)]

snowball <- data.frame(lapply(snowball, function(x) as.numeric(as.character(x))))

##########################################################################
##########################################################################
##########################################################################

prolific$from <- "Prolific"
snowball$from <- "Snowball"
qualtrics.engage$from <- "Qualtrics"

samplefrom <- rbind(prolific, snowball, qualtrics.engage)
descr::freq(samplefrom$from)

set.seed(36)

temp <- sort(sample(nrow(samplefrom), nrow(samplefrom)*.5))
together <- samplefrom[temp,]
cfa <- samplefrom[-temp,]

###################################################################################
###################################################################################
###################################################################################
#library(descr)        ## quick look at distributions of item response across samples (swapped items iteratively) - 8/9/22

#par(mfrow = c(3, 1))

#hist(prolific$B22)
#hist(snowball$B22)
#hist(qualtrics.engage$B22)

###################################################################################
###################################################################################
###################################################################################

## Drop candidates are 1,3,4 & 25,26,28

together$C14 <- 7-together$C14

psych::alpha(together[1:8])
psych::alpha(together[9:14])
psych::alpha(together[15:20])

psych::alpha(together[c(1:3, 9:10, 15:16)])
psych::alpha(together[c(4:5, 11:12, 17:18)])
psych::alpha(together[c(6:8, 13:14, 19:20)])

cor(together[1:8], use="pairwise.complete.obs")   ## probably C4 gets axed
psych::alpha(together[1:3])

cor(together[c(1:3, 9:10, 15:16)], use="pairwise.complete.obs")


library(lavaan)

bifactor <-'
Absorption = ~C1  + C3  + A5  + A8  + B10 + B11
Vigor      = ~C14 + C16 + A17 + A19 + B21 + B22
Dedication = ~C25 + C26 + C28 + A31 + A32 + B34 + B35
Cognitive  = ~C1  + C3  + C14 + C16 + C25 + C26 + C28
Affective  = ~A5  + A8  + A17 + A19 + A31 + A32
Behavioral = ~B10 + B11 + B21 + B22 + B34 + B35
Absorption ~~ 0*Affective
Absorption ~~ 0*Behavioral
Absorption ~~ 0*Cognitive
Vigor      ~~ 0*Affective
Vigor      ~~ 0*Behavioral
Vigor      ~~ 0*Cognitive
Dedication ~~ 0*Affective
Dedication ~~ 0*Behavioral
Dedication ~~ 0*Cognitive
Dedication ~~ 1*Dedication
'

Fit.mod <- lavaan::cfa(bifactor, data = together, missing = "ML", estimator = 'MLR')

# semPlot::semPaths(Fit.mod2, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3",
#                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0, pastel=FALSE)

semPlot::semPaths(Fit.mod, bifactor = c("Cognitive", "Affective", "Behavioral"), style="lisrel", "std", layout = "tree3", rainbowStart=.5,sizeLat=10, rotation = 2, sizeMan=4.5,edge.label.cex=0.75, asize=2)

standardizedSolution(Fit.mod)
write.csv(fitMeasures(Fit.mod), "minusC4.csv")

## Empirically C4 is a candidate for exclusion - conceptually we also agree it can be axed

psych::alpha(together[c(1:2,4:8)])
psych::alpha(together[c(6:8, 13:14, 19:20)])

## Empirically 25 has lowest corrected item-totals with both attitude (cognition) and substantive (dedication), however, these are marginal and the CONTENT of 26 and 28 are a bit redundant, so we would like to retain 25 and drop either 26 or 28


bifactor_28 <-'
Absorption = ~C1  + C3  + A5  + A8  + B10 + B11
Vigor      = ~C14 + C16 + A17 + A19 + B21 + B22
Dedication = ~C25 + C28 + A31 + A32 + B34 + B35
Cognitive  = ~C1  + C3  + C14 + C16 + C25 + C28
Affective  = ~A5  + A8  + A17 + A19 + A31 + A32
Behavioral = ~B10 + B11 + B21 + B22 + B34 + B35
Absorption ~~ 0*Affective
Absorption ~~ 0*Behavioral
Absorption ~~ 0*Cognitive
Vigor      ~~ 0*Affective
Vigor      ~~ 0*Behavioral
Vigor      ~~ 0*Cognitive
Dedication ~~ 0*Affective
Dedication ~~ 0*Behavioral
Dedication ~~ 0*Cognitive
Dedication ~~ 1*Dedication
'

bifactor_26 <-'
Absorption = ~C1  + C3  + A5  + A8  + B10 + B11
Vigor      = ~C14 + C16 + A17 + A19 + B21 + B22
Dedication = ~C25 + C26 + A31 + A32 + B34 + B35
Cognitive  = ~C1  + C3  + C14 + C16 + C25 + C26
Affective  = ~A5  + A8  + A17 + A19 + A31 + A32
Behavioral = ~B10 + B11 + B21 + B22 + B34 + B35
Absorption ~~ 0*Affective
Absorption ~~ 0*Behavioral
Absorption ~~ 0*Cognitive
Vigor      ~~ 0*Affective
Vigor      ~~ 0*Behavioral
Vigor      ~~ 0*Cognitive
Dedication ~~ 0*Affective
Dedication ~~ 0*Behavioral
Dedication ~~ 0*Cognitive
Dedication ~~ 1*Dedication
'


Fit.26 <- lavaan::cfa(bifactor_26, data = together, missing = "ML", estimator = 'MLR')
Fit.28 <- lavaan::cfa(bifactor_28, data = together, missing = "ML", estimator = 'MLR')

semPlot::semPaths(Fit.26, bifactor = c("Cognitive", "Affective", "Behavioral"), style="lisrel", "std", layout = "tree3", rainbowStart=.5,sizeLat=10, rotation = 2, sizeMan=4.5,edge.label.cex=0.75, asize=2)
semPlot::semPaths(Fit.28, bifactor = c("Cognitive", "Affective", "Behavioral"), style="lisrel", "std", layout = "tree3", rainbowStart=.5,sizeLat=10, rotation = 2, sizeMan=4.5,edge.label.cex=0.75, asize=2)

## correlations among cog, beh, affect higher with 28 - more moderate with 26 retained (that means we lean toward retaining 26 and deleting 28)

write.csv(fitMeasures(Fit.26), "fit26.csv")
write.csv(fitMeasures(Fit.28), "fit28.csv")

together$cog <- rowMeans(together[c(1:2,4:7)], na.rm=TRUE)
together$aff <- rowMeans(together[9:14], na.rm=TRUE)
together$beh <- rowMeans(together[15:20], na.rm=TRUE)

cor(together[22:24], use = "pairwise")

