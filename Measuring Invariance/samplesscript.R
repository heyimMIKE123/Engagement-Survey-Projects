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

dropatt <- newdata.att[ which(newdata.att$flag == "Flagged"), c(1:92)]
dropsub <- newdata.sub[ which(newdata.sub$flag == "Flagged"), c(1:92)]

dropseb.sorted <- dropsub[,c(1:47, 48:50,55:56,61:63,51:52,57:58,64:65,53:54,59:60,66:67,68:92)]
names(dropseb.sorted) <- names(useatt)           ##### focal engagement is 48:67

qualtricsgood <- as.data.frame(rbind(useseb.sorted,useatt))      ## 377 out of 743 (51% retained)
## engagement = 48-67
qualtricsbad <- as.data.frame(rbind(dropseb.sorted,dropatt))      ## 377 out of 743 (51% retained)
## engagement = 48-67
qualtrics.engage.keep <- qualtricsgood[,c(48:67)]

qualtrics.engage.keep <- data.frame(lapply(qualtrics.engage.keep, function(x) as.numeric(as.character(x))))

qualtrics.engage.drop <- qualtricsbad[,c(48:67)]

qualtrics.engage.drop <- data.frame(lapply(qualtrics.engage.drop, function(x) as.numeric(as.character(x))))

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

usegood <- use[which(use$flag == "Good"), ]
usebad <- use[which(use$flag == "Flagged"), ]   ## 121 NAs here 

## descr::freq(use2$flag)                         ## no overlap - all "good" unflagged


prolificgood <- usegood[,c(380:382,387:388,393:395,
                    383:384,389:390,396:397,
                    385:386,391:392,398:399)]     ## 568 out of 785 (72% retained)

prolificbad <- usebad[,c(380:382,387:388,393:395,
                           383:384,389:390,396:397,
                           385:386,391:392,398:399)]    

names(prolificgood) <- names(qualtrics.engage.keep)
names(prolificbad) <- names(qualtrics.engage.keep)

prolificgood <- data.frame(lapply(prolificgood, function(x) as.numeric(as.character(x))))
prolificbad <- data.frame(lapply(prolificbad, function(x) as.numeric(as.character(x))))

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

qualtrics.engage.keep$from <- "QualtricsKeep"
prolificgood$from <- "ProlificKeep"
snowball$from <- "Snowball"
qualtrics.engage.drop$from <- "QualtricsDrop"
prolificbad$from <- "ProlificDrop"

analyses <- rbind(qualtrics.engage.keep, prolificgood, snowball, qualtrics.engage.drop,prolificbad)
descr::freq(analyses$from)

