## Cleaning Qualtrics construct validation data - 10/14/21

newdata.att <- read.csv("Engagement+(Attitudinal)_October+12,+2021_08.02.csv"); names <- newdata.att[1,c(1:92)] #[47:66]
newdata.sub <- read.csv("Engagement+(Substantive)_October+12,+2021_08.01.csv")#[47:66]

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

qualtrics <- as.data.frame(rbind(useseb.sorted,useatt))

########################################### Engagement

qualtrics$C14 <- 7-qualtrics$C14

cognition.alpha  <- psych::alpha(qualtrics[c(48:55)])
affect.alpha     <- psych::alpha(qualtrics[c(56:61)])
behavior.alpha   <- psych::alpha(qualtrics[c(62:67)])

absorption.alpha <- psych::alpha(qualtrics[c(48:50,56:57,62:63)])
vigor.alpha      <- psych::alpha(qualtrics[c(51:52,58:59,64:65)])
dedication.alpha <- psych::alpha(qualtrics[c(53:55,60:61,66:67)])

qualtrics$cognition <- rowMeans(qualtrics[c(48:55)], na.rm=TRUE)
qualtrics$affect    <- rowMeans(qualtrics[c(56:61)], na.rm=TRUE)
qualtrics$behavior  <- rowMeans(qualtrics[c(62:67)], na.rm=TRUE)

qualtrics$absorption <- rowMeans(qualtrics[c(48:50,56:57,62:63)], na.rm=TRUE)
qualtrics$vigor      <- rowMeans(qualtrics[c(51:52,58:59,64:65)], na.rm=TRUE)
qualtrics$dedication <- rowMeans(qualtrics[c(53:55,60:61,66:67)], na.rm=TRUE)

cor(qualtrics[93:98], use="complete.obs")

########################################### Intent to quit

intent.quit.alpha <- psych::alpha(qualtrics[c(80:83)])
qualtrics$intentquit <- rowMeans(qualtrics[c(80:83)], na.rm=TRUE)

########################################### Pets

pets <- psych::alpha(qualtrics[c(75:79)])
qualtrics$pets <- rowMeans(qualtrics[c(75:79)], na.rm=TRUE)

########################################### Household Chores

household <- psych::alpha(qualtrics[c(68:74)])
qualtrics$household <- rowMeans(qualtrics[c(68:74)], na.rm=TRUE)

########################################### Saks

saks.job <- psych::alpha(qualtrics[c(20:24)])

qualtrics$Q8 <- 6 - qualtrics$Q8
saks.work <- psych::alpha(qualtrics[c(25:30)])

qualtrics$saks.job <- rowMeans(qualtrics[c(20:24)], na.rm=TRUE)
qualtrics$saks.work <- rowMeans(qualtrics[c(25:30)], na.rm=TRUE)

########################################### UWES

UWES.vigor      <- psych::alpha(qualtrics[c(31,34,38,42,45,47)])
UWES.dedication <- psych::alpha(qualtrics[c(32,35,37,40,43)])
UWES.absorption <- psych::alpha(qualtrics[c(33,36,39,41,44,46)])


qualtrics$UWES.vigor      <- rowMeans(qualtrics[c(31,34,38,42,45,47)], na.rm=TRUE)
qualtrics$UWES.dedication <- rowMeans(qualtrics[c(32,35,37,40,43)], na.rm=TRUE)
qualtrics$UWES.absorption <- rowMeans(qualtrics[c(33,36,39,41,44,46)], na.rm=TRUE)


cor(qualtrics[93:106], use="complete.obs")

r <- corx::corx(qualtrics[,93:100],                     ## can extend if needed
                triangle = "lower",
                stars = c(0.05, 0.01, 0.001),
                describe = c(`$M$` = mean, `$SD$` = sd))

papaja::apa_table(r$apa, # apa contains the data.frame needed for apa_table
                  caption = "Unit-weighted scale intercorrelations (all conditions).",
                  note = "* p < 0.05; ** p < 0.01; *** p < 0.001",
                  landscape = TRUE,
                  escape = F)

################################################################################
################################################################################
################################################################################
library(lavaan)

bifactor <-'
Absorption = ~Item_1 + Item_3  + Item_5  + Item_8  + Item_10 + Item_11
Vigor      = ~Item_14 + Item_16 + Item_17 + Item_19 + Item_21 + Item_22
Dedication = ~ Item_26 + Item_28 + Item_31 + Item_32 + Item_34 + Item_35
Cognitive  = ~Item_1  + Item_3  + Item_14 + Item_16 + Item_26 + Item_28
Affective  = ~Item_5 +  Item_8  + Item_17 + Item_19 + Item_31 + Item_32
Behavioral = ~Item_10 + Item_11 + Item_21 + Item_22 + Item_34 + Item_35
Absorption ~~ 0*Affective
Absorption ~~ 0*Behavioral
Absorption ~~ 0*Cognitive
Vigor      ~~ 0*Affective
Vigor      ~~ 0*Behavioral
Vigor      ~~ 0*Cognitive
Dedication ~~ 0*Affective
Dedication ~~ 0*Behavioral
Dedication ~~ 0*Cognitive
'

Fit.mod2 <- lavaan::cfa(bifactor, data = qualtrics, missing = "ML", estimator = 'MLR')

semPlot::semPaths(Fit.mod2, bifactor = c("Cognitive", "Affective", "Behavioral"), style="lisrel", "std", layout = "tree3", sizeLat=10, rotation = 2, sizeMan=4.5,edge.label.cex=0.75, edge.color="black", asize=2)



#####################################################################################
#####################################################################################
#####################################################################################
##################### Trying to figure out whether to combine datafiles or not - 7/19/22

## Why not for SIOP at least

## focal engagement is 48:67

hist(qualtrics$C1)

library(tidyr)
long <- gather(qualtrics, item, response, C1:B35, factor_key=TRUE)

library(ggplot2)
ggplot(long, aes(x = response)) + geom_histogram() + facet_wrap(~item) ## once get snowball sample, swap w/ https://stackoverflow.com/questions/43415709/how-to-use-facet-grid-with-geom-histogram


