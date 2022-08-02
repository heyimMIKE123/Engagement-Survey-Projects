## Combining data sources for 20-to-18
## Prolific first:    

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

different <- use[ which(use$item18 == 2 | use$careless_long > 20 | use$missing > 200), ]

attention <- use[which(use$item61 == 5 & use$item145 == 5 & use$item248 == 2 & use$item308 == 3), ]  ## should incorporate different as well? 8/2/22

data <- data.frame(lapply(attention, function(x) as.numeric(as.character(x))))
prolific <- data[,c(1:404)]     ## 568 out of 785 (72% retained)
                                ## engagement = 380-399

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


######################################################################################################
######################################################################################################
######################################################################################################


## The sample was expanded via further snowball sampling with Eagle newbies on/about March 2022, with the additional sample (n=232; "Engagement(post-Qualtrics)April1920221118.csv")
## The current sample has 234 using choice text from Engagement_Pos Qual_2022-05-01_Choic Text.csv. Was pulled on May 1, 2022. 

library(tidyverse)
library(readxl)

Engage <- read.csv("..//Engagement-Survey-Projects//Snowball Data//Engagement_Pos Qual_2022-05-01_Choice Text.csv", skip = 1) %>% # Skipped first row cause it was duplicate header 
  slice(-1) %>% # Slice removes rows, removed the first row because it was messy qualtrics info
  select(-IRB.FY21.22.2359.We.are.asking.you.to.take.part.in.a.research.study.being.done.by.Dr..Kulas.a.faculty.member.at.Montclair.State.University..Being.in.this.study.is.optional...If.you.choose.to.be.in.the.study..you.will.complete.a.brief.survey.consisting.of.5.short.measures.of.work.engagement.as.well.as.free.time.activities..spending.time.either.with.pets.or.game.playing...This.survey.will.help.us.learn.more.about.measurement.properties.of.scales.purported.to.assess.workers..work.attitudes.and.free.time.activities..You.have.been.offered.the.opportunity.to.participate.in.this.study.since.you.are.18.years.or.older..and.are.employed.part.time.or.full.time.in.the.U.S..The.survey.will.take.about.10.minutes.to.complete....The.survey.is.anonymous..and.no.one.will.be.able.to.link.your.answers.back.to.you...Employment.questions.or.sensitive.questions..We.strongly.advise.that.you.do.not.use.an.employer.issued.electronic.device..laptop..phone.or.WIFI.to.respond.to.this.survey..as.many.employers.monitor.use.of.all.devices...Questions.about.the.study..Please.contact.Dr..John.Kulas..Professor.of.Psychology.in.the.College.of.Humanities.and.Social.Sciences.at.kulasj.montclair.edu...If.you.have.questions.or.concerns.about.your.rights.as.a.research.participant..you.can.call.the.MSU.Institutional.Review.Board.at.973.655.7583.or.email.reviewboard.montclair.edu..This.study.has.been.approved.by.the.Montclair.St.University.Institutional.Review.Board..If.you.want.to.participate.in.this.study..please.select.the..Accept..option.to.start.the.survey.) %>%
  select(-Start.Date:-Progress,-Recorded.Date,-Recipient.Last.Name:-External.Data.Reference,-Distribution.Channel,-User.Language) # Removed usless items

Demographics <- Engage %>% 
  select(Response.ID,Duration..in.seconds.,Finished,Location.Latitude,Location.Longitude,Emploment.Status=Which.of.the.following.categories.best.describes.your.employment.status.,
         Industry=Which.of.the.following.categories.best.describes.the.industry.you.work.in.,Size.of.Organization=What.is.the.size.of.the.organization.you.work.for.,
         Work.Arrangement=Which.of.the.following.categories.best.describes.your.current.work.arrangement.,Job.Title=What.is.your.job.title.,
         Job.Managerial.Responsibilities=What.is.the.best.description.of.your.job.s.managerial.responsibilities., Age=Please.indicate.your.age, Education=Please.indicate.your.education.level,
         Gender=To.which.gender.identity.do.you.most.identify.)

Data2 <- Engage %>% 
  select(-Duration..in.seconds.,-Finished,-Location.Latitude,-Location.Longitude,-Which.of.the.following.categories.best.describes.your.employment.status.:-To.which.gender.identity.do.you.most.identify.)

Saks <- Data2 %>% 
  select(Response.ID,I.really..throw..myself.into.my.job:I.am.highly.engaged.in.this.organization) %>% 
  mutate(across(c("I.really..throw..myself.into.my.job":"I.am.highly.engaged.in.this.organization"),~case_when(str_detect(.,regex("Strongly Disagree"))~"1",
                                                                                                               str_detect(.,regex("Strongly Agree"))~"5",
                                                                                                               str_detect(.,regex("Neither Agree nor Disagree"))~"3",
                                                                                                               str_detect(.,regex("Disagree"))~"2",
                                                                                                               str_detect(.,regex("Agree"))~"4",
                                                                                                               TRUE ~ "CHECK"))) %>% 
  mutate(across(c("My.mind.often.wanders.and.I.think.of.other.things.when.doing.my.job","I.am.really.not.into.the..goings.on..in.this.organization"),~recode(.,"5"="1","4"="2","3"="3","2"="4","1"="5"))) %>% 
  mutate(across(c("I.really..throw..myself.into.my.job":"I.am.highly.engaged.in.this.organization"),~as.numeric(.))) 

UWES <- Data2 %>% 
  select(Response.ID,At.my.work..I.feel.bursting.with.energy:At.my.work.I.always.persevere..even.when.things.do.not.go.well) %>% 
  mutate(across(c("At.my.work..I.feel.bursting.with.energy":"At.my.work.I.always.persevere..even.when.things.do.not.go.well"),~case_when(str_detect(.,regex("Never"))~"1",
                                                                                                                                         str_detect(.,regex("Almost never"))~"2",
                                                                                                                                         str_detect(.,regex("Rarely"))~"3",
                                                                                                                                         str_detect(.,regex("Sometimes"))~"4",
                                                                                                                                         str_detect(.,regex("Often"))~"5",
                                                                                                                                         str_detect(.,regex("Very often"))~"6",
                                                                                                                                         str_detect(.,regex("Always"))~"7")))%>% 
  mutate(across(c("At.my.work..I.feel.bursting.with.energy":"At.my.work.I.always.persevere..even.when.things.do.not.go.well"),~as.numeric(.))) 




Engagement <- Data2 %>% 
  select(Response.ID,I.am.able.to.concentrate.on.my.work.without.getting.distracted:I.express.enthusiasm.for.my.job.while.at.work.1) %>% 
  unite(.,Item_1,I.am.able.to.concentrate.on.my.work.without.getting.distracted, I.am.able.to.concentrate.on.my.work.without.getting.distracted.1,sep = "",na.rm = T) %>% 
  unite(.,Item_2,Time.passes.quickly.while.I.m.working,Time.passes.quickly.while.I.m.working.1,sep = "", na.rm = T) %>% 
  unite(.,Item_3,I.find.it.difficult.to.mentally.disconnect.from.work, I.find.it.difficult.to.mentally.disconnect.from.work.1, sep = "", na.rm = T) %>% 
  unite(.,Item_4,Thinking.about.work.saps.my.energy, Thinking.about.work.saps.my.energy.1,sep = "",na.rm = T) %>% 
  unite(.,Item_5,I.m.able.to.maintain.good.levels.of.energy.throughout.the.workday,I.m.able.to.maintain.good.levels.of.energy.throughout.the.workday.1,sep = "", na.rm = T) %>% 
  unite(.,Item_6,I.plan.to.stay.with.this.company.as.my.career.advances, I.plan.to.stay.with.this.company.as.my.career.advances.1, sep = "", na.rm = T) %>% 
  unite(.,Item_7,I.believe.this.company.cares.about.my.career.goals, I.believe.this.company.cares.about.my.career.goals.1,sep = "",na.rm = T) %>% 
  unite(.,Item_8,This.organization.challenges.me.to.work.at.my.full.potential,This.organization.challenges.me.to.work.at.my.full.potential.1,sep = "", na.rm = T) %>% 
  unite(.,Item_9,I.enjoy.thinking.about.work.even.when.I.m.not.at.work, I.enjoy.thinking.about.work.even.when.I.m.not.at.work.1, sep = "", na.rm = T) %>% 
  unite(.,Item_10,I.love.starting.my.workday, I.love.starting.my.workday.1,sep = "",na.rm = T) %>% 
  unite(.,Item_11,I.enjoy.spending.time.completing.my.job.tasks,I.enjoy.spending.time.completing.my.job.tasks.1,sep = "", na.rm = T) %>% 
  unite(.,Item_12,I.feel.motivated.to.go.beyond.what.is.asked.of.me.at.work, I.feel.motivated.to.go.beyond.what.is.asked.of.me.at.work.1, sep = "", na.rm = T) %>% 
  unite(.,Item_13,I.feel.proud.of.my.accomplishments.within.this.organization, I.feel.proud.of.my.accomplishments.within.this.organization.1,sep = "",na.rm = T) %>% 
  unite(.,Item_14,My.job.makes.me.feel.like.I.m.part.of.something.meaningful,My.job.makes.me.feel.like.I.m.part.of.something.meaningful.1,sep = "", na.rm = T) %>% 
  unite(.,Item_15,I.have.to.be.reminded.to.take.breaks.while.I.m.at.work, I.have.to.be.reminded.to.take.breaks.while.I.m.at.work.1, sep = "", na.rm = T) %>% 
  unite(.,Item_16,I.never.miss.a.work.deadline, I.never.miss.a.work.deadline.1,sep = "",na.rm = T) %>% 
  unite(.,Item_17,When.work.is.slow.I.find.ways.to.be.productive,When.work.is.slow.I.find.ways.to.be.productive.1,sep = "", na.rm = T) %>% 
  unite(.,Item_18,I.express.enthusiasm.for.my.job.while.at.work, I.express.enthusiasm.for.my.job.while.at.work.1, sep = "", na.rm = T) %>% 
  unite(.,Item_19,I.embrace.challenging.situations.at.work,I.embrace.challenging.situations.at.work.1,sep = "", na.rm = T) %>% 
  unite(.,Item_20,I.speak.positively.about.this.organization.to.others, I.speak.positively.about.this.organization.to.others.1, sep = "", na.rm = T) %>% 
  mutate(across(c("Item_1":"Item_20"),~case_when(str_detect(.,regex("Strongly Disagree"))~"1",
                                                 str_detect(.,regex("Strongle Agree"))~"6",
                                                 str_detect(.,regex("Somewhat Disagree"))~"3",
                                                 str_detect(.,regex("Somewhat Agree"))~"4",
                                                 str_detect(.,regex("Disagree"))~"2",
                                                 str_detect(.,regex("Agree"))~"5"))) %>% 
  mutate(across(c("Item_3","Item_4"),~recode(.,"6"="1","5"="2","4"="3","4"="4","2"="5","1"="6"))) %>% 
  mutate(across(c("Item_1":"Item_20"),~as.numeric(.))) 



######################################################################################################
######################################################################################################
######################################################################################################

#### Note - might be easier/less messy just to grab columns individually from each dataset

snowball <- Engagement[,-1]

names(snowball) <- c("Item_1", "Item_3", "Item_4", "Item_14", "Item_16", "Item_25", "Item_26", "Item_28", "Item_5", "Item_8", "Item_17", "Item_19", "Item_31", "Item_32", "Item_10", "Item_11", "Item_21", "Item_22", "Item_34", "Item_35")

## Drop candidates are 1,3,4 & 25,26,28

























data$item387      <- 7- data$item387           ## only reflected engagement item

names(data)[380] <- "Item_1"
names(data)[381] <- "Item_3"
names(data)[382] <- "Item_4"
names(data)[383] <- "Item_5"
names(data)[384] <- "Item_8"
names(data)[385] <- "Item_10"
names(data)[386] <- "Item_11"
names(data)[387] <- "Item_14"
names(data)[388] <- "Item_16"
names(data)[389] <- "Item_17"
names(data)[390] <- "Item_19"
names(data)[391] <- "Item_21"
names(data)[392] <- "Item_22"
names(data)[393] <- "Item_25"
names(data)[394] <- "Item_26"
names(data)[395] <- "Item_28"
names(data)[396] <- "Item_31"
names(data)[397] <- "Item_32"
names(data)[398] <- "Item_34"
names(data)[399] <- "Item_35"

psych::alpha(data[c(380:386)])
psych::alpha(data[c(387:392)])
psych::alpha(data[c(393:399)])
psych::alpha(data[c(380:382, 387:388, 393:395)])
psych::alpha(data[c(383:384, 389:390, 396:397)])
psych::alpha(data[c(385:386, 391:392, 398:399)])


data$absorption   <- rowMeans(data[c(380:386)], na.rm=TRUE)
data$vigor        <- rowMeans(data[c(387:392)], na.rm=TRUE) 
data$dedication   <- rowMeans(data[c(393:399)], na.rm=TRUE) 

data$cognitive    <- rowMeans(data[c(380:382, 387:388, 393:395)], na.rm=TRUE)
data$affective    <- rowMeans(data[c(383:384, 389:390, 396:397)], na.rm=TRUE) 
data$behavioral   <- rowMeans(data[c(385:386, 391:392, 398:399)], na.rm=TRUE) 

cor(data[405:410])

###################################################################################
###################################################################################
###################################################################################
library(lavaan)

bifactor <-'
Absorption = ~Item_1 + Item_3  + Item_5  + Item_8  + Item_10 + Item_11
Vigor      = ~Item_14 + Item_16 + Item_17 + Item_19 + Item_21 + Item_22
Dedication = ~ NA*Item_26 + Item_28 + Item_31 + Item_32 + Item_34 + Item_35
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
Dedication ~~ 1*Dedication
'

Fit.mod <- lavaan::cfa(bifactor, data = data, missing = "ML", estimator = 'MLR')

# semPlot::semPaths(Fit.mod2, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3",
#                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0, pastel=FALSE)

semPlot::semPaths(Fit.mod, bifactor = c("Cognitive", "Affective", "Behavioral"), style="lisrel", "std", layout = "tree3", rainbowStart=.5,sizeLat=10, rotation = 2, sizeMan=4.5,edge.label.cex=0.75, asize=2)

standardizedSolution(Fit.mod)

# into data frames
fit1.1 <- as.data.frame(fitMeasures(Fit.mod))
fit1.1$`fitMeasures(fit.mod)` <- round(fit1.1$`fitMeasures(fit.mod)`, 2)

fit1.2 <- as.data.frame(fitMeasures(Fit1.2))
fit1.2$`fitMeasures(Fit1.2)` <- round(fit1.2$`fitMeasures(Fit1.2)`, 2)

fit.mod2 <- as.data.frame(fitMeasures(Fit.mod2))
fit.mod2$`fitMeasures(Fit.mod2)` <- round(fit.mod2$`fitMeasures(Fit.mod2)`, 2)

indices_att <- as.data.frame(t(fit1.1[c(3,4,23,29,9,10,19),]))        ## Chi-sq, df, RMSEA, SRMR, CFI, TLI, AIC
indices_sub <- as.data.frame(t(fit1.2[c(3,4,23,29,9,10,19),]))
indices_bi <- as.data.frame(t(fit.mod2[c(3,4,44,58,17,18,38),]))