## The sample was expanded via further snowball sampling with Eagle newbies on/about March 2022, with the additional sample (n=232; "Engagement(post-Qualtrics)April1920221118.csv")
## The current sample has 234 using choice text from Engagement_Pos Qual_2022-05-01_Choic Text.csv. Was pulled on May 1, 2022. 

library(tidyverse)
library(readxl)

Engage <- read.csv("Snowball Data/Engagement_Pos Qual_2022-05-01_Choice Text.csv", skip = 1) %>% # Skipped first row cause it was duplicate header 
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

Game <- Data2 %>% 
  select(Response.ID,Washed.dishes:Knitted..quilted..sewed..or.crocheted)

Pets <- Data2 %>% 
  select(Response.ID, Played.with.a.pet.animal:Purchased.a.pet.animal)

Intent <- Data2 %>% 
  select(Response.ID, I.am.thinking.about.leaving.this.organization:I.don.t.plan.to.be.in.this.organization.much.longer)
