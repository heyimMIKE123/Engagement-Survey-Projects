## Running bifactor on both Prolific as well as Qualtrics data
## Prolific first:

temp <- read.csv("initial_data_screen.csv", header=FALSE, na.strings="")   ## NOTE: 404 vars (9/28/21) make sure to check indexing used throughout script

x <- paste("item", sep="",1:404)
y <- t(temp[2,])
## decluttering Qualtrics excess
data <- temp[-c(1:3),]                                           ## Getting rid of all 3 weird Qualtrics rows
colnames(data) <- x

incomplete <- read.csv("inprogress.csv", header=FALSE, na.strings="")   ## NOTE: 404 vars (9/28/21) make sure to check indexing used throughout script

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

attention <- use[which(use$item61 == 5 & use$item145 == 5 & use$item248 == 2 & use$item308 == 3), ]

data <- data.frame(lapply(attention, function(x) as.numeric(as.character(x))))
data <- data[,c(1:404)]

######################################################################################################
######################################################################################################
######################################################################################################

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