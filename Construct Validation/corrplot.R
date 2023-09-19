# http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram

library(psych)
library(tidyverse)

co <- bfi %>% 
  select(starts_with("C")) %>% 
  cor(use = "pairwise.complete.obs")

corrplot(co, method = 'circle', type = "lower")

redwhiteblue <- colorRampPalette(c("red", "white", "blue"))(20) #custom color palette 


corrplot(co, type="upper", order="hclust", col=redwhiteblue,
         tl.srt=45, tl.col = "black", addCoef.col = "black", 
         diag = FALSE)

softredblue <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(200)

corrplot(co, type="upper", order="hclust", col=softredblue,
         tl.srt=45, tl.col = "black", addCoef.col = "black", 
         diag = FALSE)


corrplot(co, type="upper", order="hclust", tl.col="black", tl.srt=45)
