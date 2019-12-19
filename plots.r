library(corrplot)

#Read data
df <- read.table("divorce2.csv", header = TRUE,sep = ",", colClasses=c("NULL", 
                                                                       "double", "double", "double", 
                                                                       "double", "double", "double", "double", "double"))
labels <- read.table("divorce.csv", header = TRUE,sep = ";")[ ,c('Class')]
df$Divorce= labels
df$Divorce[df$Divorce == -1] <- 1

plot(df$ResolvingConflicts,df$QualityTime)#
plot(df$ResolvingConflicts,df$EmotionalDistance)
plot(df$ResolvingConflicts,df$GoalAlignment)#
plot(df$ResolvingConflicts,df$KnowingPartner)#
plot(df$ResolvingConflicts,df$Aggression)#
plot(df$ResolvingConflicts,df$Avoidance)#
plot(df$ResolvingConflicts,df$SelfReflection)#

plot(df$QualityTime,df$EmotionalDistance)
plot(df$QualityTime,df$GoalAlignment)#
plot(df$QualityTime,df$KnowingPartner)#
plot(df$QualityTime,df$Aggression)##
plot(df$QualityTime,df$Avoidance)##
plot(df$QualityTime,df$SelfReflection)##

plot(df$EmotionalDistance,df$GoalAlignment)
plot(df$EmotionalDistance,df$KnowingPartner)
plot(df$EmotionalDistance,df$Aggression)
plot(df$EmotionalDistance,df$Avoidance)
plot(df$EmotionalDistance,df$SelfReflection)

plot(df$GoalAlignment,df$KnowingPartner)#
plot(df$GoalAlignment,df$Aggression)#
plot(df$GoalAlignment,df$Avoidance)##
plot(df$GoalAlignment,df$SelfReflection)#

plot(df$KnowingPartner,df$Aggression)#
plot(df$KnowingPartner,df$Avoidance)##
plot(df$KnowingPartner,df$SelfReflection)##

plot(df$Aggression,df$Avoidance)#
plot(df$Aggression,df$SelfReflection)#

plot(df$Avoidance,df$SelfReflection)


cormat<-cor(df[-c(9)])
corrplot(cormat, method = "circle")


df2 <- read.table("divorce.csv", header = TRUE,sep = ";")
cormat<-cor(df2[c("Atr1","Atr2","Atr3","Atr4")])
corrplot(cormat, method = "circle")

cormat<-cor(df2[c("Atr5" , "Atr8" , "Atr9",  "Atr13")])
corrplot(cormat, method = "circle")

cormat<-cor(df2[c("Atr6","Atr7")])
corrplot(cormat, method = "circle")

cormat<-cor(df2[c("Atr10","Atr11","Atr12","Atr14","Atr15","Atr16","Atr17" , "Atr18"  , "Atr19" , "Atr20")])
corrplot(cormat, method = "circle")

cormat<-cor(df2[-c(55)])
corrplot(cormat, diag = FALSE,
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

pdf(file = "yourPlots.png")
pairs(df)
dev.off()  # important!
