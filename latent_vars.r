library(lavaan)
library(dagitty)
library(bnlearn)

df_nonscaled <- read.table("divorce.csv", header = TRUE,sep = ";")
df <- as.data.frame(scale(subset(df_nonscaled, select = -c(55))))

model <- sem("x1 =~ Atr1 + Atr2 + Atr3 + Atr4",data=df)
summary(model)
df$ResolvingConflicts = predict(model)

model2 <- sem("x2 =~ Atr5 + Atr8 + Atr9 + Atr13
              Atr8 ~~ Atr9
              ", data=df)
summary(model2)
df$QualityTime = predict(model2)
modindices(model2)

model3_string <- "U =~ l*Atr6 \n U =~ l*Atr7"
model3 <-sem(model3_string,df)
summary(model3)
df$EmotionalDistance <- predict(model3)

model4 <- sem("x4 =~ Atr10 + Atr11 + Atr12 + Atr14 + Atr15 + Atr16 + Atr17 + Atr18  + Atr19 + Atr20
              Atr17~~Atr19
              Atr10~~Atr16
              Atr11~~Atr19
              Atr12 ~~ Atr16
              Atr14 ~~ Atr15
              Atr10 ~~ Atr12
              Atr11 ~~ Atr17
              Atr14 ~~ Atr17
              Atr10 ~~ Atr11
              Atr15 ~~ Atr17
              Atr15 ~~ Atr20",data=df)
summary(model4)
df$GoalAlignment = predict(model4)

model5 <- sem("x5 =~ Atr21 + Atr22 + Atr23 + Atr24 + Atr25 + Atr26 + Atr27 + Atr28  + Atr29 + Atr30
              Atr21 ~~ Atr25
              Atr21 ~~ Atr26
              Atr21 ~~ Atr27
              Atr22 ~~ Atr28
              Atr23 ~~ Atr29
              Atr23 ~~ Atr30
              Atr24 ~~ Atr26
              Atr25 ~~ Atr29
              Atr21 ~~ Atr24
              Atr23 ~~ Atr25
              Atr23 ~~ Atr26
              Atr24 ~~ Atr30
              Atr27 ~~ Atr30
              Atr26 ~~ Atr29
              ",data=df)
summary(model5)
df$KnowingPartner = predict(model5)

model6 <- sem("x6 =~ Atr31 + Atr32 + Atr33 + Atr34 + Atr35 + Atr36 + Atr37 + Atr38  + Atr39 + Atr40 + Atr41
              Atr33 ~~ Atr40
              Atr35 ~~ Atr36
              Atr35 ~~ Atr40
              Atr36 ~~ Atr37
              Atr36 ~~ Atr39
              Atr38 ~~ Atr40
              Atr39 ~~ Atr40
              Atr39 ~~ Atr41
              Atr31 ~~ Atr38
              Atr32 ~~ Atr35
              Atr33 ~~ Atr35
              Atr33 ~~ Atr36
              Atr33 ~~ Atr38
              Atr36 ~~ Atr38
              Atr35 ~~ Atr41
              Atr40 ~~ Atr41
              Atr36 ~~ Atr41
              Atr37 ~~ Atr39
              ",data=df)
summary(model6)
df$Aggression = predict(model6)

model7 <- sem("x7 =~ Atr42 + Atr43 + Atr44 + Atr45 + Atr46 + Atr47
              Atr43 ~~ Atr45",data=df)
summary(model7)
df$Avoidance = predict(model7)

model8 <- sem("x8 =~ Atr48 + Atr49 + Atr50 + Atr51 + Atr52 + Atr53 + Atr54
              Atr49 ~~ Atr50
              Atr52 ~~ Atr53
              Atr52 ~~ Atr54
              Atr53 ~~ Atr54
              Atr48 ~~ Atr53
              Atr50 ~~ Atr53",data=df)
summary(model8)
df$SelfReflection = predict(model8)

df_new = subset(df, select = c(55,56,57,58,59,60,61,62))
write.csv(df_new,"divorce2.csv")

