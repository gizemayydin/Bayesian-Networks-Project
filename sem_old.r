library(lavaan)
library(dagitty)
library(bnlearn)

df <- read.table("divorce2.csv", header = TRUE,sep = ",", colClasses=c("NULL", 
                      "real", "real", "real", "real", "real", "real", "real", "real"))
labels <- - read.table("divorce.csv", header = TRUE,sep = ";")[ ,c('Class')]

df$Divorce= labels

df$Divorce[df$Divorce == -1] <- 1


g <- dagitty('dag{Aggression [exposure,pos="0.031,0.025"]
                  Avoidance [exposure,pos="0.042,0.264"]
                  Divorce [outcome,pos="1.000,1.000"]
                  EmotionalDistance [exposure,pos="0.519,0.332"]
                  GoalAlignment [exposure,pos="0.200,0.974"]
                  KnowingPartner [exposure,pos="0.276,0.800"]
                  QualityTime [exposure,pos="0.520,0.773"]
                  ResolvingConflicts [exposure,pos="0.992,0.103"]
                  SelfReflection [exposure,pos="0.538,-0.052"]
                  Aggression -> Divorce
                  Aggression -> EmotionalDistance
                  Aggression -> ResolvingConflicts
                  Avoidance -> Divorce
                  Avoidance -> EmotionalDistance
                  Avoidance -> GoalAlignment
                  Avoidance -> ResolvingConflicts
                  EmotionalDistance -> Divorce
                  EmotionalDistance -> QualityTime
                  GoalAlignment -> Divorce
                  GoalAlignment -> QualityTime
                  KnowingPartner -> Divorce
                  KnowingPartner -> ResolvingConflicts
                  QualityTime -> Divorce
                  ResolvingConflicts -> Divorce
                  ResolvingConflicts -> EmotionalDistance
                  SelfReflection -> Aggression
                  SelfReflection -> Divorce
                  SelfReflection -> ResolvingConflicts
                  KnowingPartner -> SelfReflection
                  }
                ')

# KnowingPartner -> SelfReflection
# 

plot(g)
impliedConditionalIndependencies(g)
localTests(g, df)
#summary(lm(Aggression ~ Avoidance, df))

net <-model2network(toString(g,"bnlearn"))
fit <- bn.fit (net, df)
fit
predictions <-predict(fit, node="Divorce", df)
plot(predictions)
