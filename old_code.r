
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
                  Avoidance -> Divorce
                  Avoidance -> ResolvingConflicts
                  EmotionalDistance -> Divorce
                  GoalAlignment -> Divorce
                  GoalAlignment -> QualityTime
                  KnowingPartner -> Divorce
                  KnowingPartner -> ResolvingConflicts
                  QualityTime -> Divorce
                  ResolvingConflicts -> Divorce
                  ResolvingConflicts -> EmotionalDistance
                  SelfReflection -> Aggression
                  SelfReflection -> Divorce
                  KnowingPartner -> SelfReflection
                  KnowingPartner-> GoalAlignment  
                  ResolvingConflicts -> GoalAlignment
                  SelfReflection <-> Aggression
                  ResolvingConflicts <-> SelfReflection
                  ResolvingConflicts -> Aggression 
                  GoalAlignment -> Aggression
                  KnowingPartner -> Aggression
                  }
                ')


# KnowingPartner -> SelfReflection
# KnowingPartner-> GoalAlignment  
# ResolvingConflicts -> GoalAlignment
# SelfReflection <-> Aggression
# ResolvingConflicts <-> SelfReflection
# ResolvingConflicts -> Aggression 
# GoalAlignment -> Aggression
# KnowingPartner -> Aggression

# removed this 
#SelfReflection -> ResolvingConflicts, 
#Aggression -> Resolvingconflicts
#Avoidance -> Goalalignment
#EmotionalDistance -> QualityTime
#Avoidance -> EmotionalDistance


plot(g)
impliedConditionalIndependencies(g)
localTests(g, df)


sem_model <- sem(toString(g, "lavaan"), df)
modindices(sem_model)
fitmeasures(sem_model)
coef(sem_model)
sem_plot <- semPlotModel(sem_model)
semPaths(sem_model,  paths="paths",whatLabels = "stand", rotation = 2)


net <-model2network(toString(g,"bnlearn"))
fit <- bn.fit (net, df)
fit
predictions <-predict(fit, node="Divorce", df)
plot(predictions)

#Commeents on g2
#Added:
#Avoidance -> SelfReflection
#EmotionalDistance -> Divorce
#EmotionalDistance -> QualityTime
#SelfReflection -> Qualit
#Aggression -> QualityTime
#EmotionalDistance -> GoalAlignment

#removed:
#divorce egdes
#Avoidance -> Resolving Conflicts
#ResolvingConflicts -> EmotionalDistance
#ResolvingConflicts -> Aggression 
#GoalAlignment -> Aggression

# prediction
net2 <-model2network(toString(g,"bnlearn"))
fit2 <- bn.fit (net2, df)
fit2
predictions2 <-predict(fit2, node="Divorce", df)
plot(predictions2)
