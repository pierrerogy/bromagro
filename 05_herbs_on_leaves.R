#Herb per quadrat
plot(predherb$propdamage ~  predherb$herb)
leafmodel_herbquadrat<- 
  glmer(propdamage+0.01 ~ 
          herb*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="Gamma"(link="log"),
        data = predherb)
plot(leafmodel_herbquadrat)
qqnorm(residuals(leafmodel_herbquadrat))
qqline(residuals(leafmodel_herbquadrat), col = "red")
summary(leafmodel_herbquadrat)
Anova(leafmodel_herbquadrat)
visreg(leafmodel_herbquadrat)

#Herb per tree
plot(predherb$propdamage ~  predherb$herbpertree)
leafmodel_herbtree<- 
  glmer(propdamage+0.01 ~ 
          herbpertree*Sampling + 
          (1|Site/alltrees/quadrats),
        family ="Gamma"(link="log"),
        data = predherb)
plot(leafmodel_herbtree)
qqnorm(residuals(leafmodel_herbtree))
qqline(residuals(leafmodel_herbtree), col = "red")
summary(leafmodel_herbtree)
Anova(leafmodel_herbtree)
visreg(leafmodel_herbtree)