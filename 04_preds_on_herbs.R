# per quadrat -------------------------------------------
plot((predherb$herb+1)/(predherb$pred+1))
hist((predherb$herb+1)/(predherb$pred+1))

predherb_modelnb <- 
  glmer.nb(herb ~ 
          log(pred+1) + (1|Site/alltrees/quadrats),
        data =predherb)
plot(predherb_modelnb)
qqnorm(residuals(predherb_modelnb))
qqline(residuals(predherb_modelnb), col ="red")
summary(predherb_modelnb)
Anova(predherb_modelnb)
visreg(predherb_modelnb)


predherb_modelpoi <- 
  glmer(herb ~ 
             log(pred+1) + (1|Site/alltrees/quadrats),
             family ="poisson"(link="log"),
           data =predherb)
plot(predherb_modelpoi)
qqnorm(residuals(predherb_modelpoi))
qqline(residuals(predherb_modelpoi), col ="red")
summary(predherb_modelpoi)
Anova(predherb_modelpoi)
visreg(predherb_modelpoi)


# per tree fignolage a faire -------------------------------------------
predherb_modeltree<- 
  glmer(herbpertree ~ 
          predpertree + (1|Site/alltrees),
        family ="poisson"(link="sqrt"),
        data =predherb)
plot(predherb_modeltree)
qqnorm(residuals(predherb_modeltree))
qqline(residuals(predherb_modeltree), col ="red")
summary(predherb_modeltree)
Anova(predherb_modeltree)
visreg(predherb_modeltree)
