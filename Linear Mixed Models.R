
###Deuterodon janeiroensis

lmm.ast.neut<-lmer(length~1+(1|cam), data=ast.s) # Neutral model
lmm.ast<-lmer(length~du+food+pred+comp+(1|cam), data=ast.s) # Mixed model

# Test Post-Hoc
summary(lmm.ast)
tab_model(lmm.ast)
plot(lmm.ast, col.line=1)
lattice::qqmath(lmm.ast)
plot(density(residuals(lmm.ast)))
anova(lmm.ast)
# Test multicolinearity
vif(lmm.ast)
# AIC Test neutral x predictors models
AIC(lmm.ast.neut, lmm.ast)

###Deuterodon hastatus

lmm.deut.neut<-lmer(length~1+(1|cam), data=deut.s)
lmm.deut<-lmer(length~du+food+pred+comp+(1|cam), data=deut.s)

summary(lmm.deut)
tab_model(lmm.deut)
plot(lmm.deut, col.line=1)
lattice::qqmath(lmm.deut)
plot(density(residuals(lmm.deut)))
anova(lmm.deut)
vif(lmm.deut)
AIC(lmm.deut.neut, lmm.deut)

###Mimagoniates 

lmm.mima.neut<-lmer(length~1+(1|cam), data=mima.s)
lmm.mima<-lmer(length~du+food+pred+comp+(1|cam), data=mima.s)

summary(lmm.mima)
tab_model(lmm.mima)
plot(lmm.mima, col.line=1)
lattice::qqmath(lmm.mima)
plot(density(residuals(lmm.mima)))
anova(lmm.mima)
vif(lmm.mima)
AIC(lmm.mima.neut, lmm.mima)

#Pimelodella lateristriga

lmm.pim.neut<-lmer(length~1+(1|cam), data=pim.s)
lmm.pim<-lmer(length~du+food+pred+comp+(1|cam), data=pim.s)

summary(lmm.pim)
tab_model(lmm.pim)
plot(lmm.pim, col.line=1)
lattice::qqmath(lmm.pim)
plot(density(residuals(lmm.pim)))
anova(lmm.pim)
vif(lmm.pim)
AIC(lmm.pim.neut, lmm.pim)

#Hoplias

lmm.hop.neut<-lmer(length~1+(1|cam), data=hop.s)
lmm.hop<-lmer(length~du+food+comp+(1|cam), data=hop.s)

summary(lmm.hop)
tab_model(lmm.hop)
plot(lmm.hop, col.line=1)
lattice::qqmath(lmm.hop)
plot(density(residuals(lmm.hop)))
anova(lmm.hop)
vif(lmm.hop)
AIC(lmm.hop.neut, lmm.hop)
