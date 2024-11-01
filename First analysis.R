
#Astyanax janeiroensis

plot(density(ast.s$length)) # Checking minimal acceptable normality

lm.ast<-lm(length~du, data=ast.s) # Model

# Tests Post-Hoc
anova(lm.ast)
summary(lm.ast)
tab_model(lm.ast)
plot(lm.ast, col.line=1)
plot(density(residuals(lm.ast)))

#Astyanax hastatus

plot(density(deut.s$length))

lm.deut<-lm(length~du, data=deut.s)

anova(lm.deut)
summary(lm.deut)
tab_model(lm.deut)
plot(lm.deut, col.line=1)
plot(density(residuals(lm.deut)))

#Mimagoniates microlepis

plot(density(mima.s$length))

mima.s <- mima.s[!(mima.s$length > 5.2), ] # Adjusting normality

lm.mima<-lm(length~du, data=mima.s)

anova(lm.mima)
summary(lm.mima)
tab_model(lm.mima)
plot(lm.mima, col.line=1)
plot(density(residuals(lm.mima)))

#Pimelodella lateristriga

plot(density(pim.s$length))

pim.s2 <- pim.s[!(pim.s$length > 10), ]

lm.pimelo<-lm(length~du, data=pim.s)

anova(lm.pimelo)
summary(lm.pimelo)
tab_model(lm.pimelo)
plot(lm.pimelo, col.line=1)
plot(density(residuals(lm.pimelo)))

#Callichthys callichthys

plot(density(calli.s$length))

calli.s <- calli.s[!(calli.s$length > 12), ]

lm.calli<-lm(length~du, data=calli.s)

anova(lm.calli)
summary(lm.calli)
tab_model(lm.calli)
plot(lm.calli, col.line=1)
plot(density(residuals(lm.calli)))

#Hoplias malabaricus

plot(density(hop.s$length))
lm.hop<-lm(length~du, data=hop.s)

anova(lm.hop)
summary(lm.hop)
tab_model(lm.hop)
plot(lm.hop, col.line=1)
plot(density(residuals(lm.hop)))

#Geophagus brasiliensis

plot(density(geo.s$length))

geo.s<- geo.s[!(geo.s$length > 13), ]

glm.geo<-glm(length~du,family=Gamma,data=geo.s)

anova(glm.geo, test = "F")
summary(glm.geo)
tab_model(glm.geo)
plot(glm.geo, col.line=1)
plot(density(residuals(glm.geo)))

#Hypostomus punctatus

plot(density(hypos.s$length))

hypos.s2 <- hypos.s[!(hypos.s$length > 15), ]

glm.hypo<-glm(length~du,family=Gamma,data=hypos.s)

anova(glm.hypo, test = "F")
summary(glm.hypo)
tab_model(glm.hypo)
plot(glm.hypo, col.line=1)
plot(density(residuals(glm.hypo)))