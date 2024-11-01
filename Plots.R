

# Deuterodon janeiroensis

# Model fitted values

ast.s <- ast.s %>%ungroup() %>%
  mutate(fit.m.ast = predict(lmm.ast, re.form = NA),  # Marginal fit
         fit.c.ast = predict(lmm.ast, re.form = NULL),  # Conditional fit
         resid = resid(lmm.ast))  # Residuals

# PLOT 

ast.s %>%
  ggplot(aes(x = dd, y = fit.m.ast + resid)) +
  geom_jitter(width=0.5, alpha=1, color="grey")+
  geom_smooth(aes(x = dd, y = fit.c.ast, fill=cam), method="lm",se=F,linewidth=1.2,color="azure4")+
  geom_smooth(method="lm",se=F,linewidth=1.2,color="black")+
  xlab("Distance upstream (km)")+ ylab("Fish length (cm)")+
  theme(legend.title = element_blank())+theme_classic()+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size = 25), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size = 25), 
        panel.grid.major = element_line(colour="white"), 
        panel.grid.minor = element_line(colour="white"), 
        axis.text.y = element_text(size = 25, colour = "black", margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 25, colour = "black", margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        strip.text.y = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size=15))+
  scale_x_continuous(breaks = c(1,3,5,7,9,11,13,15))

#Deuterodon hastatus

deut.s <- deut.s %>%ungroup() %>%
  mutate(fit.m.deut = predict(lmm.deut, re.form = NA),  
         fit.c.deut = predict(lmm.deut, re.form = NULL),  
         resid = resid(lmm.deut)) 

deut.s %>%
  ggplot(aes(x = dd, y = fit.m.deut + resid)) +
  geom_jitter(width=0.5, alpha=1, color="grey")+
  geom_smooth(aes(x = dd, y = fit.c.deut, fill=cam), method="lm",se=F,linewidth=1.2,color="azure4")+
  geom_smooth(method="lm",se=F,linewidth=1.2,color="black")+
  xlab("Distance upstream (km)")+ ylab("Fish length (cm)")+
  theme(legend.title = element_blank())+theme_classic()+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size = 25), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size = 25), 
        panel.grid.major = element_line(colour="white"), 
        panel.grid.minor = element_line(colour="white"), 
        axis.text.y = element_text(size = 25, colour = "black", margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 25, colour = "black", margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        strip.text.y = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size=15))+
  scale_x_continuous(breaks = c(1,3,5,7,9,11,13,15))


#Pimelodella lateristriga

pim.s <- pim.s %>% ungroup()%>%
  mutate(fit.m.pim = predict(lmm.pim, re.form = NA),  # Ajuste marginal
         fit.c.pim = predict(lmm.pim, re.form = NULL),  # Ajuste condicional
         resid = resid(lmm.pim))  # Resíduos

pim.s %>%
  ggplot(aes(x = dd, y = fit.m.pim + resid)) +
  geom_jitter(width=0.5, alpha=1, color="grey")+
  geom_smooth(aes(x = dd, y = fit.c.pim, fill=cam), method="lm",se=F,linewidth=1.2,color="azure4")+
  geom_smooth(method="lm",se=F,linewidth=1.2,color="black")+
  xlab("Distance upstream (km)")+ ylab("Marginal fit + Residuals")+
  theme(legend.title = element_blank())+theme_classic()+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size = 25), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size = 25), 
        panel.grid.major = element_line(colour="white"), 
        panel.grid.minor = element_line(colour="white"), 
        axis.text.y = element_text(size = 25, colour = "black", margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 25, colour = "black", margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        strip.text.y = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size=15))+
  scale_x_continuous(breaks = c(1,3,5,7,9,11,13,15))

#Mimagoniates microlepis

mima.s <- mima.s %>% ungroup()%>%
  mutate(fit.m.mima = predict(lmm.mima, re.form = NA),  # Ajuste marginal
         fit.c.mima = predict(lmm.mima, re.form = NULL),  # Ajuste condicional
         resid = resid(lmm.mima))  # Resíduos

mima.s %>%
  ggplot(aes(x = dd, y = fit.m.mima + resid)) +
  geom_jitter(width=0.5, alpha=1, color="grey")+
  geom_smooth(aes(x = dd, y = fit.c.mima, fill=cam), method="lm",se=F,linewidth=1.2,color="azure4")+
  geom_smooth(method="lm",se=F,linewidth=1.2,color="black")+
  xlab("Distance upstream (km)")+ ylab("Marginal fit + Residuals")+
  theme(legend.title = element_blank())+theme_classic()+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size = 25), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size = 25), 
        panel.grid.major = element_line(colour="white"), 
        panel.grid.minor = element_line(colour="white"), 
        axis.text.y = element_text(size = 25, colour = "black", margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 25, colour = "black", margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        strip.text.y = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size=15))+
  scale_x_continuous(breaks = c(1,3,5,7,9,11,13,15))