
# Importing dataframe

size<-read_xlsx("fishsize.Ubatiba.xlsx")


# Creating predator parameter

size <- size %>%
  group_by(cam, site) %>%   
  mutate(pred = sum(sp %in% c("Hoplias malabaricus", "Symbranchus marmoratus", "Rhamdia sp.")))

# Creating competition parameter

size <- size %>%
  group_by(camsitesp) %>%   
  mutate(comp = n())

# Subsets by species

ast.s<- subset(size,sp=="Astyanax janeiroensis")
pim.s<-subset(size,sp=="Pimelodella lateristriga")
calli.s<-subset(size,sp=="Callichthys callichthys")
deut.s<-subset(size,sp=="Deuterodon cf. hastatus")
geo.s<-subset(size,sp=="Geophagus brasiliensis")
hop.s<-subset(size, sp=="Hoplias malabaricus")
hypos.s<-subset(size,sp=="Hypostomus cf. punctatus")
mima.s<-subset(size,sp=="Mimagoniates microlepis")

# Tranforming sampling campaign in factor

ast.s$cam<-factor(ast.s$cam)
pim.s$cam<-factor(pim.s$cam)
calli.s$cam<-factor(calli.s$cam)
deut.s$cam<-factor(deut.s$cam)
geo.s$cam<-factor(geo.s$cam)
hop.s$cam<-factor(hop.s$cam)
hypos.s$cam<-factor(hypos.s$cam)
mima.s$cam<-factor(mima.s$cam)
