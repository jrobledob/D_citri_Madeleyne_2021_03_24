#libraries----
library(dplyr)
library(ggplot2)
library(car)
library(agricolae)
library(officer)
library(rvg)

#Readin Data base----
#Field data base
db_D_citri_field<- read.csv2("./Data/BD Dinámica Poblacional -D. citri Caribia_Campo_Actualizada_21_12_2020 (1).csv", encoding = "UTF-8")

#Data cleaning----
#No important columns are deleted
db_D_citri_field<- db_D_citri_field[c("MES", "CULTIVAR", "VARIEDAD", "PORTAINJERTO", "REPETICION", 
                                       "PUNTO", "BROTES", "BDcitri")]
#"BROTES" variable is reformated 
db_D_citri_field$BROTES<- as.integer(db_D_citri_field$BROTES)
#Percentages of buds infested are calculated
db_D_citri_field$`Brotes afectados (%)`<- round((db_D_citri_field$BDcitri/db_D_citri_field$BROTES)*100, digits = 0)
#Missing data is not taking into account
db_D_citri_field<- db_D_citri_field[!is.na(db_D_citri_field$`Brotes afectados (%)`),]
#Set factors
#11 levels in "MES" factor. 4th month is not present
db_D_citri_field$MES<- as.factor(db_D_citri_field$MES)
#3 levels: Naranja mandarina y limón 
db_D_citri_field$CULTIVAR<- as.factor(db_D_citri_field$CULTIVAR)
#16 varieties
db_D_citri_field$VARIEDAD<- as.factor(db_D_citri_field$VARIEDAD)
#2 Rootstocks
db_D_citri_field$PORTAINJERTO<- as.factor(db_D_citri_field$PORTAINJERTO)
#4 Cardinal points
db_D_citri_field$PUNTO<- as.factor(db_D_citri_field$PUNTO)


#Descriptive statistics----
#Observations and distribution 
summary(db_D_citri_field)
#summary statistics byb factors
#MES
with(db_D_citri_field, tapply(`Brotes afectados (%)`, MES, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
ggplot(db_D_citri_field, aes(`Brotes afectados (%)`, fill = MES)) +
  geom_histogram(binwidth=20, position="dodge")
#CULTIVAR
with(db_D_citri_field, tapply(`Brotes afectados (%)`, CULTIVAR, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
ggplot(db_D_citri_field, aes(`Brotes afectados (%)`, fill = CULTIVAR)) +
  geom_histogram(binwidth=20, position="dodge")
#VARIEDAD
with(db_D_citri_field, tapply(`Brotes afectados (%)`, VARIEDAD, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
ggplot(db_D_citri_field, aes(`Brotes afectados (%)`, fill = VARIEDAD)) +
  geom_histogram(binwidth=20, position="dodge")
#PORTAINJERTO
with(db_D_citri_field, tapply(`Brotes afectados (%)`, PORTAINJERTO, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
ggplot(db_D_citri_field, aes(`Brotes afectados (%)`, fill = PORTAINJERTO)) +
  geom_histogram(binwidth=20, position="dodge")
#PUNTO
with(db_D_citri_field, tapply(`Brotes afectados (%)`, PUNTO, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
ggplot(db_D_citri_field, aes(`Brotes afectados (%)`, fill = PUNTO)) +
  geom_histogram(binwidth=20, position="dodge")



#Model poisson distribution https://stats.idre.ucla.edu/r/dae/poisson-regression/----
summary(m1 <- glm(`Brotes afectados (%)` ~ MES+CULTIVAR+VARIEDAD+PORTAINJERTO+PUNTO, family="poisson", data=db_D_citri_field))
# cov.m1 <- vcovHC(m1, type="HC0")
# std.err <- sqrt(diag(cov.m1))
# r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
#                "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
#                LL = coef(m1) - 1.96 * std.err,
#                UL = coef(m1) + 1.96 * std.err)
# 
# r.est




#Non-parametric approach----
#Boxplot MES
ggplot(data = db_D_citri_field, mapping = aes(x = MES, y = `Brotes afectados (%)`, colour = MES)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "none")
#Distribution MES
dis<- ggplot(data = db_D_citri_field, mapping = aes(x = `Brotes afectados (%)`, colour = MES)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(. ~ MES) +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45))
p_dml <-dml(ggobj = dis)
read_pptx() %>%
  add_slide() %>%
  ph_with(p_dml, ph_location()) %>%
  base::print("./Plots/1 Meses_distribuciones.pptx")

##Note: According to distribution plots moths months with same distribition could be:
##group_1<- c(1:3)
##group_2<- c(5:6,8)
##group_3<- c(7,9,10:12)
#KW test group_1
group_1_month<- kruskal(db_D_citri_field[db_D_citri_field$MES %in% c(1:3),]$`Brotes afectados (%)`, db_D_citri_field[db_D_citri_field$MES %in% c(1:3),]$MES,group=TRUE, p.adj = "bonferroni")
group_1_month
#KW test group_2
group_2_month<- kruskal(db_D_citri_field[db_D_citri_field$MES %in% c(5:6,8),]$`Brotes afectados (%)`, db_D_citri_field[db_D_citri_field$MES %in% c(5:6,8),]$MES,group=TRUE, p.adj = "bonferroni")
group_2_month
#KW test group_3
group_3_month<- kruskal(db_D_citri_field[db_D_citri_field$MES %in% c(7,9,10:12),]$`Brotes afectados (%)`, db_D_citri_field[db_D_citri_field$MES %in% c(7,9,10:12),]$MES,group=TRUE, p.adj = "bonferroni")
group_3_month

#Boxplot CULTIVAR
ggplot(data = db_D_citri_field, mapping = aes(x = CULTIVAR, y = `Brotes afectados (%)`, colour = CULTIVAR)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "none")
#Distribution CULTIVAR
dis<- ggplot(data = db_D_citri_field, mapping = aes(x = `Brotes afectados (%)`, colour = CULTIVAR)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(. ~ CULTIVAR) +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45))
p_dml <-dml(ggobj = dis)
read_pptx() %>%
  add_slide() %>%
  ph_with(p_dml, ph_location()) %>%
  base::print("./Plots/2 cultivar_distribuciones.pptx")

##Note: According to distribution plots CULTIVAR with same distribition could be: Limón, Mandarina, and Naranja
#KW test group_1
group_CULTIVAR<- kruskal(db_D_citri_field$`Brotes afectados (%)`, db_D_citri_field$CULTIVAR,group=TRUE, p.adj = "bonferroni")
group_CULTIVAR


#Boxplot VARIEDAD
ggplot(data = db_D_citri_field, mapping = aes(x = VARIEDAD, y = `Brotes afectados (%)`, colour = VARIEDAD)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "none")
#Distribution VARIEDAD
dis<- ggplot(data = db_D_citri_field, mapping = aes(x = `Brotes afectados (%)`, colour = VARIEDAD)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(. ~ VARIEDAD) +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45))
p_dml <-dml(ggobj = dis)
read_pptx() %>%
  add_slide() %>%
  ph_with(p_dml, ph_location()) %>%
  base::print("./Plots/3 Variedad_distribuciones.pptx")

##Note: According to distribution plots VARIEDAD with same distribition could be: 
#group 1: all except oneco
#group 2: oneco 
#However kw test coulb be done with all varieties
#KW test group_1
group_VARIEDAD<- kruskal(db_D_citri_field$`Brotes afectados (%)`, db_D_citri_field$VARIEDAD,group=TRUE, p.adj = "bonferroni")
group_VARIEDAD

#Boxplot PORTAINJERTO
ggplot(data = db_D_citri_field, mapping = aes(x = PORTAINJERTO, y = `Brotes afectados (%)`, colour = PORTAINJERTO)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "none")
#Distribution PORTAINJERTO
dis<- ggplot(data = db_D_citri_field, mapping = aes(x = `Brotes afectados (%)`, colour = PORTAINJERTO)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(. ~ PORTAINJERTO) +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45))
p_dml <-dml(ggobj = dis)
read_pptx() %>%
  add_slide() %>%
  ph_with(p_dml, ph_location()) %>%
  base::print("./Plots/4 Portainjerto_distribuciones.pptx")

##Note: According to distribution plots PORTAINJERTO with same distribition could be: CPB AND SXE
#KW test group_1
group_PORTAINJERTO<- kruskal(db_D_citri_field$`Brotes afectados (%)`, db_D_citri_field$PORTAINJERTO,group=TRUE, p.adj = "bonferroni")
group_PORTAINJERTO

#Boxplot PUNTO
ggplot(data = db_D_citri_field, mapping = aes(x = PUNTO, y = `Brotes afectados (%)`, colour = PUNTO)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "none")
#Distribution PUNTO
dis<- ggplot(data = db_D_citri_field, mapping = aes(x = `Brotes afectados (%)`, colour = PUNTO)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(. ~ PUNTO) +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45))
p_dml <-dml(ggobj = dis)
read_pptx() %>%
  add_slide() %>%
  ph_with(p_dml, ph_location()) %>%
  base::print("./Plots/5 Punto_distribuciones.pptx")

##Note: According to distribution plots PUNTO with same distribition could be: CPB AND SXE
#KW test group_1
group_PUNTO<- kruskal(db_D_citri_field$`Brotes afectados (%)`, db_D_citri_field$PUNTO,group=TRUE, p.adj = "bonferroni")
group_PUNTO

