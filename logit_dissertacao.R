# Universidade Federal de Pernambuco
# Centro de Filosofia e Ciencias Humanas
# Departamento de Ciencia Politica

setwd("/Users/LUIZFERNANANDO/Desktop")
options(scipen = 9999)
options(max.print =100000)
install.packages("readxl")
install.packages("mfx")
install.packages("tidyverse")
install.packages("broom")
library(tidyverse)
library(broom)
library(readxl)
library(mfx)
theme_set(theme_classic())
database <- read_excel("database.xlsx")

# Sumário Estatístico
adap <- database$adap_gcf
adap <- as.numeric(adap)
summary(adap)
sd(adap)

climate_risk <- database$climate_risk
climate_risk <- as.numeric(climate_risk)
summary(climate_risk)
sd(climate_risk)

climi <- database$climi
climi <- as.numeric(climi)
summary(climi)
sd(climi)

pub_op_climate <- database$pub_op_climate
pub_op_climate <- as.numeric(pub_op_climate)
summary(pub_op_climate)
sd(pub_op_climate)

pib_percap <- database$pib_percap
pib_percap <- as.numeric(pib_percap)
summary(pib_percap)
sd(pib_percap)

finance <- data.frame(adap, climate_risk, climi, pub_op_climate, pib_percap)

# Modelo Logístico Principal
logitmfx(adap ~ climate_risk + climi + pub_op_climate, data = finance)

# Usando Pib per capita
logitmfx(adap ~ climate_risk + climi + pub_op_climate + pib_percap, data = finance)

# Interação entre per capita e climi
logitmfx(adap ~ climate_risk + climi + pub_op_climate + pib_percap + 
           climi*pib_percap,  data = finance)

# Diagnósticos
# Linearidade
logit <- glm(adap ~ climate_risk + climi + pub_op_climate, 
             family = binomial)

probabilities <- predict(logit, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)

# Predictors
mydata <- finance %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)

# Dados para plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

# Valores influentes
plot(logit, which = 4, id.n = 3)
# Resíduos padrão e distância de Cook
model.data <- augment(logit) %>% 
  mutate(index = 1:n())
model.data %>% top_n(3, .cooksd)

#plot
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = adap), alpha = .5) +
  theme_bw()
