# Universidade Federal de Pernambuco
# Centro de Filosofia e Ciencias Humanas
# Departamento de Ciencia Politica

setwd("/Users/LUIZFERNANANDO/Desktop")
options(scipen = 9999)
options(max.print =100000)
install.packages("readxl")
library(readxl)
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

# Modelo Logístico
logit <- glm(adap ~ climate_risk + climi + pub_op_climate, 
             family = binomial)

summary(logit)

logitIT <- glm(adap ~ climate_risk + climi + pub_op_climate +
              (climate_risk*climi),
              family = binomial)

summary(logitIT)

# Resíduos
par(mfrow=c(2,1))
plot(logit, 2)
plot(logitIT, 2)

# Autocorrelacao e Heterocedasticidade
library(lmtest)
library(car)
bptest(logitIT)
dwtest(logitIT)
bgtest(logitIT)

# Estimacao Robusta
library(sandwich)
coeftest(logitIT, 
         vcov = vcovHC(logitIT, type = "HC3"))