# ilustro las regresiones poisson, quasi poisson y BN en un modelo donde la variable de respuesta
# es Behaviors y hay dos VE categoricas, qeu son DormancyTime (3 niveles) y Parasitism (2 niveles)

rm(list=ls())
library(readxl)
library(tidyverse)
library(lmtest)
library(emmeans)
library(car) # para distancia de cook
library("MASS") # para la regresion binomial negativ


setwd("C:/Users/lbabi/Google Drive/Lucia/IEGEBA/Cristian Di Batista (Mosquitos - GLM2)/datos_crudos")

datos_orig <- read_excel("cantComportamientos.xlsx")
# paso los tratamientos a factor
datos <- datos_orig %>% mutate(Parasitism = as.factor(Parasitism), 
                               DormancyTime = as.factor(DormancyTime))


str(datos)

# --------------------------------------------------
# analisis exploratorio
# --------------------------------------------------

# veo cuantos datos hay en cada tratamiento

datos %>%
  group_by(Parasitism, DormancyTime) %>%
  summarise(cont = n())

names(datos)

# histograma

ggplot(datos,aes(x = Behaviors)) + 
  geom_histogram() +
  facet_grid(Parasitism ~ DormancyTime) +
  theme_bw()

# boxplot horizontales

ggplot(datos,aes(x = Behaviors)) + 
  geom_boxplot() +
  facet_grid(Parasitism ~ DormancyTime) +
  theme_bw()

# boxplot verticales

ggplot(datos,aes(x = Behaviors)) + 
  geom_boxplot() +
  facet_grid(Parasitism ~ DormancyTime) +
  theme_bw() + 
  coord_flip()

# se ven dos outliers en el tratamiento 6 - 0

# pruebo con una distribucion poisson que se adapta a la definicion de la variable

# --------------------------------------------------
# modelos
# --------------------------------------------------

# --------------------------------------------------
# 1) modelo poisson
# --------------------------------------------------

mod_pois <- glm(Behaviors ~  Parasitism * DormancyTime, family = poisson, data = datos)
summary(mod_pois)



# evalúo la significación global del modelo 
mod_pois_nulo <- glm(Behaviors ~  1 , family = poisson, data = datos)
sal_anova <- anova(mod_pois, mod_pois_nulo, test = "Chisq")
sal_anova

# el modelo es significativo (es decir, es significativamente mejor que el modelo nulo)

# calculo el porcenaje de deviance explicada
100*(sal_anova$`Resid. Dev`[2] - sal_anova$`Resid. Dev`[1]) / sal_anova$`Resid. Dev`[2]
# 9.576203

# ---------------------------------------
# validacion del modelo
# ---------------------------------------

# a) grafico de residuos vs predichos
# ---------------------------------------

e <- resid(mod_pois, type = 'pearson')
pre <- predict(mod_pois, type = "response")
#par(mfrow=c(1,2))
plot(pre, e, xlab = "Predichos", ylab = "Residuos de pearson", cex.main = .8 )
abline(0,0)
# hay muchos residuos positivos muy grandes

# b) distancia de cook
# ---------------------------------------

plot(cooks.distance(mod_pois))
# hay un valor grande, veo a que obs corresponde

cdist <- cooks.distance(mod_pois)
datos %>% filter(cdist == max(cdist))

# DormancyTime Parasitism Behaviors
# <fct>        <fct>          <dbl>
#   1 6            0                 49

# es el valor maximo (que esta en el trat 6 - 0)

# c) parametro de sobre dispersion
# ---------------------------------------

e <- resid(mod_pois, type = 'pearson')
dispersion <- sum(e^2) / df.residual(mod_pois)
dispersion 
# 6.292688

# hay sobre dispersion, no parece haber exceso de ceros
# es mayor a 1.5 pero menor a 15 => segun Zuur habria que usar un modelo quasi poisson 

# ------------------------------
# sobre dispersion
# ------------------------------

# Zuur et al (2009) propone:
#   
# 1 <= Sobredisp <1.5 Poisson
# 
# 1.5< Sobredisp <15 o 20 modificar los EE ( cuasi Poisson o factores aleatorios)

# Sobredisp >15 o 20 utilizar otras distribuciones (binomial negativa , etc)

# ------------------------------

# tire el outlier y encontre otro
# genere un data frame sin los dos outliers y lo exporte


# leo los datos sin los outlieres
# ----------------------------------

datos_sout2 <- read.table("C:/Users/lbabi/Google Drive/Lucia/IEGEBA/Cristian Di Batista (Mosquitos - GLM2)/datos/datos_sout2.txt")
# paso los tratamientos a factor
datos_sout2 <- datos_sout2 %>% mutate(Parasitism = as.factor(Parasitism), 
                               DormancyTime = as.factor(DormancyTime))
str(datos_sout2)


# ajuste un modelo poisson a estos datos y sigue dando sobredispersion (ahora da 5.064523)
# => ajusto un quasi poisson

# --------------------------------
# 2) modelo quasi poisson
# --------------------------------------------------

mod_qpois<- glm(Behaviors ~  Parasitism * DormancyTime, family = quasipoisson, data = datos_sout2)
summary(mod_qpois)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 1.7830     0.2117   8.422 2.77e-14 ***
#   Parasitism1                -0.0830     0.3058  -0.271    0.786    
# DormancyTime4               0.3423     0.2646   1.294    0.198    
# DormancyTime6               0.4023     0.2553   1.576    0.117    
# Parasitism1:DormancyTime4  -0.3076     0.3789  -0.812    0.418    
# Parasitism1:DormancyTime6  -0.1282     0.3705  -0.346    0.730 

# no analizo bondad de ajuste porque no cambian los residuos ni los predichos respecto del modelo poisson
# solo corrige los errores standard de los estimadores


# -----------------------------------
# seleccion de variables
# -----------------------------------

# uso el test F porque en el curso de modelos mixtos dijeron que habia que usar ese para 
# los modelos quasi verosimiles (tb lo dice el help del anova)


mod_qpois_ad <- glm(Behaviors ~  Parasitism + DormancyTime, family = quasipoisson, data = datos_sout2)
anova(mod_qpois, mod_qpois_ad, test = "F")

# anova(mod_qpois, mod_qpois_ad, test = "Chisq")

# Analysis of Deviance Table
# 
# Model 1: Behaviors ~ Parasitism * DormancyTime
# Model 2: Behaviors ~ Parasitism + DormancyTime
# Resid. Df Resid. Dev Df Deviance      F Pr(>F)
# 1       150     708.81                          
# 2       152     712.48 -2   -3.678 0.3631 0.6961

# la interaccion no es significativa

# veo los p-valores de Parasitism y DormancyTime

mod_qpois_dor <- glm(Behaviors ~  DormancyTime, family = quasipoisson, data = datos_sout2)
mod_qpois_par <- glm(Behaviors ~  Parasitism, family = quasipoisson, data = datos_sout2)

anova(mod_qpois_dor, mod_qpois_ad, test = "F")
# Analysis of Deviance Table
# 
# Model 1: Behaviors ~ DormancyTime
# Model 2: Behaviors ~ Parasitism + DormancyTime
# Resid. Df Resid. Dev Df Deviance      F  Pr(>F)  
# 1       153     729.69                             
# 2       152     712.48  1   17.208 3.4474 0.06529 .

# parasitism es sign al 10 % (p-valor = 0.06529)

# anova(mod_qpois_dor, mod_qpois_ad, test = "Chisq")


anova(mod_qpois_par, mod_qpois_ad, test = "F")

# Analysis of Deviance Table
# 
# Model 1: Behaviors ~ Parasitism
# Model 2: Behaviors ~ Parasitism + DormancyTime
# Resid. Df Resid. Dev Df Deviance      F Pr(>F)
# 1       154     730.83                          
# 2       152     712.48  2   18.343 1.8374 0.1627

# dormancy no es sign

# anova(mod_qpois_par, mod_qpois_ad, test = "Chisq")


# me quedo con el modelo que solo tiene parasitism


# -----------------------------------
# presentacion de resultados
# -----------------------------------

# boxplot
# ------------------------------------
# como me quedo con el modelo que solo tiene parasitism voy a hacer un boxplot solo con esa variable


boxPar <- ggplot(datos_sout2, aes(x = Parasitism, y = Behaviors)) + 
  geom_boxplot() +
  theme_bw() 

boxPar

# summary
# -----------------------------------------------------------

summary(mod_qpois_par)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2.07060    0.09541  21.703   <2e-16 ***
#   Parasitism1 -0.25176    0.13742  -1.832   0.0689 .  


mod_qpois_nulo <- glm(Behaviors ~  1, family = quasipoisson, data = datos_sout2)
anova(mod_qpois_par, mod_qpois_nulo, test = "Chisq")

# Parasitism es sign al 10 % (p-valor = 0.0689)


# calculo medias marginales estimadas (MME) en la escala de la variable respuesta con sus IC
# --------------------------------------------

mme <-  emmeans (mod_qpois_par,  pairwise ~ Parasitism , type = "response")
mme$emmeans

# Parasitism rate    SE  df asymp.LCL asymp.UCL
# 0          7.93 0.757 Inf      6.58      9.56
# 1          6.16 0.610 Inf      5.08      7.48
# 
# Confidence level used: 0.95 
# Intervals are back-transformed from the log scale 

# grafico de barras con las MME y sus IC
# ------------------------------------------------------

mme_data <- mme$emmeans %>%
  as.data.frame()

barplot <- ggplot(mme_data, aes(x = Parasitism, y = rate)) +
  geom_col() +
  geom_errorbar( aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.4, 
                 size = 1) +
  theme_bw() + 
  scale_fill_grey() +
  labs( x = "Parasitism", y = "Behaviours mean (CI)")
barplot


# calculo el cociente entre las medias
#  ----------------------------------

mme$contrasts

# contrast ratio    SE  df z.ratio p.value
# 0 / 1     1.29 0.177 Inf 1.832   0.0669 
# 
# Tests are performed on the log scale 

# la media estimada de la cantidad de comportamientos es un 29% en el grupo no parasitado que en el 
# parasitado



# -----------------------------------------------
# 3) modelo binomaial negativo ----------------
# -----------------------------------------------

# pruebo por las dudas un BN

mod_bn <- glm.nb(Behaviors ~  Parasitism * DormancyTime, data = datos_sout2)
summary(mod_bn)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                 1.7830     0.1965   9.072   <2e-16 ***
# Parasitism1                -0.0830     0.2793  -0.297    0.766    
# DormancyTime4               0.3423     0.2592   1.321    0.187    
# DormancyTime6               0.4023     0.2507   1.605    0.109    
# Parasitism1:DormancyTime4  -0.3076     0.3566  -0.863    0.388    
# Parasitism1:DormancyTime6  -0.1282     0.3546  -0.361    0.718   


# seleccion de variables
# ------------------------------------

mod_bn_ad <- glm.nb(Behaviors ~  Parasitism + DormancyTime, data = datos_sout2)
anova(mod_bn, mod_bn_ad, test = "Chisq")

# da muy parecido a la quasi binomial, la inter no es sign
# veo los p-valores de Parasitism y DormancyTime

mod_bn_dor <- glm.nb(Behaviors ~  DormancyTime, data = datos_sout2)
mod_bn_par <- glm.nb(Behaviors ~  Parasitism, data = datos_sout2)

anova(mod_bn_dor, mod_bn_ad, test = "Chisq")

# muy parecido, parasitism es sign al 10 % 

anova(mod_bn_par, mod_bn_ad, test = "Chisq")

# muy parecido, dormancy no es sign















