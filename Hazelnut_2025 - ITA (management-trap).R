# 0. Caricamento del file Excel
library(readxl)
library(lme4)
library(multcompView)
library(emmeans)
library(multcomp)
library(dplyr)
library(ggplot2)
library(Matrix)
library(AICcmodavg)
library(glmmTMB)
library(pscl)
library(lsmeans)
library(DHARMa)
library("bbmle") ## for AICtab
library(performance)
library(broom.mixed)


df <- read_excel("C:/Users/luigi/Desktop/Georgia/Hazelnut monitoring_22_23_24.xlsx", sheet = 1)


# Preparazione dataset
df$Anno <- format(df$Data, "%Y")
df$Mese <- format(df$Data, "%m")
df_filtrato <- subset(df, Conteggio != -9999 & Anno != 2022 & !(Mese %in% c("11")))
df_filtrato <- df_filtrato[!is.na(df_filtrato$Conteggio), ]
df_filtrato$Sito <- as.factor(df_filtrato$Sito)
livelli_specie <- unique(df_filtrato$Specie)
df_filtrato$Anno <- as.factor(df_filtrato$Anno)


# GLM negative binomial random effect
# Modello 1: solo SPECIE
mod1 <- glmer.nb(Conteggio ~ Specie + (1 | Anno) + (1 | Code/Blocco), data = df_filtrato)

##Controllo overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type="pearson")
  chi_sq <- sum(rp^2)
  ratio <- chi_sq / rdf
  return(ratio)
}
overdisp_fun(mod1)

###Controllo zero-inflation
check_zeroinflation(mod1)

####Residual diagnostics
sim_res <- simulateResiduals(fittedModel = mod1, n = 1000)
plot(sim_res)         # residui vs predetti, distribuzione, qqplot
testDispersion(sim_res)  # controlla dispersion
testUniformity(sim_res)  # verifica uniformità dei residui
testZeroInflation(sim_res)

####intervalli confidenza
tidy(mod1, effects = "fixed", conf.int = TRUE, conf.method = "Wald")



# Modello 2: TRAPPOLA
mod2 <- glmer.nb(Conteggio ~ Trappola + (1 | Anno) + (1 | Code/Blocco), data = df_filtrato)

##Controllo overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type="pearson")
  chi_sq <- sum(rp^2)
  ratio <- chi_sq / rdf
  return(ratio)
}
overdisp_fun(mod2)

###Controllo zero-inflation
check_zeroinflation(mod2)

####Residual diagnostics
sim_res <- simulateResiduals(fittedModel = mod2, n = 1000)
plot(sim_res)         # residui vs predetti, distribuzione, qqplot
testDispersion(sim_res)  # controlla dispersion
testUniformity(sim_res)  # verifica uniformità dei residui
testZeroInflation(sim_res)

####intervalli confidenza
tidy(mod2, effects = "fixed", conf.int = TRUE, conf.method = "Wald")




# Modello 3: GESTIONE
mod3 <- glmer.nb(Conteggio ~ Gestione + (1 | Anno) + (1 | Code/Blocco), data = df_filtrato)

##Controllo overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type="pearson")
  chi_sq <- sum(rp^2)
  ratio <- chi_sq / rdf
  return(ratio)
}
overdisp_fun(mod3)

###Controllo zero-inflation
check_zeroinflation(mod3)

####Residual diagnostics
sim_res <- simulateResiduals(fittedModel = mod3, n = 1000)
plot(sim_res)         # residui vs predetti, distribuzione, qqplot
testDispersion(sim_res)  # controlla dispersion
testUniformity(sim_res)  # verifica uniformità dei residui
testZeroInflation(sim_res)

####intervalli confidenza
tidy(mod3, effects = "fixed", conf.int = TRUE, conf.method = "Wald")


#####

# ANALISI 1 ================ n° catture x specie

print(summary(mod1))

# Pairwise comparison:
marginal = emmeans(mod1, ~ Specie)
pairs(marginal, adjust="bh")

# Letters of significance:
lettere <- cld(marginal, alpha=0.05, Letters=letters, adjust="bh")
print(lettere)

#Grafico:
barplot_species_effect <- ggplot(data = df_filtrato, aes(x = Specie, y = Conteggio)) +
  stat_summary(fun = "mean", geom = "bar", width = 0.5,  
               fill = c("orchid3", "darkseagreen3", "lightsalmon", "burlywood3", "lightskyblue2")) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", 
               width = 0.2, colour = "black") +
  scale_x_discrete(labels = c(
    "A. dispar" = expression(italic("A. dispar")),
    "H. eruditus" = expression(italic("H. eruditus")),
    "X. germanus" = expression(italic("X. germanus")),
    "X. monographus" = expression(italic("X. monographus")),
    "X. saxesenii" = expression(italic("X. saxesenii"))
  )) +
  labs(x = "Species", y = "Mean ± SE of catches per trap") +
  theme(
    panel.background = element_rect(fill = "#f2f2f2", color = NA),
    plot.background = element_rect(fill = "#f9f9f9", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5, family = "sans"),
    axis.text.y = element_text(size = 14, family = "sans"),
    axis.title.x = element_text(size = 14, family = "sans", 
                                margin = margin(t = 15)),  # spazio sopra il titolo x
    axis.title.y = element_text(size = 14, family = "sans", 
                                margin = margin(r = 10)),  # spazio a destra del titolo y
    legend.title = element_text(size = 14, family = "sans"),
    legend.text = element_text(size = 12, family = "sans")
  )
plot(barplot_species_effect)

ggsave("barplot_species_effect_ITA.png",       # nome file
       plot = barplot_species_effect,          # oggetto grafico
       width = 8, height = 6, units = "in",    # dimensioni (in pollici)
       dpi = 300,                              # risoluzione (300 = stampa, 150 = schermo)
       bg = "white")                           # sfondo bianco




# ANALISI 2 ================ n° catture x tipo trappola
print(summary(mod2))

# Pairwise comparison:
marginal = emmeans(mod2, ~ Trappola)
pairs(marginal, adjust="bh")

# Letters of significance:
lettere <- cld(marginal, alpha=0.05, Letters=letters, adjust="bh")
print(lettere)

#Grafico:
barplot_traps_effect <- ggplot(data = df_filtrato, aes(x = Trappola, y = Conteggio, )) +
  stat_summary(fun = "mean", geom = "bar", width = 0.5,  
               fill = c("#F08048","#749EB5", "#8BAF92")) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", 
               width = 0.2, colour = "black") +
  labs(x = "Trap type", y = "Mean ± SE of catches per trap") +
  theme(
    panel.background = element_rect(fill = "#f2f2f2", color = NA),
    plot.background = element_rect(fill = "#f9f9f9", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5, family = "sans"),
    axis.text.y = element_text(size = 14, family = "sans"),
    axis.title.x = element_text(size = 14, family = "sans", 
                                margin = margin(t = 15)),  # spazio sopra il titolo x
    axis.title.y = element_text(size = 14, family = "sans", 
                                margin = margin(r = 10)),  # spazio a destra del titolo y
    legend.title = element_text(size = 14, family = "sans"),
    legend.text = element_text(size = 12, family = "sans")
  )
plot(barplot_traps_effect)

ggsave("barplot_traps_effect.png",       # nome file
       plot = barplot_traps_effect,          # oggetto grafico
       width = 8, height = 6, units = "in",    # dimensioni (in pollici)
       dpi = 300,                              # risoluzione (300 = stampa, 150 = schermo)
       bg = "white")                           # sfondo bianco





# ANALISI 3 ================ n° catture x tipo gestione
print(summary(mod3))

# Pairwise comparison:
marginal = emmeans(mod3, ~ Gestione)
pairs(marginal, adjust="bh")

# Letters of significance:
lettere <- cld(marginal, alpha=0.05, Letters=letters, adjust="bh")
print(lettere)

#Grafico:
barplot_management_effect <- ggplot(data = df_filtrato, aes(x = Gestione, y = Conteggio, )) +
  stat_summary(fun = "mean", geom = "bar", width = 0.5,  
               fill = c("#C3C4D6", "#D08046", "#CFC45A")) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", 
               width = 0.2, colour = "black") +
  scale_x_discrete(labels = c("INT" = "IPM", "BIO" = "ORG", "RIN" = "REN")) +
  labs(x = "Management type", y = "Mean ± SE of catches per trap") +
  theme(
    panel.background = element_rect(fill = "#f2f2f2", color = NA),
    plot.background = element_rect(fill = "#f9f9f9", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5, family = "sans"),
    axis.text.y = element_text(size = 14, family = "sans"),
    axis.title.x = element_text(size = 14, family = "sans", 
                                margin = margin(t = 15)),  # spazio sopra il titolo x
    axis.title.y = element_text(size = 14, family = "sans", 
                                margin = margin(r = 10)),  # spazio a destra del titolo y
    legend.title = element_text(size = 14, family = "sans"),
    legend.text = element_text(size = 12, family = "sans")
  )
plot(barplot_management_effect)

ggsave("barplot_management_effect.png",       # nome file
       plot = barplot_management_effect,          # oggetto grafico
       width = 8, height = 6, units = "in",    # dimensioni (in pollici)
       dpi = 300,                              # risoluzione (300 = stampa, 150 = schermo)
       bg = "white")                           # sfondo bianco




# # # # # # # modelli gestione specie singole # # # # # # 

#SAXE
# Modello 3.1: 1 SPECIE + GESTIONE
df_subset <- subset(df_filtrato, Specie == "X. saxesenii")

# Costruisci il modello
mod3.1_sax <- glmer.nb(Conteggio ~ Gestione + (1 | Anno) + (1 | Code/Blocco), data = df_subset)

##Controllo overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type="pearson")
  chi_sq <- sum(rp^2)
  ratio <- chi_sq / rdf
  return(ratio)
}
overdisp_fun(mod3.1_sax)

###Controllo zero-inflation
check_zeroinflation(mod3.1_sax)

####Residual diagnostics
sim_res <- simulateResiduals(fittedModel = mod3.1_sax, n = 1000)
plot(sim_res)         # residui vs predetti, distribuzione, qqplot
testDispersion(sim_res)  # controlla dispersion
testUniformity(sim_res)  # verifica uniformità dei residui
testZeroInflation(sim_res)

tidy(mod3.1_sax, effects = "fixed", conf.int = TRUE, conf.method = "Wald")

# Stampa il modello
print(summary(mod3.1_sax))
# Pairwise comparison:
marginal = emmeans(mod3.1_sax, ~ Gestione)
pairs(marginal, adjust="bh")
# Letters of significance:
lettere <- cld(marginal, alpha=0.05, Letters=letters, adjust="bh")
print(lettere)


# ANALISI 3.1 ================ n° catture x tipo gestione per specie
#Grafico:
barplot_management_effect_SP <- ggplot(data = df_subset, aes(x = Gestione, y = Conteggio, )) +
  stat_summary(fun = "mean", geom = "bar", width = 0.5,  
               fill = c("#C3C4D6", "#D08046", "#CFC45A")) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", 
               width = 0.2, colour = "black") +
  scale_x_discrete(labels = c("INT" = "IPM", "BIO" = "ORG", "RIN" = "REN")) +
  labs(x = "Management type", y = "Mean ± SE of catches per trap") +
  theme(
    panel.background = element_rect(fill = "#f2f2f2", color = NA),
    plot.background = element_rect(fill = "#f9f9f9", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5, family = "sans"),
    axis.text.y = element_text(size = 14, family = "sans"),
    axis.title.x = element_text(size = 14, family = "sans", 
                                margin = margin(t = 15)),  # spazio sopra il titolo x
    axis.title.y = element_text(size = 14, family = "sans", 
                                margin = margin(r = 10)),  # spazio a destra del titolo y
    legend.title = element_text(size = 14, family = "sans"),
    legend.text = element_text(size = 12, family = "sans")
  )
plot(barplot_management_effect_SP)

ggsave(sprintf("barplot_management_effect_SP_Xsaxesenii.png"),       # nome file
       plot = barplot_management_effect_SP,          # oggetto grafico
       width = 8, height = 6, units = "in",    # dimensioni (in pollici)
       dpi = 300,                              # risoluzione (300 = stampa, 150 = schermo)
       bg = "white")                           # sfondo bianco




# SPECIE ANALIZZATA: "A. dispar"

# Filtra il dataset per la singola species
df_subset <- subset(df_filtrato, Specie == "A. dispar")

# Costruisci il modello
mod3.1_dis <- glmer.nb(Conteggio ~ Gestione + (1 | Anno) + (1 | Code/Blocco), data = df_subset)

##Controllo overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type="pearson")
  chi_sq <- sum(rp^2)
  ratio <- chi_sq / rdf
  return(ratio)
}
overdisp_fun(mod3.1_dis)

###Controllo zero-inflation
check_zeroinflation(mod3.1_dis)

####Residual diagnostics
sim_res <- simulateResiduals(fittedModel = mod3.1_dis, n = 1000)
plot(sim_res)         # residui vs predetti, distribuzione, qqplot
testDispersion(sim_res)  # controlla dispersion
testUniformity(sim_res)  # verifica uniformità dei residui
testZeroInflation(sim_res)

tidy(mod3.1_dis, effects = "fixed", conf.int = TRUE, conf.method = "Wald")


# Stampa il modello
print(summary(mod3.1_dis))
# Pairwise comparison:
marginal = emmeans(mod3.1_dis, ~ Gestione)
pairs(marginal, adjust="bh")
# Letters of significance:
lettere <- cld(marginal, alpha=0.05, Letters=letters, adjust="bh")
print(lettere)


# ANALISI 3.1 ================ n° catture x tipo gestione per specie
#Grafico:
barplot_management_effect_SP <- ggplot(data = df_subset, aes(x = Gestione, y = Conteggio, )) +
  stat_summary(fun = "mean", geom = "bar", width = 0.5,  
               fill = c("#C3C4D6", "#D08046", "#CFC45A")) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", 
               width = 0.2, colour = "black") +
  scale_x_discrete(labels = c("INT" = "IPM", "BIO" = "ORG", "RIN" = "REN")) +
  labs(x = "Management type", y = "Mean ± SE of catches per trap") +
  theme(
    panel.background = element_rect(fill = "#f2f2f2", color = NA),
    plot.background = element_rect(fill = "#f9f9f9", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5, family = "sans"),
    axis.text.y = element_text(size = 14, family = "sans"),
    axis.title.x = element_text(size = 14, family = "sans", 
                                margin = margin(t = 15)),  # spazio sopra il titolo x
    axis.title.y = element_text(size = 14, family = "sans", 
                                margin = margin(r = 10)),  # spazio a destra del titolo y
    legend.title = element_text(size = 14, family = "sans"),
    legend.text = element_text(size = 12, family = "sans")
  )
plot(barplot_management_effect_SP)

ggsave(sprintf("barplot_management_effect_SP_Adispar.png"),       # nome file
       plot = barplot_management_effect_SP,          # oggetto grafico
       width = 8, height = 6, units = "in",    # dimensioni (in pollici)
       dpi = 300,                              # risoluzione (300 = stampa, 150 = schermo)
       bg = "white")      





# SPECIE ANALIZZATA: "X. germanus"
# Filtra il dataset per la singola species
df_subset <- subset(df_filtrato, Specie == "X. germanus")

# Costruisci il modello
mod3.1_ger <- glmer.nb(Conteggio ~ Gestione + (1 | Anno) + (1 | Code/Blocco), data = df_subset)

##Controllo overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type="pearson")
  chi_sq <- sum(rp^2)
  ratio <- chi_sq / rdf
  return(ratio)
}
overdisp_fun(mod3.1_ger)

###Controllo zero-inflation
check_zeroinflation(mod3.1_ger)

####Residual diagnostics
sim_res <- simulateResiduals(fittedModel = mod3.1_ger, n = 1000)
plot(sim_res)         # residui vs predetti, distribuzione, qqplot
testDispersion(sim_res)  # controlla dispersion
testUniformity(sim_res)  # verifica uniformità dei residui
testZeroInflation(sim_res)


tidy(mod3.1_ger, effects = "fixed", conf.int = TRUE, conf.method = "Wald")


# Stampa il modello
print(summary(mod3.1_ger))
# Pairwise comparison:
marginal = emmeans(mod3.1_ger, ~ Gestione)
pairs(marginal, adjust="bh")
# Letters of significance:
lettere <- cld(marginal, alpha=0.05, Letters=letters, adjust="bh")
print(lettere)


# ANALISI 3.1 ================ n° catture x tipo gestione per specie
#Grafico:
barplot_management_effect_SP <- ggplot(data = df_subset, aes(x = Gestione, y = Conteggio, )) +
  stat_summary(fun = "mean", geom = "bar", width = 0.5,  
               fill = c("#C3C4D6", "#D08046", "#CFC45A")) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", 
               width = 0.2, colour = "black") +
  scale_x_discrete(labels = c("INT" = "IPM", "BIO" = "ORG", "RIN" = "REN")) +
  labs(x = "Management type", y = "Mean ± SE of catches per trap") +
  theme(
    panel.background = element_rect(fill = "#f2f2f2", color = NA),
    plot.background = element_rect(fill = "#f9f9f9", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5, family = "sans"),
    axis.text.y = element_text(size = 14, family = "sans"),
    axis.title.x = element_text(size = 14, family = "sans", 
                                margin = margin(t = 15)),  # spazio sopra il titolo x
    axis.title.y = element_text(size = 14, family = "sans", 
                                margin = margin(r = 10)),  # spazio a destra del titolo y
    legend.title = element_text(size = 14, family = "sans"),
    legend.text = element_text(size = 12, family = "sans")
  )
plot(barplot_management_effect_SP)

ggsave(sprintf("barplot_management_effect_SP_Xgermanus.png"),       # nome file
       plot = barplot_management_effect_SP,          # oggetto grafico
       width = 8, height = 6, units = "in",    # dimensioni (in pollici)
       dpi = 300,                              # risoluzione (300 = stampa, 150 = schermo)
       bg = "white")    



# SPECIE ANALIZZATA: "H. eruditus"

# Filtra il dataset per la singola species
df_subset <- subset(df_filtrato, Specie == "H. eruditus")

# Costruisci il modello
mod3.1_eru <- glmer.nb(Conteggio ~ Gestione + (1 | Anno) + (1 | Code/Blocco), data = df_subset)

##Controllo overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type="pearson")
  chi_sq <- sum(rp^2)
  ratio <- chi_sq / rdf
  return(ratio)
}
overdisp_fun(mod3.1_eru)

###Controllo zero-inflation
check_zeroinflation(mod3.1_eru)

####Residual diagnostics
sim_res <- simulateResiduals(fittedModel = mod3.1_eru, n = 1000)
plot(sim_res)         # residui vs predetti, distribuzione, qqplot
testDispersion(sim_res)  # controlla dispersion
testUniformity(sim_res)  # verifica uniformità dei residui
testZeroInflation(sim_res)

tidy(mod3.1_eru, effects = "fixed", conf.int = TRUE, conf.method = "Wald")


# Stampa il modello
print(summary(mod3.1_eru))
# Pairwise comparison:
marginal = emmeans(mod3.1_eru, ~ Gestione)
pairs(marginal, adjust="bh")
# Letters of significance:
lettere <- cld(marginal, alpha=0.05, Letters=letters, adjust="bh")
print(lettere)


# ANALISI 3.1 ================ n° catture x tipo gestione per specie
#Grafico:
barplot_management_effect_SP <- ggplot(data = df_subset, aes(x = Gestione, y = Conteggio, )) +
  stat_summary(fun = "mean", geom = "bar", width = 0.5,  
               fill = c("#C3C4D6", "#D08046", "#CFC45A")) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", 
               width = 0.2, colour = "black") +
  scale_x_discrete(labels = c("INT" = "IPM", "BIO" = "ORG", "RIN" = "REN")) +
  labs(x = "Management type", y = "Mean ± SE of catches per trap") +
  theme(
    panel.background = element_rect(fill = "#f2f2f2", color = NA),
    plot.background = element_rect(fill = "#f9f9f9", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5, family = "sans"),
    axis.text.y = element_text(size = 14, family = "sans"),
    axis.title.x = element_text(size = 14, family = "sans", 
                                margin = margin(t = 15)),  # spazio sopra il titolo x
    axis.title.y = element_text(size = 14, family = "sans", 
                                margin = margin(r = 10)),  # spazio a destra del titolo y
    legend.title = element_text(size = 14, family = "sans"),
    legend.text = element_text(size = 12, family = "sans")
  )
plot(barplot_management_effect_SP)

ggsave(sprintf("barplot_management_effect_SP_Heruditus.png"),       # nome file
       plot = barplot_management_effect_SP,          # oggetto grafico
       width = 8, height = 6, units = "in",    # dimensioni (in pollici)
       dpi = 300,                              # risoluzione (300 = stampa, 150 = schermo)
       bg = "white")                           # sfondo bianco
