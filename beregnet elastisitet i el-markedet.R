library(readr)
library(tidyverse)
library(dplyr)
library(scales)
library(AER)
library(sjPlot)
library(sjmisc)
library(sjlabelled)


data <- read_csv("data.csv")
data_annual <- read_csv("data_annual.csv")
data_elkraft <- read_csv("data_balance (1).csv")

#Elastisitet i el-markedet beregnet fra data med kvartalvise observasjoner 

#Tilbudselastisitet
S1 <- ivreg(produksjon ~ pris + d_2 + d_3 + d_4 | pris + d_2 + d_3 + d_4 + tilsig, data = data)
print(summary(S1))

#Etterspørselselastisitet
D1 <- ivreg(forbruk ~ pris + d_2 + d_3 + d_4 | pris + d_2 + d_3 + d_4 + bnp_cap, data = data)
print(summary(D1))


Reg1 <- tab_model(S1, D1)
Reg1


#Elastisitet i el-markedet beregnet fra data med årlige observasjoner

#Tilbudselastisitet
S2 <- ivreg(produksjon ~ pris | pris + tilsig, data = data_annual)
summary(S2) 

#Etterspørselselastisitet
D2 <- ivreg(forbruk ~ pris | pris + bnp_cap, data = data_annual)
print(summary(D2))

Tabell_1 <- tibble(Tilbud = c(coef(S1)["pris"], coef(S1)["(Intercept)"]),
                   Etterspørsel = c(coef(D1)["pris"], coef(D1)["(Intercept)"]),
                   Mål = c("Priselastisitet", "Intercept"))


Tabell_1


data %>% 
  ggplot(aes(y = pris))+
  geom_line(aes(x = produksjon, color ="Tilbud"), linetype = "solid", group=1) +
  geom_line(aes(x = forbruk, color = "Etterspørsel"), linetype = "solid", group=1) +
  labs(title = "Elekrtisk kraft marked",
       x = "% Prosentvise endringer Q", 
       y = "% Prosentvise endringer P")+
  scale_color_manual(values = c("mediumvioletred", "midnightblue"), 
                     breaks = c("Tilbud", "Etterspørsel"), 
                     name = "",
                     labels = c("Tilbud", "Etterspørsel"))+ 
  theme_classic()+
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.ticks = element_blank(),
        axis.title.y = element_text(size = 10, 
                                    face = "bold"),
        axis.title.x = element_text(size = 10, 
                                    face = "bold"),
        plot.caption = element_text(size = 10, 
                                    face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 8), 
        legend.position = "bottom")







######kode med masse feil og mangler som jeg kanskje har bruk for senere####



Demand_function_p <- function(p) {
  Demand <- coef(D2)["(Intercept)"] + coef(D2)["pris"] * p
  return(Demand)
}

Supply_function_p <- function(p) {
  Supply <- coef(S2)["(Intercept)"] + coef(S2)["pris"] * p
  return(Supply)
}

Prices <- seq(1,100, by =1)
P_simulation_data <- data.frame(Prices)

P_simulation_data$Demand <- sapply(P_simulation_data$Prices, Demand_function_p)
P_simulation_data$Supply <- sapply(P_simulation_data$Prices, Supply_function_p)




# Forventet tilbud og etterspørsel beregnet fra kvartal data
data$forventet_produksjon <- coef(S1)["pris"] * data$pris + coef(S1)["(Intercept)"]
data$forventet_forbruk <- coef(D1)["pris"] * data$pris + coef(D1)["(Intercept)"]


# Forventet tilbud og etterspørsel årlig data
data_annual$forventet_produksjon <- coef(S2)["pris"] * data_annual$pris + coef(S2)["(Intercept)"]
data_annual$forventet_forbruk <- coef(D2)["pris"] * data_annual$pris + coef(D2)["(Intercept)"]
data_annual$Df_q <- coef(D2)

# Plot estimert forventet tilbud og etterspørsel

#Alternativ A
options(repr.plot.width = 3, repr.plot.height = 3)

data_annual %>% 
  ggplot(aes(y=pris)) +
  geom_line(aes(x=forventet_produksjon, color = "S"), linetype="solid", size=1) +
  geom_line(aes(x=forventet_forbruk, color = "D"), linetype="solid", size=1) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")+ 
  #geom_area(aes(y = ))+
  #geom_ribbon(aes(), fill = "yellow", alpha = 0.5)+
  labs(title ="Markedstilpassning", 
       #subtitle = "en liten oversikt over markedstilpasningen",
       y="P", x="Q")+
  #scale_x_continuous(limits = c(-0.038, -0.02))+
  #scale_y_continuous(lab limits = c(-0.06, 0.03))+
  scale_color_manual(values = c("mediumvioletred", "midnightblue"), 
                     breaks = c("S", "D"), 
                     name = "Elektrisk-kraft",
                     labels = c("Tilbud", "Etterspørsel"))+ 
  theme_classic()+
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.ticks = element_blank(),
        axis.title.y = element_text(size = 10, 
                                    face = "bold", 
                                    hjust = 1, 
                                    angle = 0), 
        axis.title.x = element_text(size = 10, 
                                    face = "bold", 
                                    hjust = 1),
        plot.subtitle = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 8))



color="mediumvioletred"
color="midnightblue"
#Alternativ B: stryk
ggplot(data_annual, aes(x=pris)) +
  geom_line(aes(y=forventet_produksjon, color = "S"), color="mediumvioletred", linetype="solid", size=1) +
  geom_line(aes(y=forventet_forbruk, color = "D"), color="midnightblue", linetype="solid",  size=1) +
  labs(title ="Forventet tilbud og etterspørsel i el-markedet",
       y="Pris", x="Kvantum MWh")  +
  geom_hline(aes(yintercept = 0))+
  geom_vline(aes(xintercept = 0))+
  scale_x_continuous(limits = c(-0.08, 0.08))+
  scale_y_continuous(limits = c(-0.06, 0.06))+
  theme_classic()

# Omgjør dataen fra log-differanser tilbake til nivåer

# Finner startverdiene med utgangspunkt i opprinnelig data fra ssb
# for produksjon og forbruk 
start_values <- data_elkraft[1, c("produksjon", "forbruk")]

# Formaterer datasett
data2 <- data.frame(pris = rep(NA, nrow(data_annual)), 
                    produksjon = rep(NA, nrow(data_annual)), 
                    forbruk = rep(NA, nrow(data_annual)))
  
# Gjør slik at det nye datasettet, har de samme startverdiene 
data2[1, c("produksjon", "forbruk")] <- start_values


# Beregner verdien av produksjon og forbruk fra et log-differanse format
# Gjøres ved å kalkulere endringene i observert produksjons eller forbruksnivå
# henholdsvis til startverdiene, som multipliseres med vekstratene. 
# Bergningene gjøres for hver observasjon (i) gjennom en for-løkke. 
for (i in 2:nrow(data2)) {
  data2[i, "produksjon"] <- data2[i-1, "produksjon"] * (1 + data_annual[i-1, "produksjon"])
  data2[i, "forbruk"] <- data2[i-1, "forbruk"] * (1 + data_annual[i-1, "forbruk"])
}



# Estimerer virkninger for el-kraftmarkedet ved skift i etterspørselen og tilbudet


x_intrcpt_D1 <- coef(D2)["(Intercept)"] + 350000
x_intrcpt_S1 <- coef(S2)["(Intercept)"] + 760000

data2$adjusted_demand <- coef(D2)["pris"] * (data2$pris - x_intrcpt_D1)
data2$adjusted_supply <- coef(S2)["pris"] * (data2$pris - x_intrcpt_S1)

# Plot de korrigerte kurvene
ggplot(data2, aes(y=pris)) +
  geom_line(aes(x=forbruk, color="Opprinnelig Etterspørsel")) +
  geom_line(aes(x=adjusted_demand, color="Ny Etterspørsel"), linetype="dashed") +
  geom_line(aes(x=produksjon, color="Opprinnelig Tilbud")) +
  geom_line(aes(x=adjusted_supply, color="Nytt Tilbud"), linetype="dashed") +
  labs(title="Skift i Tilbud og Etterspørsel i El-markedet",
       x="Kvantum (MWh)", y="Pris")+
  theme_classic()





# Beregnerer opprinnelige skjæringspunkter med x-aksen
x_intrcpt_demand_0 <- -coef(D2)["(Intercept)"] / coef(D2)["pris"]
x_intrcpt_supply_0 <- -coef(S2)["(Intercept)"] / coef(S2)["pris"]

# Beregner nye skjæringspunkter med x-aksen
adj_x_intrcpt_demand_1 <- x_intrcpt_demand_0 + 350
adj_x_intrcpt_supply_1 <- x_intrcpt_supply_0 + 760

# Beregner nye etterspørsels-og tilbudskurver
data_annual$Demand_1 <- coef(D2)["pris"] * (data_annual$pris - adj_x_intrcpt_demand_1)
data_annual$Supply_1 <- coef(S2)["pris"] * (data_annual$pris - adj_x_intrcpt_supply_1)

ggplot(data_annual, aes(y=pris)) +
  geom_line(aes(x=forventet_produksjon, color="Opprinnelig Tilbud")) +
  geom_line(aes(x=Supply_1, color="Nytt Tilbud"), linetype="dashed") +
  geom_line(aes(x=forventet_forbruk, color="Opprinnelig Etterspørsel")) +
  geom_line(aes(x=Demand_1, color="Ny Etterspørsel"), linetype="dashed") +
  labs(title="Skift i Tilbud og Etterspørsel i El-markedet",
       x="Kvantum (MWh)", y="Pris",
       color="Legend") +
  theme_minimal()




demand_1 <- demand_1 + 350
supply_1 <- supply_1 + 760

# Beregner nye etterspørsels- og tilbudskurver
data_annual$demand_1<- coef(D2)["pris"] * (data_annual$pris - demand_1)
data_annual$supply_1 <- coef(S2)["pris"] * (data_annual$pris - supply_1)


# Beregner etterspørsels - og tilbudskurven 
data_annual$Demand_1 <- coef(D2)["pris"] * data_annual$pris + intercept_demand
data_annual$Supply_1 <- coef(S2)["pris"] * data_annual$pris + intercept_supply

# Ny markedslikevekt visualisert
data_annual %>% 
  ggplot(aes(y = pris))+
  geom_line(aes(x = forventet_forbruk, color = "D_0"))+
  geom_line(aes(x = Demand_1, color = "D_1"))+
  geom_line(aes(x = forventet_produksjon, color = "S_0"))+
  geom_line(aes(x = Supply_1, color = "S_1"))+
  labs(title = "Ny markedslikevekt som følge skift i etterspørsel og tilbud", 
       x = "Kvantum", y = "pris")+
  theme_classic()
  



 
#Plot observasjoner for produksjon og forbruk, samt trendlinje
ggplot(data_annual, aes(x=pris)) +
  geom_point(aes(y=produksjon, color="Tilbud")) +
  geom_point(aes(y=forbruk, color="Etterspørsel")) +
  geom_smooth(aes(y=produksjon, color="Tilbud"), formula = S2$formula) +
  geom_smooth(aes(y=forbruk, color="Etterspørsel"), formula = D2$formula) +
  labs(title="Markedsstruktur i El-markedet",
       x="Pris", y="Kvantum",
       color="Legend") +
  theme_minimal()
