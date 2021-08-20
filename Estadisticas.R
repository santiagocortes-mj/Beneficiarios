<<<<<<< HEAD

# Libraries ====

library(tidyverse)
library(readr)
library(readxl)
library(hrbrthemes)
library(devtools)
library(mxmaps)
=======
library(tidyverse)
library(readr)
>>>>>>> b0fd3aff0553abf6b990713eacc45a5c75bb9c34

# Data Import ====

pa_2019 <- read_csv("Padrones/Padron2019/pa_2019.csv")
sd_2019 <- read_csv("Padrones/Padron2019/sd_2019.csv")

pa_2018 <- read_csv("Padrones/Padron2018/pa_2018.csv")
sd_2018 <- read_csv("Padrones/Padron2018/sd_2018.csv")

pa_2017 <- read_csv("Padrones/Padron2017/pa_2017.csv")
sd_2017 <- read_csv("Padrones/Padron2017/sd_2017.csv")

pa_2016 <- read_csv("Padrones/Padron2016/pa_2016.csv")
sd_2016 <- read_csv("Padrones/Padron2016/sd_2016.csv")

pa_2015 <- read_csv("Padrones/Padron2015/pa_2015.csv")
sd_2015 <- read_csv("Padrones/Padron2015/sd_2015.csv")

pa_2014 <- read_csv("Padrones/Padron2014/pa_2014.csv")
sd_2014 <- read_csv("Padrones/Padron2014/sd_2014.csv")

<<<<<<< HEAD
monto_pa <- read_excel("Padrones/Montos.xlsx", sheet = 1)
monto_sd <- read_excel("Padrones/Montos.xlsx", sheet = 2)

# Data Wrangle ====

pa_2019_2 <- pa_2019 %>% select(-monto)
pa_2018_2 <- pa_2018 %>% select(-monto)
pa_2017_2 <- pa_2017 %>% select(-monto)

pa_merge <- bind_rows(pa_2014, pa_2015, pa_2016, 
                      pa_2017_2, pa_2018_2, pa_2019_2, 
=======
pa_2019 <- pa_2019 %>% select(-monto)
pa_2018 <- pa_2018 %>% select(-monto)
pa_2017 <- pa_2017 %>% select(-monto)

pa_merge <- rbind(pa_2014, pa_2015, pa_2016, pa_2017, pa_2018, pa_2019)

pa_merge <- bind_rows(pa_2014, pa_2015, pa_2016, 
                      pa_2017, pa_2018, pa_2019, 
>>>>>>> b0fd3aff0553abf6b990713eacc45a5c75bb9c34
                      .id = "id") # 1 = 2014, 2 = 2015, ... , 6 = 2019

pa_merge$id[pa_merge$id == "1"] <- "2014"
pa_merge$id[pa_merge$id == "2"] <- "2015"
pa_merge$id[pa_merge$id == "3"] <- "2016"
pa_merge$id[pa_merge$id == "4"] <- "2017"
pa_merge$id[pa_merge$id == "5"] <- "2018"
pa_merge$id[pa_merge$id == "6"] <- "2019"

<<<<<<< HEAD
sd_2019_2 <- sd_2019 %>% select(-monto)
sd_2017_2 <- sd_2017 %>% select(-monto)

sd_2019_2$delegacion[sd_2019_2$delegacion == "CUAJIMALPA DE MORELOS"] <- "CUAJIMALPA"

sd_merge <- bind_rows(sd_2014, sd_2015, sd_2016,
                      sd_2017_2, sd_2018, sd_2019_2,
=======
sd_2019 <- sd_2019 %>% select(-monto)
sd_2017 <- sd_2017 %>% select(-monto)

sd_merge <- bind_rows(sd_2014, sd_2015, sd_2016,
                      sd_2017, sd_2018, sd_2019,
>>>>>>> b0fd3aff0553abf6b990713eacc45a5c75bb9c34
                      .id = "id") # 1 = 2014, 2 = 2015, ... , 6 = 2019

sd_merge$id[sd_merge$id == "1"] <- "2014"
sd_merge$id[sd_merge$id == "2"] <- "2015"
sd_merge$id[sd_merge$id == "3"] <- "2016"
sd_merge$id[sd_merge$id == "4"] <- "2017"
sd_merge$id[sd_merge$id == "5"] <- "2018"
sd_merge$id[sd_merge$id == "6"] <- "2019"

sd_merge$sexo[sd_merge$sexo == "h"] <- "H"
sd_merge$sexo[sd_merge$sexo == "m"] <- "M"

# Statistics ====

## Edad

edad_pa_2019 <- summary(pa_2019$edad)
edad_sd_2019 <- summary(sd_2019$edad)

edad_pa_2018 <- summary(pa_2018$edad)
edad_sd_2018 <- summary(sd_2018$edad)

edad_pa_2017 <- summary(pa_2017$edad)
edad_sd_2017 <- summary(sd_2017$edad)

edad_pa_2016 <- summary(pa_2016$edad)
edad_sd_2016 <- summary(sd_2016$edad)

edad_pa_2015 <- summary(pa_2015$edad)
edad_sd_2015 <- summary(sd_2015$edad)

edad_pa_2014 <- summary(pa_2014$edad)
edad_sd_2014 <- summary(sd_2014$edad)

age_mean_pa2019 <- mean(pa_2019$edad)
age_mean_pa2018 <- mean(pa_2018$edad)
age_mean_pa2017 <- mean(pa_2017$edad)
age_mean_pa2016 <- mean(pa_2016$edad)
age_mean_pa2015 <- mean(pa_2015$edad)
age_mean_pa2014 <- mean(pa_2014$edad)

age_mean_sd2019 <- mean(sd_2019$edad)
age_mean_sd2018 <- mean(sd_2018$edad)
age_mean_sd2017 <- mean(sd_2017$edad)
age_mean_sd2016 <- mean(sd_2016$edad)
age_mean_sd2015 <- mean(sd_2015$edad)
age_mean_sd2014 <- mean(sd_2014$edad)

data_years <- c("2014", "2015", "2016", "2017", "2018", "2019")

pa_data <- c(age_mean_pa2014, age_mean_pa2015, age_mean_pa2016,
             age_mean_pa2017, age_mean_pa2018, age_mean_pa2019)
pa_age_mean <- data.frame(data_years, pa_data)


sd_data <- c(age_mean_sd2014, age_mean_sd2015, age_mean_sd2016,
             age_mean_sd2017, age_mean_sd2018, age_mean_sd2019)
sd_age_mean <- data.frame(data_years, sd_data)

## Sexo

sex_mean_h <- function(data) {
    filtro_h <- data %>% filter(sexo == "H")
    nrow(filtro_h) / nrow(data)
}

sex_mean_m <- function(data) {
    filtro_m <- data %>% filter(sexo == "M")
    nrow(filtro_m) / nrow(data)
}

h_pa_2019 <- sex_mean_h(pa_2019)
m_pa_2019 <- sex_mean_m(pa_2019)

h_sd_2019 <- sex_mean_h(sd_2019)
m_sd_2019 <- sex_mean_m(sd_2019)

h_pa_2018 <- sex_mean_h(pa_2018)
m_pa_2018 <- sex_mean_m(pa_2018)

h_sd_2018 <- sex_mean_h(sd_2018)
m_sd_2018 <- sex_mean_m(sd_2018)

h_pa_2017 <- sex_mean_h(pa_2017)
m_pa_2017 <- sex_mean_m(pa_2017)

h_sd_2017 <- sex_mean_h(sd_2017)
m_sd_2017 <- sex_mean_m(sd_2017)

h_pa_2016 <- sex_mean_h(pa_2016)
m_pa_2016 <- sex_mean_m(pa_2016)

h_sd_2016 <- sex_mean_h(sd_2016)
m_sd_2016 <- sex_mean_m(sd_2016)

h_pa_2015 <- sex_mean_h(pa_2015)
m_pa_2015 <- sex_mean_m(pa_2015)

h_sd_2015 <- sex_mean_h(sd_2015)
m_sd_2015 <- sex_mean_m(sd_2015)

h_pa_2014 <- sex_mean_h(pa_2014)
m_pa_2014 <- sex_mean_m(pa_2014)

h_sd_2014 <- sex_mean_h(sd_2014)
m_sd_2014 <- sex_mean_m(sd_2014)

sex_mean_pa <- data.frame(
    "data_years" = data_years,
    "H" = c(h_pa_2014, h_pa_2015, h_pa_2016, h_pa_2017, h_pa_2018, h_pa_2019),
    "M" = c(m_pa_2014, m_pa_2015, m_pa_2016, m_pa_2017, m_pa_2018, m_pa_2019)
)

sex_mean_sd <- data.frame(
    "data_years" = data_years,
    "H" = c(h_sd_2014, h_sd_2015, h_sd_2016, h_sd_2017, h_sd_2018, h_sd_2019),
    "M" = c(m_sd_2014, m_sd_2015, m_sd_2016, m_sd_2017, m_sd_2018, m_sd_2019)
)

## Delegacion

del_mean <- function(data) {
    filtro_del <- 
        data %>% 
        group_by(delegacion) %>% 
        tally
    med <- filtro_del %>% mutate(media = n / nrow(data))
    med
}

del_mean_pa2019 <- del_mean(pa_2019)
del_mean_sd2019 <- del_mean(sd_2019)

del_mean_pa2018 <- del_mean(pa_2018)
del_mean_sd2018 <- del_mean(sd_2018)

del_mean_pa2017 <- del_mean(pa_2017)
del_mean_sd2017 <- del_mean(sd_2017)

del_mean_pa2016 <- del_mean(pa_2016)
del_mean_sd2016 <- del_mean(sd_2016)

del_mean_pa2015 <- del_mean(pa_2015)
del_mean_sd2015 <- del_mean(sd_2015)

del_mean_pa2014 <- del_mean(pa_2014)
del_mean_sd2014 <- del_mean(sd_2014)

<<<<<<< HEAD
# Visualizations ====
=======
# Visualization ====
>>>>>>> b0fd3aff0553abf6b990713eacc45a5c75bb9c34

## Edad (mean)

pa_age_mean_plot <- ggplot(data = pa_age_mean) + 
    geom_bar(mapping = aes(x = data_years, y = pa_data, fill = data_years),
             stat = "identity") +
    ggtitle("Promedio de Edad (Pensión Alimenticia)") +
    xlab("Años") + ylab("Edad") +
    labs(fill = "Años")

sd_age_mean_plot <- ggplot(data = sd_age_mean) + 
    geom_bar(mapping = aes(x = data_years, y = sd_data, fill = data_years),
             stat = "identity") +
    ggtitle("Promedio de Edad (Seguro de Desempleo)") +
    xlab("Años") + ylab("Edad") +
    labs(fill = "Años")

<<<<<<< HEAD
## Edad - Boxplot
=======
## Edad Boxplot
>>>>>>> b0fd3aff0553abf6b990713eacc45a5c75bb9c34

pa_edad_boxplot <- ggplot(data = pa_merge, aes(x = id, y = edad)) + 
    stat_summary(fun = mean, colour = "red", geom = "point") +
    geom_boxplot(aes(fill = id), alpha = 0.5, width = 1, 
                 position = position_dodge(width = 1),  
                 outlier.colour = "dark gray", outlier.size = 1) +
    ggtitle("Edad (Pensión Alimenticia)") +
    xlab("Años") + ylab("Edad") +
    theme(legend.position = "none")

sd_edad_boxplot <- ggplot(data = sd_merge, aes(x = id, y = edad)) + 
    stat_summary(fun = mean, colour = "red", geom = "point") +
    geom_boxplot(aes(fill = id), alpha = 0.5, width = 1, 
                 position = position_dodge(width = 1),  
                 outlier.colour = "dark gray", outlier.size = 1) +
    ggtitle("Edad (Seguro de Desempleo)") +
    xlab("Años") + ylab("Edad") +
    theme(legend.position = "none")

## Sexo

pa_prop <- ggplot(data = pa_merge, aes(x = id,fill = sexo)) + 
    geom_bar(position = "fill") +
    ggtitle("Proporción de Mujeres y Hombres (Pensión Alimenticia)") +
    xlab("Año") + ylab("Proporción") 

sd_prop <- ggplot(data = sd_merge, aes(x = id,fill = sexo)) + 
    geom_bar(position = "fill") +
    ggtitle("Proporción de Mujeres y Hombres (Seguro de Desempleo)") +
    xlab("Año") + ylab("Proporción") 

## Edad + Sexo

pa_daes <- ggplot(data = pa_merge, aes(x = edad, fill = sexo)) + 
    geom_histogram(stat = "count", colour = "black") +
    facet_grid(id ~ ., scales = "free") +
    ggtitle("Distribución Anual de Edad por Sexo (Pensión Alimenticia)") +
    xlab("Edad") + ylab("Beneficiarios") 

sd_daes <- ggplot(data = sd_merge, aes(x = edad, fill = sexo)) +
    geom_histogram(stat = "count", colour = "black") +
    facet_grid(id ~., scales = "free") +
    ggtitle("Distribución Anual de Edad por Sexo (Seguro de Desempleo)") +
    xlab("Edad") + ylab("Beneficiarios")

## Delegacion

<<<<<<< HEAD
### Version 1

pa_delegacion <- pa_merge %>% 
    group_by(delegacion, id) %>% 
    tally() 

sd_delegacion <- sd_merge %>% 
    group_by(delegacion, id) %>% 
    tally()

pa_dpd <- ggplot(data = pa_delegacion, aes(x = delegacion, y = n)) +
    geom_segment(aes(x = delegacion, xend = delegacion, y = 0, yend = n), 
                 color = "grey") +
    geom_point(size = 3, color = "#69b3a2") +
    facet_grid(id ~., scales = "free") +
    coord_flip() +
    theme_ipsum() +
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 4)) +
    ggtitle("Distribución de la Población por Delegación (Pensión Alimenticia)") +
    xlab("Delegación") + ylab("Población")

sd_dpd <- ggplot(data = sd_delegacion, aes(x = delegacion, y = n)) +
    geom_segment(aes(x = delegacion, xend = delegacion, y = 0, yend = n), 
                 color = "grey") +
    geom_point(size = 3, color = "#33ccff") +
    facet_grid(id ~., scales = "free") +
    coord_flip() +
    theme_ipsum() +
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 4)) +
    ggtitle("Distribución de la Población por Delegación (Seguro de Desempleo)") +
    xlab("Delegación") + ylab("Población")

### Version 2

### nuevo

pa_dpd_2 <- ggplot(data = pa_delegacion, mapping = aes(x = id, 
                                           y = value, 
                                           group = delegacion, 
                                           color = delegacion)) +
    geom_line() +
    geom_point() +
    theme(legend.position = "none") +
    ggtitle("Distribución de la Población por Delegación 2.0 (Pensión Alimenticia)") +
    xlab("Población") + ylab("Año") +
    scale_color_discrete("Delegación") +
    theme_ipsum() 

sd_dpd_2 <- ggplot(data = sd_delegacion, mapping = aes(x = id, 
                                                       y = value, 
                                                       group = delegacion, 
                                                       color = delegacion)) +
    geom_line() +
    geom_point() +
    theme(legend.position = "none") +
    ggtitle("Distribución de la Población por Delegación 2.0 (Seguro de Desempleo)") +
    xlab("Población") + ylab("Año") +
    scale_color_discrete("Delegación") +
    theme_ipsum() 



## Monto - Time Series

pa_monto_ts <- ggplot(data = monto_pa, mapping = aes(x = Año, y = Monto)) +
    geom_line(color = "#ff0000") + 
    geom_point(color = "#990000",size = 3) +
    ggtitle("Evolución del Monto (Pensión Alimenticia)") +
    ylab("Monto (MXN)")

sd_monto_ts <- ggplot(data = monto_sd, mapping = aes(x = Año, y = Monto)) +
    geom_line(color = "#0000ff") + 
    geom_point(color = "#000066",size = 3) +
    ggtitle("Evolución del Monto (Seguro de Desempleo)") +
    ylab("Monto (MXN)")


# Heat Maps ====

### Necesario: devtools::install_github("diegovalle/mxmaps")

## Pensión Alimenticia

pa_delegacion <- pa_delegacion %>% 
    mutate(region = case_when(
        endsWith(delegacion, "AZCAPOTZALCO") ~ "09002",
        endsWith(delegacion, "COYOACAN") ~ "09003",
        endsWith(delegacion, "CUAJIMALPA") ~ "09004",
        endsWith(delegacion, "GUSTAVO A MADERO") ~ "09005",
        endsWith(delegacion, "IZTACALCO") ~ "09006",
        endsWith(delegacion, "IZTAPALAPA") ~ "09007",
        endsWith(delegacion, "MAGDALENA CONTRERAS") ~ "09008",
        endsWith(delegacion, "MILPA ALTA") ~ "09009",
        endsWith(delegacion, "ALVARO OBREGON") ~ "09010",
        endsWith(delegacion, "TLAHUAC") ~ "09011",
        endsWith(delegacion, "TLALPAN") ~ "09012",
        endsWith(delegacion, "XOCHIMILCO") ~ "09013",
        endsWith(delegacion, "BENITO JUAREZ") ~ "09014",
        endsWith(delegacion, "CUAUHTEMOC") ~ "09015",
        endsWith(delegacion, "MIGUEL HIDALGO") ~ "09016",
        endsWith(delegacion, "VENUSTIANO CARRANZA") ~ "09017"
    )) %>% 
    rename("value" = n) %>% 
    mutate(metro_area = "Valle de México")

pa_del_2014 <- pa_delegacion %>% filter(id == "2014")
pa_del_2015 <- pa_delegacion %>% filter(id == "2015")
pa_del_2016 <- pa_delegacion %>% filter(id == "2016")
pa_del_2017 <- pa_delegacion %>% filter(id == "2017")
pa_del_2018 <- pa_delegacion %>% filter(id == "2018")
pa_del_2019 <- pa_delegacion %>% filter(id == "2019")

pa_2014_heat <- mxmunicipio_choropleth(pa_del_2014, num_colors = 9, 
                                       zoom = subset(pa_del_2014, 
                                                     metro_area %in% "Valle de México")$region,
                                       title = "Distribución de la población por alcaldía (Pensión Alimenticia) - 2014",
                                       legend = "Población")

pa_2015_heat <- mxmunicipio_choropleth(pa_del_2015, num_colors = 9, 
                                        zoom = subset(pa_del_2015, 
                                                     metro_area %in% "Valle de México")$region,
                                       title = "Distribución de la población por alcaldía (Pensión Alimenticia) - 2015",
                                       legend = "Población")

pa_2016_heat <- mxmunicipio_choropleth(pa_del_2016, num_colors = 9, 
                                       zoom = subset(pa_del_2016, 
                                                     metro_area %in% "Valle de México")$region,
                                       title = "Distribución de la población por alcaldía (Pensión Alimenticia) - 2016",
                                       legend = "Población")

pa_2017_heat <- mxmunicipio_choropleth(pa_del_2017, num_colors = 9, 
                                       zoom = subset(pa_del_2017, 
                                                     metro_area %in% "Valle de México")$region,
                                       title = "Distribución de la población por alcaldía (Pensión Alimenticia) - 2017",
                                       legend = "Población")

pa_2018_heat <- mxmunicipio_choropleth(pa_del_2018, num_colors = 9, 
                                       zoom = subset(pa_del_2018, 
                                                     metro_area %in% "Valle de México")$region,
                                       title = "Distribución de la población por alcaldía (Pensión Alimenticia) - 2018",
                                       legend = "Población")

pa_2019_heat <- mxmunicipio_choropleth(pa_del_2019, num_colors = 9, 
                                       zoom = subset(pa_del_2019, 
                                                     metro_area %in% "Valle de México")$region,
                                       title = "Distribución de la población por alcaldía (Pensión Alimenticia) - 2019",
                                       legend = "Población")

## Seguro de Desempleo

sd_delegacion <- sd_delegacion %>% 
    mutate(region = case_when(
        endsWith(delegacion, "AZCAPOTZALCO") ~ "09002",
        endsWith(delegacion, "COYOACAN") ~ "09003",
        endsWith(delegacion, "CUAJIMALPA") ~ "09004",
        endsWith(delegacion, "GUSTAVO A MADERO") ~ "09005",
        endsWith(delegacion, "IZTACALCO") ~ "09006",
        endsWith(delegacion, "IZTAPALAPA") ~ "09007",
        endsWith(delegacion, "MAGDALENA CONTRERAS") ~ "09008",
        endsWith(delegacion, "MILPA ALTA") ~ "09009",
        endsWith(delegacion, "ALVARO OBREGON") ~ "09010",
        endsWith(delegacion, "TLAHUAC") ~ "09011",
        endsWith(delegacion, "TLALPAN") ~ "09012",
        endsWith(delegacion, "XOCHIMILCO") ~ "09013",
        endsWith(delegacion, "BENITO JUAREZ") ~ "09014",
        endsWith(delegacion, "CUAUHTEMOC") ~ "09015",
        endsWith(delegacion, "MIGUEL HIDALGO") ~ "09016",
        endsWith(delegacion, "VENUSTIANO CARRANZA") ~ "09017"
    )) %>% 
    rename("value" = n) %>% 
    mutate(metro_area = "Valle de México")

sd_del_2014 <- sd_delegacion %>% filter(id == "2014")
sd_del_2015 <- sd_delegacion %>% filter(id == "2015")
sd_del_2016 <- sd_delegacion %>% filter(id == "2016")
sd_del_2017 <- sd_delegacion %>% filter(id == "2017")
sd_del_2018 <- sd_delegacion %>% filter(id == "2018")
sd_del_2019 <- sd_delegacion %>% filter(id == "2019")

sd_2014_heat <- mxmunicipio_choropleth(sd_del_2014, num_colors = 9, 
                                       zoom = subset(sd_del_2014, 
                                                     metro_area %in% "Valle de México")$region,
                                       title = "Distribución de la población por alcaldía (Seguro de Desempleo) - 2014",
                                       legend = "Población")

sd_2015_heat <- mxmunicipio_choropleth(sd_del_2015, num_colors = 9, 
                                       zoom = subset(sd_del_2015, 
                                                     metro_area %in% "Valle de México")$region,
                                       title = "Distribución de la población por alcaldía (Seguro de Desempleo) - 2015",
                                       legend = "Población")

sd_2016_heat <- mxmunicipio_choropleth(sd_del_2016, num_colors = 9, 
                                       zoom = subset(sd_del_2016, 
                                                     metro_area %in% "Valle de México")$region,
                                       title = "Distribución de la población por alcaldía (Seguro de Desempleo) - 2016",
                                       legend = "Población")

sd_2017_heat <- mxmunicipio_choropleth(sd_del_2017, num_colors = 9, 
                                       zoom = subset(sd_del_2017, 
                                                     metro_area %in% "Valle de México")$region,
                                       title = "Distribución de la población por alcaldía (Seguro de Desempleo) - 2017",
                                       legend = "Población")

sd_2018_heat <- mxmunicipio_choropleth(sd_del_2018, num_colors = 9, 
                                       zoom = subset(sd_del_2018, 
                                                     metro_area %in% "Valle de México")$region,
                                       title = "Distribución de la población por alcaldía (Seguro de Desempleo) - 2018",
                                       legend = "Población")

sd_2019_heat <- mxmunicipio_choropleth(sd_del_2019, num_colors = 9, 
                                       zoom = subset(sd_del_2019, 
                                                     metro_area %in% "Valle de México")$region,
                                       title = "Distribución de la población por alcaldía (Seguro de Desempleo) - 2019",
                                       legend = "Población")

## Monto - Delegación 

#### nuevo

sd_2019$delegacion[sd_2019$delegacion == "LA MAGDALENA CONTRERAS"] <- "MAGDALENA CONTRERAS"
sd_2019$delegacion[sd_2019$delegacion == "CUAJIMALPA DE MORELOS"] <- "CUAJIMALPA"


sd_2017_2019 <- bind_rows(sd_2019, sd_2017, .id = "año")

sd_montos <- sd_2017_2019 %>% select(año, delegacion, monto)

sd_montos$año[sd_montos$año == "1"] <- "2019"
sd_montos$año[sd_montos$año == "2"] <- "2017"    

sd_dmd <- ggplot(data = sd_montos, mapping = aes(x = delegacion, 
                                                    y = monto, 
                                                    fill = año)) +
    geom_bar(position = "dodge", stat = "identity") +
    xlab("Delegación") + 
    ylab("Monto") +
    coord_flip() +
    theme_ipsum() +
    scale_fill_manual(values = c("#009966", "#990000")) +
    labs(fill = "Año") +
    ggtitle("Distribución del Monto por Delegación 2017 - 2019 (Seguro de Desempleo)")
=======







>>>>>>> b0fd3aff0553abf6b990713eacc45a5c75bb9c34


