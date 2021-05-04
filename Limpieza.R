
# Libraries ====

library(tidyverse)
library(xml2)
library(rvest)
library(openxlsx)
library(janitor)
library(readxl)

# Data Import ====

## Funcionarios (xlsx)

url_xlsx_21 <- read.xlsx(xlsxFile = "https://datos.cdmx.gob.mx/dataset/52f97506-ef52-449a-b2c9-29b6197abaa6/resource/84101103-c49f-4443-8e59-a6b9142b8a8e/download/base_nomina_completa_no_id_q02_21.xlsx",
                         sheet = 1, startRow = 1, colNames = TRUE)

funcionarios_2021 <- as.data.frame(url_xlsx_21)

## Beneficiarios

raw_sd_2019 <- read_excel("Padrones/Padron2019/SD_2019.xlsx")
raw_sd_2018 <- read_excel("Padrones/Padron2018/SD_2018_3.0.xlsx") # sólo lee la primera página (cuando es la version 1)
raw_sd_2017 <- read_excel("Padrones/Padron2017/SD_2017.xlsx")
raw_sd_2016 <- read_excel("Padrones/Padron2016/SD_2016.xlsx")
raw_sd_2015 <- read_excel("Padrones/Padron2015/SD_2015.xlsx")

raw_pa_2018 <- read_excel("Padrones/Padron2018/PA_2018.xlsx")
raw_pa_2017 <- read_excel("Padrones/Padron2017/PA_2017.xlsx")
raw_pa_2016 <- read_excel("Padrones/Padron2016/PA_2016.xlsx")
raw_pa_2015 <- read_excel("Padrones/Padron2015/PA_2015.xlsx")


# Data Wrangling ====

## Funcionarios

funcionarios_21 <- funcionarios_2021 %>% clean_names() 

funcionarios_21 <- funcionarios_21 %>% 
    rename(dependencia = desc_unidad_responsable, apellido_paterno = apellido_1,
           apellido_materno = apellido_2, cargo = n_puesto) %>% 
    select(apellido_paterno, apellido_materno, nombre, cargo, 
           sueldo_tabular_bruto) %>% 
    arrange(apellido_paterno)

View(funcionarios_21)

## Beneficiarios

benef_con_monto <- function(data) {
    data %>% 
        select(apellido_paterno, apellido_materno, nombre, sexo, edad, monto) %>% 
        arrange(apellido_paterno)
}

benef_sin_monto <- function(data) {
    data %>% 
        select(apellido_paterno, apellido_materno, nombre, sexo, edad) %>% 
        arrange(apellido_paterno)
}

## SD

raw_sd_2019 <- raw_sd_2019[-c(1:11, 54899), ]
colnames(raw_sd_2019) <- c("consecutivo", "apellido_paterno", "apellido_materno",
                           "nombre", "unidad", "delegacion", "sexo", "edad", 
                           "monto")
sd_2019 <- benef_con_monto(raw_sd_2019)

raw_sd_2018 <- raw_sd_2018[-c(1:5), ]
colnames(raw_sd_2018) <- c("consecutivo", "apellido_paterno", "apellido_materno",
                           "nombre", "unidad", "delegacion", "sexo", "edad", 
                           "monto")
sd_2018 <- benef_con_monto(raw_sd_2018) # mal acomodados (version 3)

raw_sd_2017 <- raw_sd_2017[-c(1:14), ]
colnames(raw_sd_2017) <- c("consecutivo", "apellido_paterno", "apellido_materno",
                           "nombre", "unidad", "delegacion", "sexo", "edad", 
                           "monto")
sd_2017 <- benef_con_monto(raw_sd_2017)

raw_sd_2016 <- raw_sd_2016[-c(1:50), ]
colnames(raw_sd_2016) <- c("consecutivo", "apellido_paterno", "apellido_materno",
                           "nombre", "unidad", "delegacion", "sexo", "edad")
sd_2016 <- benef_sin_monto(raw_sd_2016)

colnames(raw_sd_2015) <- c("consecutivo", "apellido_paterno", "apellido_materno",
                           "nombre", "unidad", "delegacion", "sexo", "edad")
sd_2015 <- benef_sin_monto(raw_sd_2015)

## PA

raw_pa_2018 <- raw_pa_2018[-c(1:685), ]
colnames(raw_pa_2018) <- c("consecutivo", "apellido_paterno", "apellido_materno",
                           "nombre", "unidad", "delegacion", "sexo", "edad", 
                           "monto")
pa_2018 <- benef_con_monto(raw_pa_2018)
pa_2018 <- pa_2018[-1, ]

raw_pa_2017 <- raw_pa_2017[-c(1:17), ]
colnames(raw_pa_2017) <- c("consecutivo", "apellido_paterno", "apellido_materno",
                           "nombre", "unidad", "delegacion", "sexo", "edad", 
                           "monto")
pa_2017 <- benef_con_monto(raw_pa_2017)
pa_2017 <- pa_2017[-c(1:6414), ]

raw_pa_2016 <- raw_pa_2016[-c(1:595), ]
colnames(raw_pa_2016) <- c("consecutivo", "apellido_paterno", "apellido_materno",
                           "nombre", "unidad", "delegacion", "sexo", "edad")
pa_2016 <- benef_sin_monto(raw_pa_2016)

raw_pa_2015 <- raw_pa_2015[-c(1:15), ]
colnames(raw_pa_2015) <- c("consecutivo", "apellido_paterno", "apellido_materno",
                           "nombre", "unidad", "delegacion", "sexo", "edad")
pa_2015 <- benef_sin_monto(raw_pa_2015)
pa_2015 <- pa_2015[-c(1:305), ]











