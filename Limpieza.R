
# Libraries ====

library(tidyverse)
library(readxl)
library(xml2)
library(rvest)
library(openxlsx)
library(janitor)


# Data Import ====

## Funcionarios (xlsx)

url_xlsx_21 <- read.xlsx(xlsxFile = "https://datos.cdmx.gob.mx/dataset/52f97506-ef52-449a-b2c9-29b6197abaa6/resource/84101103-c49f-4443-8e59-a6b9142b8a8e/download/base_nomina_completa_no_id_q02_21.xlsx",
                         sheet = 1, startRow = 1, colNames = TRUE)

funcionarios_2021 <- as.data.frame(url_xlsx_21)

## Beneficiarios

raw_sd_2019 <- read_excel("Padrones/Padron2019/SD_2019.xlsx")
raw_sd_2017 <- read_excel("Padrones/Padron2017/SD_2017.xlsx")
raw_sd_2016 <- read_excel("Padrones/Padron2016/SD_2016.xlsx")
raw_sd_2015 <- read_excel("Padrones/Padron2015/SD_2015.xlsx")
raw_sd_2014 <- read_excel("Padrones/Padron2014/SD_2014.xlsx")

raw_pa_2018 <- read_excel("Padrones/Padron2018/PA_2018.xlsx")
raw_pa_2017 <- read_excel("Padrones/Padron2017/PA_2017.xlsx")
raw_pa_2016 <- read_excel("Padrones/Padron2016/PA_2016.xlsx")
raw_pa_2015 <- read_excel("Padrones/Padron2015/PA_2015.xlsx")

### SD 2018

path_2018 <- "Padrones/Padron2018/SD_2018.xlsx"
sheets <- excel_sheets(path_2018)
sheets <- as.list(sheets)

raw_sd_2018 <- data.frame()

for (i in 1:length(sheets)) {
    data <- read_excel(path = path_2018, sheet = sheets[[i]])
    raw_sd_2018 <- bind_rows(raw_sd_2018, data, .id = )
}

### PA 2019 

raw_pa_2019_1 <- read_excel("Padrones/Padron2019/XLSX/PA_2019_1.xlsx") 
raw_pa_2019_2 <- read_excel("Padrones/Padron2019/XLSX/PA_2019_2.xlsx")
raw_pa_2019_3 <- read_excel("Padrones/Padron2019/XLSX/PA_2019_3.xlsx")
raw_pa_2019_4 <- read_excel("Padrones/Padron2019/XLSX/PA_2019_4.xlsx")
raw_pa_2019_5 <- read_excel("Padrones/Padron2019/XLSX/PA_2019_5.xlsx")
raw_pa_2019_6 <- read_excel("Padrones/Padron2019/XLSX/PA_2019_6.xlsx")
raw_pa_2019_7 <- read_excel("Padrones/Padron2019/XLSX/PA_2019_7.xlsx") 
raw_pa_2019_8 <- read_excel("Padrones/Padron2019/XLSX/PA_2019_8.xlsx")

### PA 2014

raw_pa_2014_AOB <- read_excel("Padrones/Padron2014/PA/PA_AOB_2014.xlsx")
raw_pa_2014_AZC <- read_excel("Padrones/Padron2014/PA/PA_AZC_2014.xlsx")
raw_pa_2014_BJU <- read_excel("Padrones/Padron2014/PA/PA_BJU_2014.xlsx")
raw_pa_2014_COY <- read_excel("Padrones/Padron2014/PA/PA_COY_2014.xlsx")
raw_pa_2014_CUH <- read_excel("Padrones/Padron2014/PA/PA_CUH_2014.xlsx")
raw_pa_2014_CUJ <- read_excel("Padrones/Padron2014/PA/PA_CUJ_2014.xlsx")
raw_pa_2014_GAM <- read_excel("Padrones/Padron2014/PA/PA_GAM_2014.xlsx")
raw_pa_2014_IZC <- read_excel("Padrones/Padron2014/PA/PA_IZC_2014.xlsx")
raw_pa_2014_IZP <- read_excel("Padrones/Padron2014/PA/PA_IZP_2014.xlsx")
raw_pa_2014_MAC <- read_excel("Padrones/Padron2014/PA/PA_MAC_2014.xlsx")
raw_pa_2014_MIH <- read_excel("Padrones/Padron2014/PA/PA_MIH_2014.xlsx")
raw_pa_2014_MIL <- read_excel("Padrones/Padron2014/PA/PA_MIL_2014.xlsx")
raw_pa_2014_TLH <- read_excel("Padrones/Padron2014/PA/PA_TLH_2014.xlsx")
raw_pa_2014_TLP <- read_excel("Padrones/Padron2014/PA/PA_TLP_2014.xlsx")
raw_pa_2014_VCA <- read_excel("Padrones/Padron2014/PA/PA_VCA_2014.xlsx")
raw_pa_2014_XOC <- read_excel("Padrones/Padron2014/PA/PA_XOC_2014.xlsx")

# Data Wrangling

## Wrangling Funcionarios ====

funcionarios_21 <- funcionarios_2021 %>% clean_names() 

funcionarios_21 <- funcionarios_21 %>% 
    rename(dependencia = desc_unidad_responsable, apellido_paterno = apellido_1,
           apellido_materno = apellido_2, cargo = n_puesto) %>% 
    select(apellido_paterno, apellido_materno, nombre, cargo, 
           sueldo_tabular_bruto) %>% 
    arrange(apellido_paterno)

View(funcionarios_21)

## Wrangling Beneficiarios ====

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

sd_2018 <- benef_sin_monto(raw_sd_2018)
sd_2018 <- sd_2018[-c(1:2), ]

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

colnames(raw_sd_2014) <- c("consecutivo", "apellido_paterno", "apellido_materno",
                           "nombre", "unidad", "delegacion", "sexo", "edad")
sd_2014 <- benef_sin_monto(raw_sd_2014)

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

### PA 2019

raw_pa_2019_1 <- raw_pa_2019_1[-c(1:25), ]
colnames(raw_pa_2019_1) <- c("consecutivo", "apellido_paterno", "apellido_materno",
                               "nombre", "unidad", "delegacion", "sexo", "edad", 
                               "monto")
pa_2019_1 <- benef_con_monto(raw_pa_2019_1)

colnames(raw_pa_2019_2) <- c("consecutivo", "apellido_paterno", 
                             "apellido_materno","nombre", "unidad", 
                             "delegacion", "sexo", "edad", "monto")
pa_2019_2 <- benef_con_monto(raw_pa_2019_2)

colnames(raw_pa_2019_3) <- c("consecutivo", "apellido_paterno", 
                             "apellido_materno","nombre", "unidad", 
                             "delegacion", "sexo", "edad", "monto")
pa_2019_3 <- benef_con_monto(raw_pa_2019_3)

colnames(raw_pa_2019_4) <- c("consecutivo", "apellido_paterno", 
                             "apellido_materno","nombre", "unidad", 
                             "delegacion", "sexo", "edad", "monto")
pa_2019_4 <- benef_con_monto(raw_pa_2019_4)

colnames(raw_pa_2019_5) <- c("consecutivo", "apellido_paterno", 
                             "apellido_materno","nombre", "unidad", 
                             "delegacion", "sexo", "edad", "monto")
pa_2019_5 <- benef_con_monto(raw_pa_2019_5)

colnames(raw_pa_2019_6) <- c("consecutivo", "apellido_paterno", 
                             "apellido_materno","nombre", "unidad", 
                             "delegacion", "sexo", "edad", "monto")
pa_2019_6 <- benef_con_monto(raw_pa_2019_6)

colnames(raw_pa_2019_7) <- c("consecutivo", "apellido_paterno", 
                             "apellido_materno","nombre", "unidad", 
                             "delegacion", "sexo", "edad", "monto")
pa_2019_7 <- benef_con_monto(raw_pa_2019_7)

colnames(raw_pa_2019_8) <- c("consecutivo", "apellido_paterno", 
                             "apellido_materno","nombre", "unidad", 
                             "delegacion", "sexo", "edad", "monto")
pa_2019_8 <- benef_con_monto(raw_pa_2019_8)

pa_2019 <- bind_rows(pa_2019_1, pa_2019_2)
pa_2019 <- bind_rows(pa_2019, pa_2019_3)
pa_2019 <- bind_rows(pa_2019, pa_2019_4)
pa_2019 <- bind_rows(pa_2019, pa_2019_4)
pa_2019 <- bind_rows(pa_2019, pa_2019_5)
pa_2019 <- bind_rows(pa_2019, pa_2019_6)
pa_2019 <- bind_rows(pa_2019, pa_2019_7)
pa_2019 <- bind_rows(pa_2019, pa_2019_8)

### PA 2014

raw_pa_2014_AOB <- raw_pa_2014_AOB[-c(1:2), ]
colnames(raw_pa_2014_AOB) <- c("consecutivo", "apellido_paterno", 
                               "apellido_materno", "nombre", "unidad", 
                               "delegacion", "sexo", "edad")
pa_2014_1 <- benef_sin_monto(raw_pa_2014_AOB)

colnames(raw_pa_2014_AZC) <- c("consecutivo", "apellido_paterno", 
                               "apellido_materno", "nombre", "unidad", 
                               "delegacion", "sexo", "edad")
pa_2014_2 <- benef_sin_monto(raw_pa_2014_AZC)

colnames(raw_pa_2014_BJU) <- c("consecutivo", "apellido_paterno", 
                               "apellido_materno", "nombre", "unidad", 
                               "delegacion", "sexo", "edad")
pa_2014_3 <- benef_sin_monto(raw_pa_2014_BJU)

colnames(raw_pa_2014_COY) <- c("consecutivo", "apellido_paterno", 
                               "apellido_materno", "nombre", "unidad", 
                               "delegacion", "sexo", "edad")
pa_2014_4 <- benef_sin_monto(raw_pa_2014_COY)

colnames(raw_pa_2014_CUH) <- c("consecutivo", "apellido_paterno", 
                               "apellido_materno", "nombre", "unidad", 
                               "delegacion", "sexo", "edad")
pa_2014_5 <- benef_sin_monto(raw_pa_2014_CUH)

colnames(raw_pa_2014_CUJ) <- c("consecutivo", "apellido_paterno", 
                               "apellido_materno", "nombre", "unidad", 
                               "delegacion", "sexo", "edad")
pa_2014_6 <- benef_sin_monto(raw_pa_2014_CUJ)

colnames(raw_pa_2014_GAM) <- c("consecutivo", "apellido_paterno", 
                               "apellido_materno", "nombre", "unidad", 
                               "delegacion", "sexo", "edad")
pa_2014_7 <- benef_sin_monto(raw_pa_2014_GAM)

colnames(raw_pa_2014_IZC) <- c("consecutivo", "apellido_paterno", 
                               "apellido_materno", "nombre", "unidad", 
                               "delegacion", "sexo", "edad")
pa_2014_8 <- benef_sin_monto(raw_pa_2014_IZC)

colnames(raw_pa_2014_IZP) <- c("consecutivo", "apellido_paterno", 
                               "apellido_materno", "nombre", "unidad", 
                               "delegacion", "sexo", "edad")
pa_2014_9 <- benef_sin_monto(raw_pa_2014_IZP)

colnames(raw_pa_2014_MAC) <- c("consecutivo", "apellido_paterno", 
                               "apellido_materno", "nombre", "unidad", 
                               "delegacion", "sexo", "edad")
pa_2014_10 <- benef_sin_monto(raw_pa_2014_MAC)

colnames(raw_pa_2014_MIH) <- c("consecutivo", "apellido_paterno", 
                               "apellido_materno", "nombre", "unidad", 
                               "delegacion", "sexo", "edad")
pa_2014_11 <- benef_sin_monto(raw_pa_2014_MIH)

colnames(raw_pa_2014_MIL) <- c("consecutivo", "apellido_paterno", 
                               "apellido_materno", "nombre", "unidad", 
                               "delegacion", "sexo", "edad")
pa_2014_12 <- benef_sin_monto(raw_pa_2014_MIL)

colnames(raw_pa_2014_TLH) <- c("consecutivo", "apellido_paterno", 
                               "apellido_materno", "nombre", "unidad", 
                               "delegacion", "sexo", "edad")
pa_2014_13 <- benef_sin_monto(raw_pa_2014_TLH)

colnames(raw_pa_2014_TLP) <- c("consecutivo", "apellido_paterno", 
                               "apellido_materno", "nombre", "unidad", 
                               "delegacion", "sexo", "edad")
pa_2014_14 <- benef_sin_monto(raw_pa_2014_TLP)

colnames(raw_pa_2014_VCA) <- c("consecutivo", "apellido_paterno", 
                               "apellido_materno", "nombre", "unidad", 
                               "delegacion", "sexo", "edad")
pa_2014_15 <- benef_sin_monto(raw_pa_2014_VCA)

colnames(raw_pa_2014_XOC) <- c("consecutivo", "apellido_paterno", 
                               "apellido_materno", "nombre", "unidad", 
                               "delegacion", "sexo", "edad")
pa_2014_16 <- benef_sin_monto(raw_pa_2014_XOC)

pa_2014 <- bind_rows(pa_2014_1, pa_2014_2)
pa_2014 <- bind_rows(pa_2014, pa_2014_3)
pa_2014 <- bind_rows(pa_2014, pa_2014_4)
pa_2014 <- bind_rows(pa_2014, pa_2014_5)
pa_2014 <- bind_rows(pa_2014, pa_2014_6)
pa_2014 <- bind_rows(pa_2014, pa_2014_7)
pa_2014 <- bind_rows(pa_2014, pa_2014_8)
pa_2014 <- bind_rows(pa_2014, pa_2014_9)
pa_2014 <- bind_rows(pa_2014, pa_2014_10)
pa_2014 <- bind_rows(pa_2014, pa_2014_11)
pa_2014 <- bind_rows(pa_2014, pa_2014_12)
pa_2014 <- bind_rows(pa_2014, pa_2014_13)
pa_2014 <- bind_rows(pa_2014, pa_2014_14)
pa_2014 <- bind_rows(pa_2014, pa_2014_15)
pa_2014 <- bind_rows(pa_2014, pa_2014_16)

# Combination ====

name_selection <- function(data) {
    data <- data %>% select(apellido_paterno, apellido_materno, nombre)
}

## 2019

sd_2019 <- name_selection(sd_2019)
pa_2019 <- name_selection(pa_2019)

padron_2019 <- bind_rows(sd_2019, pa_2019, .id = "id") # 1 = SD, 2 = PA

## 2018

sd_2018 <- name_selection(sd_2018)
pa_2018 <- name_selection(pa_2018)

padron_2018 <- bind_rows(sd_2018, pa_2018, .id = "id")

## 2017

sd_2017 <- name_selection(sd_2017)
pa_2017 <- name_selection(pa_2017)

padron_2017 <- bind_rows(sd_2017, pa_2017, .id = "id")

## 2016

sd_2016 <- name_selection(sd_2016)
pa_2016 <- name_selection(pa_2016)

padron_2016 <- bind_rows(sd_2016, pa_2016, .id = "id")

## 2015

sd_2015 <- name_selection(sd_2015)
pa_2015 <- name_selection(pa_2015)

padron_2015 <- bind_rows(sd_2015, pa_2015, .id = "id")


## 2014

sd_2014 <- name_selection(sd_2014)
pa_2014 <- name_selection(pa_2014)

padron_2014 <- bind_rows(sd_2014, pa_2014, .id = "id")

# CSV ====

write.csv(padron_2014, "Padrones/Padron2014/padron_2014.csv", row.names = FALSE)

write.csv(padron_2015, "Padrones/Padron2015/padron_2015.csv", row.names = FALSE)

write.csv(padron_2016, "Padrones/Padron2016/padron_2016.csv", row.names = FALSE)

write.csv(padron_2017, "Padrones/Padron2017/padron_2017.csv", row.names = FALSE)

write.csv(padron_2018, "Padrones/Padron2018/padron_2018.csv", row.names = FALSE)

write.csv(padron_2019, "Padrones/Padron2019/padron_2019.csv", row.names = FALSE)


