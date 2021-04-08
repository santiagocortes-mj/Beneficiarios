
# Libraries ====

library(xml2)
library(rvest)
library(tidyverse)
library(openxlsx)
library(janitor)
library(readxl)
library(pdftools)
library(tabulizer)

# Data Import ====

## Funcionarios (loop)

url_tudinero <- "https://tudinero.cdmx.gob.mx/buscador_personas"

base_webpage <- read_html(url_tudinero)

new_urls <- "https://tudinero.cdmx.gob.mx/buscador_personas/%s"

table_base <- html_table(base_webpage)[[1]] %>% 
    as_tibble(.name_repair = "unique")

table_new <- data.frame()
df <- data.frame()    

i <- 30
while (i < 251788) {
    new_webpage <- read_html(sprintf(new_urls, i))
    table_new <- html_table(new_webpage)[[1]] %>% 
        as_tibble(.name_repair = "unique")
    df <- rbind(df, table_new)
    i = i + 30
}

funcionarios <- merge(table_base, df, all = TRUE)

## Funcionarios (xlsx)

url_xlsx_21 <- read.xlsx(xlsxFile = "https://datos.cdmx.gob.mx/dataset/52f97506-ef52-449a-b2c9-29b6197abaa6/resource/84101103-c49f-4443-8e59-a6b9142b8a8e/download/base_nomina_completa_no_id_q02_21.xlsx",
                         sheet = 1, startRow = 1, colNames = TRUE)

funcionarios_2021 <- as.data.frame(url_xlsx_21)

## Beneficiarios

raw_sd_2019 <- read_excel("Padrones/Padron2019/SD_2019.xlsx")

raw_pa_2019 <- pdf_data("Padrones/Padron2019/PA_2019.pdf")

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

raw_sd_2019 <- raw_sd_2019[-c(1:11, 54899), ]
colnames(raw_sd_2019) <- c("consecutivo", "apellido_paterno", "apellido_materno",
                           "nombre", "unidad", "delegacion", "sexo", "edad", 
                           "monto")
sd_2019 <- raw_sd_2019 %>% 
    select(apellido_paterno, apellido_materno, nombre, sexo, edad, monto) %>% 
    arrange(apellido_paterno)




