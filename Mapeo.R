library(tidyverse)
library(readr)

# Data ====

padron_2014 <- read.csv("https://www.dropbox.com/s/522uhqpgnbdmtl3/padron_2014.csv?dl=1")

padron_2015 <- read.csv("https://www.dropbox.com/s/rzj3rupzd29tkuh/padron_2015.csv?dl=1")

padron_2016 <- read.csv("https://www.dropbox.com/s/s2s0ld9joa60h9n/padron_2016.csv?dl=1")

padron_2017 <- read.csv("https://www.dropbox.com/s/xao97kgp7afw4ba/padron_2017.csv?dl=1")

padron_2018 <- read.csv("https://www.dropbox.com/s/47hc9lzbhto3ypi/padron_2018.csv?dl=1")

padron_2019 <- read.csv("https://www.dropbox.com/s/pjq29bl5gbeo2wm/padron_2019.csv?dl=1")

# Collapse ====

View(funcionarios_21) # no podemos crear un CSV hasta tener la base completa

funcionarios_21_2 <- funcionarios_21 %>% 
    group_by(apellido_paterno, apellido_materno) %>% 
    summarise(n = n())

# Merge ====

ejemplo_1 <- left_join(padron_2019, funcionarios_21_2, 
                       by = c("apellido_paterno", "apellido_materno"))
