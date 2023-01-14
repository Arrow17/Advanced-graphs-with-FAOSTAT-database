library(openxlsx)
library(xlsx)
library(tidyverse) #load dplyr, ggplot2, stringr, etc
library(sf) # working with geographic simple features in R
library(rnaturalearth) #World map data from natural earth
library(countrycode) #get ISO code from country´s names
library(ggrepel) #ggplot2 extension for overlapping text labels
library(CGPfunctions)
library(ggplot2)
library(gridExtra)
library(viridis)
library(hrbrthemes)
library(ggplot2)

setwd("~/R Scripts")

#### A nivel regional
base_regiones <- read.xlsx("FAOSTAT_data_reg.xlsx", sheetIndex = 1)

#### A nivel de paises
base_paises <- read.xlsx("FAOSTAT_data.xlsx", sheetIndex = 1)

# PRODUCCION ---------------------------------------------------

#Limpieza
data_g1 <- base_regiones %>% 
  filter(Año == "2010" | Año == "2015" | Año == "2021" |
        Año == "2000" | Año == "2005", Elemento == "Producción") %>%
  mutate(Valr_redu = round(as.numeric(Valor)/1000000, 1)) %>% 
  rename(Años = "Código.año")

#Grafico
newggslopegraph(dataframe = data_g1,
                       Times = Años ,
                       Measurement = Valr_redu,
                       Grouping = Área,
                       Title = "Evolución de la producción de granos de cacao",
                       SubTitle = "En millones de Tn, 2020 - 2021",
                       XTextSize = 11,
                       YTextSize = 3,
                       TitleJustify = "C",
                       SubTitleJustify = "C",
                       DataTextSize = 3.3,
                       LineThickness = 0.6)

#### A nivel de principales paises

#Limpiamos la base
base_paises$Valor <- as.numeric(base_paises$Valor)
g_geografico <-base_paises %>% 
                select(Área, Elemento, Año, Valor, 'Código.área..ISO3.') %>% 
                filter(Elemento == "Producción" & Año == "2021") %>% 
                mutate(percentpro = round((Valor/sum(Valor))*100, 2)) %>% 
                rename(ISO3 = 'Código.área..ISO3.') %>%
                filter(percentpro > 1)

#Cargamos la base de datos geograficos
world <- ne_countries(scale = "small", returnclass = "sf")

#Agregamos las coordenadas de la misma base de datos world
world <- world %>% 
  mutate(centroid = map(geometry, st_centroid),
         coords = map(centroid, st_coordinates),
         coords_x = map_dbl(coords, 1),
         coords_y = map_dbl(coords, 2))

#Nos deshacemos de paises que no queremos que sean graficados
world_2 <- world %>% 
            filter(admin != "Antarctica" & sov_a3 != "US1" & sov_a3 != "CH1" &
                     sov_a3 != "RUS" &  sov_a3 != "CAN" & sov_a3 != "GRL" &
                     sov_a3 != "ESP" &  sov_a3 != "BGR" & sov_a3 != "DN1" &
                     sov_a3 != "FR1" &  sov_a3 != "DEU" & sov_a3 != "GRC" &
                     sov_a3 != "IND" &  sov_a3 != "ITA" & sov_a3 != "JPN" &
                     sov_a3 != "PRK" &  sov_a3 != "NOR" & sov_a3 != "POL" &
                     sov_a3 != "PRT" &  sov_a3 != "KOR" & sov_a3 != "SWZ" &
                     sov_a3 != "TWN" &  sov_a3 != "UKR" & sov_a3 != "GB1" &
                     sov_a3 != "MNG" &  sov_a3 != "KAZ" & sov_a3 != "ISL" &
                     sov_a3 != "SWE" &  sov_a3 != "FI1" & sov_a3 != "EST" &
                     sov_a3 != "LTU" &  sov_a3 != "BLR" & sov_a3 != "LVA" &
                     sov_a3 != "IRL" &  sov_a3 != "BEL" & sov_a3 != "BIH" &
                     sov_a3 != "HUN" &  sov_a3 != "LUX")

#Uniendo base de datos para formar el grafico
g_geografico2 <- world_2 %>% 
                  select(geometry, name, iso_a3, coords_x, coords_y) %>%
                  left_join(g_geografico, by=c("iso_a3" = "ISO3"))

g_geografico2 <- na.omit(g_geografico2)

#Graficamos el mapa
ggplot(data = world_2) +
        geom_sf(fill = "white", color = "black") +
        geom_sf(data = g_geografico2, aes(fill = g_geografico2$percentpro)) +
        geom_text_repel(data = g_geografico2, aes(x = g_geografico2$coords_x, 
                                                   y = g_geografico2$coords_y,
                                                   label = g_geografico2$name),
                         nudge_x = c(25,-15,15,-25,25,-25,-5,25,5,-25),
                         nudge_y = c(-10,8,10, 8,10,10,10,10,10,3), size = 5) +
        scale_fill_gradientn(colours = hsv(0.5, seq(0,.50,length.out = 12), .85), name = "Porcentaje de participación", limits = c(0,40)) +
        xlim(-110, 150) +
        theme_void() +
        theme(legend.position = "bottom") +
        labs(title = "Producción de granos de cacao de los principales países, 2021") +
        theme(plot.title=element_text(size=20, face='bold', color='black', vjust = 1.4)) +
        geom_text(data = g_geografico2, aes(x = g_geografico2$coords_x, y = g_geografico2$coords_y, label = round(g_geografico2$percentpro,1)), nudge_x = 1, nudge_y = -1, check_overlap = T)

#### Evolucion a nivel de los principales paises

#Usamos la base de paises
data_g3 <-base_paises %>% 
            select(Área, Elemento, Año, Valor, 'Código.área..ISO3.') %>% 
            filter(Año == "2021" | Año == "2020" |
                     Año == "2019" | Año == "2018" |
                     Año == "2017" | Año == "2016" |
                     Año == "2015" | Año == "2014" |
                     Año == "2013" | Año == "2012" |
                     Año == "2011" | Año == "2010") %>%
            mutate(Valor_num = as.numeric(Valor)) %>% 
            select(-Valor)

# Creacion de distintas bases para obtener el porcentaje anual
df_list <- vector(mode = "list")
años <- c("2010", "2011", "2012", "2013", "2014", "2015",
          "2016", "2017", "2018", "2019", "2020", "2021")

for (i in años) {
    bass <- data_g3 %>%
      filter(Año==i & Elemento == "Producción") %>% 
      mutate(Valor_percnt = round(Valor_num/sum(Valor_num), digits = 4)*100) %>%
      filter(Valor_percnt > 1)
    df_list[[i]] <- bass 
}
rm(i)

# Vertical Join
full_base <- rbind(a2010, a2011, a2012, a2013,
                   a2014, a2015, a2016, a2017,
                   a2018, a2019, a2020, a2021)

full_base <- full_base[full_base$Área != "Togo" &
                       full_base$Área != "Sierra Leona"  &
                       full_base$Área != "México" &
                       full_base$Área != "Papua Nueva Guinea",]

# Grafico de barras apiladas
full_base$Área <- factor(full_base$Área, levels = c("Colombia", "Ecuador",
                                                    "República Dominicana",
                                                    "Camerún", "Brasil","Perú",
                                                    "Nigeria",
                                                    "Indonesia", "Ghana",
                                                    "Côte d'Ivoire"))

ggplot(full_base, aes(x = full_base$Año, y = full_base$Valor_percnt,
                       group = full_base$Área, fill = full_base$Área)) +
  geom_bar(stat = "identity", colour = "black", position = "stack") +
  scale_fill_brewer(palette = "RdYlBu") +
  theme_classic() +
  facet_wrap(~Área, nrow = 5, ncol = 2) +
  xlab("Años") +
  ylab("Porcentaje de participación anual") +
  guides(fill = guide_legend(title="Países")) +
  scale_x_discrete(breaks = c("2010", "2016", "2021"))
  
# geom_text(aes(label = full_base$Valor_percnt), vjust = -0.1)

 
# RENDIMIENTO -------------------------------------------------------------

#### A nivel regional

#Limpieza
data_g1 <- base_regiones %>% 
  filter(Año == "2010" | Año == "2015" | Año == "2021" |
           Año == "2000" | Año == "2005", Elemento == "Rendimiento") %>%
  mutate(Valr_redu = round(as.numeric(Valor)/1000, 1)) %>% 
  rename(Años = "Código.año")

#Grafico
newggslopegraph(dataframe = data_g1,
                Times = Años ,
                Measurement = Valr_redu,
                Grouping = Área,
                Title = "Evolución de la producción de granos de cacao",
                SubTitle = "En millones de Tn, 2020 - 2021",
                XTextSize = 11,
                YTextSize = 3,
                TitleJustify = "C",
                SubTitleJustify = "C",
                DataTextSize = 3.3,
                LineThickness = 0.6)

#### A nivel de principales paises

#Limpiamos la base
base_paises$Valor <- as.numeric(base_paises$Valor)

g_geografico <-base_paises %>% 
  select(Área, Elemento, Año, Valor, 'Código.área..ISO3.') %>% 
  filter(Elemento == "Rendimiento" & Año == "2021") %>% 
  rename(ISO3 = 'Código.área..ISO3.') %>% 
  filter(Valor > 7000)

#Cargamos la base de datos geograficos
world <- ne_countries(scale = "small", returnclass = "sf")

#Agregamos las coordenadas de la misma base de datos world
world <- world %>% 
  mutate(centroid = map(geometry, st_centroid),
         coords = map(centroid, st_coordinates),
         coords_x = map_dbl(coords, 1),
         coords_y = map_dbl(coords, 2))

#Nos deshacemos de paises que no queremos que sean graficados
world_2 <- world %>% 
  filter(admin != "Antarctica" & sov_a3 != "US1" & sov_a3 != "CH1" &
           sov_a3 != "RUS" &  sov_a3 != "CAN" & sov_a3 != "GRL" &
           sov_a3 != "ESP" &  sov_a3 != "BGR" & sov_a3 != "DN1" &
           sov_a3 != "FR1" &  sov_a3 != "DEU" & sov_a3 != "GRC" &
           sov_a3 != "IND" &  sov_a3 != "ITA" & sov_a3 != "JPN" &
           sov_a3 != "PRK" &  sov_a3 != "NOR" & sov_a3 != "POL" &
           sov_a3 != "PRT" &  sov_a3 != "KOR" & sov_a3 != "SWZ" &
           sov_a3 != "TWN" &  sov_a3 != "UKR" & sov_a3 != "GB1" &
           sov_a3 != "MNG" &  sov_a3 != "KAZ" & sov_a3 != "ISL" &
           sov_a3 != "SWE" &  sov_a3 != "FI1" & sov_a3 != "EST" &
           sov_a3 != "LTU" &  sov_a3 != "BLR" & sov_a3 != "LVA" &
           sov_a3 != "IRL" &  sov_a3 != "BEL" & sov_a3 != "BIH" &
           sov_a3 != "HUN" &  sov_a3 != "LUX")

#Uniendo base de datos para formar el grafico
g_geografico2 <- world_2 %>% 
  select(geometry, name, iso_a3, coords_x, coords_y) %>%
  left_join(g_geografico, by=c("iso_a3" = "ISO3"))

g_geografico2 <- na.omit(g_geografico2)

#Graficamos el mapa
ggplot(data = world_2) +
  geom_sf(fill = "white", color = "black") +
  geom_sf(data = g_geografico2, aes(fill = g_geografico2$Valor)) +
  geom_text_repel(data = g_geografico2, aes(x = g_geografico2$coords_x, 
                                            y = g_geografico2$coords_y,
                                            label = g_geografico2$name),
                  nudge_x = c(10,-12, 1, 25,-15,1, 14,25),
                  nudge_y = c(5,-3,8, 8,2,-10,10,10), size = 5) +
  scale_fill_gradientn(colours = hsv(0.3, seq(0,.50,length.out = 12), .85), name = "Hg/Ha", limits = c(0,33000)) +
  xlim(-110, 220) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Rendimiento de granos de cacao de los principales países, 2021") +
  theme(plot.title=element_text(size=20, face='bold', color='black', hjust = 0.5))

 
#### Evolucion a nivel de los principales paises

#Usamos la base de paises
data_g3 <-base_paises %>% 
  select(Área, Elemento, Año, Valor, 'Código.área..ISO3.') %>% 
  filter(Año == "2021" | Año == "2020" |
           Año == "2019" | Año == "2018" |
           Año == "2017" | Año == "2016" |
           Año == "2015" | Año == "2014" |
           Año == "2013" | Año == "2012" |
           Año == "2011" | Año == "2010") %>%
  mutate(Valor_num = as.numeric(Valor)) %>% 
  select(-Valor)

# Creacion de distintas bases para obtener el porcentaje anual
df_list_2 <- vector(mode = "list")
años <- c("2010", "2011", "2012", "2013", "2014", "2015",
          "2016", "2017", "2018", "2019", "2020", "2021")

for (i in años) {
  bass_2 <- data_g3 %>%
            filter(Año==i & Elemento == "Rendimiento" & Valor_num > 6000)
  
  df_list_2[[i]] <- bass_2
}
rm(i)

# Vertical Join
full_base <- rbind(a2010, a2011, a2012, a2013,
                   a2014, a2015, a2016, a2017,
                   a2018, a2019, a2020, a2021)

full_base <- full_base[full_base$Área != "República Democrática del Congo" &
                         full_base$Área != "Nicaragua"  &
                         full_base$Área != "Honduras" &
                         full_base$Área != "Guinea" &
                         full_base$Área != "Ecuador" &
                         full_base$Área != "Belice" &
                         full_base$Área != "Colombia" &
                         full_base$Área != "Malasia",]

# Grafico de barras apiladas
full_base$Área <- factor(full_base$Área, levels = c("Colombia", "Ecuador",
                                                    "República Dominicana",
                                                    "Camerún", "Brasil","Perú",
                                                    "Nigeria",
                                                    "Indonesia", "Ghana",
                                                    "Côte d'Ivoire"))

ggplot(full_base, aes(x = full_base$Año, y = full_base$Valor_num,
                      group = full_base$Área, fill = full_base$Área)) +
  geom_bar(stat = "identity", colour = "black", position = "stack") +
  scale_fill_brewer(palette = "RdYlBu") +
  theme_classic() +
  facet_wrap(~Área, nrow = 5, ncol = 2) +
  xlab("Años") +
  ylab("Porcentaje de participación anual") +
  guides(fill = guide_legend(title="Países")) +
  scale_x_discrete(breaks = c("2010", "2016", "2021"))

# AREA COSECHADA ----------------------------------------------------------

#### A nivel regional

#Limpieza
data_g1 <- base_regiones %>% 
  filter(Año == "2010" | Año == "2015" | Año == "2021" |
           Año == "2000" | Año == "2005", Elemento == "Área cosechada") %>%
  mutate(Valr_redu = round(as.numeric(Valor)/1000000, 1)) %>% 
  rename(Años = "Código.año")

#Grafico
newggslopegraph(dataframe = data_g1,
                Times = Años ,
                Measurement = Valr_redu,
                Grouping = Área,
                Title = "Evolución del área cosechada de granos de cacao",
                SubTitle = "En millones de Ha, 2020 - 2021",
                XTextSize = 11,
                YTextSize = 3,
                TitleJustify = "C",
                SubTitleJustify = "C",
                DataTextSize = 3.3,
                LineThickness = 0.6)

#### A nivel de principales paises

#Limpiamos la base
base_paises$Valor <- as.numeric(base_paises$Valor)

g_geografico <-base_paises %>% 
  select(Área, Elemento, Año, Valor, 'Código.área..ISO3.') %>% 
  filter(Elemento == "Área cosechada" & Año == "2021") %>% 
  rename(ISO3 = 'Código.área..ISO3.') %>% 
  filter(Valor > 110000)

#Cargamos la base de datos geograficos
world <- ne_countries(scale = "small", returnclass = "sf")

#Agregamos las coordenadas de la misma base de datos world
world <- world %>% 
  mutate(centroid = map(geometry, st_centroid),
         coords = map(centroid, st_coordinates),
         coords_x = map_dbl(coords, 1),
         coords_y = map_dbl(coords, 2))

#Nos deshacemos de paises que no queremos que sean graficados
world_2 <- world %>% 
  filter(admin != "Antarctica" & sov_a3 != "US1" & sov_a3 != "CH1" &
           sov_a3 != "RUS" &  sov_a3 != "CAN" & sov_a3 != "GRL" &
           sov_a3 != "ESP" &  sov_a3 != "BGR" & sov_a3 != "DN1" &
           sov_a3 != "FR1" &  sov_a3 != "DEU" & sov_a3 != "GRC" &
           sov_a3 != "IND" &  sov_a3 != "ITA" & sov_a3 != "JPN" &
           sov_a3 != "PRK" &  sov_a3 != "NOR" & sov_a3 != "POL" &
           sov_a3 != "PRT" &  sov_a3 != "KOR" & sov_a3 != "SWZ" &
           sov_a3 != "TWN" &  sov_a3 != "UKR" & sov_a3 != "GB1" &
           sov_a3 != "MNG" &  sov_a3 != "KAZ" & sov_a3 != "ISL" &
           sov_a3 != "SWE" &  sov_a3 != "FI1" & sov_a3 != "EST" &
           sov_a3 != "LTU" &  sov_a3 != "BLR" & sov_a3 != "LVA" &
           sov_a3 != "IRL" &  sov_a3 != "BEL" & sov_a3 != "BIH" &
           sov_a3 != "HUN" &  sov_a3 != "LUX")

#Uniendo base de datos para formar el grafico
g_geografico2 <- world_2 %>% 
  select(geometry, name, iso_a3, coords_x, coords_y) %>%
  left_join(g_geografico, by=c("iso_a3" = "ISO3"))

g_geografico2 <- na.omit(g_geografico2)

#Graficamos el mapa
ggplot(data = world_2) +
  geom_sf(fill = "white", color = "black") +
  geom_sf(data = g_geografico2, aes(fill = g_geografico2$Valor)) +
  geom_text_repel(data = g_geografico2, aes(x = g_geografico2$coords_x, 
                                            y = g_geografico2$coords_y,
                                            label = g_geografico2$name),
                  nudge_x = c(10,-12, 1, 25,-15,1, 14,25, 14, 14),
                  nudge_y = c(5,-3,8, 8,2,-10,10,10, 13, 13), size = 5) +
  scale_fill_gradientn(colours = hsv(0.3, seq(0,.50,length.out = 12), .85), name = "Ha", limits = c(120000,4300000)) +
  xlim(-110, 220) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Area cosechada de granos de cacao de los principales países, 2021") +
  theme(plot.title=element_text(size=20, face='bold', color='black', hjust = 0.5))


#### Evolucion a nivel de los principales paises

#Usamos la base de paises
data_g3 <-base_paises %>% 
  select(Área, Elemento, Año, Valor, 'Código.área..ISO3.') %>% 
  filter(Año == "2021" | Año == "2020" |
           Año == "2019" | Año == "2018" |
           Año == "2017" | Año == "2016" |
           Año == "2015" | Año == "2014" |
           Año == "2013" | Año == "2012" |
           Año == "2011" | Año == "2010") %>%
  mutate(Valor_num = as.numeric(Valor)) %>% 
  select(-Valor)

# Creacion de distintas bases para obtener el porcentaje anual

df_list_3 <- vector(mode = "list")
años <- c("2010", "2011", "2012", "2013", "2014", "2015",
          "2016", "2017", "2018", "2019", "2020", "2021")

for (i in años) {
  bass_3 <- data_g3 %>%
    filter(Año==i & Elemento == "Área cosechada" & Valor_num > 100000)
  
  df_list_3[[i]] <- bass_3
}
rm(i)

# Vertical Join
full_base <- rbind(a2010, a2011, a2012, a2013,
                   a2014, a2015, a2016, a2017,
                   a2018, a2019, a2020, a2021)

full_base <- full_base[full_base$Área != "India" &
                         full_base$Área != "Liberia"  &
                         full_base$Área != "Sierra Leona" &
                         full_base$Área != "Togo" &
                         full_base$Área != "Papua Nueva Guinea",]

# Grafico de barras apiladas
full_base$Área <- factor(full_base$Área, levels = c("Colombia", "Ecuador",
                                                    "República Dominicana",
                                                    "Camerún", "Brasil","Perú",
                                                    "Nigeria",
                                                    "Indonesia", "Ghana",
                                                    "Côte d'Ivoire"))

ggplot(full_base, aes(x = full_base$Año, y = full_base$Valor_num,
                      group = full_base$Área, fill = full_base$Área)) +
  geom_bar(stat = "identity", colour = "black", position = "stack") +
  scale_fill_brewer(palette = "RdYlBu") +
  theme_classic() +
  facet_wrap(~Área, nrow = 6, ncol = 2) +
  xlab("Años") +
  ylab("Porcentaje de participación anual") +
  guides(fill = guide_legend(title="Países")) +
  scale_x_discrete(breaks = c("2010", "2016", "2021"))





