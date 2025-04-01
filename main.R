setwd("C:/Users/Davi/Documents/ECONOMIA/2024.2/Trabalho ecoflu")
library(sf)
library(dplyr)
library(readxl)
library(writexl)
library(ggrepel)
library(tidyverse)
library(paletteer)
# install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))


dados = st_read("RJ_Municipios_2022.shp")

exportacoes = read_excel("exportacao_municipio.xlsx")

exportacoes = exportacoes %>%
  group_by(codigo)%>%
  slice_max(`2023 - Valor US$ FOB`, n=1)%>%
  ungroup()

importacoes = read_excel("importacoes_municipio.xlsx")

importacoes = importacoes %>%
  group_by(codigo)%>%
  slice_max(`2023 - Valor US$ FOB`, n=1)%>%
  ungroup()

idh = read_excel("idh_municipios.xlsx")%>%
  mutate(CD_MUN=as.character(CD_MUN))

alfabetizacao = read_excel("alfabetizacao.xlsx")%>%
  mutate(CD_MUN=as.character(CD_MUN))

pop = read_excel("pop_estimada_municipios.xls", sheet=2)

salarios = read_excel("salario_medio_empresas.xlsx")%>%
  mutate(CD_MUN=as.character(CD_MUN))
  
                 
mapa = dados%>%
  filter(CD_MUN %in% c(3300605,3302056,3302205,3302304,3303104,3304102,3306156,3300159,
                       3300902,3302106,3303005,3305133,3304706))

dados_ibge_2021 = read_excel("dados_ibge_noroestefluminese_2021.xlsx")%>%
  rename(CD_MUN = `Código do Município`)%>%
  mutate(CD_MUN=as.character(CD_MUN))

mapa = dados%>%
  right_join(dados_ibge_2021)%>%
  left_join(pop)%>%
  left_join(salarios)%>%
  left_join(idh)%>%
  left_join(alfabetizacao)%>%
  mutate(participacao_agropecuaria = 100*`Valor adicionado bruto da Agropecuária, \r\na preços correntes\r\n(R$ 1.000)`/`Valor adicionado bruto total, \r\na preços correntes\r\n(R$ 1.000)`,
         participacao_industria = 100*`Valor adicionado bruto da Indústria,\r\na preços correntes\r\n(R$ 1.000)`/`Valor adicionado bruto total, \r\na preços correntes\r\n(R$ 1.000)`,
         participacao_servicos = 100*`Valor adicionado bruto dos Serviços,\r\na preços correntes \r\n- exceto Administração, defesa, educação e saúde públicas e seguridade social\r\n(R$ 1.000)`/`Valor adicionado bruto total, \r\na preços correntes\r\n(R$ 1.000)`)%>%
  left_join(importacoes)




ggplot(data=mapa)+
                  # AQUI
  geom_sf(aes(fill=`Descrição SH2`))+
  geom_label_repel(
    data = mapa %>% filter(!is.na(`Descrição SH2`)),
    aes(
      label = paste0(NM_MUN, "\n",
                     "US$ ",
                     format(`2023 - Valor US$ FOB`, big.mark=".", decimal.mark=",")),
      geometry = geometry
    ),
    stat = "sf_coordinates",  
    size = 3,                 
    box.padding = 0.5,        
    force = 100,
    point.padding = 0.3,      
    segment.color = "black",  
    color = "black",          
    fill = "white"            
  )+
  scale_colour_paletteer_d("PNWColors::Bay")+
scale_fill_paletteer_d("PNWColors::Bay",
                       name="Bem ou serviço",
                       labels = function(x) str_wrap(x, 100))+
  
              # AQUI
  labs(title=str_wrap("Principal produto importado, por Município - 2023", width = 50),
       caption="Fonte: SECEX/MDIC")+
  coord_sf(expand = FALSE)+
  theme_minimal()+
  theme(
    legend.title = element_text(),
    axis.title = element_blank(),   
    axis.text = element_blank(),    
    axis.ticks = element_blank(),   
    panel.grid = element_blank(),
    plot.caption = element_text(hjust=0),
    plot.margin = margin(10, 0, 10, 0),
    plot.title = element_text(hjust=0.5, size=11),
    legend.key.spacing.y = unit(0.3, "lines")
  )

