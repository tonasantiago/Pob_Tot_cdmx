# Cargar datos ------------------------------------------------------------

URL <- "https://www.inegi.org.mx/contenidos/programas/ccpv/2020/microdatos/ageb_manzana/RESAGEBURB_09_2020_csv.zip"
targetfile <- "RESAGEBURB_09_2020_csv.zip"
if (!file.exists(targetfile)){
  download.file(URL, destfile = targetfile)
}
if (!file.exists("./~")){
  unzip(targetfile)
}
rm(targetfile)
rm(URL)

bdd <-  fread("RESAGEBURB_09CSV20.csv", na.strings = c("*", "N/D"))


# Tabla de POB60YMAS ------------------------------------------------------
library(dplyr, gt)
  bdd %>% filter(NOM_LOC=="Total del municipio")%>% group_by(NOM_MUN)%>% 
  summarize(P_60YMAS) %>%  
  mutate(porcentaje = (P_60YMAS/sum(P_60YMAS))*100) %>%
  arrange(desc(porcentaje)) %>% 
   gt() %>%
  tab_header(
    title = "Habitantes de 60 años por alcaldías de la CDMX, año 2020",
    subtitle = "Total de habitanes de 60 años y más, y porcentaje respecto al 
    total de habitantes de 60 años y
    más en la CDMX ") %>%
  cols_label(
    NOM_MUN = md("**Alcaldía**"),
    P_60YMAS = md("**Número**"),
    porcentaje = md("**Porcentaje**")
  ) %>%
    fmt_number(
      columns = vars(porcentaje),
      decimals = 1,
      use_seps = TRUE,
      sep_mark = ",",
      dec_mark = ".",
    )%>%
    fmt_number(
      columns = vars(P_60YMAS),
      decimals = 0,
      use_seps = TRUE,
      sep_mark = ",",
      dec_mark = ".",
      ) %>% 
  summary_rows(
    columns = vars(P_60YMAS, porcentaje),
    fns = list(Total = "sum"),
    formatter = fmt_number, decimals = 0
  ) %>% 
  tab_source_note(
    source_note = "Fuente: Censo de Población y Vivienda 2020"
  )%>%
  gtsave("tabla1.png")


# Tabla población total  --------------------------------------------------

  bdd %>% filter(NOM_LOC=="Total del municipio")%>% group_by(NOM_MUN)%>% 
    summarize(POBTOT) %>%  
    mutate(porcentaje = (POBTOT/sum(POBTOT))*100) %>%
    arrange(desc(porcentaje)) %>% 
    gt() %>%
    tab_header(
      title = "Total de habitantes por alcaldías de la CDMX, año 2020",
      subtitle = "Total de habitanes y porcentaje respecto al 
    total de habitantes en la CDMX ") %>%
    cols_label(
      NOM_MUN = md("**Alcaldía**"),
      POBTOT = md("**Número**"),
      porcentaje = md("**Porcentaje**")
    ) %>%
    fmt_number(
      columns = vars(porcentaje),
      decimals = 1,
      use_seps = TRUE,
      sep_mark = ",",
      dec_mark = ".",
    )%>%
    fmt_number(
      columns = vars(POBTOT),
      decimals = 0,
      use_seps = TRUE,
      sep_mark = ",",
      dec_mark = ".",
    ) %>% 
    summary_rows(
      columns = vars(POBTOT, porcentaje),
      fns = list(Total = "sum"),
      formatter = fmt_number, decimals = 0
    ) %>% 
  tab_source_note(
    source_note = "Fuente: Censo de Población y Vivienda 2020"
    )%>% 
    gtsave("tabla2.png")


