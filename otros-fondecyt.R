library(pacman)
p_load(tidyverse,
       readr,
       gt,
       writexl,
       readxl,
       janitor)

data <- read_csv("input/proyectos_vigentes.csv")
anid <- read_xlsx("input/BDH_HISTORICA.xlsx")

anid <- anid |>  clean_names()
anid <- anid |>  select(codigo_proyecto, institucion_principal)

data <- data |> left_join(anid, by="codigo_proyecto")

overheads <- data |>
  filter(concurso %in% c("Fondecyt Regular", "Iniciación en Investigación")) |>
  mutate(
 across(starts_with("financ_ano"), as.numeric),
 monto_adjudicado = ifelse(is.na(monto_adjudicado), (financ_ano_1+financ_ano_2+financ_ano_3+financ_ano_4)*100, monto_adjudicado),
  ppto_promedio_anual = (monto_adjudicado-7500)/duracion*12000,
  ppto_total_2026 = case_when(
    anio_concurso == 2026 ~ financ_ano_1,
    anio_concurso == 2025 ~ financ_ano_2,
    anio_concurso == 2024 ~ financ_ano_3,
    anio_concurso == 2023 ~ financ_ano_4
  ),
  ppto_total_2026= ifelse(is.na(ppto_total_2026) | ppto_total_2026==0, ppto_promedio_anual, ppto_total_2026),
  h = 7500000,
  v = round((ppto_total_2026-h)/1.2, 0),
  overhead_total = (ppto_total_2026-h)/6,
  overhead_facso = overhead_total*0.75,
  overhead_dip = overhead_facso*0.1
  ) |>
  select(
    anio_concurso,
    codigo_proyecto,
    investigador_responsable,
    departamento_ir,
    instrumento = concurso,
    ppto_total_2026,
    ppto_solicitado = v,
    overhead_total,
    overhead_facso,
    overhead_dip
  ) |> 
  filter(
    codigo_proyecto %in% c("11240192", "11240611", "1231651", "1240606", "1240922",
    "1250444", "1250603", "1251916")
  )


planilla <- overheads |> 
  rename(
    `Año concurso` = anio_concurso,
    `Código` = codigo_proyecto,
    `IR` = investigador_responsable,
    "Departamento IR" = departamento_ir,
    `Instrumento` = instrumento,
    `Ppto anual total ($)` = ppto_total_2026,
    "Ppto solicitado 2026 ($)" = ppto_solicitado,
    `Overheads Total ($)` = overhead_total,
    "Overheads FACSO ($)" = overhead_facso,
    `Overheads DIP ($)` = overhead_dip,
  )

write_xlsx(planilla, "overheads_otros.xlsx")
