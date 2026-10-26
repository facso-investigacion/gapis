library(pacman)
p_load(tidyverse,
       readr,
       gt,
       writexl,
       readxl,
       janitor)

data <- read_csv("input/proyectos_vigentes.csv")

overheads_antropo <- data |>
  filter(concurso %in% c("Fondecyt Regular", "Iniciación en Investigación"),
         departamento_ir == "Antropología") |>
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
  vig_2027 = fecha_termino > 2027.5
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
    vig_2027
  )

write_xlsx(overheads_antropo, "overheads_antropo.xlsx")

overheads_antropo_2027 <- data |>
  filter(concurso %in% c("Fondecyt Regular", "Iniciación en Investigación"),
         departamento_ir == "Antropología",
        fecha_termino >= 2028) |>
  mutate(
 across(starts_with("financ_ano"), as.numeric),
 monto_adjudicado = ifelse(is.na(monto_adjudicado), (financ_ano_2+financ_ano_3+financ_ano_4)*100, monto_adjudicado),
  ppto_promedio_anual = (monto_adjudicado-7500)/duracion*12000,
  ppto_total_2027 = case_when(
    anio_concurso == 2026 ~ financ_ano_2,
    anio_concurso == 2025 ~ financ_ano_3,
    anio_concurso == 2024 ~ financ_ano_4,
  ),
  ppto_total_2027= ifelse(is.na(ppto_total_2027) | ppto_total_2027==0, ppto_promedio_anual, ppto_total_2027),
  h = 7500000,
  v = round((ppto_total_2027-h)/1.2, 0),
  overhead_total = (ppto_total_2027-h)/6,
  overhead_facso = overhead_total*0.75,
  vig_2027 = fecha_termino > 2027.5
  ) |>
  select(
    anio_concurso,
    codigo_proyecto,
    investigador_responsable,
    departamento_ir,
    instrumento = concurso,
    ppto_total_2027,
    ppto_solicitado = v,
    overhead_total,
    overhead_facso,
    vig_2027
  )

write_xlsx(overheads_antropo_2027, "overheads_antropo_2027.xlsx")
