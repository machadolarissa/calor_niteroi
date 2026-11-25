# =====================================================================
# coletar_dados_calor.R   (v2 - corrige falhas de process_sim e httr2/curl)
# Coleta e EXPORTACAO de dados publicos para analise de calor em um municipio (padrao: Niteroi) (CISNit)
#
# Fontes:
#   1. Open-Meteo (serie meteorologica, sem chave de API)
#   2. INMET (arquivos anuais de estacoes automaticas)
#   3. DATASUS: SIM (obitos) e SIH (internacoes) via pacote microdatasus
#   4. InfoDengue (API publica)
#   5. Open Data BCN (refugios climaticos de Barcelona, modelo de esquema)
#
# Saidas (pasta dados/): CSVs por fonte + base consolidada diaria
#   base_calor_niteroi_diaria.csv  e  base_calor_niteroi_diaria.xlsx
#
# Dependencias: readr, dplyr, lubridate, jsonlite, stringr (sem httr2).
# =====================================================================

# ---- Configuracao ----------------------------------------------------
PASTA_SAIDA <- "dados"
ANO_INICIO  <- 2015
ANO_FIM     <- as.integer(format(Sys.Date(), "%Y"))

# >>> Municipio a analisar: troque nome e UF. O codigo IBGE e as coordenadas sao buscados sozinhos.
MUNICIPIO <- "Niterói"
UF        <- "RJ"
# Se a busca automatica falhar, preencha a mao (senao deixe NULL):
COD_IBGE_7_MANUAL <- NULL      # ex.: "3304557"
LAT_MANUAL <- NULL; LON_MANUAL <- NULL

pacotes <- c("readr", "dplyr", "lubridate", "jsonlite", "stringr")
faltam  <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltam)) install.packages(faltam)
invisible(lapply(pacotes, library, character.only = TRUE))

dir.create(PASTA_SAIDA, showWarnings = FALSE, recursive = TRUE)
options(timeout = 600)
log_msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), " | ", ...)

# Download robusto com base R (3 tentativas), sem httr2/curl novo
baixar <- function(url, destino, forcar = FALSE) {
  if (file.exists(destino) && !forcar) { log_msg("ja existe: ", destino); return(invisible(destino)) }
  for (i in 1:3) {
    ok <- tryCatch({ utils::download.file(url, destino, mode = "wb", quiet = TRUE, method = "libcurl",
                                         headers = c("User-Agent" = "Mozilla/5.0 (CISNit coleta publica)")); file.exists(destino) && file.size(destino) > 0 },
                   error = function(e) { log_msg("tentativa ", i, " falhou: ", conditionMessage(e)); FALSE },
                   warning = function(w) { log_msg("aviso: ", conditionMessage(w)); FALSE })
    if (ok) { log_msg("baixado: ", destino); return(invisible(destino)) }
    Sys.sleep(3)
  }
  stop("download falhou: ", url)
}

# ---- Municipio: codigo IBGE e coordenadas -----------------------------
tira_acento <- function(x) tolower(gsub("[^A-Za-z0-9]+", "_", iconv(x, to = "ASCII//TRANSLIT")))
resolver_municipio <- function(nome, uf) {
  if (tira_acento(nome) == "niteroi" && uf == "RJ")                       # padrao conhecido, sem depender de API
    return(list(cod = "3303302", lat = -22.8832, lon = -43.1034))
  cod <- COD_IBGE_7_MANUAL; lat <- LAT_MANUAL; lon <- LON_MANUAL
  if (is.null(cod)) {
    f <- tempfile(fileext = ".json")
    baixar(paste0("https://servicodados.ibge.gov.br/api/v1/localidades/estados/", uf, "/municipios"), f, forcar = TRUE)
    m <- jsonlite::fromJSON(f)
    hit <- m[tira_acento(m$nome) == tira_acento(nome), ]
    if (!nrow(hit)) stop("Municipio nao encontrado na UF ", uf, ": ", nome)
    cod <- as.character(hit$id[1])
  }
  if (is.null(lat) || is.null(lon)) {
    f <- tempfile(fileext = ".json")
    baixar(paste0("https://geocoding-api.open-meteo.com/v1/search?name=", utils::URLencode(nome),
                  "&count=20&language=pt&country_code=BR"), f, forcar = TRUE)
    g <- jsonlite::fromJSON(f)$results
    if (is.null(g) || !nrow(g)) stop("Coordenadas nao encontradas; preencha LAT_MANUAL e LON_MANUAL")
    # CONFERIR: escolhe o primeiro resultado da UF (nome do estado vem do IBGE)
    fe <- tempfile(fileext = ".json"); baixar(paste0("https://servicodados.ibge.gov.br/api/v1/localidades/estados/", uf), fe, forcar = TRUE)
    est <- jsonlite::fromJSON(fe)$nome
    g1 <- g[tira_acento(g$admin1) == tira_acento(est), ]; if (!nrow(g1)) g1 <- g
    lat <- g1$latitude[1]; lon <- g1$longitude[1]
  }
  list(cod = cod, lat = lat, lon = lon)
}
M <- resolver_municipio(MUNICIPIO, UF)
COD_IBGE_7 <- M$cod; COD_IBGE_6 <- substr(COD_IBGE_7, 1, 6); LAT <- M$lat; LON <- M$lon
SLUG <- tira_acento(MUNICIPIO)
# Arquivo lido pelo painel para saber qual municipio exibir
readr::write_csv(tibble::tibble(nome = MUNICIPIO, uf = UF, cod_ibge_7 = COD_IBGE_7, lat = LAT, lon = LON),
                 file.path(PASTA_SAIDA, "municipio.csv"))
log_msg("Municipio: ", MUNICIPIO, "/", UF, " | IBGE ", COD_IBGE_7, " | lat ", round(LAT, 4), " lon ", round(LON, 4))

# ---- Indice de calor (Rothfusz/NWS), C -> C --------------------------
# CONFERIR: alinhar formula e limiares com o CIE-Rio antes de comparar niveis.
indice_calor <- function(t_c, ur) {
  tf <- t_c * 9/5 + 32
  hi <- -42.379 + 2.04901523*tf + 10.14333127*ur - 0.22475541*tf*ur -
        0.00683783*tf^2 - 0.05481717*ur^2 + 0.00122874*tf^2*ur +
        0.00085282*tf*ur^2 - 0.00000199*tf^2*ur^2
  hi <- ifelse(tf < 80, tf, hi)
  (hi - 32) * 5/9
}

# ---- 1. Open-Meteo ---------------------------------------------------
coletar_openmeteo <- function(inicio = sprintf("%d-01-01", ANO_INICIO),
                              fim    = as.character(Sys.Date() - 7)) {
  url <- paste0("https://archive-api.open-meteo.com/v1/archive?latitude=", LAT, "&longitude=", LON,
                "&start_date=", inicio, "&end_date=", fim,
                "&hourly=temperature_2m,relative_humidity_2m,apparent_temperature",
                "&timezone=America%2FSao_Paulo")
  tmp <- tempfile(fileext = ".json"); baixar(url, tmp, forcar = TRUE)
  h <- jsonlite::fromJSON(tmp)$hourly
  horario <- tibble(datahora = ymd_hm(h$time, tz = "America/Sao_Paulo"),
                    temp_c = h$temperature_2m, umid_pct = h$relative_humidity_2m, sens_c = h$apparent_temperature)
  diario <- horario |>
    mutate(data = as_date(datahora)) |>
    group_by(data) |>
    summarise(tmax = max(temp_c, na.rm = TRUE), tmin = min(temp_c, na.rm = TRUE),
              tmed = mean(temp_c, na.rm = TRUE), umid_med = mean(umid_pct, na.rm = TRUE),
              ic_max = max(indice_calor(temp_c, umid_pct), na.rm = TRUE), .groups = "drop") |>
    mutate(across(where(is.numeric), ~ ifelse(is.finite(.x), .x, NA_real_))) |>
    filter(!is.na(tmax))   # dias sem dado horario saem como NA, nao como -Inf
  write_csv(horario, file.path(PASTA_SAIDA, paste0("openmeteo_horario_", SLUG, ".csv")))
  write_csv(diario,  file.path(PASTA_SAIDA, paste0("openmeteo_diario_", SLUG, ".csv")))
  log_msg("Open-Meteo: ", nrow(diario), " dias")
  invisible(diario)
}

# ---- 2. INMET --------------------------------------------------------
# CONFERIR: nome da estacao de interesse (o script lista as do RJ no log).
coletar_inmet <- function(anos = ANO_INICIO:ANO_FIM, filtro_estacao = toupper(tira_acento(MUNICIPIO))) {
  tmp <- tempfile("inmet"); dir.create(tmp)
  saida <- list()
  for (a in anos) {
    zip_local <- file.path(tmp, paste0(a, ".zip"))
    ok <- tryCatch({ baixar(sprintf("https://portal.inmet.gov.br/uploads/dadoshistoricos/%d.zip", a), zip_local); TRUE },
                   error = function(e) { log_msg("INMET ", a, " falhou: ", conditionMessage(e)); FALSE })
    if (!ok) next
    arqs <- unzip(zip_local, list = TRUE)$Name
    rj   <- arqs[str_detect(arqs, paste0("_", UF, "_"))]
    log_msg("INMET ", a, " estacoes ", UF, ": ", paste(unique(str_extract(rj, "_[A-Z]\\d+_[^_]+")), collapse = ", "))
    alvo <- rj[str_detect(toupper(rj), filtro_estacao)]
    for (f in alvo) {
      unzip(zip_local, files = f, exdir = tmp)
      d <- read_delim(file.path(tmp, f), delim = ";", skip = 8,
                      locale = locale(decimal_mark = ",", encoding = "latin1"),
                      col_types = cols(.default = col_character()), show_col_types = FALSE)
      d$arquivo <- basename(f); d$ano <- a
      saida[[length(saida) + 1]] <- d
    }
  }
  if (!length(saida)) { log_msg("INMET: nenhuma estacao casou com o filtro"); return(invisible(NULL)) }
  dados <- bind_rows(saida)
  write_csv(dados, file.path(PASTA_SAIDA, "inmet_estacoes_bruto.csv"))
  log_msg("INMET: ", nrow(dados), " linhas")
  invisible(dados)
}

# ---- 3. DATASUS: SIM e SIH -------------------------------------------
# Nao usa process_sim()/process_sih() (falham com 'tabNaturalidade'); le data e
# municipio direto dos campos brutos. Guardar so agregados (LGPD).
coletar_datasus <- function(ano_ini = ANO_INICIO, ano_fim = ANO_FIM - 1) {
  if (!requireNamespace("microdatasus", quietly = TRUE)) {
    stop("Instale: remotes::install_github('rfsaldanha/microdatasus')")
  }
  # Baixa ano a ano, com 3 tentativas (o FTP do DATASUS as vezes entrega arquivo vazio)
  baixar_ano <- function(f) {
    for (i in 1:3) {
      r <- tryCatch(f(), error = function(e) { log_msg("  tentativa ", i, " falhou: ", conditionMessage(e)); NULL })
      if (!is.null(r) && nrow(r) > 0) return(r)
      Sys.sleep(5)
    }
    NULL
  }
  # SIM: obitos por municipio de residencia
  sim <- list()
  for (a in ano_ini:ano_fim) {
    log_msg("SIM ", a)
    r <- baixar_ano(function() microdatasus::fetch_datasus(year_start = a, year_end = a, uf = UF,
           information_system = "SIM-DO", vars = c("CODMUNRES", "DTOBITO", "CAUSABAS", "SEXO", "IDADE", "BAIRES", "CODBAIRES")))
    if (is.null(r)) { log_msg("  SIM ", a, " nao baixado; rode de novo para completar"); next }
    sim[[length(sim) + 1]] <- r |> filter(CODMUNRES == COD_IBGE_6)
  }
  sim <- bind_rows(sim) |> mutate(data = as.Date(DTOBITO, format = "%d%m%Y"))
  obitos_dia <- sim |> filter(!is.na(data)) |> count(data, name = "obitos")
  # Obitos por bairro de residencia (BAIRES, campo livre digitado na DO: pode ter grafias diferentes e vazios)
  if ("BAIRES" %in% names(sim)) {
    ob_bairro <- sim |> filter(!is.na(data)) |>
      mutate(bairro = ifelse(is.na(BAIRES) | trimws(BAIRES) == "", "(sem bairro)", trimws(BAIRES))) |>
      count(data, bairro, name = "obitos")
    write_csv(ob_bairro, file.path(PASTA_SAIDA, paste0("sim_obitos_bairro_", SLUG, ".csv")))
    log_msg("SIM por bairro: ", n_distinct(ob_bairro$bairro), " grafias de bairro de residencia")
  }
  write_csv(obitos_dia, file.path(PASTA_SAIDA, paste0("sim_obitos_diarios_", SLUG, ".csv")))
  saveRDS(sim, file.path(PASTA_SAIDA, paste0("sim_", SLUG, ".rds")))
  log_msg("SIM: ", nrow(sim), " obitos de residentes de ", MUNICIPIO, "")

  # SIH: internacoes por municipio de residencia (mes a mes, com 3 tentativas por arquivo)
  sih <- list()
  for (a in ano_ini:ano_fim) for (m in 1:12) {
    log_msg("SIH ", a, "-", sprintf("%02d", m))
    r <- baixar_ano(function() microdatasus::fetch_datasus(year_start = a, month_start = m, year_end = a, month_end = m,
           uf = UF, information_system = "SIH-RD", vars = c("MUNIC_RES", "DT_INTER", "DIAG_PRINC", "IDADE", "COD_IDADE")))
    if (is.null(r)) { log_msg("  SIH ", a, "-", m, " nao baixado; rode de novo para completar"); next }
    sih[[length(sih) + 1]] <- r |> filter(MUNIC_RES == COD_IBGE_6)
  }
  sih <- bind_rows(sih) |> mutate(data = ymd(DT_INTER))
  sih_dia <- sih |> filter(!is.na(data)) |> count(data, name = "internacoes")
  write_csv(sih_dia, file.path(PASTA_SAIDA, paste0("sih_internacoes_diarias_", SLUG, ".csv")))
  saveRDS(sih, file.path(PASTA_SAIDA, paste0("sih_", SLUG, ".rds")))
  log_msg("SIH: ", nrow(sih), " internacoes de residentes de ", MUNICIPIO, "")
  invisible(list(sim = sim, sih = sih))
}

# ---- 4. InfoDengue ---------------------------------------------------
coletar_infodengue <- function(doenca = "dengue", ano_ini = ANO_INICIO, ano_fim = ANO_FIM) {
  url <- paste0("https://info.dengue.mat.br/api/alertcity?geocode=", COD_IBGE_7, "&disease=", doenca,
                "&format=csv&ew_start=1&ew_end=53&ey_start=", ano_ini, "&ey_end=", ano_fim)
  tmp <- tempfile(fileext = ".csv"); baixar(url, tmp, forcar = TRUE)
  d <- read_csv(tmp, show_col_types = FALSE)
  write_csv(d, file.path(PASTA_SAIDA, paste0("infodengue_", doenca, paste0("_", SLUG, ".csv"))))
  log_msg("InfoDengue: ", nrow(d), " semanas")
  invisible(d)
}

# ---- 5. Barcelona: refugios climaticos (modelo de esquema) -----------
coletar_refugios_bcn <- function() {
  base <- "https://opendata-ajuntament.barcelona.cat/data/dataset/8f9da263-ff41-4765-ab0d-61b97d7a00b2/resource/"
  csv  <- paste0(base, "7ecae024-6cb2-427d-b2d0-e170500e2a38/download")
  json <- paste0(base, "d88129fe-7aaa-4ae6-b9fd-908ad3f7480d/download")
  destino <- file.path(PASTA_SAIDA, "bcn_refugios_climaticos.csv")
  ok <- tryCatch({ baixar(csv, destino, forcar = TRUE); TRUE }, error = function(e) FALSE)
  if (!ok) {   # alternativa: JSON do mesmo conjunto, convertido para CSV
    log_msg("CSV indisponivel; tentando JSON")
    tmp <- tempfile(fileext = ".json"); baixar(json, tmp, forcar = TRUE)
    j <- jsonlite::fromJSON(tmp, flatten = TRUE)
    write_csv(as_tibble(j), destino)
  }
  d <- read_csv(destino, show_col_types = FALSE)
  log_msg("Barcelona: ", nrow(d), " refugios")
  invisible(d)
}

# ---- 6. EXPORTACAO: base consolidada diaria (CSV e Excel) ------------
exportar_base <- function() {
  f <- function(nome) file.path(PASTA_SAIDA, nome)
  if (!file.exists(f(paste0("openmeteo_diario_", SLUG, ".csv")))) stop("falta openmeteo_diario_niteroi.csv (rode coletar_openmeteo())")
  base <- read_csv(f(paste0("openmeteo_diario_", SLUG, ".csv")), show_col_types = FALSE) |> mutate(data = as.Date(data))
  if (file.exists(f(paste0("sim_obitos_diarios_", SLUG, ".csv"))))
    base <- base |> left_join(read_csv(f(paste0("sim_obitos_diarios_", SLUG, ".csv")), show_col_types = FALSE) |>
                                mutate(data = as.Date(data)), by = "data")
  if (file.exists(f(paste0("sih_internacoes_diarias_", SLUG, ".csv"))))
    base <- base |> left_join(read_csv(f(paste0("sih_internacoes_diarias_", SLUG, ".csv")), show_col_types = FALSE) |>
                                mutate(data = as.Date(data)), by = "data")
  # Dias sem registro, dentro do periodo coberto pelo DATASUS, contam como zero
  if ("obitos" %in% names(base)) {
    lim <- max(as.Date(read_csv(f(paste0("sim_obitos_diarios_", SLUG, ".csv")), show_col_types = FALSE)$data))
    base$obitos <- ifelse(is.na(base$obitos) & base$data <= lim, 0L, base$obitos)
  }
  if ("internacoes" %in% names(base)) {
    lim <- max(as.Date(read_csv(f(paste0("sih_internacoes_diarias_", SLUG, ".csv")), show_col_types = FALSE)$data))
    base$internacoes <- ifelse(is.na(base$internacoes) & base$data <= lim, 0L, base$internacoes)
  }
  write_csv(base, f("base_calor_niteroi_diaria.csv"))
  if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(base, f("base_calor_niteroi_diaria.xlsx"))
  } else log_msg("Para exportar em Excel: install.packages('writexl') e rode exportar_base() de novo")
  log_msg("Base consolidada exportada: ", nrow(base), " dias em ", normalizePath(PASTA_SAIDA))
  invisible(base)
}

# ---- Execucao --------------------------------------------------------
executar_tudo <- function() {
  etapas <- list(open_meteo = coletar_openmeteo, inmet = coletar_inmet, datasus = coletar_datasus,
                 infodengue = coletar_infodengue, refugios_bcn = coletar_refugios_bcn, exportar = exportar_base)
  for (nome in names(etapas)) {
    log_msg("=== ", nome, " ===")
    tryCatch(etapas[[nome]](), error = function(e) log_msg("FALHOU ", nome, ": ", conditionMessage(e)))
  }
  writeLines(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), file.path(PASTA_SAIDA, "ultima_atualizacao.txt"))
}

if (sys.nframe() == 0) executar_tudo()   # Rscript roda tudo; no RStudio, chame executar_tudo() ou cada funcao

# ---- Para virar "bot" (execucao agendada) ----------------------------
# Windows: install.packages("taskscheduleR");
#   taskscheduleR::taskscheduler_create(taskname = "coleta_calor",
#     rscript = "C:/caminho/coletar_dados_calor.R", schedule = "WEEKLY", days = "MON")
