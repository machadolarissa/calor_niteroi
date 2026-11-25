# =====================================================================
# gerar_bairros_censo.R
# Baixa o Censo 2022 (IBGE, agregados por bairro) de Niteroi e gera
# dados/bairros_vulnerabilidade.csv para o painel de calor.
#
# O que sai do Censo: % da populacao com 65 anos ou mais, por bairro.
# O que NAO sai (colunas ficam vazias para a equipe preencher):
#   renda_mediana  (o Censo por bairro nao traz renda nesta pasta; ver nota abaixo)
#   cobertura_arborea_pct, n_refugios, cobertura_refugio_10min_pct  (dados da Prefeitura)
#
# Uso: source("gerar_bairros_censo.R")  -> roda e, na 1a vez, PARA para voce conferir
# as variaveis de idade escolhidas (CONFIRMAR). Depois mude CONFIRMADO <- TRUE.
# Nao testado: escrito sem acesso aos arquivos do IBGE. Pontos incertos marcados com CONFERIR.
# =====================================================================
COD_MUN   <- "3303302"                  # Niteroi
PASTA     <- "dados"
BASE      <- "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/"
CONFIRMADO <- FALSE                     # mude para TRUE depois de conferir a lista impressa
# Se preferir, informe os codigos a mao (ex.: c("V01040","V01041")) e o total de pessoas:
VAR_65MAIS <- NULL
VAR_TOTAL  <- NULL

for (p in c("readr", "dplyr", "stringr", "readxl")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressPackageStartupMessages({ library(readr); library(dplyr); library(stringr); library(readxl) })
dir.create(PASTA, showWarnings = FALSE, recursive = TRUE)
options(timeout = 600)

baixar <- function(url, destino) {
  utils::download.file(url, destino, mode = "wb", quiet = TRUE, method = "libcurl",
                       headers = c("User-Agent" = "Mozilla/5.0 (CISNit coleta publica)"))
  stopifnot(file.exists(destino), file.size(destino) > 0); destino
}
# Procura no indice da pasta o link que casa com o padrao (os nomes levam data e mudam)
achar <- function(pasta_url, padrao) {
  h <- paste(readLines(pasta_url, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  l <- str_match_all(h, 'href="([^"]+)"')[[1]][, 2]
  l <- l[str_detect(l, padrao)]
  if (!length(l)) stop("arquivo nao encontrado em ", pasta_url, " (padrao: ", padrao, ")")
  paste0(pasta_url, tail(sort(l), 1))
}
ler_zip_csv <- function(url) {
  zf <- tempfile(fileext = ".zip"); baixar(url, zf)
  td <- tempfile(); dir.create(td); unzip(zf, exdir = td)
  arq <- list.files(td, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)[1]
  enc <- readr::guess_encoding(arq, n_max = 5000)$encoding[1]
  read_delim(arq, delim = ";", locale = locale(decimal_mark = ",", encoding = enc),
             col_types = cols(.default = col_character()), show_col_types = FALSE)
}

pasta_csv <- paste0(BASE, "Agregados_por_Bairro_csv/")
demo  <- ler_zip_csv(achar(pasta_csv, "bairros_demografia_BR.*\\.zip$"))
basic <- ler_zip_csv(achar(pasta_csv, "bairros_basico_BR.*\\.zip$"))
message("Colunas de demografia: ", paste(head(names(demo), 12), collapse = ", "), " ...")

so_niteroi <- function(x) {
  col_mun <- intersect(c("CD_MUN", "CD_MUNICIPIO"), names(x))[1]
  if (!is.na(col_mun)) x |> filter(.data[[col_mun]] == COD_MUN)       # CONFERIR: nome da coluna do municipio
  else x |> filter(str_detect(toupper(.data$NM_MUN), "NITER"))
}
demo <- so_niteroi(demo); basic <- so_niteroi(basic)
stopifnot("Nenhuma linha de Niteroi no arquivo" = nrow(demo) > 0)
col_bairro <- intersect(c("NM_BAIRRO", "NM_BAIRRO_"), names(demo))[1]; col_cd <- intersect(c("CD_BAIRRO"), names(demo))[1]

# ---- Escolha das variaveis (via dicionario de dados do IBGE) ---------
if (is.null(VAR_65MAIS)) {
  dic_url <- achar(BASE, "dicionario_de_dados.*\\.xlsx$")
  dic_f <- tempfile(fileext = ".xlsx"); baixar(dic_url, dic_f)
  dic <- bind_rows(lapply(excel_sheets(dic_f), function(s)
    tryCatch(read_excel(dic_f, sheet = s, col_types = "text") |> mutate(.aba = s), error = function(e) NULL)))
  linhas <- apply(dic, 1, function(r) paste(r[!is.na(r)], collapse = " | "))
  cod <- str_extract(linhas, "V\\d{4,5}")
  cand <- tibble(codigo = cod, descricao = linhas) |>
    filter(!is.na(codigo), codigo %in% names(demo),
           str_detect(descricao, regex("(65|70|75|80|85|90|95) a \\d+ anos|100 anos ou mais|65 anos ou mais", ignore_case = TRUE)))
  cat("\n=== Candidatas a '65 anos ou mais' (CONFERIR: evitar duplicar por sexo) ===\n")
  print(cand, n = 60, width = 200)
  tot <- tibble(codigo = cod, descricao = linhas) |>
    filter(!is.na(codigo), codigo %in% c(names(demo), names(basic)),
           str_detect(descricao, regex("total de pessoas|pessoas residentes|popula[cç][aã]o residente", ignore_case = TRUE)))
  cat("\n=== Candidatas a total de pessoas ===\n"); print(tot, n = 20, width = 200)
  if (!CONFIRMADO) {
    cat("\nConfira as listas acima. Depois defina VAR_65MAIS e VAR_TOTAL (ou CONFIRMADO <- TRUE para usar todas as candidatas",
        "de 65+ e a 1a de total) e rode de novo.\n")
    stop("Parando para conferencia das variaveis.", call. = FALSE)
  }
  VAR_65MAIS <- cand$codigo
  VAR_TOTAL  <- tot$codigo[1]
}
num <- function(x) suppressWarnings(as.numeric(str_replace(x, ",", ".")))     # "X" (sigilo) vira NA

base_t <- if (VAR_TOTAL %in% names(demo)) demo else basic
chave  <- col_cd
res <- demo |> transmute(cd = .data[[chave]], bairro = .data[[col_bairro]],
                         n65 = rowSums(across(all_of(VAR_65MAIS), num), na.rm = FALSE)) |>
  left_join(base_t |> transmute(cd = .data[[chave]], total = num(.data[[VAR_TOTAL]])), by = "cd") |>
  transmute(bairro, pop_65mais_pct = round(100 * n65 / total, 1), pop_total = total)

saida <- res |> transmute(bairro, pop_65mais_pct,
  renda_mediana = NA_real_, cobertura_arborea_pct = NA_real_, n_refugios = NA_integer_, cobertura_refugio_10min_pct = NA_real_)
write_csv(saida, file.path(PASTA, "bairros_vulnerabilidade.csv"), na = "")
write_csv(res, file.path(PASTA, "bairros_censo2022_apoio.csv"))
message("Gerado: ", file.path(PASTA, "bairros_vulnerabilidade.csv"), " (", nrow(saida), " bairros)")
message("Confira os totais: soma da populacao = ", format(sum(res$pop_total, na.rm = TRUE), big.mark = "."),
        " (Niteroi no Censo 2022: 481.749; pode diferir se houver area fora de bairro).")
