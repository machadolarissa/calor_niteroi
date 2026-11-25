# =====================================================================
# Painel de Calor e Saude - Niteroi (modelo)  |  CISNit
# Layout inspirado no Painel de Indicadores de Tuberculose (IESC/UFRJ)
# Cores: paleta do CISNit, com foco no laranja.
#
# Le os arquivos gerados por coletar_dados_calor.R (pasta dados/):
#   openmeteo_diario_niteroi.csv       (data, tmax, tmin, tmed, umid_med, ic_max)
#   sim_obitos_diarios_niteroi.csv     (data, obitos)
#   sih_internacoes_diarias_niteroi.csv(data, internacoes)      [opcional]
#   bairros_vulnerabilidade.csv        (bairro, pop_65mais_pct, renda_mediana,
#                                       cobertura_arborea_pct, n_refugios,
#                                       cobertura_refugio_10min_pct)  [opcional]
# Se algum arquivo nao existir, o painel usa DADOS SIMULADOS e avisa na tela.
# STATUS: escrito sem poder executar R no ambiente de desenvolvimento;
# rode localmente e ajuste o que for preciso.
# =====================================================================
library(shiny)
library(bslib)
library(echarts4r)
library(jsonlite)
library(dplyr)
library(readr)
library(lubridate)
library(DT)

# ---- Paleta CISNit ---------------------------------------------------
COR <- list(
  petroleo = "#1A4146", verde = "#40A98A", turquesa = "#12B2AA",
  laranja_queimado = "#E04E05", laranja = "#F75D07", ambar = "#F7AA42",
  fundo = "#FFF6EE", cinza = "#EDE7E1"
)
# Escala dos niveis de calor (do mais ameno ao mais critico)
NIVEIS <- c("Calor 1", "Calor 2", "Calor 3", "Calor 4", "Calor 5")
COR_NIVEL <- c("#40A98A", "#F7AA42", "#F75D07", "#C23E00", "#7A2300")

# ---- Funcoes auxiliares ---------------------------------------------
# Indice de calor (Rothfusz/NWS), C -> C. CONFERIR com o CIE-Rio se for comparar niveis.
indice_calor <- function(t_c, ur) {
  tf <- t_c * 9/5 + 32
  hi <- -42.379 + 2.04901523*tf + 10.14333127*ur - 0.22475541*tf*ur -
    0.00683783*tf^2 - 0.05481717*ur^2 + 0.00122874*tf^2*ur +
    0.00085282*tf*ur^2 - 0.00000199*tf^2*ur^2
  hi <- ifelse(tf < 80, tf, hi)
  (hi - 32) * 5/9
}
classificar <- function(ic, cortes) findInterval(ic, sort(cortes)) + 1L
suprimir <- function(x, minimo = 5) ifelse(x < minimo, paste0("<", minimo), as.character(x))  # LGPD

episodios <- function(d, corte, min_dias = 3) {
  if (nrow(d) == 0) return(tibble())
  r <- rle(d$ic_max >= corte)
  fim <- cumsum(r$lengths); ini <- fim - r$lengths + 1
  idx <- which(r$values & r$lengths >= min_dias)
  if (!length(idx)) return(tibble())
  bind_rows(lapply(idx, function(i) {
    sub <- d[ini[i]:fim[i], ]
    tibble(inicio = min(sub$data), fim = max(sub$data), dias = nrow(sub),
           ic_max = round(max(sub$ic_max), 1), tmax = round(max(sub$tmax), 1),
           observado = sum(sub$desfecho, na.rm = TRUE))
  }))
}

# ---- Dados -----------------------------------------------------------
# Procura a pasta dados/ ao lado do app, na pasta acima (onde coletar_dados_calor.R grava) ou em ~/dados
PASTA <- Filter(dir.exists, c("dados", file.path("..", "dados")))[1]
if (is.na(PASTA)) PASTA <- "dados"
# Municipio: lido de dados/municipio.csv (gerado por coletar_dados_calor.R); padrao Niteroi
tira_acento <- function(x) tolower(gsub("[^A-Za-z0-9]+", "_", iconv(x, to = "ASCII//TRANSLIT")))
MUN <- if (file.exists(file.path(PASTA, "municipio.csv"))) read_csv(file.path(PASTA, "municipio.csv"), show_col_types = FALSE) else
  tibble(nome = "Niterói", uf = "RJ", cod_ibge_7 = "3303302", lat = -22.8832, lon = -43.1034)
NOME <- MUN$nome[1]; SLUG <- tira_acento(NOME); EH_NIT <- SLUG == "niteroi"
fmt_coord <- function(x) format(round(x, 2), decimal.mark = ",")
simular_clima <- function() {
  set.seed(2026)
  datas <- seq(as.Date("2015-01-01"), Sys.Date() - 1, by = "day")
  n <- length(datas); doy <- yday(datas)
  base <- 27.5 + 4.5 * cos(2 * pi * (doy - 40) / 365)
  onda <- numeric(n)
  for (k in sample(seq_len(n - 10), 45)) { len <- sample(3:8, 1); onda[k:(k + len)] <- onda[k:(k + len)] + runif(1, 2.5, 5) }
  tmax <- base + onda + rnorm(n, 0, 1.6)
  tmin <- tmax - runif(n, 5, 8)
  umid <- pmin(95, pmax(40, 78 - 0.6 * (tmax - 28) * 3 + rnorm(n, 0, 6)))
  tibble(data = datas, tmax = tmax, tmin = tmin, tmed = (tmax + tmin) / 2,
         umid_med = umid, ic_max = indice_calor(tmax, umid))
}
carregar <- function() {
  partes <- character(0)
  f1 <- file.path(PASTA, paste0("openmeteo_diario_", SLUG, ".csv"))
  clima <- if (file.exists(f1)) read_csv(f1, show_col_types = FALSE) else { partes <- c(partes, "clima"); simular_clima() }
  clima <- clima |> mutate(data = as.Date(data)) |>
    mutate(across(where(is.numeric), ~ ifelse(is.finite(.x), .x, NA_real_))) |>
    filter(!is.na(data), !is.na(tmax), !is.na(ic_max)) |> arrange(data)

  f2 <- file.path(PASTA, paste0("sim_obitos_diarios_", SLUG, ".csv"))
  if (file.exists(f2)) {
    ob <- read_csv(f2, show_col_types = FALSE) |> mutate(data = as.Date(data))
    d <- clima |> left_join(ob, by = "data") |> mutate(obitos = coalesce(obitos, 0L))
  } else {
    partes <- c(partes, "óbitos"); set.seed(7)
    d <- clima |> mutate(obitos = rpois(n(), 10 * (1 + 0.035 * pmax(ic_max - 30, 0))))
  }
  f3 <- file.path(PASTA, paste0("sih_internacoes_diarias_", SLUG, ".csv"))
  if (file.exists(f3)) {
    ih <- read_csv(f3, show_col_types = FALSE) |> mutate(data = as.Date(data))
    d <- d |> left_join(ih, by = "data") |> mutate(internacoes = coalesce(internacoes, 0L))
  } else {
    set.seed(8); d <- d |> mutate(internacoes = rpois(n(), 22 * (1 + 0.02 * pmax(ic_max - 30, 0))))
    partes <- c(partes, "internações")
  }
  f4 <- file.path(PASTA, "bairros_vulnerabilidade.csv")
  if (file.exists(f4)) {
    bairros <- read_csv(f4, show_col_types = FALSE)
  } else {
    partes <- c(partes, "bairros e refúgios"); set.seed(11)
    nomes <- c("Centro", "Icarai", "Ingá", "Santa Rosa", "Fonseca", "Barreto", "Engenhoca",
               "Largo da Batalha", "Pendotiba", "Piratininga", "Itaipu", "Charitas",
               "Jurujuba", "Sao Francisco", "Varzea das Moças")
    bairros <- tibble(bairro = nomes,
      pop_65mais_pct = round(runif(15, 8, 24), 1), renda_mediana = round(runif(15, 1300, 7500)),
      cobertura_arborea_pct = round(runif(15, 6, 38), 1), n_refugios = rpois(15, 2),
      cobertura_refugio_10min_pct = round(runif(15, 15, 95), 1))
  }
  # Indice de prioridade (0-100): usa so as colunas que tem dado (idosos ↑, renda ↓, arvores ↓, refugio ↓)
  zs <- function(x) if (sum(!is.na(x)) > 1 && sd(x, na.rm = TRUE) > 0) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE) else rep(NA_real_, length(x))
  comp <- cbind(zs(bairros$pop_65mais_pct), -zs(bairros$renda_mediana), -zs(bairros$cobertura_arborea_pct),
                -zs(bairros$cobertura_refugio_10min_pct))
  bruto <- rowMeans(comp, na.rm = TRUE); bruto[is.nan(bruto)] <- NA
  bairros$indice_prioridade <- round(100 * pmin(1, pmax(0, bruto / 2 + 0.5)), 1)
  list(d = d, bairros = bairros, simulado = length(partes) > 0, partes = partes)
}
DADOS <- carregar()

# Indicadores das fichas de acao do Plano (valores a preencher pela equipe; nao simulados)

# Arboviroses (Acao 20): InfoDengue, se existir; senao simulado
carregar_arbo <- function() {
  f <- file.path(PASTA, paste0("infodengue_dengue_", SLUG, ".csv"))
  if (file.exists(f)) {
    a <- read_csv(f, show_col_types = FALSE)
    dt <- a$data_iniSE
    a$data <- if (is.numeric(dt)) as.Date(as.POSIXct(dt / 1000, origin = "1970-01-01", tz = "UTC")) else as.Date(dt)
    a <- a |> transmute(data, casos = casos, tempmin = if ("tempmin" %in% names(a)) tempmin else NA_real_)
    return(list(a = a, simulado = FALSE))
  }
  set.seed(5); sem <- seq(as.Date("2018-01-07"), Sys.Date(), by = "week")
  list(a = tibble(data = sem, casos = rpois(length(sem), pmax(1, 25 + 60 * pmax(0, cos(2 * pi * (yday(sem) - 60) / 365)))),
                  tempmin = 20 + 4 * cos(2 * pi * (yday(sem) - 40) / 365)), simulado = TRUE)
}
ARBO <- carregar_arbo()

# ---- Tema e CSS ------------------------------------------------------
tema <- bs_theme(version = 5, primary = COR$laranja_queimado, secondary = COR$petroleo,
                 success = COR$verde, warning = COR$ambar, info = COR$turquesa,
                 base_font = "Segoe UI, system-ui, sans-serif", "body-color" = COR$petroleo,
                 "body-bg" = "#FFFFFF")
css <- sprintf("
.hdr{background:linear-gradient(100deg,%s 0%%,%s 100%%);color:#fff;padding:14px 22px 0 22px;}
.hdr-top{display:flex;align-items:center;justify-content:space-between;gap:16px;}
.hdr h1{font-size:1.6rem;font-weight:800;margin:0;text-align:center;}
.hdr p{margin:2px 0 8px;text-align:center;font-size:.95rem;opacity:.95;}
.chip{background:#fff;border-radius:10px;padding:6px 10px;display:flex;align-items:center;}
.chip img{height:46px;}
.hdr-sub{background:%s;margin:0 -22px;padding:7px 22px;font-size:.85rem;color:#fff;}
.nav-tabs{background:%s;border-bottom:3px solid %s;padding:6px 8px 0 8px;flex-wrap:wrap;}
.nav-tabs .nav-link{color:%s;font-weight:600;border:0;border-radius:6px 6px 0 0;}
.nav-tabs .nav-link.active{background:%s;color:#fff;}
.sidebar-box{background:transparent;font-size:.85rem;}
.def-bloco{border-top:1px solid #d9cfc6;margin-top:10px;padding-top:8px;}
.titulo-pag{color:%s;font-weight:800;text-align:center;margin:18px 0 4px;}
.sub-pag{text-align:center;color:#6b6b6b;margin-bottom:6px;}
.divisor{width:80px;height:3px;background:%s;margin:12px auto 16px;border:0;opacity:1;}
.meta-box{background:%s !important;color:#fff;}
.aviso-sim{background:#FFF3DF;border:1px solid %s;border-radius:6px;padding:8px 12px;margin-bottom:12px;font-size:.88rem;}
.card-header{font-weight:700;color:%s;background:#fff;}
.btn-primary{background:%s;border-color:%s;}
.ch-titulo{font-size:1.1rem;font-weight:700;}
.dl-ic{font-size:1.5rem;color:%s;margin-left:10px;line-height:1;}
.nota-graf{font-style:italic;text-align:center;font-size:.78rem;color:#555;margin:6px 24px 10px;}
.kpi{border-radius:8px;overflow:hidden;box-shadow:0 1px 4px rgba(0,0,0,.15);}
.selo{display:inline-block;color:#fff;font-size:.68rem;font-weight:600;border-radius:10px;padding:2px 9px;margin-left:4px;white-space:nowrap;}
", COR$laranja_queimado, COR$laranja, COR$petroleo, COR$cinza, COR$laranja_queimado,
   COR$petroleo, COR$laranja_queimado, COR$petroleo, COR$laranja, COR$laranja,
   COR$ambar, COR$petroleo, COR$laranja_queimado, COR$laranja_queimado, COR$petroleo)

css <- paste0(css, ".tab-content>.tab-pane{width:100%;} .tab-content{padding-top:22px;} .bslib-sidebar-layout{margin-top:6px;} .bslib-sidebar-layout>.main{padding-top:6px;}")
css <- paste0(css, ".bslib-sidebar-layout>.sidebar{background:transparent !important;border-right:0 !important;}",
  ".bslib-sidebar-layout>.sidebar>.sidebar-content{background:", COR$cinza, " !important;border-radius:8px;",
  "height:fit-content !important;flex:0 0 auto !important;margin:6px 8px 6px 0;padding:14px !important;}")

cabecalho <- div(class = "hdr",
  div(class = "hdr-top",
    div(class = "chip", if (file.exists("www/logo_cisnit.png")) img(src = "logo_cisnit.png", alt = "CISNit") else strong("CISNit")),
    div(h1("Painel de Calor e Saúde"),
        p("Município de ", NOME, " · Monitoramento de calor extremo, níveis de alerta e impactos na saúde")),
    div(class = "chip", if (file.exists("www/logo_prefeitura.png")) img(src = "logo_prefeitura.png", alt = "Prefeitura de Niterói") else strong("Prefeitura de Niterói"))),
  div(class = "hdr-sub", if (EH_NIT) "Produzido pelo Centro de Inteligência em Saúde de Niterói (CISNit) · Modelo para subsidiar a gestão (2026)" else paste0("Modelo de painel de apoio à gestão · ", NOME, " · baseado no painel do CISNit (Niterói)")))

aviso <- function() {
  txt <- "Os níveis e limiares exibidos são provisórios e devem ser calibrados com a análise local."
  if (DADOS$simulado)
    div(class = "aviso-sim", strong("Dados simulados: "), paste0(paste(DADOS$partes, collapse = ", "), ". "),
        if ("bairros e refúgios" %in% DADOS$partes) "Para os bairros, crie dados/bairros_vulnerabilidade.csv (ver README). ",
        "Os demais indicadores vêm dos arquivos de dados/. ", txt)
  else div(class = "aviso-sim", strong("Dados reais carregados de "), normalizePath(PASTA, mustWork = FALSE), ". ", txt)
}

# ---- Abrangencia geografica de cada dado (selo nos cartoes) -----------
ESCOPO <- list(
  mun   = list(txt = paste0("Município de ", NOME), cor = "#1A4146"),
  grade = list(txt = paste0("Ponto de grade em ", NOME, " (modelo)"), cor = "#C77700"),
  ref   = list(txt = "Referência externa", cor = "#6b6b6b"),
  br    = list(txt = "Brasil", cor = "#12B2AA"))
selo <- function(tipo) span(ESCOPO[[tipo]]$txt, class = "selo", style = paste0("background:", ESCOPO[[tipo]]$cor, ";"))
ch <- function(titulo, escopo = NULL, dl = NULL) card_header(div(style = "display:flex;justify-content:space-between;align-items:center;gap:8px;flex-wrap:wrap;",
  span(titulo, class = "ch-titulo"),
  div(style = "display:flex;align-items:center;", lapply(escopo, selo),
      if (!is.null(dl)) downloadLink(dl, label = icon("file-csv"), class = "dl-ic", title = "Baixar dados (CSV)"))))
nota <- function(...) p(..., class = "nota-graf")

bloco_def <- function(...) div(class = "def-bloco", ...)
def <- function(rotulo, texto) p(strong(paste0(rotulo, ": ")), texto, style = "margin:2px 0;")
# Base comum dos graficos echarts: fonte, barra de ferramentas (salvar PNG) e paleta CISNit
ec_base <- function(p, trigger = "axis") {
  p |> e_text_style(fontFamily = "Segoe UI", color = COR$petroleo) |>
    e_toolbox_feature(feature = "dataView", title = "Ver dados", readOnly = TRUE, lang = c("Dados", "Fechar", "Atualizar")) |>
    e_toolbox_feature(feature = "saveAsImage", title = "Salvar imagem") |>
    e_grid(top = 70, containLabel = TRUE) |>
    e_tooltip(trigger = trigger) |>
    e_color(c(COR$laranja, COR$petroleo, COR$ambar, COR$turquesa, COR$verde))
}


ROT_VUL <- c(indice_prioridade = "Índice de prioridade (0-100)", pop_65mais_pct = "População 65+ (%)",
             renda_mediana = "Renda mediana (R$)", cobertura_arborea_pct = "Cobertura arbórea (%)",
             n_refugios = "Refúgios (n)", cobertura_refugio_10min_pct = "Cobertura de refúgio em 10 min a pé (%)")
rotulo_vul <- function(v) unname(ROT_VUL[v])

# GeoJSON dos bairros (opcional). Casa o nome da feicao com a coluna bairro por comparacao sem acento/caixa.
carregar_mapa <- function(arq = "dados/bairros_niteroi.geojson", bairros) {
  if (!file.exists(arq)) return(NULL)
  chave <- function(x) toupper(trimws(iconv(as.character(x), to = "ASCII//TRANSLIT")))
  gj <- tryCatch(jsonlite::fromJSON(arq, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(gj) || is.null(gj$features)) return(NULL)
  campos <- c("bairro", "BAIRRO", "Bairro", "nome", "NOME", "name", "NM_BAIRRO", "NM_BAIRR")
  gj$features <- lapply(gj$features, function(f) {
    cp <- intersect(campos, names(f$properties))
    nm <- if (length(cp)) f$properties[[cp[1]]] else ""
    alvo <- bairros[chave(bairros) == chave(nm)]
    f$properties$name <- if (length(alvo)) alvo[1] else nm
    f })
  as.character(jsonlite::toJSON(gj, auto_unbox = TRUE, digits = 6))
}
MAPA <- carregar_mapa(file.path(PASTA, paste0("bairros_", SLUG, ".geojson")), DADOS$bairros$bairro)

# ---- Territorio (bairro) ---------------------------------------
chave_b <- function(x) toupper(trimws(gsub("[^A-Za-z0-9 ]", "", iconv(as.character(x), to = "ASCII//TRANSLIT"))))
OB_BAIRRO <- { f <- file.path(PASTA, paste0("sim_obitos_bairro_", SLUG, ".csv"))
  if (file.exists(f)) read_csv(f, show_col_types = FALSE) |> mutate(data = as.Date(data), k = chave_b(bairro)) else NULL }
DADOS$bairros$k <- chave_b(DADOS$bairros$bairro)
BAIRROS_ALL <- sort(unique(c(DADOS$bairros$bairro, if (!is.null(OB_BAIRRO)) setdiff(unique(OB_BAIRRO$bairro), "(sem bairro)"))))

card_acao <- function(num, titulo, plano, cisnit, painel, aba_id, aba_nome) {
  card(card_header(paste0("Ação ", num, " · ", titulo)),
    card_body(
      p(strong("O que o Plano prevê: "), plano),
      p(strong("Interface do CISNit: "), cisnit),
      p(strong("O que este painel entrega: "), painel),
      actionLink(paste0("ir_", aba_id), paste0("Abrir: ", aba_nome), class = "btn btn-sm btn-primary", style = "width:fit-content;color:#fff;")))
}

# Centraliza a Apresentacao em relacao a janela, qualquer que seja a largura do container pai
js_centro <- "
(function(){
  function centra(){
    var w = Math.min(900, document.documentElement.clientWidth - 32);
    document.querySelectorAll('.pag-centro').forEach(function(el){
      el.style.maxWidth = 'none'; el.style.width = w + 'px'; el.style.marginRight = '0'; el.style.marginLeft = '0';
      var pl = el.parentElement.getBoundingClientRect().left;
      el.style.marginLeft = (((document.documentElement.clientWidth - w) / 2) - pl) + 'px';
    });
  }
  window.addEventListener('load', centra); window.addEventListener('resize', centra);
  document.addEventListener('shown.bs.tab', centra);
  setTimeout(centra, 100); setTimeout(centra, 600);
})();"

# Caixas de indicador em echarts (fundo colorido, rotulo e valor)
kpi <- function(id) div(class = "kpi", echarts4rOutput(id, height = 112))
kpi_ec <- function(valor, rotulo, cor) {
  e_charts() |> e_list(list(backgroundColor = cor, title = list(
    list(text = rotulo, left = "center", top = "16%", textStyle = list(color = "#fff", fontSize = 14, fontWeight = "normal", fontFamily = "Segoe UI")),
    list(text = as.character(valor), left = "center", top = "40%", textStyle = list(color = "#fff", fontSize = 36, fontWeight = "bold", fontFamily = "Segoe UI")))))
}

# ---- UI --------------------------------------------------------------
ui <- page_fluid(theme = tema, tags$head(tags$style(HTML(css)), tags$script(HTML(js_centro))), cabecalho,
  navset_tab(id = "abas",
  # ---------- Apresentacao
  nav_panel("Apresentação", value = "apres",
    div(class = "pag-centro", style = "width:100%;max-width:900px;margin-left:auto;margin-right:auto;padding:10px 16px;",
      h2(class = "titulo-pag", paste0("Painel de Calor e Saúde de ", NOME)),
      p(class = "sub-pag", "Modelo de painel de apoio à decisão · CISNit"),
      hr(class = "divisor"),
      aviso(),
      p("Este painel reúne séries de temperatura e umidade, níveis de calor, desfechos em saúde (óbitos e internações) e indicadores de vulnerabilidade e refúgios climáticos do município de ", NOME, ", para subsidiar a gestão da Secretaria Municipal de Saúde, da Defesa Civil e da Secretaria do Clima."),
      p("Ele responde a quatro perguntas de gestão: (1) qual é a situação de calor hoje e nos últimos dias; (2) a partir de que nível de calor o risco à saúde aumenta em ", NOME, "; (3) quanto os episódios de calor já custaram em óbitos e internações; (4) onde priorizar refúgios climáticos, arborização e ações junto à população vulnerável."),
      if (EH_NIT) p("A estrutura segue as ações do Plano de Adaptação, Mitigação e Resiliência de Niterói (2026) ligadas a calor e saúde, tratadas na seção 6.3 do relatório técnico: refúgios climáticos (Ação 11), monitoramento de temperatura (Ação 17), Plano de Contingência para ondas de calor (Ação 18), arboviroses (Ação 20) e Plano de Clima e Saúde (Ação 21). A aba Ações do Plano (6.3) mostra, para cada ação, o que o painel entrega e acompanha os indicadores das fichas."),
      p("Cada gráfico traz um selo com a abrangência do dado: município de ", NOME, ", ponto de grade em ", NOME, " (modelo), Brasil ou referência externa."),
      p("Os dados são agregados. Não há acesso a registros individuais, e contagens com menos de 5 casos são ocultadas (LGPD)."),
      h5("Abrangência geográfica das fontes", style = "margin-top:18px;"),
      tags$table(class = "table table-sm", style = "font-size:.85rem;",
        tags$thead(tags$tr(tags$th("Fonte"), tags$th("Abrangência"), tags$th("O que significa"))),
        tags$tbody(
          tags$tr(tags$td("Open-Meteo (reanálise)"), tags$td(selo("grade")), tags$td("Temperatura e umidade modeladas para o ponto de ", NOME, " (lat ", fmt_coord(MUN$lat[1]), "; lon ", fmt_coord(MUN$lon[1]), "), grade de cerca de 10 km. Não é estação.")),
          tags$tr(tags$td("INMET (estações)"), tags$td(selo("br")), tags$td("Rede nacional; usa-se a estação mais próxima de ", NOME, ", que pode ficar em outro município. Conferir a estação antes do uso.")),
          tags$tr(tags$td("SIM (óbitos)"), tags$td(selo("mun")), tags$td("Residentes de ", NOME, " (código IBGE ", substr(MUN$cod_ibge_7[1], 1, 6), "), qualquer local de ocorrência.")),
          tags$tr(tags$td("SIH/SUS (internações)"), tags$td(selo("mun")), tags$td("Residentes de ", NOME, " internados na rede SUS; não inclui a rede privada.")),
          tags$tr(tags$td("InfoDengue"), tags$td(selo("mun")), tags$td("Casos estimados de dengue no município, por semana epidemiológica.")),
          tags$tr(tags$td("Bairros e refúgios"), tags$td(selo("mun")), tags$td("Indicadores por bairro de ", NOME, " (arquivo próprio do CISNit).")),
          tags$tr(tags$td("Open Data BCN (refúgios)"), tags$td(selo("ref")), tags$td("Barcelona: serve só de modelo de esquema e de lição; não entra nas análises de ", NOME, "."))))
    )),
  # ---------- Acoes do Plano (secao 6.3 do relatorio)
  nav_panel("Ações do Plano (6.3)", value = "acoes",
    div(style = "padding:10px 16px;",
      h2(class = "titulo-pag", "Ações do Plano com interface direta com o CISNit"),
      p(class = "sub-pag", "Seção 6.3 do relatório técnico · Plano de Adaptação, Mitigação e Resiliência de Niterói (2026)"),
      hr(class = "divisor"),
      aviso(),
      layout_columns(col_widths = c(6, 6),
        card_acao(11, "Arborização e refúgios climáticos",
          "Expandir e monitorar a arborização urbana e implementar refúgios climáticos (aspersores, piscinas públicas, sombreamento, bebedouros). Líder: SECONSER; curto prazo (até 2030).",
          "Fornecer a camada de vulnerabilidade e de demanda de saúde para priorizar locais (11.4) e propor indicador de uso e de cobertura.",
          "Índice de prioridade por bairro (idosos, renda, cobertura arbórea, cobertura de refúgio) e cobertura de refúgio em 10 minutos a pé.", "vul", "Vulnerabilidade e refúgios"),
        card_acao(17, "Centro de Resiliência, monitoramento e alerta",
          "Criar o Centro de Resiliência Municipal e aprimorar o monitoramento e alerta da Defesa Civil, incluindo estações de temperatura (17.6) e registro de eventos extremos com consequências (17.4).",
          "Usar as estações de temperatura como insumo de alerta e fornecer ao registro de eventos extremos os dados de impacto em saúde.",
          "Situação diária do índice de calor, níveis de calor, janela configurável e série de temperatura.", "sit", "Situação atual"),
        card_acao(18, "Plano de Contingência para ondas de calor",
          "Elaborar e publicar Plano de Contingência para ondas de calor (18.4) e atualizar anualmente o Plano de Verão e Defesa Civil (18.2). Líder: SMPD; curtíssimo prazo.",
          "Redigir o componente de saúde: gatilhos, grupos prioritários e fluxos da rede de saúde.",
          "Cortes editáveis dos níveis de calor e risco relativo por faixa de exposição, para fundamentar os gatilhos.", "lim", "Limiares e níveis"),
        card_acao(21, "Diagnóstico e Plano de Clima e Saúde",
          "Elaborar diagnóstico e plano estratégico de mudança do clima e saúde (21.1), identificar áreas para abrigos (21.2), qualificar a atenção básica (21.3) e consolidar rede de abrigos (21.4). Líder: SMS; curtíssimo prazo (até 2026).",
          "Coordenação técnica natural do diagnóstico, com séries de morbimortalidade e indicadores de resultado.",
          "Episódios de calor com óbitos e internações observados, esperados e excesso, razão entre dias em episódio e fora, e proporção de idosos por bairro.", "des", "Desfechos em saúde"),
        card_acao(20, "Arboviroses (referência clima-saúde)",
          "Combater e monitorar arboviroses; Wolbachia cobre 100% do território, com redução reportada de cerca de 70% dos casos de dengue, 60% de chikungunya e 40% de zika nas áreas de intervenção.",
          "Modelo de integração clima-saúde com monitoramento contínuo de resultados, replicável para calor.",
          "Série semanal de dengue (InfoDengue) junto à temperatura mínima, para acompanhar a ameaça vizinha ao calor.", "arbo", "Arboviroses")))),
  # ---------- Situacao atual
  nav_panel("Situação atual", value = "sit",
    layout_sidebar(
      sidebar = sidebar(width = 300, open = "always", class = "sidebar-box",
        h6("Janela de análise"),
        sliderInput("janela", NULL, min = 30, max = 365, value = 90, step = 10, post = " dias"),
        h6("Cortes dos níveis (índice de calor, °C)"),
        numericInput("c2", "Calor 2 a partir de", 32, step = 0.5),
        numericInput("c3", "Calor 3 a partir de", 36, step = 0.5),
        numericInput("c4", "Calor 4 a partir de", 40, step = 0.5),
        numericInput("c5", "Calor 5 a partir de", 44, step = 0.5),
        bloco_def(def("Ação do Plano", "Ação 17 · monitoramento e alerta"), def("Indicador", "Índice de calor máximo diário"), def("Fonte", "Open-Meteo / INMET"),
                  def("Medida", "°C"), def("Observação", "Cortes provisórios; calibrar com a aba Limiares"))),
      aviso(),
      layout_columns(col_widths = c(4, 4, 4),
        kpi("k_data"), kpi("k_ic"), kpi("k_nivel")),
      div(class = "card meta-box mb-3", div(class = "card-body", style = "padding:10px 16px;",
        span("Meta de gestão", style = "font-size:.8rem;"), h4(textOutput("txt_meta"), style = "margin:0;"))),
      card(ch("Índice de calor máximo diário e níveis", "grade", "dl_g_sit"), echarts4rOutput("g_sit", height = 330),
        nota("Índice de calor (Rothfusz) calculado com temperatura e umidade modeladas (Open-Meteo) para ", NOME, ". Linhas pontilhadas: cortes provisórios dos níveis de calor.")),
      layout_columns(col_widths = c(6, 6),
        card(ch("Temperatura máxima e mínima", "grade", "dl_g_temp"), echarts4rOutput("g_temp", height = 280),
          nota("Temperatura do ar a 2 m (Open-Meteo), ponto de grade em ", NOME, ".")),
        card(ch("Dias por nível de calor na janela", "grade", "dl_g_dias"), echarts4rOutput("g_dias", height = 280),
          nota("Dias da janela escolhida classificados pelo índice de calor máximo diário."))))),
  # ---------- Limiares
  nav_panel("Limiares e níveis", value = "lim",
    layout_sidebar(
      sidebar = sidebar(width = 300, open = "always", class = "sidebar-box",
        h6("Exposição"),
        selectInput("lim_var", NULL, c("Índice de calor máximo" = "ic_max", "Temperatura máxima" = "tmax", "Temperatura mínima" = "tmin")),
        checkboxInput("lim_ma3", "Média móvel de 3 dias (como no SACS francês)", TRUE),
        h6("Desfecho"),
        selectInput("lim_des", NULL, c("Óbitos" = "obitos", "Internações" = "internacoes")),
        h6("Período"),
        uiOutput("lim_anos_ui"),
        bloco_def(def("Ação do Plano", "Ação 18 · Plano de Contingência de calor"), def("Método", "Regressão quasi-Poisson por faixas da exposição, com ajuste por mês e ano"),
                  def("Referência", "Faixa mais baixa"), def("Medida", "Risco relativo (IC 95%)"),
                  def("Limite", "Modelo simplificado; um modelo de defasagem distribuída (DLNM) é o passo seguinte"))),
      aviso(),
      card(ch("Risco relativo por faixa de exposição", c("grade","mun"), "dl_rr"), echarts4rOutput("g_rr", height = 360),
           nota("Regressão quasi-Poisson por faixas da exposição, com ajuste por mês e ano; barras: IC 95%. Óbitos e internações por residência em ", NOME, " (SIM e SIH).")),
      card(ch("Tabela de risco relativo", c("grade","mun")), DTOutput("t_rr")))),
  # ---------- Desfechos
  nav_panel("Desfechos em saúde", value = "des",
    layout_sidebar(
      sidebar = sidebar(width = 300, open = "always", class = "sidebar-box",
        h6("Desfecho"),
        selectInput("des_var", NULL, c("Óbitos" = "obitos", "Internações" = "internacoes")),
        uiOutput("terr_des"),
        h6("Episódio de calor"),
        numericInput("ep_corte", "Índice de calor mínimo (°C)", 36, step = 0.5),
        numericInput("ep_dias", "Duração mínima (dias)", 3, min = 1, step = 1),
        uiOutput("des_anos_ui"),
        bloco_def(def("Ação do Plano", "Ação 21 · Plano de Clima e Saúde"), def("Esperado", "Média diária do mesmo mês nos dias fora de episódio × duração"),
                  def("Privacidade", "Contagens < 5 ocultadas"))),
      aviso(),
      layout_columns(col_widths = c(4, 4, 4),
        kpi("k_ep"), kpi("k_epd"), kpi("k_razao")),
      layout_columns(col_widths = c(6, 6),
        card(ch("Média diária do desfecho por nível de calor", c("grade","mun"), "dl_g_des_nivel"), echarts4rOutput("g_des_nivel", height = 300),
          nota("Média diária do desfecho selecionado nos dias de cada nível de calor (residentes de ", NOME, ").")),
        card(ch("Desfecho mensal e episódios", "mun", "dl_g_des_mes"), echarts4rOutput("g_des_mes", height = 300),
          nota("Total mensal do desfecho selecionado, residentes de ", NOME, " (SIM e SIH)."))),
      card(ch("Episódios de calor: observado, esperado e excesso", c("grade","mun"), "dl_ep"), DTOutput("t_ep")))),
  # ---------- Vulnerabilidade
  nav_panel("Vulnerabilidade e refúgios", value = "vul",
    layout_sidebar(
      sidebar = sidebar(width = 300, open = "always", class = "sidebar-box",
        uiOutput("terr_vul"),
        h6("Indicador"),
        selectInput("vul_var", NULL, c("Índice de prioridade (0-100)" = "indice_prioridade",
          "População 65+ (%)" = "pop_65mais_pct", "Renda mediana (R$)" = "renda_mediana",
          "Cobertura arbórea (%)" = "cobertura_arborea_pct", "Refúgios (n)" = "n_refugios",
          "Cobertura de refúgio em 10 min a pé (%)" = "cobertura_refugio_10min_pct")),
        bloco_def(def("Ação do Plano", "Ação 11 · arborização e refúgios (11.4, 11.5, 21.2)"), def("Índice de prioridade", "Combina maior proporção de idosos, menor renda, menor cobertura arbórea e menor cobertura de refúgios"),
                  def("Uso", "Orientar Ações 11.4, 11.5 e 21.2 do Plano de Niterói"),
                  def("Cobertura de refúgio", "Parcela da população a até 10 minutos a pé de um refúgio (lição de Barcelona)"))),
      aviso(),
      card(ch("Mapa dos bairros pelo indicador selecionado", "mun"), uiOutput("mapa_ui")),
      card(ch("Bairros ordenados pelo indicador selecionado", "mun", "dl_g_vul"), echarts4rOutput("g_vul", height = 460),
           nota("Fonte: dados/bairros_vulnerabilidade.csv (Censo 2022 do IBGE e dados da Prefeitura de Niterói).")),
      card(ch("Tabela por bairro", "mun"), DTOutput("t_vul")))),
  # ---------- Arboviroses (Acao 20)
  nav_panel("Arboviroses", value = "arbo",
    layout_sidebar(
      sidebar = sidebar(width = 300, open = "always", class = "sidebar-box",
        h6("Período"), uiOutput("arbo_anos_ui"),
        bloco_def(def("Ação do Plano", "Ação 20 · Combater e monitorar arboviroses"), def("Fonte", "InfoDengue (casos estimados por semana epidemiológica)"),
                  def("Observação", "Uso como referência de integração clima-saúde; não substitui o boletim da vigilância"))),
      if (ARBO$simulado) div(class = "aviso-sim", strong("Dados simulados para demonstração. "), "Gere dados/infodengue_dengue_niteroi.csv com coletar_dados_calor.R."),
      card(ch("Casos semanais de dengue e temperatura mínima", c("mun","grade"), "dl_g_arbo"), echarts4rOutput("g_arbo", height = 380),
        nota("Casos estimados de dengue por semana epidemiológica (InfoDengue) e temperatura mínima (Open-Meteo)."))))
  ))

# ---- Server ----------------------------------------------------------
server <- function(input, output, session) {
  if (!EH_NIT) bslib::nav_hide("abas", "acoes")
  d <- DADOS$d
  cortes <- reactive(c(input$c2, input$c3, input$c4, input$c5))
  anos_all <- sort(unique(year(d$data)))
  output$lim_anos_ui <- renderUI(sliderInput("lim_anos", NULL, min(anos_all), max(anos_all),
                                             c(min(anos_all), max(anos_all)), sep = "", step = 1))
  output$des_anos_ui <- renderUI(sliderInput("des_anos", "Período", min(anos_all), max(anos_all),
                                             c(min(anos_all), max(anos_all)), sep = "", step = 1))



  # ----- Downloads dos dados de cada grafico (botao CSV no cabecalho do cartao)
  output$dl_g_sit <- downloadHandler(paste0("situacao_atual_calor_", SLUG, ".csv"),
    function(file) write_csv(dsit() |> mutate(nivel = unname(NIVEIS[nivel])), file))
  output$dl_g_temp <- downloadHandler(paste0("temperatura_", SLUG, ".csv"), function(file) write_csv(dsit() |> select(data, tmax, tmin, tmed, umid_med), file))
  output$dl_g_dias <- downloadHandler(paste0("dias_por_nivel_calor_", SLUG, ".csv"),
    function(file) write_csv(dsit() |> count(nivel) |> mutate(nivel = unname(NIVEIS[nivel])), file))
  output$dl_g_des_nivel <- downloadHandler(paste0("desfecho_por_nivel_calor_", SLUG, ".csv"),
    function(file) write_csv(dep() |> group_by(nivel) |> summarise(media_diaria = mean(desfecho), dias = n(), .groups = "drop") |>
                               mutate(nivel = unname(NIVEIS[nivel])), file))
  output$dl_g_des_mes <- downloadHandler(paste0("desfecho_mensal_", SLUG, ".csv"),
    function(file) write_csv(dep() |> group_by(mes = floor_date(data, "month")) |> summarise(total = sum(desfecho), .groups = "drop"), file))
  output$dl_g_vul <- downloadHandler(paste0("bairros_indicadores_", SLUG, ".csv"), function(file) write_csv(bairros_sel() |> select(-any_of("k")), file))
  output$dl_g_arbo <- downloadHandler(paste0("dengue_semanal_", SLUG, ".csv"), function(file) write_csv(ARBO$a, file))

  # ----- Filtro de territorio (bairro) em Desfechos e Vulnerabilidade
  terr_ui <- function(suf) {
    tagList(h6("Território"),
      selectInput(paste0("bai_", suf), "Bairro", c("Todos", BAIRROS_ALL)))
  }
  output$terr_des <- renderUI(terr_ui("des"))
  output$terr_vul <- renderUI(terr_ui("vul"))
  # chaves de bairro selecionadas (NULL = municipio todo)
  sel_k <- function(suf) {
    bai <- input[[paste0("bai_", suf)]]
    if (!is.null(bai) && bai != "Todos") return(chave_b(bai))
    NULL
  }
  bairros_sel <- reactive({ k <- sel_k("vul"); b <- DADOS$bairros; if (is.null(k)) b else b[b$k %in% k, ] })

  # ----- Navegacao a partir dos cartoes de acao
  for (id in c("vul", "sit", "lim", "des", "arbo")) local({
    i <- id; observeEvent(input[[paste0("ir_", i)]], updateNavsetTab(session, "abas", selected = i)) })
  # ----- Arboviroses
  output$arbo_anos_ui <- renderUI({ an <- sort(unique(year(ARBO$a$data)))
    sliderInput("arbo_anos", NULL, min(an), max(an), c(max(min(an), max(an) - 3), max(an)), sep = "", step = 1) })
  output$g_arbo <- renderEcharts4r({
    req(input$arbo_anos)
    a <- ARBO$a |> filter(year(data) >= input$arbo_anos[1], year(data) <= input$arbo_anos[2])
    a$d <- format(a$data, "%Y-%m-%d")
    p <- a |> e_charts(d) |>
      e_bar(casos, name = "Casos", itemStyle = list(color = COR$laranja)) |>
      e_y_axis(index = 0, name = "Casos por semana", nameLocation = "middle", nameGap = 38)
    if (any(!is.na(a$tempmin)))
      p <- p |> e_line(tempmin, name = "Temp. mínima (°C)", y_index = 1, symbol = "none",
                       lineStyle = list(color = COR$petroleo, width = 2), itemStyle = list(color = COR$petroleo)) |>
        e_y_axis(index = 1, name = "°C", splitLine = list(show = FALSE))
    p |> e_x_axis(axisLabel = list(hideOverlap = TRUE)) |> e_datazoom(type = "slider") |>
      e_legend(left = 0, top = 0) |> ec_base()
  })

  # ----- Situacao atual
  dsit <- reactive({
    x <- tail(d, input$janela); x$nivel <- classificar(x$ic_max, cortes()); x })
  output$k_data <- renderEcharts4r(kpi_ec(format(max(d$data), "%d/%m/%Y"), "Último dia com dado", COR$petroleo))
  output$k_ic <- renderEcharts4r(kpi_ec(paste0(round(tail(d$ic_max, 1), 1), " °C"), "Índice de calor máximo", COR$laranja_queimado))
  output$k_nivel <- renderEcharts4r({
    n <- classificar(tail(d$ic_max, 1), cortes())
    kpi_ec(NIVEIS[n], "Nível de calor no último dia", unname(COR_NIVEL[n]))
  })
  output$txt_meta <- renderText("A definir com a gestão (por exemplo: zero óbitos em excesso em episódios de Calor 3 ou mais)")
  output$g_sit <- renderEcharts4r({
    x <- dsit(); cs <- cortes()
    x$d <- format(x$data, "%Y-%m-%d"); x$color <- unname(COR_NIVEL[x$nivel])
    linhas <- lapply(seq_along(cs), function(i) list(yAxis = cs[i], name = NIVEIS[i + 1],
      lineStyle = list(color = unname(COR_NIVEL[i + 1]), type = "dotted", width = 1.5), label = list(formatter = NIVEIS[i + 1])))
    x |> e_charts(d) |>
      e_line(ic_max, name = "Índice de calor máx. (°C)", symbolSize = 7,
             lineStyle = list(color = COR$petroleo, width = 1.5)) |>
      e_add_nested("itemStyle", color) |>
      e_mark_line(data = linhas, symbol = "none") |>
      e_y_axis(name = "Índice de calor máx. (°C)", nameLocation = "middle", nameGap = 38, scale = TRUE) |>
      e_x_axis(axisLabel = list(hideOverlap = TRUE)) |>
      e_legend(show = FALSE) |> ec_base()
  })
  output$g_temp <- renderEcharts4r({
    x <- dsit(); x$d <- format(x$data, "%Y-%m-%d")
    x |> e_charts(d) |>
      e_line(tmax, name = "Máxima", symbol = "none", lineStyle = list(color = COR$laranja), itemStyle = list(color = COR$laranja)) |>
      e_line(tmin, name = "Mínima", symbol = "none", lineStyle = list(color = COR$turquesa), itemStyle = list(color = COR$turquesa)) |>
      e_y_axis(name = "°C", scale = TRUE) |> e_x_axis(axisLabel = list(hideOverlap = TRUE)) |>
      e_legend(left = 0, top = 0) |> ec_base()
  })
  output$g_dias <- renderEcharts4r({
    x <- dsit() |> count(nivel) |> mutate(nome = unname(NIVEIS[nivel]), color = unname(COR_NIVEL[nivel]))
    x |> e_charts(nome) |> e_bar(n, name = "Dias", legend = FALSE) |>
      e_add_nested("itemStyle", color) |> e_labels(position = "top") |>
      e_y_axis(name = "Dias") |> ec_base()
  })

  # ----- Limiares
  rr_tab <- reactive({
    req(input$lim_anos)
    x <- d |> filter(year(data) >= input$lim_anos[1], year(data) <= input$lim_anos[2])
    v <- x[[input$lim_var]]
    if (isTRUE(input$lim_ma3)) v <- as.numeric(stats::filter(v, rep(1/3, 3), sides = 1))
    x$expo <- v; x$y <- x[[input$lim_des]]; x <- x[!is.na(x$expo), ]
    cortes_q <- unique(quantile(x$expo, c(0, .5, .75, .9, .95, .99, 1), na.rm = TRUE))
    validate(need(length(cortes_q) >= 4, "Variação insuficiente na exposição."))
    x$faixa <- cut(x$expo, cortes_q, include.lowest = TRUE, dig.lab = 3)
    m <- glm(y ~ faixa + factor(month(data)) + factor(year(data)), data = x, family = quasipoisson())
    co <- summary(m)$coefficients; ci <- confint.default(m)
    sel <- grep("^faixa", rownames(co))
    tibble(faixa = levels(x$faixa)[-1], rr = exp(co[sel, 1]), lo = exp(ci[sel, 1]), hi = exp(ci[sel, 2]),
           dias = as.integer(table(x$faixa)[-1]))
  })
  output$g_rr <- renderEcharts4r({
    t <- rr_tab()
    t |> e_charts(faixa) |>
      e_line(rr, name = "Risco relativo", symbolSize = 12, lineStyle = list(opacity = 0),
             itemStyle = list(color = COR$laranja_queimado)) |>
      e_error_bar(lo, hi, name = "IC 95%", itemStyle = list(color = COR$petroleo)) |>
      e_mark_line(data = list(yAxis = 1), symbol = "none", title = "RR = 1",
                  lineStyle = list(color = COR$verde, type = "dashed")) |>
      e_x_axis(name = "Faixa da exposição (referência: faixa mais baixa)", nameLocation = "middle", nameGap = 30) |>
      e_y_axis(name = "Risco relativo", scale = TRUE) |>
      e_legend(left = 0, top = 0) |> ec_base()
  })
  output$t_rr <- renderDT(datatable(rr_tab() |> mutate(across(c(rr, lo, hi), ~ round(.x, 2))) |>
    rename(Faixa = faixa, RR = rr, `IC95% inf.` = lo, `IC95% sup.` = hi, Dias = dias),
    rownames = FALSE, options = list(dom = "t", language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/pt-BR.json"))))
  output$dl_rr <- downloadHandler(paste0("risco_relativo_calor_", SLUG, ".csv"), function(file) write_csv(rr_tab(), file))

  # ----- Desfechos
  dep <- reactive({
    req(input$des_anos)
    x <- d |> filter(year(data) >= input$des_anos[1], year(data) <= input$des_anos[2])
    k <- sel_k("des")
    if (!is.null(k)) {
      validate(need(!is.null(OB_BAIRRO), paste0("Para filtrar por bairro, gere dados/sim_obitos_bairro_", SLUG, ".csv com coletar_dados_calor.R.")),
               need(input$des_var == "obitos", "Internações (SIH) não estão disponíveis por bairro; escolha Óbitos ou o município todo."))
      ob <- OB_BAIRRO |> filter(k %in% !!k) |> group_by(data) |> summarise(o = sum(obitos), .groups = "drop")
      x$obitos <- coalesce(ob$o[match(x$data, ob$data)], 0L)
    }
    x$desfecho <- x[[input$des_var]]; x$nivel <- classificar(x$ic_max, cortes()); x })
  ep <- reactive({
    x <- dep(); e <- episodios(x, input$ep_corte, input$ep_dias)
    if (!nrow(e)) return(e)
    fora <- x |> mutate(em_ep = FALSE)
    for (i in seq_len(nrow(e))) fora$em_ep[fora$data >= e$inicio[i] & fora$data <= e$fim[i]] <- TRUE
    media_mes <- fora |> filter(!em_ep) |> group_by(mes = month(data)) |> summarise(m = mean(desfecho), .groups = "drop")
    e$esperado <- vapply(seq_len(nrow(e)), function(i) {
      mm <- media_mes$m[media_mes$mes == month(e$inicio[i])]
      round((if (length(mm)) mm else mean(fora$desfecho[!fora$em_ep])) * e$dias[i], 1) }, numeric(1))
    e$excesso <- round(e$observado - e$esperado, 1); e })
  output$k_ep <- renderEcharts4r(kpi_ec(nrow(ep()), "Episódios no período", COR$laranja_queimado))
  output$k_epd <- renderEcharts4r(kpi_ec(if (nrow(ep())) sum(ep()$dias) else 0, "Dias em episódio", COR$petroleo))
  output$k_razao <- renderEcharts4r({
    x <- dep(); e <- ep()
    v <- if (!nrow(e)) "—" else {
      em <- rep(FALSE, nrow(x)); for (i in seq_len(nrow(e))) em[x$data >= e$inicio[i] & x$data <= e$fim[i]] <- TRUE
      sprintf("%.2f", mean(x$desfecho[em]) / mean(x$desfecho[!em])) }
    kpi_ec(v, "Razão: média diária em episódio / fora", COR$laranja_queimado)
  })
  output$g_des_nivel <- renderEcharts4r({
    x <- dep() |> group_by(nivel) |> summarise(media = round(mean(desfecho), 2), dias = n(), .groups = "drop") |>
      mutate(nome = unname(NIVEIS[nivel]), color = unname(COR_NIVEL[nivel]))
    x |> e_charts(nome) |> e_bar(media, name = "Média diária", legend = FALSE) |>
      e_add_nested("itemStyle", color) |> e_labels(position = "top") |>
      e_y_axis(name = "Média diária") |> ec_base()
  })
  output$g_des_mes <- renderEcharts4r({
    x <- dep() |> group_by(mes = floor_date(data, "month")) |> summarise(total = sum(desfecho), .groups = "drop")
    x$d <- format(x$mes, "%Y-%m")
    x |> e_charts(d) |> e_line(total, name = "Total mensal", symbol = "none",
                              lineStyle = list(color = COR$laranja_queimado), itemStyle = list(color = COR$laranja_queimado)) |>
      e_y_axis(name = "Total mensal") |> e_x_axis(axisLabel = list(hideOverlap = TRUE)) |>
      e_legend(show = FALSE) |> ec_base()
  })
  tab_ep_exib <- reactive({
    e <- ep(); if (!nrow(e)) return(tibble(Aviso = "Nenhum episódio com os parâmetros escolhidos."))
    e |> transmute(Início = format(inicio, "%d/%m/%Y"), Fim = format(fim, "%d/%m/%Y"), Dias = dias,
                   `IC máx. (°C)` = ic_max, `Tmax (°C)` = tmax,
                   Observado = suprimir(observado), Esperado = ifelse(observado < 5, "—", as.character(esperado)),
                   Excesso = ifelse(observado < 5, "—", as.character(excesso))) })
  output$t_ep <- renderDT(datatable(tab_ep_exib(), rownames = FALSE, options = list(pageLength = 8, dom = "tp")))
  output$dl_ep <- downloadHandler(paste0("episodios_calor_", SLUG, ".csv"), function(file) write_csv(tab_ep_exib(), file))

  # ----- Vulnerabilidade
  output$g_vul <- renderEcharts4r({
    v <- input$vul_var; b <- bairros_sel() |> arrange(.data[[v]])
    b$valor <- round(b[[v]], 1)
    b |> e_charts(bairro) |> e_bar(valor, name = rotulo_vul(v), legend = FALSE,
                                   itemStyle = list(color = COR$laranja)) |>
      e_labels(position = "right") |> e_flip_coords() |> ec_base(trigger = "item")
  })
  # Mapa coropletico (echarts). Precisa de dados/bairros_niteroi.geojson (ver README).
  output$mapa_ui <- renderUI({
    if (is.null(MAPA)) return(div(class = "aviso-sim", strong("Mapa indisponível. "),
      "Coloque dados/bairros_niteroi.geojson (limites dos bairros) para ativar o mapa; veja o README."))
    echarts4rOutput("g_mapa", height = 520)
  })
  output$g_mapa <- renderEcharts4r({
    req(MAPA); v <- input$vul_var
    b <- bairros_sel(); b$valor <- round(b[[v]], 1)
    b |> e_charts(bairro) |> e_map_register("niteroi", MAPA) |>
      e_map(valor, map = "niteroi", name = rotulo_vul(v), roam = TRUE,
            itemStyle = list(areaColor = "#eeeeee", borderColor = "#ffffff"),
            emphasis = list(label = list(show = TRUE))) |>
      e_visual_map(valor, inRange = list(color = c("#FDE9D9", COR$ambar, COR$laranja, COR$laranja_queimado, COR$petroleo)),
                   text = c("maior", "menor")) |>
      ec_base(trigger = "item")
  })
  output$t_vul <- renderDT(datatable(bairros_sel() |> select(-any_of("k")) |> arrange(desc(indice_prioridade)) |>
    rename(Bairro = bairro, `65+ (%)` = pop_65mais_pct, `Renda mediana` = renda_mediana, `Cob. arbórea (%)` = cobertura_arborea_pct,
           Refúgios = n_refugios, `Cob. refúgio 10 min (%)` = cobertura_refugio_10min_pct, `Índice de prioridade` = indice_prioridade),
    rownames = FALSE, options = list(pageLength = 15, dom = "tp")))
}

shinyApp(ui, server)
