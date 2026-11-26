# calor_niteroi

Calor extremo e saúde em Niterói (painel e scripts de coleta com dados abertos).

- `app.R` + `www/`: painel Shiny (bslib, echarts4r). Lê os arquivos de `dados/`.
- `coletar_dados_calor.R`: coleta dados públicos (Open-Meteo, INMET, SIM/SIH via DATASUS, InfoDengue) e gera `dados/`. A cidade é configurada no topo do script.
- `gerar_bairros_censo.R`: gera `dados/bairros_vulnerabilidade.csv` a partir do Censo 2022 (IBGE).

Para rodar localmente: abra a pasta no RStudio e use `shiny::runApp()`.

Para publicar (Posit Connect Cloud): na raiz do projeto rode `rsconnect::writeManifest()`, faça commit do `manifest.json` e publique a partir deste repositório com `app.R` como arquivo principal.

Os microdados (`*.rds`), o INMET bruto e a série horária não são versionados; rode o script de coleta para gerá-los.
