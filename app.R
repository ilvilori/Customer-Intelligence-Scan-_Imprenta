# app.R — Customer Intelligence Scan
# Orden de pestañas: Pirámide valor, Comportamiento, Experiencia y
# Fidelización, Captación, Contactabilidad, Gestión de Ventas.
#
# Lee data_vista2.xlsx (debe estar en la misma carpeta / repo que este
# archivo). Cada vista tiene sus propios filtros, así que pueden abrir con
# valores por defecto distintos.vvvvv

#shiny::runApp("app_imp.R")
#install.packages("rsconnect")
#install.packages("usethis")
#writeManifest()
library(shiny)
library(bslib)
library(dplyr)
library(readxl)
library(plotly)
library(scales)
library(wordcloud)

# ---------- datos ----------
datos <- read_excel("data_vista2.xlsx") |>
  select(
    Annual, Tier, Ventas, Margen_Bruto_Porc, Margen_Bruto_USD, Margen_Neto_USD, Clientes,
    Compra_anual, Frecuencia, Pdtos_compra, Ticket_Prom,
    Marg_Cli_USD, Marg_Cli_Porc, Precio_prom = Precio_prom_pdtos_cli,
    Pago_CD, Pago_OFP, NPS, Recompra, Cuali_NPS,
    Contactable, Mail, WSP,
    Clientes_Nuevos, Clientes_Nuevos_Porc,
    Ventas_USD_Nuevos = `Ventas.USD_Clientes_Nuevos`,
    Ventas_Porc_Nuevos = `Ventas.Porc_Clientes_Nuevos`,
    Recompra_Nuevos,
    Rent_Neto_Cli_USD, Rent_Neto_Cli_Porc, Mix_Cat_Cli, Cross_Sell_Rate,
    Penet_Comercial, Penet_Cajas, Penet_Etiquetas,
    Mix_Ventas_Comercial, Mix_Ventas_Cajas, Mix_Ventas_Etiquetas,
    Clientes_Perdidos, Fact_Perdida_USD, Fact_Perdida_Porc,
    Vendedores, Costo_Comercial_USD, Costo_Comercial_Porc,
    Cotizaciones_Num, Cotizaciones_Valor_USD, Tasa_Conv_Num, Tasa_Conv_Valor,
    Conv_Num_Comercial, Conv_Num_Cajas, Conv_Num_Etiquetas,
    Conv_Valor_Comercial, Conv_Valor_Cajas, Conv_Valor_Etiquetas, Tiempo_Conv_Dias
  ) |>
  mutate(Tier = as.character(Tier))

# Hoja "categorias" del mismo archivo: ventas, márgenes y clientes compradores por
# categoría, año y Tier (se usa en la Parte 3 de Comportamiento)
nombres_cat <- c("Impresión Comercial" = "Comercial", "Cajas Plegadizas" = "Cajas",
                 "Etiquetas Autoadhesivas" = "Etiquetas")
categorias <- read_excel("data_vista2.xlsx", sheet = "categorias") |>
  mutate(Tier = as.character(Tier),
         Cat_Corta = factor(unname(nombres_cat[Categoria]), levels = unname(nombres_cat)))

# Hoja "vendedores_tier_cat": cotizaciones por año, vendedor, Tier y categoría
# (base del mapa de calor de Gestión de Ventas)
vtc <- read_excel("data_vista2.xlsx", sheet = "vendedores_tier_cat") |>
  mutate(Tier = as.character(Tier), Cat_Corta = unname(nombres_cat[Categoria]))

# 2021 está en los datos solo como base para calcular las variaciones de 2022:
# no aparece en los filtros de año ni en la serie de Captación.
ANIOS_SOLO_COMPARACION <- 2021
anios <- sort(setdiff(unique(datos$Annual), ANIOS_SOLO_COMPARACION))

# Año con el que abren todas las pestañas que tienen filtro de año
ANIO_INICIAL <- 2024

paleta_tier <- c("1" = "#2B5F5A", "2" = "#1C2D42", "3" = "#C98A3B", "4" = "#B4483A")

fmt_usd  <- function(x) scales::dollar(x, accuracy = 1, scale_cut = scales::cut_short_scale())
fmt_usd2 <- function(x) scales::dollar(x, accuracy = 0.01)
fmt_pct  <- function(x) scales::percent(x, accuracy = 0.1)
fmt_n    <- function(x) format(round(x), big.mark = ",")
fmt_dec  <- function(x) formatC(x, format = "f", digits = 2)
# Formatos de los gráficos de barras: como máximo 1 decimal
fmt_usd1 <- function(x) scales::dollar(x, accuracy = 0.1, scale_cut = scales::cut_short_scale())
fmt_dec1 <- function(x) formatC(x, format = "f", digits = 1)
# Encabezados: montos sin decimales
fmt_usd0 <- function(x) scales::dollar(x, accuracy = 1)

# Colores de las tres categorías de producto
paleta_cat <- c("Comercial" = "#6A4C93", "Cajas" = "#3D8FC4", "Etiquetas" = "#4D5560")  # violeta, azul claro, gris oscuro: no se usan en otros gráficos

# Eje Y de 0 hasta el primer valor "redondo" por encima del máximo, con una
# marca visible en ese tope (así el valor más alto siempre queda dentro del eje).
eje_hasta_max <- function(valores, formato) {
  marcas <- pretty(c(0, max(valores, na.rm = TRUE) * 1.05))
  list(range = c(0, max(marcas)), tickmode = "array", tickvals = marcas, ticktext = formato(marcas))
}

# Variación vs. año anterior con color de semáforo:
# verde si sube más de 0.5%, rojo si baja (menor a 0%), amarillo en otro caso.
COLOR_VERDE    <- "#2E7D32"
COLOR_ROJO     <- "#C62828"
COLOR_AMARILLO <- "#D4A017"

# Color de semáforo para una variación v. modo = "rel" (verde si sube más de 0.5%)
# o "pp" (verde desde UMBRAL_VERDE_PP). invertir = TRUE para indicadores donde
# bajar es bueno (costo comercial, tiempo de conversión).
color_semaforo <- function(v, modo = "rel", invertir = FALSE) {
  if (invertir) v <- -v
  verde <- if (modo == "pp") v >= UMBRAL_VERDE_PP else v > 0.005
  ifelse(verde, COLOR_VERDE, ifelse(v < 0, COLOR_ROJO, COLOR_AMARILLO))
}

variacion_anual <- function(actual, previo, modo = "rel", invertir = FALSE) {
  if (length(previo) == 0 || is.na(previo) || previo == 0) {
    return(tags$span(" (sin año anterior)",
                     style = "font-size: 0.65rem; font-weight: 400; color: var(--bs-secondary-color);"))
  }
  v <- if (modo == "pp") actual - previo else actual / previo - 1
  v <- round(v * 100, 1) / 100   # evita un "-0.0" en rojo
  tags$span(paste0(" (", if (v >= 0) "+" else "", formatC(v * 100, format = "f", digits = 1),
                   if (modo == "pp") " pp" else "%", ")"),
            style = paste0("font-size: 0.8rem; font-weight: 600; color: ",
                           color_semaforo(v, modo, invertir), ";"))
}

# Variación interanual para las etiquetas de los gráficos de barras: texto pequeño
# en color de semáforo, en una línea debajo del valor. modo = "rel" (variación %)
# o "pp" (puntos porcentuales, para indicadores que ya son tasas). Sin año
# anterior devuelve "" (no se muestra nada).
TAM_VAR_GRAFICO <- 8   # tamaño de letra de la variación en los gráficos
# Corte para verde en las variaciones en puntos porcentuales (márgenes % y cross sell):
# verde desde +0.1 pp, amarillo en 0.0 pp, rojo si baja. En variaciones % sigue el 0.5%.
UMBRAL_VERDE_PP <- 0.001

# salto = FALSE devuelve solo la variación, sin la línea en blanco previa
var_html <- function(actual, previo, modo = "rel", salto = TRUE, tam = TAM_VAR_GRAFICO, invertir = FALSE) {
  out <- rep("", length(actual))
  ok <- !is.na(previo) & previo != 0
  if (!any(ok)) return(out)
  v <- if (modo == "pp") actual - previo else actual / previo - 1
  # se redondea a 1 decimal antes de elegir signo y color (evita un "-0.0" en rojo)
  v <- round(v * 100, 1) / 100
  txt <- paste0(ifelse(v >= 0, "+", ""), formatC(v * 100, format = "f", digits = 1),
                if (modo == "pp") " pp" else "%")
  color <- color_semaforo(v, modo, invertir)
  out[ok] <- paste0(if (salto) "<br>" else "", "<span style='font-size:", tam, "px;color:", color[ok], "'>",
                    txt[ok], "</span>")
  out
}

# Tamaños (ajústalos aquí si quieres afinarlos)
ALTO_KPI_V1     <- "63px"   # Vista 1: encabezados
ALTO_GRAFICO_V1 <- "145px"  # Vista 1: gráficos, a un tercio del alto original
ALTO_KPI        <- "64px"   # Vista 2: encabezados (3% menos que los 66px anteriores)
ALTO_GRAFICO    <- "128px"  # Vista 2: gráficos más chicos, para caber sin scroll
ALTO_NUBE       <- "160px"  # Vista 3: nube de palabras (antes 320px), para que la pestaña quepa en una pantalla
COLOR_NUBE_BAJA <- "#E67E22"  # Vista 3: naranja para las palabras menos mencionadas
COLOR_NUBE_ALTA <- "#C62828"  # Vista 3: rojo para las palabras más mencionadas
ALTO_MAPA       <- "380px"  # Gestión de Ventas: mapa de calor por vendedor
TAM_TEXTO_PIE   <- 9        # Vista 1: tamaño de letra del Tier y % dentro de cada pie

css_vista2 <- "
  .vb-compacto-v1 .card-body { padding: 0.25rem 0.6rem; }
  .vb-compacto-v1 .value-box-title { font-size: 0.78rem; }
  .vb-compacto-v1 .value-box-value { font-size: 1.4rem; line-height: 1.15; }
  .vb-compacto .card-body { padding: 0.19rem 0.58rem; }
  .vb-compacto .value-box-title { font-size: 0.63rem; }
  .vb-compacto .value-box-value { font-size: 1.12rem; line-height: 1.1; }
  .cmp-card .card-header { padding: 0.2rem 0.6rem; font-size: 0.72rem; }
  .cmp-card .card-body { padding: 0.15rem; }
"

# Mapa de calor de Gestión de Ventas: cada vendedor se compara con el promedio del
# equipo en la misma columna (fila "Total equipo"). Tonos claros para leer el número.
MAPA_VERDE    <- "#A5D6A7"   # por encima del promedio del equipo
MAPA_AMARILLO <- "#FFE082"   # a menos de MAPA_BANDA_PP del promedio
MAPA_ROJO     <- "#EF9A9A"   # por debajo del promedio
MAPA_EQUIPO   <- "#C9CED6"   # fila "Total equipo" (referencia)
MAPA_BANDA_PP <- 0.005       # ±0.5 pp se considera "en el promedio"

# Encabezados de Gestión de Ventas (se repiten en Parte 1 y Parte 2)
kpis_gv <- function(sufijo = "") {
  id <- function(x) paste0("gv_", x, sufijo)
  vb <- function(titulo, x) value_box(titulo, uiOutput(id(x)), height = ALTO_KPI, class = "vb-compacto")
  tagList(
    layout_column_wrap(width = 1/3,
                       vb("Número de vendedores", "vendedores"),
                       vb("Costo comercial USD (% de ventas)", "costo"),
                       vb("Cotizaciones generadas (#)", "cot_num")
    ),
    layout_column_wrap(width = 1/3,
                       vb("Tasa de conversión cotizaciones (#)", "conv_num"),
                       vb("Valor cotizaciones generadas", "cot_val"),
                       vb("Tasa de conversión cotizaciones (valor)", "conv_val")
    )
  )
}

# Encabezados de la Vista 2 (se repiten en las dos partes; el sufijo evita
# ids duplicados en Shiny). Cada uno muestra el total del año y su variación.
kpis_vista2 <- function(sufijo = "") {
  id <- function(x) paste0("v2_", x, sufijo)
  vb <- function(titulo, x) value_box(titulo, uiOutput(id(x)), height = ALTO_KPI, class = "vb-compacto")
  tagList(
    layout_column_wrap(width = 1/4,
                       vb("Clientes", "clientes"),
                       vb("Compra anual promedio cliente", "compra"),
                       vb("Margen bruto promedio cliente USD (%)", "mb_cli"),
                       vb("Margen neto promedio cliente USD (%)", "mn_cli")
    ),
    layout_column_wrap(width = 1/3,
                       vb("Ticket promedio", "ticket"),
                       vb("Pedidos promedio", "pedidos"),
                       vb("Categorías promedio cliente", "categorias")
    )
  )
}

# ---------- UI ----------
ui <- page_navbar(
  title = "Customer Intelligence Scan",
  theme = bs_theme(bootswatch = "materia", primary = "#2B5F5A"),
  header = tags$head(tags$style(HTML(css_vista2))),
  
  # ===== Vista 1 =====
  nav_panel("Pirámide valor",
            layout_sidebar(
              sidebar = sidebar(
                title = "Filtros",
                selectInput("anio", "Año", choices = anios, selected = ANIO_INICIAL)
              ),
              layout_column_wrap(width = 1/4,
                                 value_box("Ventas", uiOutput("kpi_ventas"), height = ALTO_KPI_V1, class = "vb-compacto-v1"),
                                 value_box("Margen Bruto USD", uiOutput("kpi_margen_bruto"), height = ALTO_KPI_V1, class = "vb-compacto-v1"),
                                 value_box("Margen Neto USD", uiOutput("kpi_margen_neto"), height = ALTO_KPI_V1, class = "vb-compacto-v1"),
                                 value_box("Clientes", uiOutput("kpi_clientes"), height = ALTO_KPI_V1, class = "vb-compacto-v1")
              ),
              layout_column_wrap(width = 1/3,
                                 card(card_header("Ventas y Margen Bruto por Tier"), plotlyOutput("plot_ventas_mbruto", height = ALTO_GRAFICO_V1)),
                                 card(card_header("Ventas y Margen Neto por Tier"), plotlyOutput("plot_ventas_mneto", height = ALTO_GRAFICO_V1)),
                                 card(card_header("Número de Clientes por Tier"), plotlyOutput("plot_clientes", height = ALTO_GRAFICO_V1))
              ),
              layout_column_wrap(width = 1/4,
                                 card(card_header("Share de Clientes"), plotlyOutput("pie_clientes", height = ALTO_GRAFICO_V1)),
                                 card(card_header("Share de Ventas"), plotlyOutput("pie_ventas", height = ALTO_GRAFICO_V1)),
                                 card(card_header("Share de Margen Bruto"), plotlyOutput("pie_margen_bruto", height = ALTO_GRAFICO_V1)),
                                 card(card_header("Share de Margen Neto"), plotlyOutput("pie_margen_neto", height = ALTO_GRAFICO_V1))
              )
            )
  ),
  
  # ===== Vista 2 =====
  nav_panel("Comportamiento",
            layout_sidebar(
              sidebar = sidebar(
                title = "Filtros",
                selectInput("anio2", "Año", choices = anios, selected = ANIO_INICIAL),
                helpText("Los indicadores superiores muestran el total del año seleccionado.",
                         "Los gráficos comparan los cuatro Tiers frente al total.")
              ),
              navset_pill(
                nav_panel("Parte 1",
                          kpis_vista2(""),
                          layout_column_wrap(width = 1/3,
                                             card(class = "cmp-card", card_header("Número de Clientes por Tier y Total"), plotlyOutput("cmp_clientes", height = ALTO_GRAFICO)),
                                             card(class = "cmp-card", card_header("Compra anual promedio cliente por Tier y Total"), plotlyOutput("cmp_compra", height = ALTO_GRAFICO)),
                                             card(class = "cmp-card", card_header("Margen bruto promedio cliente USD por Tier y Total"), plotlyOutput("cmp_mb_cli_usd", height = ALTO_GRAFICO)),
                                             card(class = "cmp-card", card_header("Margen bruto cliente % por Tier y Total"), plotlyOutput("cmp_mb_cli_pct", height = ALTO_GRAFICO)),
                                             card(class = "cmp-card", card_header("Margen neto promedio cliente USD por Tier y Total"), plotlyOutput("cmp_mn_cli_usd", height = ALTO_GRAFICO)),
                                             card(class = "cmp-card", card_header("Margen neto cliente % por Tier y Total"), plotlyOutput("cmp_mn_cli_pct", height = ALTO_GRAFICO))
                          )
                ),
                nav_panel("Parte 2",
                          kpis_vista2("_b"),
                          layout_column_wrap(width = 1/2,
                                             card(class = "cmp-card", card_header("Pedidos promedio por Tier y Total"), plotlyOutput("cmp_pedidos", height = ALTO_GRAFICO)),
                                             card(class = "cmp-card", card_header("Ticket promedio por Tier y Total"), plotlyOutput("cmp_ticket", height = ALTO_GRAFICO)),
                                             card(class = "cmp-card", card_header("Categorías promedio por cliente por Tier y Total"), plotlyOutput("cmp_categorias", height = ALTO_GRAFICO)),
                                             card(class = "cmp-card", card_header("Cross sell rate por Tier y Total"), plotlyOutput("cmp_cross", height = ALTO_GRAFICO))
                          )
                ),
                nav_panel("Parte 3",
                          kpis_vista2("_c"),
                          layout_column_wrap(width = 1/2,
                                             card(class = "cmp-card", card_header("Mix de categorías (% de ventas) por Tier"), plotlyOutput("cmp_mix_cat", height = ALTO_GRAFICO)),
                                             card(class = "cmp-card", card_header("Penetración de categorías por Tier y Total"), plotlyOutput("cmp_penet", height = ALTO_GRAFICO))
                          ),
                          card(class = "cmp-card", card_header("Rentabilidad neta por categoría: margen neto USD promedio por cliente que compra la categoría, por Tier y Total"),
                               plotlyOutput("cmp_rent_cat_cli", height = ALTO_GRAFICO))
                )
              )
            )
  ),
  
  # ===== Vista 3 =====
  nav_panel("Experiencia y Fidelización",
            layout_sidebar(
              sidebar = sidebar(
                title = "Filtros",
                selectInput("anio3", "Año", choices = anios, selected = ANIO_INICIAL),
                selectInput("tier_nube", "Tier (nube de palabras)",
                            choices = c("Todos los Tiers" = "Todos", "Tier 1" = "1", "Tier 2" = "2",
                                        "Tier 3" = "3", "Tier 4" = "4"),
                            selected = "Todos"),
                helpText("Los gráficos comparan los cuatro Tiers frente al total.",
                         "Clientes perdidos: compraron el año anterior y no compraron en el año elegido;",
                         "se cuentan en el Tier que tenían el año anterior.",
                         "El filtro de Tier solo aplica a la nube de palabras.")
              ),
              layout_column_wrap(width = 1/2,
                                 card(class = "cmp-card", card_header("NPS total y por Tier"), plotlyOutput("cmp_nps", height = ALTO_GRAFICO)),
                                 card(class = "cmp-card", card_header("Recompra total y por Tier"), plotlyOutput("cmp_recompra", height = ALTO_GRAFICO)),
                                 card(class = "cmp-card", card_header("Clientes perdidos: número y % de los clientes del año anterior"), plotlyOutput("cmp_perdidos", height = ALTO_GRAFICO)),
                                 card(class = "cmp-card", card_header("Facturación perdida USD por Tier y Total"), plotlyOutput("cmp_fact_perdida", height = ALTO_GRAFICO))
              ),
              card(class = "cmp-card", card_header("Comentarios del NPS (nube de palabras)"), plotOutput("nube_nps", height = ALTO_NUBE))
            )
  ),
  
  # ===== Vista 5 =====
  nav_panel("Captación",
            p("Evolución anual clientes nuevos (Total empresa, sin filtro de año o Tier).",
              style = "color: var(--bs-secondary-color); margin: 4px 0 14px;"),
            layout_column_wrap(width = 1/1,
                               card(class = "cmp-card", card_header("Clientes nuevos: cantidad y % del total"), plotlyOutput("cap_clientes", height = ALTO_GRAFICO))
            ),
            layout_column_wrap(width = 1/2,
                               card(class = "cmp-card", card_header("Ventas de clientes nuevos: USD y % del total"), plotlyOutput("cap_ventas", height = ALTO_GRAFICO)),
                               card(class = "cmp-card", card_header("Recompra de clientes nuevos"), plotlyOutput("cap_recompra", height = ALTO_GRAFICO))
            )
  ),
  
  # ===== Vista 4 =====
  nav_panel("Contactabilidad",
            layout_sidebar(
              sidebar = sidebar(
                title = "Filtros",
                selectInput("anio4", "Año", choices = anios, selected = ANIO_INICIAL),
                helpText("Los gráficos comparan los cuatro Tiers frente al total.",
                         "Solo 2024 tiene datos de contactabilidad en el archivo actual.")
              ),
              layout_column_wrap(width = 1/3,
                                 card(class = "cmp-card", card_header("Contactabilidad por Tier"), plotlyOutput("cmp_contactable", height = ALTO_GRAFICO)),
                                 card(class = "cmp-card", card_header("Mail por Tier"), plotlyOutput("cmp_mail", height = ALTO_GRAFICO)),
                                 card(class = "cmp-card", card_header("Whatsapp por Tier"), plotlyOutput("cmp_wsp", height = ALTO_GRAFICO))
              )
            )
  ),

  # ===== Gestión de Ventas =====
  nav_panel("Gestión de Ventas",
            layout_sidebar(
              sidebar = sidebar(
                title = "Filtros",
                selectInput("anio_gv", "Año", choices = anios, selected = ANIO_INICIAL),
                helpText("Los encabezados muestran el total del año seleccionado.",
                         "En costo comercial y tiempo de conversión, bajar es verde.",
                         "Categoría y Medida (Parte 2) solo aplican al mapa de calor.")
              ),
              navset_pill(
                nav_panel("Parte 1",
                          kpis_gv(""),
                          layout_column_wrap(width = 1/2,
                                             card(class = "cmp-card", card_header("Tasa de conversión cotizaciones (#) por Tier y Total"), plotlyOutput("gv_graf_conv_num", height = ALTO_GRAFICO)),
                                             card(class = "cmp-card", card_header("Tasa de conversión cotizaciones (valor) por Tier y Total"), plotlyOutput("gv_graf_conv_val", height = ALTO_GRAFICO)),
                                             card(class = "cmp-card", card_header("Tiempo promedio de conversión (días) por Tier y Total"), plotlyOutput("gv_tiempo", height = ALTO_GRAFICO)),
                                             card(class = "cmp-card", card_header("Costo comercial USD y % sobre ventas por Tier y Total"), plotlyOutput("gv_graf_costo", height = ALTO_GRAFICO))
                          )
                ),
                nav_panel("Parte 2",
                          kpis_gv("_b"),
                          layout_columns(col_widths = c(3, 4),
                                         selectInput("cat_gv", "Categoría",
                                                     choices = c("Todas las categorías" = "Todas", "Comercial", "Cajas", "Etiquetas"),
                                                     selected = "Todas"),
                                         radioButtons("medida_gv", "Medida",
                                                      choices = c("Cotizaciones (#)" = "num", "Valor" = "valor"),
                                                      selected = "num", inline = TRUE)
                          ),
                          card(class = "cmp-card", card_header(textOutput("titulo_mapa_gv", inline = TRUE)),
                               plotlyOutput("mapa_gv", height = ALTO_MAPA),
                               p("Verde: por encima del promedio del equipo en ese Tier · Amarillo: a menos de 0.5 pp · Rojo: por debajo · Gris: el vendedor no atiende ese Tier. Columna Total: se compara con lo esperado según los Tiers que atiende cada vendedor.",
                                 style = "font-size: 0.7rem; color: var(--bs-secondary-color); margin: 2px 6px;"))
                )
              )
            )
  )
)

# ---------- server ----------
server <- function(input, output, session) {
  
  # ================= Vista 1 =================
  datos_anio <- reactive({
    datos |> filter(Annual == input$anio)
  })
  
  total <- reactive({
    datos_anio() |> filter(Tier == "Tot")
  })
  
  por_tier <- reactive({
    datos_anio() |> filter(Tier != "Tot") |> arrange(Tier)
  })
  
  # Fila "Tot" del año anterior (vacía si el año elegido es el primero de la base)
  total_previo <- reactive({
    datos |> filter(Annual == as.numeric(input$anio) - 1, Tier == "Tot")
  })
  
  output$kpi_ventas <- renderUI({
    tagList(fmt_usd(total()$Ventas), variacion_anual(total()$Ventas, total_previo()$Ventas))
  })
  # Monto en USD y, entre paréntesis, el % sobre ventas
  # Margen en USD, su % sobre ventas y la variación del monto en USD vs. año anterior
  output$kpi_margen_bruto <- renderUI({
    t <- total()
    tagList(paste0(fmt_usd(t$Margen_Bruto_USD), " (", fmt_pct(t$Margen_Bruto_Porc), ")"),
            variacion_anual(t$Margen_Bruto_USD, total_previo()$Margen_Bruto_USD))
  })
  output$kpi_margen_neto <- renderUI({
    t <- total()
    tagList(paste0(fmt_usd(t$Margen_Neto_USD), " (", fmt_pct(t$Margen_Neto_USD / t$Ventas), ")"),
            variacion_anual(t$Margen_Neto_USD, total_previo()$Margen_Neto_USD))
  })
  output$kpi_clientes <- renderUI({
    tagList(fmt_n(total()$Clientes), variacion_anual(total()$Clientes, total_previo()$Clientes))
  })
  
  # Barras agrupadas Ventas vs un margen (bruto o neto), por Tier
  barras_ventas_margen <- function(campo_margen, nombre_margen) {
    d <- por_tier()
    plot_ly(d, x = ~Tier) |>
      add_trace(y = ~Ventas, type = "bar", name = "Ventas", marker = list(color = "#1C2D42"),
                text = fmt_usd1(d$Ventas), textposition = "none", hoverinfo = "name+text") |>
      add_trace(y = d[[campo_margen]], type = "bar", name = nombre_margen, marker = list(color = "#9E9E9E"),
                text = fmt_usd1(d[[campo_margen]]), textposition = "none", hoverinfo = "name+text") |>
      layout(barmode = "group", xaxis = list(title = "Tier"),
             yaxis = c(list(title = "USD"), eje_hasta_max(c(d$Ventas, d[[campo_margen]]), fmt_usd)),
             legend = list(orientation = "h", x = 0, y = 1.2)) |>
      config(displayModeBar = FALSE)
  }
  output$plot_ventas_mbruto <- renderPlotly({ barras_ventas_margen("Margen_Bruto_USD", "Margen bruto") })
  output$plot_ventas_mneto  <- renderPlotly({ barras_ventas_margen("Margen_Neto_USD", "Margen neto") })
  
  output$plot_clientes <- renderPlotly({
    d <- por_tier()
    plot_ly(d, x = ~Tier, y = ~Clientes, type = "bar",
            marker = list(color = paleta_tier[d$Tier]),
            text = fmt_n(d$Clientes), textposition = "none", hoverinfo = "x+text") |>
      layout(xaxis = list(title = "Tier"),
             yaxis = c(list(title = "Clientes"), eje_hasta_max(d$Clientes, fmt_n))) |>
      config(displayModeBar = FALSE)
  })
  
  hacer_pie <- function(campo) {
    d <- por_tier()
    plot_ly(d, labels = ~paste("Tier", Tier), values = d[[campo]], type = "pie",
            hole = 0.5, marker = list(colors = paleta_tier[d$Tier]),
            textinfo = "label+percent", textposition = "inside",
            insidetextorientation = "horizontal",
            textfont = list(size = TAM_TEXTO_PIE, color = "white"),
            hoverinfo = "label+percent") |>
      layout(showlegend = FALSE, margin = list(t = 4, b = 4, l = 4, r = 4)) |>
      config(displayModeBar = FALSE)
  }
  output$pie_clientes     <- renderPlotly({ hacer_pie("Clientes") })
  output$pie_ventas       <- renderPlotly({ hacer_pie("Ventas") })
  output$pie_margen_bruto <- renderPlotly({ hacer_pie("Margen_Bruto_USD") })
  output$pie_margen_neto  <- renderPlotly({ hacer_pie("Margen_Neto_USD") })
  
  # ================= Vista 2 =================
  datos2 <- reactive({
    req(input$anio2)
    datos |>
      filter(Annual == input$anio2) |>
      mutate(Etiqueta = factor(if_else(Tier == "Tot", "Total", paste("Tier", Tier)),
                               levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4", "Total")))
  })
  
  total2 <- reactive({ datos2() |> filter(Tier == "Tot") })
  
  # Fila "Tot" del año anterior, para las variaciones (vacía si no existe)
  total2_previo <- reactive({
    datos |> filter(Annual == as.numeric(input$anio2) - 1, Tier == "Tot")
  })
  
  # ---- encabezados (fila "Tot" del año + variación vs. año anterior), en ambas partes ----
  kpi_defs <- list(
    clientes   = function(t, p) tagList(fmt_n(t$Clientes), variacion_anual(t$Clientes, p$Clientes)),
    compra     = function(t, p) tagList(fmt_usd0(t$Compra_anual), variacion_anual(t$Compra_anual, p$Compra_anual)),
    mb_cli     = function(t, p) tagList(paste0(fmt_usd0(t$Marg_Cli_USD), " (", fmt_pct(t$Marg_Cli_Porc), ")"),
                                        variacion_anual(t$Marg_Cli_USD, p$Marg_Cli_USD)),
    mn_cli     = function(t, p) tagList(paste0(fmt_usd0(t$Rent_Neto_Cli_USD), " (", fmt_pct(t$Rent_Neto_Cli_Porc), ")"),
                                        variacion_anual(t$Rent_Neto_Cli_USD, p$Rent_Neto_Cli_USD)),
    ticket     = function(t, p) tagList(fmt_usd0(t$Ticket_Prom), variacion_anual(t$Ticket_Prom, p$Ticket_Prom)),
    pedidos    = function(t, p) tagList(fmt_dec1(t$Frecuencia), variacion_anual(t$Frecuencia, p$Frecuencia)),
    categorias = function(t, p) tagList(fmt_dec1(t$Mix_Cat_Cli), variacion_anual(t$Mix_Cat_Cli, p$Mix_Cat_Cli))
  )
  for (nm in names(kpi_defs)) {
    for (suf in c("", "_b", "_c")) {
      local({
        n <- nm; s <- suf
        output[[paste0("v2_", n, s)]] <- renderUI({ kpi_defs[[n]](total2(), total2_previo()) })
      })
    }
  }
  
  # ---- comparativos por Tier (Tier 1-4 + Total como referencia en gris) ----
  colores_cmp <- unname(c(paleta_tier[c("1", "2", "3", "4")], "#8A8F9A"))
  
  # variacion: NULL (sin variación), "rel" (variación %) o "pp" (puntos porcentuales)
  barras_tier <- function(campo, formato, pct = FALSE, variacion = NULL) {
    d <- datos2() |> arrange(Etiqueta)
    y <- d[[campo]]
    etiqueta <- formato(y)
    if (!is.null(variacion)) {
      previo <- datos |> filter(Annual == as.numeric(input$anio2) - 1)
      y_previo <- previo[[campo]][match(d$Tier, previo$Tier)]
      etiqueta <- paste0(etiqueta, var_html(y, y_previo, variacion))
    }
    # más espacio arriba cuando la etiqueta tiene dos líneas
    tope <- if (is.null(variacion)) 1.25 else 1.45
    p <- plot_ly(d, x = ~Etiqueta, y = y, type = "bar",
                 marker = list(color = colores_cmp),
                 text = etiqueta, textposition = "outside",
                 hoverinfo = "x+text") |>
      layout(margin = list(t = 8, b = 24, l = 36, r = 8), font = list(size = 10),
             xaxis = list(title = ""),
             yaxis = list(title = "", range = c(0, max(y) * tope),
                          tickformat = if (pct) ".0%" else NULL))%>%
      config(
        displayModeBar = FALSE
      )
    p
  }
  
  # ---- Parte 1 ----
  output$cmp_clientes   <- renderPlotly({ barras_tier("Clientes", fmt_n, variacion = "rel") })
  output$cmp_compra     <- renderPlotly({ barras_tier("Compra_anual", fmt_usd1, variacion = "rel") })
  output$cmp_mb_cli_usd <- renderPlotly({ barras_tier("Marg_Cli_USD", fmt_usd1, variacion = "rel") })
  output$cmp_mn_cli_usd <- renderPlotly({ barras_tier("Rent_Neto_Cli_USD", fmt_usd1, variacion = "rel") })
  output$cmp_mb_cli_pct <- renderPlotly({ barras_tier("Marg_Cli_Porc", fmt_pct, pct = TRUE, variacion = "pp") })
  output$cmp_mn_cli_pct <- renderPlotly({ barras_tier("Rent_Neto_Cli_Porc", fmt_pct, pct = TRUE, variacion = "pp") })
  output$cmp_ticket     <- renderPlotly({ barras_tier("Ticket_Prom", fmt_usd1, variacion = "rel") })
  
  # ---- Parte 2 ----
  output$cmp_pedidos    <- renderPlotly({ barras_tier("Frecuencia", fmt_dec1, variacion = "rel") })
  output$cmp_categorias <- renderPlotly({ barras_tier("Mix_Cat_Cli", fmt_dec1, variacion = "rel") })
  output$cmp_cross      <- renderPlotly({ barras_tier("Cross_Sell_Rate", fmt_pct, pct = TRUE, variacion = "pp") })
  
  # Leyenda a la derecha del gráfico (Mix y Penetración de categorías)
  leyenda_derecha <- list(orientation = "v", x = 1.02, xanchor = "left", y = 0.5, yanchor = "middle",
                          font = list(size = 9))
  margen_cmp      <- list(t = 8, b = 24, l = 36, r = 8)
  
  # Mix de categorías: barra apilada al 100% por Tier (% de las ventas de cada categoría)
  output$cmp_mix_cat <- renderPlotly({
    d <- datos2() |> filter(Tier != "Tot") |> arrange(Etiqueta) |> mutate(Etiqueta = droplevels(Etiqueta))
    campos <- c(Comercial = "Mix_Ventas_Comercial", Cajas = "Mix_Ventas_Cajas", Etiquetas = "Mix_Ventas_Etiquetas")
    p <- plot_ly(d, x = ~Etiqueta)
    for (cat in names(campos)) {
      y <- d[[campos[[cat]]]]
      p <- p |> add_trace(y = y, type = "bar", name = cat, marker = list(color = paleta_cat[[cat]]),
                          text = fmt_pct(y), textposition = "inside", insidetextanchor = "middle",
                          textfont = list(size = 8, color = "white"), hoverinfo = "x+name+text")
    }
    p |>
      layout(barmode = "stack", margin = margen_cmp, font = list(size = 10), legend = leyenda_derecha,
             xaxis = list(title = ""), yaxis = list(title = "", tickformat = ".0%", range = c(0, 1))) |>
      config(displayModeBar = FALSE)
  })
  
  # Penetración: % de clientes que compra cada categoría, por Tier y Total
  # Sobre cada barra va solo la variación vs. año anterior (pp); el valor se ve al pasar el mouse
  output$cmp_penet <- renderPlotly({
    d <- datos2() |> arrange(Etiqueta)
    previo <- datos |> filter(Annual == as.numeric(input$anio2) - 1)
    campos <- c(Comercial = "Penet_Comercial", Cajas = "Penet_Cajas", Etiquetas = "Penet_Etiquetas")
    p <- plot_ly(d, x = ~Etiqueta)
    for (cat in names(campos)) {
      y <- d[[campos[[cat]]]]
      y_previo <- previo[[campos[[cat]]]][match(d$Tier, previo$Tier)]
      p <- p |> add_trace(y = y, type = "bar", name = cat, marker = list(color = paleta_cat[[cat]]),
                          text = var_html(y, y_previo, "pp", salto = FALSE, tam = TAM_VAR_GRAFICO + 2), textposition = "outside",
                          hovertext = paste0(cat, ": ", fmt_pct(y), var_html(y, y_previo, "pp")),
                          hoverinfo = "x+text")
    }
    p |>
      layout(barmode = "group", margin = margen_cmp, font = list(size = 10), legend = leyenda_derecha,
             xaxis = list(title = ""), yaxis = list(title = "", tickformat = ".0%", range = c(0, 1.12))) |>
      config(displayModeBar = FALSE)
  })
  
  # Rentabilidad neta por categoría y cliente: margen neto USD de cada categoría
  # dividido por los clientes que compran esa categoría, por Tier y Total
  output$cmp_rent_cat_cli <- renderPlotly({
    d <- categorias |>
      filter(Annual == input$anio2) |>
      mutate(Rent_Cli = Margen_Neto_USD / Clientes_Compradores,
             Etiqueta = factor(if_else(Tier == "Tot", "Total", paste("Tier", Tier)),
                               levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4", "Total"))) |>
      arrange(Etiqueta, Cat_Corta)
    validate(need(nrow(d) > 0, "Sin datos de categorías para este año."))
    previo <- categorias |>
      filter(Annual == as.numeric(input$anio2) - 1) |>
      mutate(Rent_Cli = Margen_Neto_USD / Clientes_Compradores)
    p <- plot_ly()
    for (cat in levels(d$Cat_Corta)) {
      dc <- d |> filter(Cat_Corta == cat)
      pc <- previo |> filter(Cat_Corta == cat)
      rent_previa <- pc$Rent_Cli[match(dc$Tier, pc$Tier)]
      p <- p |> add_trace(data = dc, x = ~Etiqueta, y = ~Rent_Cli, type = "bar", name = cat,
                          marker = list(color = paleta_cat[[cat]]),
                          text = paste0(fmt_usd1(dc$Rent_Cli), var_html(dc$Rent_Cli, rent_previa, "rel")),
                          textposition = "outside",
                          textfont = list(size = 8),
                          hovertext = paste0(cat, ": ", fmt_usd1(dc$Rent_Cli), " por cliente (",
                                             fmt_n(dc$Clientes_Compradores), " clientes, margen neto ",
                                             fmt_pct(dc$Margen_Neto), ")"),
                          hoverinfo = "x+text")
    }
    p |>
      layout(barmode = "group", margin = margen_cmp, font = list(size = 10), legend = leyenda_derecha,
             xaxis = list(title = ""),
             yaxis = list(title = "", range = c(0, max(d$Rent_Cli, na.rm = TRUE) * 1.45))) |>
      config(displayModeBar = FALSE)
  })

  # ================= Vista 3 =================
  datos3 <- reactive({
    req(input$anio3)
    datos |>
      filter(Annual == input$anio3) |>
      mutate(Etiqueta = factor(if_else(Tier == "Tot", "Total", paste("Tier", Tier)),
                               levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4", "Total")))
  })
  
  output$cmp_nps <- renderPlotly({
    d <- datos3() |> arrange(Etiqueta)
    validate(need(nrow(d) > 0, "Sin datos para este año."))
    y <- d$NPS
    plot_ly(d, x = ~Etiqueta, y = y, type = "bar",
            marker = list(color = colores_cmp),
            text = fmt_dec1(y), textposition = "outside", hoverinfo = "x+text") |>
      layout(margin = list(t = 8, b = 24, l = 36, r = 8), font = list(size = 10),
             xaxis = list(title = ""),
             yaxis = list(title = "", range = c(0, max(y, na.rm = TRUE) * 1.2)))%>%
      config(
        displayModeBar = FALSE
      )
  })
  
  output$cmp_recompra <- renderPlotly({
    d <- datos3() |> arrange(Etiqueta)
    validate(need(nrow(d) > 0, "Sin datos para este año."))
    y <- d$Recompra
    plot_ly(d, x = ~Etiqueta, y = y, type = "bar",
            marker = list(color = colores_cmp),
            text = fmt_pct(y), textposition = "outside", hoverinfo = "x+text") |>
      layout(margin = list(t = 8, b = 24, l = 36, r = 8), font = list(size = 10),
             xaxis = list(title = ""),
             yaxis = list(title = "", tickformat = ".0%", range = c(0, max(y, na.rm = TRUE) * 1.3)))%>%
      config(
        displayModeBar = FALSE
      )
  })
  
  # Clientes perdidos: número (barra) y % sobre los clientes que tenía el Tier el año anterior
  output$cmp_perdidos <- renderPlotly({
    d <- datos3() |> arrange(Etiqueta)
    validate(need(!all(is.na(d$Clientes_Perdidos)), "Sin datos de clientes perdidos para este año."))
    previo <- datos |> filter(Annual == as.numeric(input$anio3) - 1)
    pct <- d$Clientes_Perdidos / previo$Clientes[match(d$Tier, previo$Tier)]
    etiqueta <- paste0(fmt_n(d$Clientes_Perdidos),
                       "<br><span style='font-size:", TAM_VAR_GRAFICO + 1, "px;color:#5A5F66'>(",
                       fmt_pct(pct), ")</span>")
    plot_ly(d, x = ~Etiqueta, y = ~Clientes_Perdidos, type = "bar",
            marker = list(color = colores_cmp),
            text = etiqueta, textposition = "outside", hoverinfo = "x+text") |>
      layout(margin = list(t = 8, b = 24, l = 36, r = 8), font = list(size = 10),
             xaxis = list(title = ""),
             yaxis = list(title = "", range = c(0, max(d$Clientes_Perdidos, na.rm = TRUE) * 1.45))) |>
      config(displayModeBar = FALSE)
  })
  
  # Facturación perdida: ventas del año anterior de los clientes perdidos
  output$cmp_fact_perdida <- renderPlotly({
    d <- datos3() |> arrange(Etiqueta)
    validate(need(!all(is.na(d$Fact_Perdida_USD)), "Sin datos de facturación perdida para este año."))
    plot_ly(d, x = ~Etiqueta, y = ~Fact_Perdida_USD, type = "bar",
            marker = list(color = colores_cmp),
            text = fmt_usd1(d$Fact_Perdida_USD), textposition = "outside",
            hovertext = paste0(fmt_usd1(d$Fact_Perdida_USD), " (", fmt_pct(d$Fact_Perdida_Porc),
                               " de las ventas del año anterior)"),
            hoverinfo = "x+text") |>
      layout(margin = list(t = 8, b = 24, l = 36, r = 8), font = list(size = 10),
             xaxis = list(title = ""),
             yaxis = list(title = "", range = c(0, max(d$Fact_Perdida_USD, na.rm = TRUE) * 1.25))) |>
      config(displayModeBar = FALSE)
  })
  
  # Cuali_NPS trae palabras/temas separados por coma (a veces por un apóstrofe
  # suelto cuando dos celdas quedaron pegadas); se cuenta cuántas veces
  # aparece cada palabra entre los Tiers 1 a 4 del año seleccionado.
  output$nube_nps <- renderPlot({
    req(input$tier_nube)
    tiers_nube <- if (input$tier_nube == "Todos") c("1", "2", "3", "4") else input$tier_nube
    texto <- datos3() |> filter(Tier %in% tiers_nube) |> pull(Cuali_NPS)
    palabras <- texto |>
      strsplit("[,']") |>
      unlist() |>
      trimws()
    palabras <- palabras[palabras != "" & !is.na(palabras)]
    frecuencia <- as.data.frame(table(palabras), stringsAsFactors = FALSE) |>
      rename(word = palabras, freq = Freq) |>
      arrange(desc(freq))
    validate(need(nrow(frecuencia) > 0, "No hay comentarios para este año."))
    # Semáforo de incidencia: naranja = menos menciones, rojo = más menciones
    paleta_nube <- colorRampPalette(c(COLOR_NUBE_BAJA, COLOR_NUBE_ALTA))(100)
    rango <- max(frecuencia$freq) - min(frecuencia$freq)
    nivel <- if (rango == 0) rep(100, nrow(frecuencia)) else
      1 + round(99 * (frecuencia$freq - min(frecuencia$freq)) / rango)
    par(mar = c(0, 0, 0, 0))
    wordcloud::wordcloud(
      words = frecuencia$word, freq = frecuencia$freq,
      scale = c(1.8, 0.6), min.freq = 1, random.order = FALSE, rot.per = 0,
      colors = paleta_nube[nivel], ordered.colors = TRUE
    )
  })
  
  # ================= Vista 4 =================
  datos4 <- reactive({
    req(input$anio4)
    datos |>
      filter(Annual == input$anio4) |>
      mutate(Etiqueta = factor(if_else(Tier == "Tot", "Total", paste("Tier", Tier)),
                               levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4", "Total")))
  })
  
  barra_pct_tier <- function(campo) {
    d <- datos4() |> arrange(Etiqueta)
    validate(need(nrow(d) > 0, "Sin datos para este año."))
    y <- d[[campo]]
    validate(need(!all(is.na(y)), "Este año no tiene datos de contactabilidad."))
    plot_ly(d, x = ~Etiqueta, y = y, type = "bar",
            marker = list(color = colores_cmp),
            text = fmt_pct(y), textposition = "outside", hoverinfo = "x+text") |>
      layout(margin = list(t = 8, b = 24, l = 36, r = 8), font = list(size = 10),
             xaxis = list(title = ""),
             yaxis = list(title = "", tickformat = ".0%", range = c(0, max(y, na.rm = TRUE) * 1.3)))%>%
      config(
        displayModeBar = FALSE
      )
  }
  
  output$cmp_contactable <- renderPlotly({ barra_pct_tier("Contactable") })
  output$cmp_mail        <- renderPlotly({ barra_pct_tier("Mail") })
  output$cmp_wsp         <- renderPlotly({ barra_pct_tier("WSP") })
  
  # ================= Vista 5 =================
  # Serie de tiempo con el Total de la empresa (fila "Tot") en cada año,
  # sin filtro de año ni comparación por Tier.
  datos_captacion <- reactive({
    datos |> filter(Tier == "Tot", Annual %in% anios) |> arrange(Annual) |> mutate(Annual = factor(Annual))
  })
  
  output$cap_clientes <- renderPlotly({
    d <- datos_captacion()
    plot_ly(d, x = ~Annual) |>
      add_trace(y = ~Clientes_Nuevos, type = "bar", name = "Clientes nuevos",
                yaxis = "y1", marker = list(color = "#2B5F5A"),
                text = fmt_n(d$Clientes_Nuevos), textposition = "none", hoverinfo = "name+text") |>
      add_trace(y = ~Clientes_Nuevos_Porc, type = "scatter", mode = "lines+markers",
                name = "% del total", yaxis = "y2",
                line = list(color = "#C98A3B", dash = "dot"), marker = list(color = "#C98A3B"),
                text = fmt_pct(d$Clientes_Nuevos_Porc), hoverinfo = "name+text") |>
      layout(margin = list(t = 8, b = 24, l = 48, r = 48), font = list(size = 10),
             xaxis = list(title = ""),
             yaxis = list(title = "Clientes"),
             yaxis2 = list(title = "% del total", overlaying = "y", side = "right", tickformat = ".0%"),
             legend = list(orientation = "h", x = 0, y = 1.15))%>%
      config(
        displayModeBar = FALSE
      )
  })
  
  output$cap_ventas <- renderPlotly({
    d <- datos_captacion()
    plot_ly(d, x = ~Annual) |>
      add_trace(y = ~Ventas_USD_Nuevos, type = "bar", name = "Ventas (USD)",
                yaxis = "y1", marker = list(color = "#1C2D42"),
                text = fmt_usd1(d$Ventas_USD_Nuevos), textposition = "none", hoverinfo = "name+text") |>
      add_trace(y = ~Ventas_Porc_Nuevos, type = "scatter", mode = "lines+markers",
                name = "% del total", yaxis = "y2",
                line = list(color = "#C98A3B", dash = "dot"), marker = list(color = "#C98A3B"),
                text = fmt_pct(d$Ventas_Porc_Nuevos), hoverinfo = "name+text") |>
      layout(margin = list(t = 8, b = 24, l = 56, r = 48), font = list(size = 10),
             xaxis = list(title = ""),
             yaxis = list(title = "USD"),
             yaxis2 = list(title = "% del total", overlaying = "y", side = "right", tickformat = ".0%"),
             legend = list(orientation = "h", x = 0, y = 1.15))%>%
      config(
        displayModeBar = FALSE
      )
  })
  
  output$cap_recompra <- renderPlotly({
    d <- datos_captacion()
    y <- d$Recompra_Nuevos
    plot_ly(d, x = ~Annual, y = y, type = "bar",
            marker = list(color = "#5B8C87"),
            text = fmt_pct(y), textposition = "outside", hoverinfo = "x+text") |>
      layout(margin = list(t = 8, b = 24, l = 36, r = 8), font = list(size = 10),
             xaxis = list(title = ""),
             yaxis = list(title = "", tickformat = ".0%", range = c(0, max(y, na.rm = TRUE) * 1.3)))%>%
      config(
        displayModeBar = FALSE
      )
  })
  

  # ================= Gestión de Ventas =================
  datos_gv <- reactive({
    req(input$anio_gv)
    datos |>
      filter(Annual == input$anio_gv) |>
      mutate(Etiqueta = factor(if_else(Tier == "Tot", "Total", paste("Tier", Tier)),
                               levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4", "Total")))
  })
  previo_gv <- reactive({ datos |> filter(Annual == as.numeric(input$anio_gv) - 1) })
  total_gv  <- reactive({ datos_gv() |> filter(Tier == "Tot") })
  total_gv_previo <- reactive({ previo_gv() |> filter(Tier == "Tot") })

  # ---- encabezados (total del año + variación vs. año anterior), en ambas partes ----
  kpi_gv_defs <- list(
    vendedores = function(t, p) tagList(fmt_n(t$Vendedores), variacion_anual(t$Vendedores, p$Vendedores)),
    costo      = function(t, p) tagList(paste0(fmt_usd1(t$Costo_Comercial_USD), " (", fmt_pct(t$Costo_Comercial_Porc), ")"),
                                        variacion_anual(t$Costo_Comercial_USD, p$Costo_Comercial_USD, invertir = TRUE)),
    cot_num    = function(t, p) tagList(fmt_n(t$Cotizaciones_Num), variacion_anual(t$Cotizaciones_Num, p$Cotizaciones_Num)),
    conv_num   = function(t, p) tagList(fmt_pct(t$Tasa_Conv_Num), variacion_anual(t$Tasa_Conv_Num, p$Tasa_Conv_Num, "pp")),
    cot_val    = function(t, p) tagList(fmt_usd1(t$Cotizaciones_Valor_USD), variacion_anual(t$Cotizaciones_Valor_USD, p$Cotizaciones_Valor_USD)),
    conv_val   = function(t, p) tagList(fmt_pct(t$Tasa_Conv_Valor), variacion_anual(t$Tasa_Conv_Valor, p$Tasa_Conv_Valor, "pp"))
  )
  for (nm in names(kpi_gv_defs)) {
    for (suf in c("", "_b")) {
      local({
        n <- nm; s <- suf
        output[[paste0("gv_", n, s)]] <- renderUI({ kpi_gv_defs[[n]](total_gv(), total_gv_previo()) })
      })
    }
  }

  # ---- Parte 1: barras por Tier y Total con variación ----
  barras_gv <- function(campo, formato, pct = FALSE, variacion = NULL, invertir = FALSE) {
    d <- datos_gv() |> arrange(Etiqueta)
    y <- d[[campo]]
    etiqueta <- formato(y)
    if (!is.null(variacion)) {
      pr <- previo_gv()
      etiqueta <- paste0(etiqueta, var_html(y, pr[[campo]][match(d$Tier, pr$Tier)], variacion, invertir = invertir))
    }
    plot_ly(d, x = ~Etiqueta, y = y, type = "bar", marker = list(color = colores_cmp),
            text = etiqueta, textposition = "outside", hoverinfo = "x+text") |>
      layout(margin = list(t = 8, b = 24, l = 36, r = 8), font = list(size = 10),
             xaxis = list(title = ""),
             yaxis = list(title = "", range = c(0, max(y, na.rm = TRUE) * 1.45),
                          tickformat = if (pct) ".0%" else NULL)) |>
      config(displayModeBar = FALSE)
  }

  output$gv_graf_conv_num <- renderPlotly({ barras_gv("Tasa_Conv_Num", fmt_pct, pct = TRUE, variacion = "pp") })
  output$gv_graf_conv_val <- renderPlotly({ barras_gv("Tasa_Conv_Valor", fmt_pct, pct = TRUE, variacion = "pp") })
  # Costo comercial: monto USD (barra) con su % sobre ventas y la variación del monto
  # vs. año anterior (semáforo invertido: bajar es verde)
  output$gv_graf_costo <- renderPlotly({
    d <- datos_gv() |> arrange(Etiqueta)
    pr <- previo_gv()
    y <- d$Costo_Comercial_USD
    y_previo <- pr$Costo_Comercial_USD[match(d$Tier, pr$Tier)]
    etiqueta <- paste0(fmt_usd1(y), " (", fmt_pct(d$Costo_Comercial_Porc), ")",
                       var_html(y, y_previo, "rel", invertir = TRUE))
    plot_ly(d, x = ~Etiqueta, y = y, type = "bar", marker = list(color = colores_cmp),
            text = etiqueta, textposition = "outside",
            hovertext = paste0(fmt_usd1(y), " · ", fmt_pct(d$Costo_Comercial_Porc), " de las ventas · ",
                               fmt_usd1(d$Costo_Comercial_USD / d$Clientes), " por cliente"),
            hoverinfo = "x+text") |>
      layout(margin = list(t = 8, b = 24, l = 36, r = 8), font = list(size = 10),
             xaxis = list(title = ""),
             yaxis = list(title = "", range = c(0, max(y, na.rm = TRUE) * 1.45))) |>
      config(displayModeBar = FALSE)
  })

  output$gv_tiempo   <- renderPlotly({
    barras_gv("Tiempo_Conv_Dias", function(x) paste0(fmt_dec1(x), " d"), variacion = "rel", invertir = TRUE)
  })

  # ---- Parte 2: mapa de calor vendedor × Tier ----
  output$titulo_mapa_gv <- renderText({
    paste0("Tasa de conversión ", if (input$medida_gv == "num") "en número (#)" else "en valor",
           " por vendedor y Tier — ",
           if (input$cat_gv == "Todas") "todas las categorías" else input$cat_gv)
  })

  output$mapa_gv <- renderPlotly({
    req(input$anio_gv, input$cat_gv, input$medida_gv)
    d <- vtc |> filter(Annual == input$anio_gv)
    if (input$cat_gv != "Todas") d <- d |> filter(Cat_Corta == input$cat_gv)
    validate(need(nrow(d) > 0, "Sin cotizaciones para esta selección."))
    en_num <- input$medida_gv == "num"
    num <- if (en_num) "Cotizaciones_Ganadas_Num" else "Cotizaciones_Ganadas_Valor_USD"
    den <- if (en_num) "Cotizaciones_Num" else "Cotizaciones_Valor_USD"
    fmt_base <- if (en_num) fmt_n else fmt_usd1

    vend <- d |> distinct(Vendedor, Tipo) |> arrange(Vendedor)
    filas <- c(paste0(vend$Vendedor, if_else(vend$Tipo == "Key account", " (KA)", "")), "Total equipo")
    columnas <- c("Tier 1", "Tier 2", "Tier 3", "Tier 4", "Total")
    z <- matrix(NA_real_, nrow = length(filas), ncol = length(columnas))
    hover <- matrix("", nrow = length(filas), ncol = length(columnas))
    base_m <- matrix(NA_real_, nrow = length(filas), ncol = length(columnas))
    celda <- function(g, i, j) {
      if (nrow(g) == 0 || sum(g[[den]]) == 0) return(invisible(NULL))
      z[i, j] <<- sum(g[[num]]) / sum(g[[den]])
      base_m[i, j] <<- sum(g[[den]])
      hover[i, j] <<- paste0(filas[i], " · ", columnas[j], ": ", fmt_pct(z[i, j]),
                             "<br>", fmt_base(sum(g[[num]])), " ganadas de ", fmt_base(sum(g[[den]])))
    }
    for (i in seq_len(nrow(vend))) {
      dv <- d |> filter(Vendedor == vend$Vendedor[i])
      for (j in 1:4) celda(dv |> filter(Tier == as.character(j)), i, j)
      celda(dv, i, 5)
    }
    n <- length(filas)
    for (j in 1:4) celda(d |> filter(Tier == as.character(j)), n, j)
    celda(d, n, 5)

    # Color: cada vendedor contra el promedio del equipo en la misma columna
    # (1 = rojo, 2 = amarillo, 3 = verde, 0 = fila del equipo)
    promedio <- z[n, ]
    dif <- sweep(z, 2, promedio)
    # Columna Total: se compara con la tasa esperada según los Tiers que atiende cada
    # vendedor (promedio del equipo en cada Tier, ponderado por sus cotizaciones en ese Tier)
    esperado <- rep(NA_real_, n)
    for (i in seq_len(n - 1)) {
      w <- base_m[i, 1:4]
      esperado[i] <- sum(promedio[1:4] * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
      dif[i, 5] <- z[i, 5] - esperado[i]
    }
    codigo <- ifelse(dif > MAPA_BANDA_PP, 3, ifelse(dif < -MAPA_BANDA_PP, 1, 2))
    codigo[n, ] <- 0
    codigo[is.na(z)] <- NA
    for (i in seq_len(n - 1)) for (j in seq_along(columnas)) if (!is.na(z[i, j])) {
      ref_txt <- if (j == 5) "Esperado según sus Tiers: " else "Promedio equipo: "
      ref_val <- if (j == 5) esperado[i] else promedio[j]
      hover[i, j] <- paste0(hover[i, j], "<br>", ref_txt, fmt_pct(ref_val), " (",
                            if (dif[i, j] >= 0) "+" else "", formatC(dif[i, j] * 100, format = "f", digits = 1), " pp)")
    }

    # texto de cada celda ("—" donde el vendedor no atiende ese Tier)
    celdas <- expand.grid(i = seq_along(filas), j = seq_along(columnas))
    etiquetas <- ifelse(is.na(z[cbind(celdas$i, celdas$j)]), "—", fmt_pct(z[cbind(celdas$i, celdas$j)]))

    plot_ly(x = columnas, y = filas, z = codigo, type = "heatmap",
            colorscale = list(c(0, MAPA_EQUIPO), c(0.125, MAPA_EQUIPO), c(0.125, MAPA_ROJO), c(0.375, MAPA_ROJO),
                              c(0.375, MAPA_AMARILLO), c(0.625, MAPA_AMARILLO), c(0.625, MAPA_VERDE), c(1, MAPA_VERDE)),
            zmin = 0, zmax = 3, showscale = FALSE,
            text = hover, hoverinfo = "text", hoverongaps = FALSE,
            xgap = 2, ygap = 2) |>
      add_annotations(x = columnas[celdas$j], y = filas[celdas$i], text = etiquetas,
                      showarrow = FALSE, font = list(size = 9, color = "#1A1A1A")) |>
      layout(plot_bgcolor = "#E6E8EB", font = list(size = 10),
             margin = list(t = 24, b = 8, l = 90, r = 8),
             xaxis = list(title = "", side = "top", type = "category"),
             yaxis = list(title = "", type = "category", autorange = "reversed")) |>
      config(displayModeBar = FALSE)
  })

}

shinyApp(ui, server)