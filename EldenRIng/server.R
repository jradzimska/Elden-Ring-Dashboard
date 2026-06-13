library(shiny)
library(dplyr)
library(tidyr)
library(jsonlite)
library(DT)
library(plotly)

parse_elden_json <- function(column, prefix, value_key) {
  list_of_rows <- lapply(seq_along(column), function(i) {
    x <- column[i]
    if (is.na(x) || x == "" || x == "[]" || x == "None") return(data.frame(.dummy = NA))
    tryCatch({
      json_clean <- gsub("'", '"', x)
      parsed <- fromJSON(json_clean)
      if (!is.data.frame(parsed) || nrow(parsed) == 0) return(data.frame(.dummy = NA))
      parsed <- parsed[parsed$name != "-", , drop = FALSE]
      if (nrow(parsed) == 0) return(data.frame(.dummy = NA))
      out <- as.data.frame(t(parsed[[value_key]]))
      colnames(out) <- paste0(prefix, parsed$name)
      return(out)
    }, error = function(e) data.frame(.dummy = NA))
  })
  bind_rows(list_of_rows) %>% select(-matches("^\\.dummy$"))
}

parse_defence_json <- function(column) {
  list_of_rows <- lapply(seq_along(column), function(i) {
    x <- column[i]
    if (is.na(x) || x == "" || x == "[]" || x == "None") return(data.frame(.dummy = NA))
    tryCatch({
      json_clean <- gsub("'", '"', x)
      parsed <- fromJSON(json_clean)
      if (!is.data.frame(parsed) || nrow(parsed) == 0) return(data.frame(.dummy = NA))
      out <- as.data.frame(t(parsed[["amount"]]))
      colnames(out) <- paste0("def_", parsed$name)
      return(out)
    }, error = function(e) data.frame(.dummy = NA))
  })
  bind_rows(list_of_rows) %>% select(-matches("^\\.dummy$"))
}

weapons_raw <- read.csv("weapons.csv", stringsAsFactors = FALSE)

atk_flat   <- parse_elden_json(weapons_raw$attack,             "atk_",   "amount")
scale_flat <- parse_elden_json(weapons_raw$scalesWith,         "scale_", "scaling")
req_flat   <- parse_elden_json(weapons_raw$requiredAttributes, "req_",   "amount")
def_flat   <- parse_defence_json(weapons_raw$defence)

weapons_flat <- bind_cols(weapons_raw, atk_flat, scale_flat, req_flat, def_flat)

core_stats <- c("Str","Dex","Int","Fai","Arc")
core_atks  <- c("Phy","Mag","Fire","Ligt","Holy")

for (s in core_stats) {
  if (!paste0("scale_", s) %in% names(weapons_flat)) weapons_flat[[paste0("scale_", s)]] <- "None"
  if (!paste0("req_",   s) %in% names(weapons_flat)) weapons_flat[[paste0("req_",   s)]] <- 0
}
for (a in core_atks) {
  if (!paste0("atk_", a) %in% names(weapons_flat)) weapons_flat[[paste0("atk_", a)]] <- 0
}
for (a in c("Phy","Mag","Fire","Ligt","Holy","Boost")) {
  if (!paste0("def_", a) %in% names(weapons_flat)) weapons_flat[[paste0("def_", a)]] <- 0
}

scaling_map <- c("S"=1.50,"A"=1.20,"B"=0.90,"C"=0.60,"D"=0.40,"E"=0.20,"None"=0.00)

weapons_flat <- weapons_flat %>%
  mutate(across(starts_with("scale_"), ~ {
    val <- as.character(.)
    val[is.na(val) | val == ""] <- "None"
    scaling_map[val]
  })) %>%
  mutate(across(starts_with("req_") | starts_with("atk_") | starts_with("def_"), ~ {
    val <- suppressWarnings(as.numeric(.))
    replace_na(val, 0)
  }))

get_stat_efficiency <- function(lvl) {
  dplyr::case_when(
    lvl <= 20 ~ (lvl - 1) * 0.02,
    lvl <= 55 ~ 0.40 + (lvl - 20) * 0.012,
    lvl <= 80 ~ 0.82 + (lvl - 55) * 0.0064,
    TRUE      ~ 0.98 + (lvl - 80) * 0.001
  )
}

calc_ar <- function(df, str, dex, int, fai, arc) {
  df %>% mutate(
    Base_AR   = atk_Phy + atk_Mag + atk_Fire + atk_Ligt + atk_Holy,
    Multiplier = 1 +
      scale_Str * get_stat_efficiency(str) +
      scale_Dex * get_stat_efficiency(dex) +
      scale_Int * get_stat_efficiency(int) +
      scale_Fai * get_stat_efficiency(fai) +
      scale_Arc * get_stat_efficiency(arc),
    Total_AR  = round(Base_AR * Multiplier, 0)
  )
}

all_categories <- sort(unique(weapons_flat$category))


GOLD <- "#C9A84C"; GOLD_LT <- "#F0D080"; GOLD_DK <- "#7A5A10"
BG   <- "#0A0A0A"; PANEL <- "#111111"; BORDER <- "#2A2218"
TEXT <- "#D4C5A0"; DIMTEXT <- "#8A7B60"

er_layout <- function(p, title="", xlab="", ylab="") {
  layout(p,
         title       = list(text=title, font=list(family="Cinzel", color=GOLD, size=14)),
         paper_bgcolor = "rgba(0,0,0,0)",
         plot_bgcolor  = "rgba(0,0,0,0)",
         font          = list(family="Crimson Text", color=TEXT, size=13),
         xaxis = list(title=xlab, gridcolor=BORDER, zerolinecolor=BORDER,
                      tickfont=list(color=DIMTEXT), titlefont=list(color=GOLD, family="Cinzel", size=11)),
         yaxis = list(title=ylab, gridcolor=BORDER, zerolinecolor=BORDER,
                      tickfont=list(color=DIMTEXT), titlefont=list(color=GOLD, family="Cinzel", size=11)),
         legend = list(bgcolor="rgba(0,0,0,0.5)", bordercolor=BORDER, borderwidth=1,
                       font=list(color=TEXT, size=11))
  )
}

cat_palette <- c(
  "#C9A84C","#D4C5A0","#8B1A1A","#1A3A5C","#1A3A1A","#3A1A5C",
  "#5C3A1A","#1A5C3A","#5C1A3A","#3A5C1A","#1A1A5C","#5C5C1A",
  "#5C1A1A","#1A5C5C","#3A3A5C","#5C3A5C","#5C5C3A","#3A5C5C",
  "#9A7ACA","#7ACA9A","#CA9A7A","#7A9ACA","#CA7A9A","#9ACA7A",
  "#D4884C","#4CD488","#4C88D4","#884CD4","#D44C88","#88D44C","#4CD4D4"
)



function(input, output, session) {
  
  observe({
    updateCheckboxGroupInput(session, "cat_filter",
                             choices  = all_categories,
                             selected = all_categories)
    updateSelectInput(session, "dmg_category", choices = all_categories, selected = all_categories[1])
    updateCheckboxGroupInput(session, "weight_cat", choices = all_categories, selected = all_categories)
  })
  
  computed <- reactive({
    req(input$str, input$dex, input$int, input$fai, input$arc)
    cats <- if (length(input$cat_filter) == 0) all_categories else input$cat_filter
    weapons_flat %>%
      filter(
        req_Str <= input$str, req_Dex <= input$dex,
        req_Int <= input$int, req_Fai <= input$fai, req_Arc <= input$arc,
        category %in% cats
      ) %>%
      calc_ar(input$str, input$dex, input$int, input$fai, input$arc)
  })
  
  output$weapon_table <- renderDT({
    df <- computed() %>%
      arrange(desc(Total_AR)) %>%
      select(Name=name, Category=category, Weight=weight,
             `Base AR`=Base_AR, `Total AR`=Total_AR)
    
    datatable(
      df,
      selection   = "single",
      rownames    = FALSE,
      options = list(
        pageLength   = 15,
        lengthChange = FALSE,
        scrollX      = FALSE,
        dom          = "ftp",
        columnDefs   = list(list(className="dt-right", targets=2:4))
      )
    ) %>%
      formatStyle("Total AR",
                  background = styleColorBar(range(df$`Total AR`, na.rm=TRUE), "#7A5A10"),
                  backgroundSize = "100% 80%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center"
      ) %>%
      formatStyle(0:9,
                  color           = TEXT,
                  backgroundColor = "transparent"
      )
  })
  
  
  output$weapon_detail <- renderUI({
    sel <- input$weapon_table_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      return(tags$p(style="color:#8A7B60; font-style:italic; text-align:center; margin-top:40px;",
                    "← Click a weapon row to inspect"))
    }
    
    
    df_sorted <- computed() %>% arrange(desc(Total_AR))
    if (sel > nrow(df_sorted)) return(NULL)
    w <- df_sorted[sel, , drop = FALSE]
    
    
    sv_Str <- as.numeric(w$scale_Str[[1]])
    sv_Dex <- as.numeric(w$scale_Dex[[1]])
    sv_Int <- as.numeric(w$scale_Int[[1]])
    sv_Fai <- as.numeric(w$scale_Fai[[1]])
    sv_Arc <- as.numeric(w$scale_Arc[[1]])
    
    scale_labels <- c("S","A","B","C","D","E","-")
    scale_breaks <- c(1.50, 1.20, 0.90, 0.60, 0.40, 0.20, 0.00)
    grade_for <- function(v) {
      v <- as.numeric(v)
      if (is.na(v) || length(v) == 0) return("-")
      idx <- which.min(abs(scale_breaks - v))
      if (isTRUE(scale_breaks[idx] == 0)) "-" else scale_labels[idx]
    }
    
    
    bar_pct <- function(v) {
      v <- as.numeric(v)
      if (is.na(v) || length(v) == 0 || v == 0) return("0%")
      paste0(min(100, round(v / 1.50 * 100)), "%")   # 1.50 = S grade = max
    }
    
    stat_bar <- function(label, css_cls, v) {
      tags$div(class = "stat-row",
               tags$div(class = "stat-label", label),
               tags$div(class = "stat-bar-bg",
                        tags$div(class = paste0("stat-bar-fill stat-bar-", css_cls),
                                 style = paste0("width:", bar_pct(v)))
               ),
               tags$div(class = "stat-value", grade_for(v))
      )
    }
    
    # Safe scalar helpers
    w_weight  <- as.numeric(w$weight[[1]])
    w_base_ar <- as.numeric(w$Base_AR[[1]])
    w_total_ar <- as.numeric(w$Total_AR[[1]])
    w_name    <- as.character(w$name[[1]])
    w_cat     <- as.character(w$category[[1]])
    w_image   <- as.character(w$image[[1]])
    w_desc    <- as.character(w$description[[1]])
    
    tags$div(class = "weapon-detail-inner",
             
             tags$div(class = "weapon-detail-left",
                      if (!is.na(w_image) && nchar(w_image) > 4) {
                        tags$img(src = w_image, class = "weapon-img-frame",
                                 onerror = "this.style.display='none'")
                      },
                      tags$div(style = sprintf("font-family:Cinzel,serif; font-size:19px; font-weight:700; color:%s; text-align:center;", GOLD),
                               w_total_ar),
                      tags$div(style = "font-family:Cinzel,serif; font-size:9px; letter-spacing:1px; color:#8A7B60; text-align:center; margin-bottom:6px;",
                               "TOTAL AR"),
                      tags$div(style = "font-size:12px; color:#8A7B60; text-align:center;",
                               paste0("Base: ", w_base_ar)),
                      tags$div(style = "font-size:12px; color:#8A7B60; text-align:center;",
                               paste0("Wt: ", w_weight))
             ),
             
             tags$div(class = "weapon-detail-right",
                      tags$div(class = "badge-category", w_cat),
                      tags$h4(style = sprintf("font-family:Cinzel,serif; color:%s; font-size:14px; margin:0 0 8px 0; line-height:1.3;", GOLD_LT),
                              w_name),
                      tags$div(style = "font-family:Cinzel,serif; font-size:9px; letter-spacing:1.5px; color:#8A7B60; margin-bottom:5px;",
                               "SCALING"),
                      tags$div(class = "scale-grid",
                               stat_bar("STR", "str", sv_Str),
                               stat_bar("DEX", "dex", sv_Dex),
                               stat_bar("INT", "int", sv_Int),
                               stat_bar("FAI", "fai", sv_Fai),
                               stat_bar("ARC", "arc", sv_Arc)
                      ),
                      if (!is.na(w_desc) && nchar(w_desc) > 5) {
                        tags$div(class = "weapon-desc", w_desc)
                      }
             )
    )
  })
  
  scatter_data <- reactive({
    weapons_flat %>%
      calc_ar(
        isolate(input$str), isolate(input$dex), isolate(input$int),
        isolate(input$fai), isolate(input$arc)
      )
  }) %>% bindEvent(input$explore_x, input$explore_y,
                   input$str, input$dex, input$int, input$fai, input$arc,
                   ignoreNULL=FALSE)
  
  output$scatter_plot <- renderPlotly({
    df  <- scatter_data()
    xcol <- input$explore_x; ycol <- input$explore_y
    
    p <- plot_ly(df,
                 x    = ~get(xcol),
                 y    = ~get(ycol),
                 color = ~category,
                 colors = cat_palette,
                 type  = "scatter",
                 mode  = "markers",
                 text  = ~paste0("<b>", name, "</b><br>",
                                 "Category: ", category, "<br>",
                                 xcol, ": ", round(get(xcol),1), "<br>",
                                 ycol, ": ", round(get(ycol),1), "<br>",
                                 "Weight: ", weight),
                 hoverinfo = "text",
                 marker = list(opacity = 0.82, size = 8)
    )
    er_layout(p, xlab=xcol, ylab=ycol)
  })
  
  output$dmg_bar_plot <- renderPlotly({
    df <- weapons_flat %>%
      filter(category == input$dmg_category) %>%
      calc_ar(
        isolate(input$str), isolate(input$dex), isolate(input$int),
        isolate(input$fai), isolate(input$arc)
      ) %>%
      arrange(desc(Total_AR)) %>%
      head(input$dmg_top_n) %>%
      select(name, atk_Phy, atk_Mag, atk_Fire, atk_Ligt, atk_Holy)
    
    dmg_long <- df %>%
      pivot_longer(cols=starts_with("atk_"), names_to="Type", values_to="DMG") %>%
      mutate(Type = sub("atk_","",Type),
             Type = factor(Type, levels=c("Phy","Mag","Fire","Ligt","Holy")))
    
    dmg_colors <- c(Phy="#C9A84C", Mag="#4A8FD4", Fire="#D44C1A", Ligt="#D4C84C", Holy="#CA9ACA")
    
    barmode <- if (input$dmg_chart_type == "stacked") "stack" else "group"
    
    p <- plot_ly()
    for (typ in levels(dmg_long$Type)) {
      sub <- dmg_long %>% filter(Type == typ)
      p <- add_trace(p,
                     x    = sub$DMG,
                     y    = sub$name,
                     type = "bar",
                     orientation = "h",
                     name = typ,
                     marker = list(color=dmg_colors[typ]),
                     hovertemplate = paste0("<b>%{y}</b><br>", typ, ": %{x}<extra></extra>")
      )
    }
    p %>% layout(
      barmode     = barmode,
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font  = list(family="Crimson Text", color=TEXT),
      xaxis = list(title="Damage", gridcolor=BORDER, zerolinecolor=BORDER,
                   tickfont=list(color=DIMTEXT), titlefont=list(color=GOLD, family="Cinzel")),
      yaxis = list(title="", gridcolor=BORDER, tickfont=list(color=TEXT),
                   autorange="reversed"),
      legend = list(bgcolor="rgba(0,0,0,0.5)", bordercolor=BORDER, borderwidth=1,
                    font=list(color=TEXT))
    )
  })
  
  observe({
    stat <- req(input$scale_stat)
    col  <- paste0("scale_", stat)
    eligible <- weapons_flat %>%
      filter(!!sym(col) > 0) %>%
      arrange(name) %>%
      pull(name)
    prev     <- isolate(input$scale_weapons)
    kept     <- intersect(prev, eligible)
    selected <- if (length(kept) > 0) kept else head(eligible, 3)
    
    updateSelectizeInput(session, "scale_weapons",
                         choices  = eligible,
                         selected = selected,
                         options  = list(
                           plugins     = list("remove_button"),
                           maxItems    = 5,
                           placeholder = "Select weapons..."
                         )
    )
  })
  
  output$scaling_line <- renderPlotly({
    sel_stat    <- req(input$scale_stat)
    sel_weapons <- input$scale_weapons
    if (length(sel_weapons) == 0) return(plotly_empty())
    
    stat_rng <- seq(input$scale_range[1], input$scale_range[2], by=1)
    scale_col <- paste0("scale_", sel_stat)
    
    w_sub <- weapons_flat %>% filter(name %in% sel_weapons)
    
    plot_data <- lapply(sel_weapons, function(wname) {
      w <- w_sub %>% filter(name == wname)
      base_ar <- w$atk_Phy + w$atk_Mag + w$atk_Fire + w$atk_Ligt + w$atk_Holy
      sc      <- w[[scale_col]]
      
      ar_vec <- sapply(stat_rng, function(lv) {
        eff <- get_stat_efficiency(lv)
        mult <- 1 + sc * eff
        round(base_ar * mult, 0)
      })
      data.frame(stat_lv=stat_rng, AR=ar_vec, weapon=wname)
    })
    
    df_lines <- bind_rows(plot_data)
    
    p <- plot_ly()
    line_colors <- cat_palette
    for (i in seq_along(sel_weapons)) {
      sub <- df_lines %>% filter(weapon == sel_weapons[i])
      p <- add_trace(p,
                     data = sub, x=~stat_lv, y=~AR,
                     type="scatter", mode="lines",
                     name=sel_weapons[i],
                     line=list(color=line_colors[i], width=2.5),
                     hovertemplate=paste0("<b>", sel_weapons[i], "</b><br>",
                                          sel_stat, ": %{x}<br>AR: %{y}<extra></extra>")
      )
    }
    soft_caps <- c(20, 55, 80)
    shapes      <- list()
    annotations <- list()
    for (sc in soft_caps) {
      if (sc >= input$scale_range[1] && sc <= input$scale_range[2]) {
        shapes[[length(shapes) + 1]] <- list(
          type = "line",
          x0 = sc, x1 = sc, xref = "x",
          y0 = 0,  y1 = 1,  yref = "paper",
          line = list(color = GOLD_DK, dash = "dot", width = 1)
        )
        annotations[[length(annotations) + 1]] <- list(
          x = sc, xref = "x",
          y = 1.04, yref = "paper",
          text = paste0("Cap ", sc),
          font = list(color = GOLD_DK, size = 10, family = "Cinzel"),
          showarrow = FALSE
        )
      }
    }
    
    p <- er_layout(p, xlab = paste(sel_stat, "Level"), ylab = "Total AR")
    p %>% layout(shapes = shapes, annotations = annotations)
  })
  
  output$efficiency_plot <- renderPlotly({
    cats <- if (length(input$weight_cat) == 0) all_categories else input$weight_cat
    
    df <- weapons_flat %>%
      filter(category %in% cats) %>%
      calc_ar(
        isolate(input$str), isolate(input$dex), isolate(input$int),
        isolate(input$fai), isolate(input$arc)
      ) %>%
      filter(Total_AR >= input$weight_ar_min, weight <= input$weight_max) %>%
      mutate(Efficiency = round(Total_AR / pmax(weight, 0.1), 1))
    
    if (nrow(df) == 0) return(plotly_empty())
    
    p <- plot_ly(df,
                 x    = ~weight,
                 y    = ~Total_AR,
                 color = ~category,
                 colors = cat_palette,
                 size  = ~Efficiency,
                 sizes = c(6, 50),
                 type  = "scatter",
                 mode  = "markers",
                 text  = ~paste0("<b>", name, "</b><br>",
                                 "Category: ", category, "<br>",
                                 "Weight: ", weight, "<br>",
                                 "Total AR: ", Total_AR, "<br>",
                                 "AR/Wt: ", Efficiency),
                 hoverinfo = "text",
                 marker = list(opacity=0.85, line=list(width=0.5, color="rgba(0,0,0,0.4)"))
    )
    er_layout(p, xlab="Weight", ylab="Total AR")
  })
  
  shields_raw <- local({
    s <- read.csv("shields.csv", stringsAsFactors = FALSE)
    
    def_cols <- parse_defence_json(s$defence)
    req_cols  <- parse_elden_json(s$requiredAttributes, "req_", "amount")
    s <- bind_cols(s, def_cols, req_cols)
    for (col in c("def_Phy","def_Mag","def_Fire","def_Ligt","def_Holy","def_Boost")) {
      if (!col %in% names(s)) s[[col]] <- 0
    }
    for (col in c("req_Str","req_Dex","req_Int","req_Fai","req_Arc")) {
      if (!col %in% names(s)) s[[col]] <- 0
    }
    s <- s %>% mutate(across(starts_with("def_") | starts_with("req_"), ~ {
      suppressWarnings(as.numeric(.)) %>% replace_na(0)
    }))
    s
  })
  
  shields_equippable <- reactive({
    req(input$str, input$dex, input$int, input$fai, input$arc)
    shields_raw %>%
      filter(req_Str <= input$str, req_Dex <= input$dex,
             req_Int <= input$int, req_Fai <= input$fai, req_Arc <= input$arc)
  })
  
  output$shield_table <- renderDT({
    df <- shields_equippable() %>%
      arrange(desc(def_Phy)) %>%
      select(Name=name, Category=category, Weight=weight,
             `Phy Def`=def_Phy, `Mag Def`=def_Mag, `Fire Def`=def_Fire,
             `Ligt Def`=def_Ligt, `Holy Def`=def_Holy)
    datatable(
      df,
      selection = "single",
      rownames  = FALSE,
      options = list(
        pageLength   = 15,
        lengthChange = FALSE,
        scrollX      = FALSE,
        dom          = "ftp",
        columnDefs   = list(list(className="dt-right", targets=2:7))
      )
    ) %>%
      formatStyle("Phy Def",
                  background = styleColorBar(range(df$`Phy Def`, na.rm=TRUE), "#7A5A10"),
                  backgroundSize = "100% 80%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center"
      ) %>%
      formatStyle(0:7, color = TEXT, backgroundColor = "transparent")
  })
  
  output$shield_detail <- renderUI({
    sel <- input$shield_table_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      return(tags$p(style="color:#8A7B60; font-style:italic; text-align:center; margin-top:30px;",
                    "← Click a shield row to inspect"))
    }
    df_sorted <- shields_equippable() %>% arrange(desc(def_Phy))
    if (sel > nrow(df_sorted)) return(NULL)
    s <- df_sorted[sel, , drop=FALSE]
    s_image <- as.character(s$image[[1]])
    s_name  <- as.character(s$name[[1]])
    s_desc  <- as.character(s$description[[1]])
    
    tags$div(class = "shield-detail-wrap",
             if (!is.na(s_image) && nchar(s_image) > 4)
               tags$img(src = s_image, class = "shield-img-large",
                        onerror = "this.style.display='none'"),
             tags$h4(style = sprintf("font-family:Cinzel,serif; color:%s; font-size:15px; margin:0 0 10px 0;", GOLD_LT),
                     s_name),
             if (!is.na(s_desc) && nchar(s_desc) > 5)
               tags$div(class = "shield-desc-block", s_desc)
    )
  })
  
  output$shield_scatter <- renderPlotly({
    df <- shields_equippable()
    if (nrow(df) == 0) return(plotly_empty())
    p <- plot_ly(df,
                 x = ~weight,
                 y = ~def_Phy,
                 color = ~category,
                 colors = cat_palette,
                 type = "scatter",
                 mode = "markers",
                 text = ~paste0("<b>", name, "</b><br>",
                                "Category: ", category, "<br>",
                                "Weight: ", weight, "<br>",
                                "Phy Def: ", def_Phy),
                 hoverinfo = "text",
                 marker = list(size = 9, opacity = 0.85,
                               line = list(width = 0.5, color = "rgba(0,0,0,0.4)"))
    )
    er_layout(p, xlab = "Weight", ylab = "Physical Defence")
  })
  
  talismans_data <- local({
    read.csv("talismans.csv", stringsAsFactors = FALSE) %>%
      mutate(across(everything(), as.character))
  })
  
  talisman_owned <- reactiveVal(character(0))
  
  observeEvent(input$talisman_toggle, {
    id  <- input$talisman_toggle$id
    cur <- talisman_owned()
    if (id %in% cur) {
      talisman_owned(setdiff(cur, id))
    } else {
      talisman_owned(c(cur, id))
    }
  })
  
  output$talisman_ownership <- renderUI({
    n_owned <- length(talisman_owned())
    n_total <- nrow(talismans_data)
    pct <- if (n_total > 0) round(100 * n_owned / n_total) else 0
    
    
    arc_color <- if (pct == 100) "#4CAF82"
    else if (pct >= 67) "#C9A84C"
    else if (pct >= 34) "#A07828"
    else "#7A3A1A"
    
    
    r <- 80
    half_circ <- round(pi * r, 2)   # ~251.33
    dash_fill  <- round(half_circ * pct / 100, 2)
    dash_gap   <- half_circ - dash_fill
    # Path draws the semicircle LEFT -> TOP -> RIGHT (sweep-flag=1 = clockwise)
    semi_d <- sprintf("M 20 100 A %d %d 0 0 1 180 100", r, r)
    
    HTML(sprintf('
<div style="display:flex; flex-direction:column; align-items:center; padding:6px 0 2px 0;">
  <svg viewBox="0 0 200 108" width="200" height="108" xmlns="http://www.w3.org/2000/svg">
    <!-- track -->
    <path d="%s" fill="none" stroke="#2A2218" stroke-width="16" stroke-linecap="round"/>
    <!-- fill: dasharray = filled_length gap_length, offset=0 so it starts at left -->
    <path d="%s" fill="none" stroke="%s" stroke-width="16" stroke-linecap="round"
          stroke-dasharray="%.2f %.2f" stroke-dashoffset="0"/>
    <!-- percentage text -->
    <text x="100" y="96" text-anchor="middle" font-family="Cinzel,serif" font-size="26" font-weight="700" fill="%s">%d%%</text>
  </svg>
  <div style="font-family:Cinzel,serif; font-size:11px; letter-spacing:1.5px; color:#8A7B60; margin-top:-4px;">%d of %d TALISMANS</div>
</div>',
                 semi_d,
                 semi_d, arc_color, dash_fill, dash_gap,
                 arc_color, pct,
                 n_owned, n_total
    ))
  })
  
  output$talisman_list_ui <- renderUI({
    owned <- talisman_owned()
    items <- lapply(seq_len(nrow(talismans_data)), function(i) {
      t  <- talismans_data[i, ]
      is_owned <- t$id %in% owned
      tags$div(
        class = paste("talisman-item", if (is_owned) "owned" else ""),
        onclick = sprintf("Shiny.setInputValue('talisman_toggle', {id: '%s', ts: Date.now()}, {priority: 'event'});", t$id),
        tags$img(src = t$image, class = "talisman-thumb",
                 onerror = "this.style.display='none'"),
        tags$div(class = "talisman-info",
                 tags$div(class = "talisman-name", t$name),
                 if (!is.na(t$effect) && nzchar(trimws(t$effect)) && t$effect != "NA")
                   tags$div(class = "talisman-effect", t$effect)
        ),
        tags$span(style = sprintf("font-size:18px; color:%s; flex-shrink:0;", if (is_owned) GOLD else BORDER),
                  if (is_owned) "\u2713" else "\u25cb")
      )
    })
    tags$div(class = "talisman-scroll", items)
  })
  
  output$talisman_slot_ui <- renderUI({
    n_slots <- as.integer(input$talisman_slots)
    if (is.null(n_slots) || n_slots < 1) return(NULL)
    
    owned <- talisman_owned()
    owned_data <- talismans_data[talismans_data$id %in% owned, , drop=FALSE]
    talisman_choices <- c("— None —" = "", setNames(owned_data$id, owned_data$name))
    
    lapply(seq_len(n_slots), function(i) {
      sel_input_id  <- paste0("talisman_slot_", i)
      sel_val       <- input[[sel_input_id]]
      
     
      other_slots <- setdiff(seq_len(n_slots), i)
      used_ids <- sapply(other_slots, function(j) {
        v <- input[[paste0("talisman_slot_", j)]]
        if (is.null(v) || v == "") NA_character_ else v
      })
      used_ids <- na.omit(used_ids)
      available <- talisman_choices[!talisman_choices %in% used_ids | talisman_choices == ""]
      
      selected_talisman <- if (!is.null(sel_val) && sel_val != "" && sel_val %in% talismans_data$id) {
        talismans_data[talismans_data$id == sel_val, , drop=FALSE]
      } else NULL
      
      tags$div(class = "slot-row",
               tags$div(class = "slot-label", paste("Slot", i)),
               if (!is.null(selected_talisman)) {
                 tags$img(src = selected_talisman$image, class = "slot-img",
                          onerror = "this.style.display='none'")
               } else {
                 tags$div(class = "slot-img", style = "border-style:dashed;")
               },
               tags$div(style = "flex:1; min-width:0;",
                        selectInput(sel_input_id, label = NULL,
                                    choices = available, selected = sel_val,
                                    width = "100%"),
                        if (!is.null(selected_talisman)) {
                          eff <- selected_talisman$effect
                          desc <- selected_talisman$description
                          tags$div(
                            tags$div(class = "slot-name", selected_talisman$name),
                            if (!is.na(desc) && nzchar(trimws(desc)) && desc != "NA")
                              tags$div(class = "slot-desc", desc),
                            if (!is.na(eff) && nzchar(trimws(eff)) && eff != "NA")
                              tags$div(class = "talisman-effect", style = "margin-top: 4px;", eff)
                          )
                        }
               )
      )
    })
  })
  
  output$category_bar <- renderPlotly({
    metric <- input$cat_metric
    
    df_all <- weapons_flat %>%
      calc_ar(
        isolate(input$str), isolate(input$dex), isolate(input$int),
        isolate(input$fai), isolate(input$arc)
      )
    
    cat_summary <- if (metric == "count") {
      df_all %>% group_by(category) %>% summarise(val = n(), .groups="drop")
    } else {
      df_all %>% group_by(category) %>%
        summarise(val = mean(get(metric), na.rm=TRUE), .groups="drop") %>%
        mutate(val = round(val, 1))
    }
    
    if (input$cat_sort) cat_summary <- arrange(cat_summary, val)
    
    metric_label <- switch(metric,
                           "Total_AR"="Avg Total AR", "weight"="Avg Weight", "count"="Weapon Count")
    
    p <- plot_ly(cat_summary,
                 x    = ~val,
                 y    = ~reorder(category, val),
                 type = "bar",
                 orientation = "h",
                 marker = list(
                   color = cat_palette[seq_len(nrow(cat_summary))],
                   line  = list(color="rgba(0,0,0,0.2)", width=0.5)
                 ),
                 text = ~round(val, 1),
                 textposition = "outside",
                 textfont = list(color=DIMTEXT, size=11),
                 hovertemplate = "<b>%{y}</b><br>%{x}<extra></extra>"
    ) %>% layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font   = list(family="Crimson Text", color=TEXT),
      margin = list(l=150, r=60),
      xaxis  = list(title=metric_label, gridcolor=BORDER, zerolinecolor=BORDER,
                    tickfont=list(color=DIMTEXT), titlefont=list(color=GOLD, family="Cinzel", size=11)),
      yaxis  = list(title="", tickfont=list(color=TEXT, size=12))
    )
    p
  })
  
}
