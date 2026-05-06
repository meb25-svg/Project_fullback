library(shiny)
library(bslib)
library(DT)
library(tidyverse)

df_raw <- read.csv("app_data.csv")
ui <- page_navbar(
  title = tags$span(
    style = "font-family: 'Barlow Condensed', sans-serif; font-size: 1.3rem; letter-spacing: 1px;",
    "  Fullback Analytics Dashboard"
  ),
  theme = bs_theme(
    bootswatch   = "darkly",
    primary      = "#1a3a6b",
    base_font    = font_google("Barlow"),
    heading_font = font_google("Barlow Condensed"),
    font_scale   = 1.05
  ),
  tags$head(tags$style(HTML("
    body { background-color: #0d1b2a; }
    .card { background-color: #1b2a3b !important; border: 1px solid #2e4060; }
    .card-header { background-color: #162236 !important; font-family: 'Barlow Condensed'; 
                   font-size: 1.1rem; letter-spacing: 0.5px; }
    .nav-link { font-family: 'Barlow Condensed'; font-size: 1rem; letter-spacing: 0.5px; }
    .plot-img { width: 100%; border-radius: 6px; }
    .tab-desc { color: #8fb3d9; font-size: 0.9rem; margin-bottom: 12px; }
  "))),

  # overview
  nav_panel("Overview", icon = icon("football"),
    br(),
    layout_columns(col_widths = c(12),
      card(
        card_header("The Value of the Fullback in Modern NFL Football"),
        card_body(
          p("This dashboard presents findings from an analysis of NFL play-by-play data
             from the 2023–2024 seasons, examining whether fullback presence improves
             offensive outcomes in three key situations:"),
          layout_columns(col_widths = c(4,4,4),
            card(card_header("Short Yardage"),
              card_body(p("3rd or 4th down with ≤ 2 yards to go.",
                br(), br(),
                tags$b("Success = gaining at least 1 yard")))),
            card(card_header(" Goal Line"),
              card_body(p("Any play inside the opponent's 5-yard line.",
                br(), br(),
                tags$b("Success = scoring a touchdown")))),
            card(card_header("Base Down"),
              card_body(p("1st & 10 or 2nd & 5–10.",
                br(), br(),
                tags$b("Success = positive Expected Points Added"))))
          ),
          br(),
          p("Models control for down, distance, field position, score differential,
             defenders in box, and game context to isolate the effect of fullback presence.
             Mixed models include individual fullback random effects to account for
             player quality differences.")
        )
      )
    ),
    layout_columns(col_widths = c(4,4,4),
      value_box(title = "Total Plays Analyzed",
        value = scales::comma(nrow(filter(df_raw, situation_type %in% c("goal_line","short_yardage","base_down")))),
        showcase = icon("database"), theme = "primary"),
      value_box(title = "Plays With Fullback",
        value = scales::comma(sum(filter(df_raw, situation_type %in% c("goal_line","short_yardage","base_down"))$has_fullback)),
        showcase = icon("person-running"), theme = "secondary"),
      value_box(title = "Seasons Covered",
        value = "2023 – 2024",
        showcase = icon("calendar"), theme = "info")
    )
  ),

  # success rates
  nav_panel("Success Rates", icon = icon("chart-bar"),
    br(),
    layout_columns(col_widths = c(4, 8),
      card(card_header("Filters"),
        card_body(
          selectInput("sr_situation", "Situation",
            choices = c("All Situations" = "all",
                        "Short Yardage"  = "short_yardage",
                        "Goal Line"      = "goal_line",
                        "Base Down"      = "base_down")),
          selectInput("sr_play_type", "Play Type",
            choices = c("All Plays" = "all",
                        "Rush Only" = "rush",
                        "Pass Only" = "pass")),
          hr(),
          p(class = "tab-desc",
            "Bar chart shows raw success rates with 95% confidence intervals.
             Use the pre-built chart tab for model-adjusted results.")
        )
      ),
      card(card_header("Success Rate: Fullback Present vs Not"),
        card_body(plotOutput("success_plot", height = "400px")))
    ),
    br(),
    card(card_header("Pre-Built Chart — All Situations"),
      card_body(
        p(class = "tab-desc", "From your analysis — success rates across all three situations and play types."),
        tags$img(src = knitr::image_uri("successplot.png"), class = "plot-img")
      )
    )
  ),

  # basic plus minus
  nav_panel("Plus / Minus", icon = icon("plus-minus"),
    br(),
    card(card_header("Basic Plus/Minus Results"),
      card_body(
        p(class = "tab-desc",
          "Raw difference in EPA, success rate, and yards gained between fullback and non-fullback plays,
           before controlling for situational factors."),
        tags$img(src = knitr::image_uri("basicplusminus.png"), class = "plot-img")
      )
    )
  ),

  # Regression Models
  nav_panel("Regression Models", icon = icon("diagram-project"),
    br(),
    layout_columns(col_widths = c(12),
      card(card_header("Fullback Presence Coefficient — All Models (GLM/LM)"),
        card_body(
          p(class = "tab-desc",
            "Forest plot of the has_fullback coefficient across all situation/play-type model combinations.
             Blue = statistically significant at 95% level."),
          tags$img(src = knitr::image_uri("Fullback_presence_lmglm.png"), class = "plot-img")
        )
      )
    ),
    br(),
    navset_card_tab(
      nav_panel("Short Yardage — All",  tags$img(src = knitr::image_uri("Shortyardageall.png"),  class = "plot-img")),
      nav_panel("Short Yardage — Rush", tags$img(src = knitr::image_uri("shortyardagerush.png"), class = "plot-img")),
      nav_panel("Short Yardage — Pass", tags$img(src = knitr::image_uri("shortyardagepass.png"), class = "plot-img")),
      nav_panel("Goal Line — All",      tags$img(src = knitr::image_uri("goalall.png"),          class = "plot-img")),
      nav_panel("Goal Line — Rush",     tags$img(src = knitr::image_uri("goalrush.png"),         class = "plot-img")),
      nav_panel("Goal Line — Pass",     tags$img(src = knitr::image_uri("goalpass.png"),         class = "plot-img")),
      nav_panel("Base Down — All",      tags$img(src = knitr::image_uri("baseall.png"),          class = "plot-img")),
      nav_panel("Base Down — Rush",     tags$img(src = knitr::image_uri("baserush.png"),         class = "plot-img")),
      nav_panel("Base Down — Pass",     tags$img(src = knitr::image_uri("basepass.png"),         class = "plot-img"))
    )
  ),

  #Mixed Models 
  nav_panel("Mixed Models", icon = icon("layer-group"),
    br(),
    card(card_header("Mixed Model Coefficients — Fullback Presence Effect"),
      card_body(
        p(class = "tab-desc",
          "Random effects models accounting for individual fullback quality.
           Coefficient plot shows has_fullback fixed effect across all models."),
        tags$img(src = knitr::image_uri("mixedcoeff.png"), class = "plot-img")
      )
    ),
    br(),
    card(card_header("Mixed Model Logistic Results"),
      card_body(
        tags$img(src = knitr::image_uri("mixedlog.png"), class = "plot-img")
      )
    )
  ),

  #  Player Effects
  nav_panel("Player Effects", icon = icon("person-running"),
    br(),
    p(class = "tab-desc ps-3",
      "Individual fullback random intercepts — how much each player's presence deviates
       from the average after controlling for situation, field position, and game context."),
    navset_card_tab(
      nav_panel("Short Yardage — All",  tags$img(src = knitr::image_uri("shortyardagerushplayers.png"), class = "plot-img")),
      nav_panel("Goal Line — All",      tags$img(src = knitr::image_uri("goalallplayers.png"),          class = "plot-img")),
      nav_panel("Goal Line — Rush",     tags$img(src = knitr::image_uri("goalrushplayers.png"),         class = "plot-img")),
      nav_panel("Base Down — All",      tags$img(src = knitr::image_uri("baseallplayers.png"),          class = "plot-img")),
      nav_panel("Base Down — Rush",     tags$img(src = knitr::image_uri("baserushplayers.png"),         class = "plot-img")),
      nav_panel("Base Down — Pass",     tags$img(src = knitr::image_uri("basepassplayers.png"),         class = "plot-img"))
    )
  ),

  # Play Explorer
  nav_panel("Play Explorer", icon = icon("table"),
    br(),
    layout_columns(col_widths = c(3, 9),
      card(card_header("Filters"),
        card_body(
          selectInput("pe_situation", "Situation",
            choices = c("All", "Short Yardage" = "short_yardage",
                        "Goal Line" = "goal_line", "Base Down" = "base_down")),
          selectInput("pe_play_type", "Play Type",
            choices = c("All", "Rush", "Pass")),
          selectInput("pe_fullback", "Fullback Present?",
            choices = c("Both", "Yes", "No")),
          selectInput("pe_team", "Team",
            choices = c("All", sort(unique(df_raw$posteam)))),
          sliderInput("pe_yardline", "Yardline (dist. to end zone)",
            min = 1, max = 100, value = c(1, 100)),
          actionButton("pe_go", "Apply Filters",
            class = "btn-primary w-100 mt-2"),
          hr(),
          uiOutput("pe_count")
        )
      ),
      card(card_header("Play-by-Play Data"),
        card_body(DTOutput("play_table"))
      )
    )
  )
)


server <- function(input, output, session) {

  
  output$success_plot <- renderPlot({
    d <- df_raw

    if (input$sr_situation != "all")
      d <- filter(d, situation_type == input$sr_situation)
    else
      d <- filter(d, situation_type %in% c("short_yardage","goal_line","base_down"))

    if (input$sr_play_type == "rush") d <- filter(d, rush == 1)
    if (input$sr_play_type == "pass") d <- filter(d, pass == 1)

    plot_df <- d %>%
      mutate(
        success = case_when(
          situation_type == "short_yardage" ~ short_yardage_success,
          situation_type == "goal_line"     ~ goal_line_success,
          situation_type == "base_down"     ~ as.integer(epa > 0),
          TRUE ~ NA_integer_
        )
      ) %>%
      filter(!is.na(success)) %>%
      group_by(situation_label, fb_label) %>%
      summarise(
        success_rate = mean(success, na.rm = TRUE),
        n  = n(),
        se = sqrt(success_rate * (1 - success_rate) / n),
        .groups = "drop"
      ) %>%
      mutate(situation_label = factor(situation_label,
               levels = c("Short Yardage","Goal Line","Base Down")))

    validate(need(nrow(plot_df) > 0, "No data for this selection."))

    ggplot(plot_df, aes(x = situation_label, y = success_rate, fill = fb_label)) +
      geom_col(position = position_dodge(0.7), width = 0.6) +
      geom_errorbar(aes(ymin = pmax(success_rate - 1.96*se, 0),
                        ymax = pmin(success_rate + 1.96*se, 1)),
                    position = position_dodge(0.7), width = 0.2, color = "white") +
      geom_text(aes(label = paste0(round(success_rate*100, 1), "%")),
                position = position_dodge(0.7),
                vjust = -0.8, size = 4.5, fontface = "bold", color = "white") +
      scale_fill_manual(values = c("Fullback Present" = "#1a5fa8",
                                   "No Fullback"      = "#4a4a4a")) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.05)) +
      labs(x = NULL, y = "Success Rate", fill = NULL,
           caption = "Error bars = 95% confidence interval | n shown per bar") +
      theme_minimal(base_size = 14) +
      theme(
        legend.position    = "bottom",
        panel.background   = element_rect(fill = "#1b2a3b", color = NA),
        plot.background    = element_rect(fill = "#1b2a3b", color = NA),
        text               = element_text(color = "white"),
        axis.text          = element_text(color = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_line(color = "#2e4060"),
        legend.background  = element_rect(fill = "#1b2a3b", color = NA)
      )
  }, bg = "#1b2a3b")

  
  filtered_plays <- eventReactive(input$pe_go, {
    d <- df_raw

    if (input$pe_situation != "All")
      d <- filter(d, situation_type == input$pe_situation)
    else
      d <- filter(d, situation_type %in% c("short_yardage","goal_line","base_down"))

    if (input$pe_play_type == "Rush") d <- filter(d, rush == 1)
    if (input$pe_play_type == "Pass") d <- filter(d, pass == 1)
    if (input$pe_fullback  == "Yes")  d <- filter(d, has_fullback == 1)
    if (input$pe_fullback  == "No")   d <- filter(d, has_fullback == 0)
    if (input$pe_team      != "All")  d <- filter(d, posteam == input$pe_team)

    d <- filter(d, yardline_100 >= input$pe_yardline[1],
                   yardline_100 <= input$pe_yardline[2])

    d %>%
      select(
        Season      = season,
        Team        = posteam,
        Opponent    = defteam,
        Qtr         = qtr,
        Down        = down,
        `Yds to Go` = ydstogo,
        Yardline    = yardline_100,
        `Play Type` = play_type_simple,
        `Has FB`    = fb_label,
        Situation   = situation_label,
        `Yds Gained`= yards_gained,
        EPA         = epa,
        Description = desc
      ) %>%
      mutate(EPA = round(EPA, 3)) %>%
      head(3000)
  }, ignoreNULL = FALSE)

  output$pe_count <- renderUI({
    n <- nrow(filtered_plays())
    tags$p(style = "color:#8fb3d9; font-size:0.85rem;",
           paste0(scales::comma(n), " plays shown (max 3,000)"))
  })

  output$play_table <- renderDT({
    datatable(
      filtered_plays(),
      options  = list(pageLength = 15, scrollX = TRUE, dom = "ftip"),
      rownames = FALSE,
      style    = "bootstrap4"
    ) %>%
      formatStyle("Has FB",
        backgroundColor = styleEqual(
          c("Fullback Present","No Fullback"),
          c("#1a3a6b","#2a2a2a")),
        color = "white"
      ) %>%
      formatStyle("EPA",
        color = styleInterval(0, c("#e05c5c","#4e8ef7")))
  })
}

shinyApp(ui, server)


