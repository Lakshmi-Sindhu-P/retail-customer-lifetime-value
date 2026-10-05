# Shiny dashboard -------------------------------------------------------------
# Segment explorer over the fitted CLV model. Reads the cached targets written
# by `make all`; does not refit anything, so it starts instantly.

library(shiny)
library(dplyr)
library(ggplot2)

# Run `make all` before launching: the dashboard reads the fitted artefacts
# rather than refitting anything.
#
# shiny::runApp() changes the working directory to app/ before sourcing this
# file, so the helper is located by trying both layouts rather than assuming
# one. set_project_root() then normalises the working directory to the repo
# root, which is what targets::tar_load() needs.
for (p in c(file.path("R", "root.R"), file.path("..", "R", "root.R"))) {
  if (file.exists(p)) { source(p); break }
}
if (!exists("set_project_root")) {
  stop("Could not locate R/root.R. Run the dashboard from the repo root.")
}
set_project_root()

source("R/00_config.R")
source("R/utils.R")

if (!dir.exists(file.path("_targets", "objects"))) {
  stop("No targets data store found. Run `make all` first.")
}
targets::tar_load(c(clv_tbl, clv_summary, results, customers))

PAL <- PALETTE
seg_cols <- setNames(
  c(PAL[["primary"]], PAL[["secondary"]], PAL[["accent"]],
    PAL[["warn"]], PAL[["neutral"]]),
  levels(factor(results$clv_summary$segment)))

# A titled panel. shiny::box() is no longer exported by shiny (it lives in
# shinydashboard / bs4), so use a plain bootstrap column.
panel_box <- function(title, ..., width = 6) {
  column(
    width,
    div(class = "well",
        tags$strong(title), div(...))
  )
}

app <- shinyApp(
  ui = fluidPage(
    titlePanel("Customer Lifetime Value - Segment Explorer"),
    sidebarLayout(
      sidebarPanel(
        selectInput("segment", "Segment", choices = levels(seg_cols),
                    selected = levels(seg_cols)),
        sliderInput("margin", "Assumed gross margin (%)",
                    min = 0, max = 100, value = CFG$gross_margin * 100, step = 5),
        sliderInput("horizon", "CLV horizon (weeks)", min = 13, max = 104,
                    value = CFG$clv_horizon_weeks, step = 13),
        helpText("Margin and horizon are assumptions, not measurements. The",
                 "dataset carries no cost data, so every CLV figure here",
                 "scales linearly with margin."),
        hr(),
        downloadButton("csv", "Download segment as CSV")
      ),
      mainPanel(
        width = 10,
        fluidRow(uiOutput("kpis")),
        fluidRow(
          column(7, div(class = "well", plotOutput("clv_plot", height = "340px"))),
          column(5, div(class = "well", plotOutput("scatter", height = "340px")))
        ),
        fluidRow(
          column(12, div(class = "well", plotOutput("retention", height = "300px")))
        ),
        fluidRow(
          column(12, div(class = "well", tableOutput("seg_table")))
        )
      )
    )
  ),

  server = function(input, output, session) {

    # CLV is linear in margin, so re-scale rather than refitting.
    scaled <- reactive({
      d <- clv_tbl
      m <- input$margin / 100 / CFG$gross_margin
      d$clv <- d$clv * m
      d$clv_horizon_weeks <- input$horizon
      d
    })

    output$kpis <- renderUI({
      d <- scaled() %>% filter(segment == input$segment)
      n <- nrow(d)
      stat_tile <- function(label, value) {
        column(3, div(class = "well",
                      tags$strong(label), tags$br(),
                      tags$span(class = "h4", value)))
      }
      fluidRow(
        stat_tile("Customers", format(n, big.mark = ",")),
        stat_tile("Share of base", paste0(round(100 * n / nrow(scaled()), 1), "%")),
        stat_tile("Total predicted CLV", paste0(fmt_money(sum(d$clv) / 1e6, 2), "M")),
        stat_tile("Median CLV per customer", fmt_money(median(d$clv)))
      )
    })

    output$clv_plot <- renderPlot({
      ggplot(clv_tbl, aes(clv, fill = segment)) +
        geom_histogram(bins = 45, colour = NA, alpha = 0.85) +
        scale_fill_manual(values = seg_cols, guide = "none") +
        scale_x_continuous(labels = function(x)
          paste0(CFG$currency_symbol, format(round(x), big.mark = ",")),
          trans = "log1p") +
        labs(title = "Distribution of predicted CLV, by segment",
             subtitle = "Log scale. Note how far the tails separate.",
             x = "Predicted CLV per customer", y = "Customers") +
        theme_minimal(base_size = 11)
    })

    output$scatter <- renderPlot({
      d <- scaled()
      ggplot(d, aes(recency, frequency, colour = segment)) +
        geom_point(alpha = 0.3, size = 0.9) +
        scale_y_continuous(trans = "log1p") +
        scale_colour_manual(values = seg_cols, guide = "none") +
        labs(title = "Segment profile",
             x = "Days since last purchase", y = "Orders (log)") +
        theme_minimal(base_size = 11)
    })

    output$retention <- renderPlot({
      d <- results$survival_check
      ggplot(d, aes(time)) +
        geom_line(aes(y = km_survival), colour = PAL[["neutral"]], linewidth = 1.4) +
        geom_line(aes(y = weibull_survival), colour = PAL[["secondary"]],
                  linewidth = 1.1, linetype = "dashed") +
        scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"),
                           limits = c(0, 1)) +
        labs(title = "Retention, and a rejected parametric fit",
             subtitle = paste0("Solid = Kaplan-Meier, the model behind every ",
                               "CLV figure here (no distributional assumption). ",
                               "Dashed = Weibull MLE, fitted and rejected: the ",
                               "curve is a mixture no single Weibull can fit."),
             x = "Weeks since first purchase", y = "Share still active") +
        theme_minimal(base_size = 11)
    })

    output$seg_table <- renderTable({
      d <- scaled()
      by <- d %>% group_by(segment) %>%
        summarise(Customers = n(),
                  `Median recency (days)` = median(recency),
                  `Median orders` = median(frequency),
                  `Median spend` = fmt_money(median(monetary)),
                  `Median CLV` = fmt_money(median(clv)),
                  `Total CLV` = paste0(fmt_money(sum(clv) / 1e6, 2), "M"),
                  .groups = "drop")
      as.data.frame(by)
    }, digits = 0, na.strings = "-")

    output$csv <- downloadHandler(
      filename = function() sprintf("segment-%s-clv.csv", input$segment),
      content = function(file) {
        d <- scaled() %>% filter(segment == input$segment)
        write.csv(d, file, row.names = FALSE)
      }
    )
  }
)

# shiny::runApp() uses the file's last value as the app object. `app` is
# already a shiny.appobj from the shinyApp() call above, so returning it
# directly is correct -- re-wrapping with shinyApp(app) passes the appobj as
# `ui` and fails with "argument server is missing".
app
