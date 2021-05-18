# ---- Load libraries ----
library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(DT)

# ---- Load datasets ----
lsoa <- read_rds("data/lsoa.rds")
ward <- read_rds("data/wards.rds")

imd_lsoa <- read_rds("data/imd_lsoa.rds")
imd_ward <- read_rds("data/imd_wards.rds")

# ---- UI ----
ui <- fluidPage(
  
  # Use colours from BRC style guide:
  # - https://design-system.redcross.org.uk/styles/colours/
  e_theme_register(
    theme = '{"color":["#5C747A","#193351","#6A9EAA"]}',
    name = "brc_theme"
  ),
  
  # CSS Styles
  tags$head(
    tags$style(
      HTML("
      #card {
          box-shadow: 0px 0px 3px grey;
          border-radius: 5px;
          padding: 10px 20px 10px 20px;
          margin: 0px 0px 20px 0px;
      }
      #map {
          box-shadow: 0px 0px 3px grey;
          border-radius: 5px;
          margin: 0px 0px 20px 0px;
      }
      #footer {
          background-color: #262626;
          height: 165px;
      }
      a {
          color: #5C747A;
      }
      ")
    )
  ),
  
  # - Top bar with logos -
  fluidRow(
    column(width = 4),
    column(
      width = 4,
      align = "center",
      tags$div(
        style = "padding-top: 10px;",
        tags$a(
          href = "https://redcross.org.uk",
          target = "_blank",
          img(src = "brc-team-logo.jpg", width = 400)
        ) # a
      ) # Div
    ), # Column
    column(
      width = 4,
      align = "right",
      tags$div(
        style = "padding-top: 15px; padding-right: 10px;",
        tags$a(
          href = "https://github.com/britishredcrosssociety/nhs-priority-wards",
          target = "_blank",
          icon("github", "fa-2x"),
          tags$style(".fa-github {color:#262626}")
        )
      )
    )
  ),
  
  # - Instructions -
  fluidRow(
    column(width = 2),
    column(
      width = 8,
      align = "center",
      tags$h1(
        style = "padding-top: 12px; padding-bottom: 8px",
        "Deprivation in English wards"
      )
    ),
    column(width = 2)
  ),
  
  # - Ward Search Box -
  fluidRow(
    column(
      width = 12,
      align = "center",
      selectizeInput(
        "selectbox",
        label = NULL,
        choices = sort(ward$ward_name),
        options = list(
          placeholder = "Select a ward",
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    )
  ),
  
  # - Map & Plots -
  fluidRow(
    
    # - Map and single plot underneath map -
    column(
      width = 4,
      
      # - Map -
      fluidRow(
        
        # Wrapping in column(), again, provides padding to the left of the map
        column(
          width = 12,
          leafletOutput("map", height = 670)
        )
      )
    ),
    
    # - Grid of tabsets -
    column(
      width = 12,
      
      # - Row 1 -
      fluidRow(
        # - Col 1 -
        column(
          width = 12,
          align = "center",
          tags$div(
            id = "card",
            h4("Ward deprivation"),
            
          )
        )
      ),

      # - Row 2 -
      fluidRow(
        
        # - Col 1 -
        column(
          width = 6,
          align = "center",
          tags$div(
            id = "card",
            h4("Cancer Wait Times"),
            h6("Latest data: Apr 2021"),
            tabsetPanel(
              tabPanel("Plot", echarts4rOutput("cancer_plot", height = "200px")),
              tabPanel("Data", DTOutput("cancer_table"))
            )
          )
        )
      )
    ) # - Plots -
  ), # - Maps & Plots -
  
  fluidRow(
    id = "footer",
    column(
      width = 12,
      align = "center",
      br(),
      tags$p(
        style = "font-size: 12px; color: #FFFFFF",
        "Content is distributed under ",
        a(
          style = "color: #FFFFFF",
          href = "https://github.com/britishredcrosssociety/nhs-capacity/blob/main/LICENSE",
          target = "_blank",
          "these licenses."
        )
      ),
      tags$div(
        img(src = "footer.jpg", width = 1000)
      )
    )
  )
) # fluidPage

