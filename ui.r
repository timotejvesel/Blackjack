library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(shinycssloaders)
library(shinyWidgets)


source("house-edge/he-slaba.R")
source("house-edge/he-hit-stand.r")
source("house-edge/he-hit-stand-counting.r")
source("house-edge/he-double.r")
source("house-edge/he-double-counting.r")

dashboardPage(
  dashboardHeader(title = "Blackjack"),
  
  dashboardSidebar(
    div(class = "inlay", style = "height:15px;width:100%;background-color: black;"),
  sidebarMenu(
    menuItem("Domov", tabName = "domov", icon = icon("home")),
    menuItem("Tabele osnovnih strategij", tabName = "tabele", icon = icon("table"), 
             menuSubItem('Hit & stand', tabName = 'hit_stand'),
             menuSubItem('Double', tabName = 'double')
    ),
    menuItem("House edge", tabName = "he")
    )
  ),
  # BODY ----------------------------------------------------------------- 
  dashboardBody(
    includeCSS("www/styles.css"),
    tabItems(
      
      tabItem(tabName = "domov",
              h1("O igri Blackjack"),
              includeMarkdown("markdown/blackjack.md") #naredi nov markdown, samo z opisom igre
      ),
      
      tabItem(tabName = "hit_stand",
              h2("Tabele osnovnih (optimalnih) strategij za osnovno različico"),
              fluidRow(
                tabBox(width = 8,
                       tabPanel("Hard hand", tableOutput("hit_stand_h")),
                       tabPanel("Soft hand", tableOutput("hit_stand_s"))
            )

        ),
             fluidRow(
                box(width = 8, includeMarkdown("markdown/tabele.md"))
        )),
        tabItem(tabName = "double",
                h2("Tabele osnovnih (optimalnih) strategij za različico z double"),
                fluidRow(
                  tabBox(width = 8,
                         tabPanel("Hard hand", tableOutput("double_h")),
                         tabPanel("Soft hand", tableOutput("double_s"))
                  ),
                  box(width = 8, includeMarkdown("markdown/tabele.md"))
                  )
              ),
      # HOUSE EDGE
        tabItem(tabName = "he",
                
                h2("Izračuna house edga"),
                fluidRow(
                  tabBox(width = 10,
                         tabPanel("'Slaba' strategija", selectInput('meja','Izberi mejo', c(11:21), selected = 16),
                                  selectInput('natural1','Izberi koliko izplača natural 21', c("3:2","6:5","1:1"), selected = "3:2"),
                                  selectInput('paketi1','Stevilo paketov kart', c(4,6,8), selected = 8),
                                  selectInput('iter1','Stevilo iteracij', c(1e4, 1e5, 1e6), selected = 1e5),
                                  actionButton("gumb_hs_slaba", "Izracunaj HE"),
                                  textOutput("hs_slaba") %>% withSpinner(color="#0dc5c1")
                                  ),
                         tabPanel("'Optimalna' strategija", 
                                  prettySwitch("double2", "Double", value = FALSE, bigger = TRUE),
                                  selectInput('natural2','Izberi koliko izplača natural 21', c("3:2","6:5","1:1"), selected = "3:2"),
                                  selectInput('paketi2','Stevilo paketov kart', c(4,6,8), selected = 8),
                                  selectInput('iter2','Stevilo iteracij', c(1e4, 1e5, 1e6), selected = 1e5),
                                  actionButton("gumb_hs_opt", "Izracunaj HE"),
                                  textOutput("hs_opt") %>% withSpinner(color="#cc0066")
                                  ),
                         tabPanel("Štetje kart", 
                                  prettyCheckbox("double3", "Double", value = FALSE, bigger = TRUE, animation = "pulse"),
                                  selectInput('natural3','Izberi koliko izplača natural 21', c("3:2","6:5","1:1"), selected = "3:2"),
                                  selectInput('paketi3','Stevilo paketov kart', c(4,6,8), selected = 8),
                                  selectInput('stetje','Nacin stetja kart', c("Hi-Lo","Hi-Opt II"), selected = "Hi-Lo"),
                                  selectInput('iter3','Stevilo iteracij', c(1e4, 1e5, 1e6), selected = 1e5),
                                  actionButton("gumb_hs_count", "Izracunaj HE"),
                                  textOutput("hs_stetje") %>% withSpinner(color="#6600cc")
                                  )
                           
                ),
                fluidRow(
                  box(width = 10, includeMarkdown("markdown/house-edge.md"))
                  )
            )
          )
        
      )
      
       
    )
)

