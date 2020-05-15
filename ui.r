library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(shinycssloaders)
library(shinyWidgets)
library(shinyBS)

source("funkcije.r")
source("graf.r")


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
    menuItem("House edge", tabName = "he", icon = tags$i(class="fas fa-dollar-sign")),
    menuItem("Zasluzek/izguba igralca", tabName = "zasluzek", icon = tags$i(class="fas fa-chart-line"))
    )
  ),
  # BODY ----------------------------------------------------------------- 
  dashboardBody(
    includeCSS("www/styles.css"),
    tabItems(
      ### Domov
      tabItem(tabName = "domov",
              h2("O igri Blackjack"),
              includeMarkdown("markdown/blackjack.md")
      ),
      ### Tabele opt. strategij
      tabItem(tabName = "hit_stand",
              h2("Tabele osnovnih (optimalnih) strategij za osnovno različico"),
              fluidRow(
                tabBox(width = 5,
                       tabPanel("Hard hand", tableOutput("hit_stand_h")),
                       tabPanel("Soft hand", tableOutput("hit_stand_s"))),


                box(width = 5, includeMarkdown("markdown/tabele.md")))
              ),
        tabItem(tabName = "double",
                h2("Tabele osnovnih (optimalnih) strategij za različico z double"),
                fluidRow(
                  tabBox(width = 6,
                         tabPanel("Hard hand", tableOutput("double_h")),
                         tabPanel("Soft hand", tableOutput("double_s"))
                  ),
                  box(width = 6, includeMarkdown("markdown/tabele.md"))
                  )
              ),
        ### House edge
        tabItem(tabName = "he",
                h2("Izračun house edga"),
                fluidRow(
                  tabBox(width = 5,
                         tabPanel("'Slaba' strategija", selectInput('meja','Izberi mejo', c(11:21), selected = 16),
                                  selectInput('natural1','Izberi koliko izplača natural 21', c("3:2","6:5","1:1"), selected = "3:2"),
                                  selectInput('paketi1','Stevilo paketov kart', c(4,6,8), selected = 8),
                                  selectInput('iter1','Stevilo iteracij', c(1e4, 1e5, 1e6), selected = 1e5),
                                  bsButton("gumb_hs_slaba", 
                                           label = "Izracunaj HE", 
                                           icon = tags$i(class="fas fa-dollar-sign"),
                                           style = "danger",
                                           type = "action"),
                                  textOutput("hs_slaba") %>% withSpinner(color="#0dc5c1")
                                  ),
                         tabPanel("'Optimalna' strategija", 
                                  prettySwitch("double2", "Double", value = FALSE, bigger = TRUE),
                                  selectInput('natural2','Izberi koliko izplača natural 21', c("3:2","6:5","1:1"), selected = "3:2"),
                                  selectInput('paketi2','Stevilo paketov kart', c(4,6,8), selected = 8),
                                  selectInput('iter2','Stevilo iteracij', c(1e4, 1e5, 1e6), selected = 1e5),
                                  bsButton("gumb_hs_opt", 
                                           label = "Izracunaj HE", 
                                           icon = tags$i(class="fas fa-dollar-sign"),
                                           style = "danger",
                                           type = "action"),
                                  textOutput("hs_opt") %>% withSpinner(color="#cc0066")
                                  ),
                         tabPanel("Štetje kart", 
                                  prettyCheckbox("double3", "Double", value = FALSE, bigger = TRUE, animation = "pulse"),
                                  selectInput('natural3','Izberi koliko izplača natural 21', c("3:2","6:5","1:1"), selected = "3:2"),
                                  selectInput('paketi3','Stevilo paketov kart', c(4,6,8), selected = 8),
                                  selectInput('stetje','Nacin stetja kart', c("Hi-Lo","Hi-Opt II"), selected = "Hi-Lo"),
                                  selectInput('iter3','Stevilo iteracij', c(1e4, 1e5, 1e6), selected = 1e5),
                                  bsButton("gumb_hs_count", 
                                           label = "Izracunaj HE", 
                                           icon = tags$i(class="fas fa-dollar-sign"),
                                           style = "danger",
                                           type = "action"),
                                  textOutput("hs_stetje") %>% withSpinner(color="#6600cc")
                                  )
                           
                ),
                  box(width = 7, includeMarkdown("markdown/house-edge.md"))
            )
          ),
      
      #### zasluzek igralca
      tabItem(tabName = "zasluzek",
              
              h2("Grafični prikaz zaslužka oz. izgube igralca"),
              fluidRow(
                tabBox(width = 8,
                       tabPanel("Graf", plotlyOutput("graf_zasluzek")),
                       tabPanel("Opombe", includeMarkdown("markdown/graf.md"))
                )
              ),
              fluidRow(
                box(collapsible = TRUE, 
                    title = "Izbira parametrov igre", width = 3, status = "primary", solidHeader = FALSE,
                    selectInput("tip", "Izberi tip igre", c("Hit & Stand", "Double"), selected = "Double"),
                    conditionalPanel(
                      condition = "input.tip == 'Hit & Stand'",
                      selectInput( "meja1", "Izberi vrednost roke do katere igralec jemlje karte:", c(11:21), selected = 16)
                    ),
                    selectInput('paketi4','Izberi število paketov kart:', c(4,6,8), selected = 6),
                    selectInput("natural4", "Izberi koliko izplača natural 21:", c(1,1.2,1.5), selected = 1.5),
                    sliderInput("iter4", "Izberi stevilo iger:", 1000, min = 10, max = 200000)
                ),
                box(collapsible = TRUE, 
                      title = "Izbira strategije štetja", width = 3, status = "primary", solidHeader = FALSE,
                      selectInput('stetje2','Izberi način štetja kart:', c("Hi-Lo", "Hi-Opt II"), selected = "Hi-Opt II")
                    ),
                bsButton("gumb_graf", 
                         label = "Narisi graf", 
                         icon = icon("chart-line"),
                         style = "danger",
                         type = "action")
        
          )
      
      )
    )
  )
)

