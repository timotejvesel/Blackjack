
# LoadToEnvironment <- function(RData, env=new.env()) {
#   load(RData, env)
#   return(env)
# }


rdata <- function(datoteka) {
  return(reactiveFileReader(10000, session = NULL, filePath = datoteka, load,envir = .GlobalEnv))
}


function(input, output, session) {
  
  ############## tabele opt. strategij
  output$hit_stand_h <- renderTable({
    rdata1 <- rdata("tabele-strategij/hit-stand-hard.Rda")
    rdata1()
    hit.stand
    
  }, rownames = TRUE)
  
  output$hit_stand_s <- renderTable({
    rdata2 <- rdata("tabele-strategij/hit-stand-soft.Rda")
    rdata2()
    soft1
    
  }, rownames = TRUE)
  
  output$double_h <- renderTable({
    rdata3 <- rdata("tabele-strategij/double-hard.Rda")
    rdata3()
    double
    
  }, rownames = TRUE)
  
  output$double_s <- renderTable({
    rdata4 <- rdata("tabele-strategij/double-soft.Rda")
    rdata4()
    double_soft
    
  }, rownames = TRUE)
  
  ############ house edge, hit&stand, slaba
  he_hs_slaba <- eventReactive(input$gumb_hs_slaba,{
    if(is.null(input$gumb_hs_slaba)){
      return()
    }
    
    #sprotno izpisovanje
    progress <- shiny::Progress$new()
    progress$set(message = "", value = 0)
    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL, delez) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / delez
      }
      progress$set(value = value, detail = detail)
    }
    
    if(input$natural1 == "3:2") {
      he <- suppressWarnings(he.bs(1, as.numeric(input$iter1),  
                                      as.numeric(input$paketi1), 1.5, "slaba", updateProgress, as.numeric(input$meja))[[1]]) 
    }
    else if (input$natural1 == "6:5") {
      he <- suppressWarnings(he.bs(1, as.numeric(input$iter1), 
                                      as.numeric(input$paketi1), 1.2, "slaba", updateProgress, as.numeric(input$meja))[[1]]) 
    }
    else {
      he <- suppressWarnings(he.bs(1, as.numeric(input$iter1),
                                      as.numeric(input$paketi1), 1, "slaba", updateProgress, as.numeric(input$meja))[[1]]) 
    }
    he <- round(he * 100, 2)
  })
    
  output$hs_slaba <- renderText({
    print(paste("House edge je",he_hs_slaba(),"%.")) #naredi lepši izpis, večje črke
  })
  
  ############# house edge, optimalna
  he_hs_opt <- eventReactive(input$gumb_hs_opt,{
    if(is.null(input$gumb_hs_opt)){
      return()
    }
    
    #sprotno izpisovanje
    progress <- shiny::Progress$new()
    progress$set(message = "", value = 0)
    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL, delez) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / delez
      }
      progress$set(value = value, detail = detail)
    }
    
    if (input$double2) {
      if(input$natural2 == "3:2") {
        he <- suppressWarnings(he.bs(1, as.numeric(input$iter2), as.numeric(input$paketi2), 1.5, "double", updateProgress)[[1]]) 
      }
      else if (input$natural2 == "6:5") {
        he <- suppressWarnings(he.bs(1, as.numeric(input$iter2), as.numeric(input$paketi2), 1.2, "double", updateProgress)[[1]]) 
      }
      else {
        he <- suppressWarnings(he.bs(1, as.numeric(input$iter2), as.numeric(input$paketi2), 1, "double", updateProgress)[[1]]) 
      }
      he <- round(he * 100, 2)
    }
    else { #samo hit&stand
      if(input$natural2 == "3:2") {
        he <- suppressWarnings(he.bs(1, as.numeric(input$iter2), as.numeric(input$paketi2), 1.5, "hit_stand", updateProgress)) 
        he <- he[[1]]
      }
      else if (input$natural2 == "6:5") {
        he <- suppressWarnings(he.bs(1, as.numeric(input$iter2), as.numeric(input$paketi2), 1.2, "hit_stand", updateProgress)[[1]]) 
      }
      else {
        he <- suppressWarnings(he.bs(1, as.numeric(input$iter2), as.numeric(input$paketi2), 1, "hit_stand", updateProgress)[[1]]) 
      }
      he <- round(he * 100, 2)
    }

  })
  
  output$hs_opt <- renderText({
    print(paste("House edge je",he_hs_opt(),"%.")) 
  })
  
  ########## house edge, stetje
  he_hs_count <- eventReactive(input$gumb_hs_count,{
    if(is.null(input$gumb_hs_count)){
      return()
    }
    
    #sprotno izpisovanje
    progress <- shiny::Progress$new()
    progress$set(message = "", value = 0)
    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL, delez) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / delez
      }
      progress$set(value = value, detail = detail)
    }
    
    
    if (input$double3) { #double
      if(input$natural3 == "3:2") {
        if (input$stetje == "Hi-Lo") {
          he <- suppressWarnings(counting(1, as.numeric(input$paketi3), as.numeric(input$iter3),
                                                 1.5, hi_lo, "double", updateProgress)[[1]]) 
        }
        else {
          he <- suppressWarnings(counting(1, as.numeric(input$paketi3), as.numeric(input$iter3),
                                                 1.5, hi_opt2, "double", updateProgress)[[1]])
        }
      }
      else if (input$natural3 == "6:5") {
        if (input$stetje == "Hi-Lo") {
          he <- suppressWarnings(counting(1, as.numeric(input$paketi3), as.numeric(input$iter3),
                                                 1.2, hi_lo, "double", updateProgress)[[1]]) 
        }
        else {
          he <- suppressWarnings(counting(1, as.numeric(input$paketi3), as.numeric(input$iter3)
                                                 , 1.2, hi_opt2, "double", updateProgress)[[1]])
        }
      }
      else {
        if (input$stetje == "Hi-Lo") {
          he <- suppressWarnings(counting(1, as.numeric(input$paketi3), as.numeric(input$iter3),
                                                 1, hi_lo, "double", updateProgress)[[1]]) 
        }
        else {
          he <- suppressWarnings(counting(1, as.numeric(input$paketi3), as.numeric(input$iter3),
                                                 1, hi_opt2, "double", updateProgress)[[1]])
        }
      }
      he <- round(he * 100, 2)
    }
    
    else {
      if(input$natural3 == "3:2") {
        if (input$stetje == "Hi-Lo") {
          he <- suppressWarnings(counting(1, as.numeric(input$paketi3), as.numeric(input$iter3),
                                             1.5, hi_lo, "hit_stand", updateProgress)[[1]]) 
        }
        else {
          he <- suppressWarnings(counting(1, as.numeric(input$paketi3), as.numeric(input$iter3),
                                             1.5, hi_opt2, "hit_stand", updateProgress)[[1]])
        }
      }
      else if (input$natural3 == "6:5") {
        if (input$stetje == "Hi-Lo") {
          he <- suppressWarnings(counting(1, as.numeric(input$paketi3), as.numeric(input$iter3),
                                             1.2, hi_lo, "hit_stand", updateProgress)[[1]]) 
        }
        else {
          he <- suppressWarnings(counting(1, as.numeric(input$paketi3), as.numeric(input$iter3),
                                             1.2, hi_opt2, "hit_stand", updateProgress)[[1]])
        } 
      }
      else {
        if (input$stetje == "Hi-Lo") {
          he <- suppressWarnings(counting(1, as.numeric(input$paketi3), as.numeric(input$iter3),
                                             1, hi_lo, "hit_stand", updateProgress)[[1]]) 
        }
        else {
          he <- suppressWarnings(counting(1, as.numeric(input$paketi3), as.numeric(input$iter3),
                                             1, hi_opt2, "hit_stand", updateProgress)[[1]])
        } 
      }
      he <- round(he * 100, 2)
    }

  })
  
  output$hs_stetje <- renderText({
    print(paste("House edge je", he_hs_count(),"%."))
  })
  
  ######## Zasluzek/izguba
  narisi_zasluzek <- eventReactive(input$gumb_graf,{
    if(is.null(input$gumb_graf)){
      return()
    }
    
    if (input$tip == "Double") {
      if (input$stetje2 == "Hi-Lo") {
        graf <- suppressWarnings(narisi.graf.d(1, as.numeric(input$paketi4), 
                                               as.numeric(input$iter4), as.numeric(input$natural4), hi_lo))
      }
      else {
        graf <- suppressWarnings(narisi.graf.d(1, as.numeric(input$paketi4), 
                                               as.numeric(input$iter4), as.numeric(input$natural4), hi_opt2))
      }
    }
    else {
      if (input$stetje2 == "Hi-Lo") {
        graf <- suppressWarnings(narisi.graf.hs(1, as.numeric(input$paketi4),
                                                as.numeric(input$iter4), as.numeric(input$natural4), hi_lo, input$meja1))
      }
      else {
        graf <- suppressWarnings(narisi.graf.hs(1, as.numeric(input$paketi4),
                                               as.numeric(input$iter4), as.numeric(input$natural4), hi_opt2, input$meja1))
      }
    }
  })
  
  output$graf_zasluzek <- renderPlotly({
    print(narisi_zasluzek())
  })

}