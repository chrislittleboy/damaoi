getshinyparams <- function(res, streams = NULL){
  
  # -----------------------------
  # UI
  # -----------------------------
  
  ui <- fluidPage(
    
    tags$head(
      tags$style(HTML("
        body { font-size: 20px; }
        .btn { font-size: 18px; padding: 10px 18px; }
        .control-label { font-size: 20px; font-weight: bold; }
      "))
    ),
    
    titlePanel("Reservoir Pour Point Selection"),
    
    fluidRow(
      
      column(
        width = 8,
        
        radioButtons(
          "mode",
          "Click mode",
          choices = c(
            "🔵 Add pour-in point" = "in",
            "🔴 Set pour-out point" = "out"
          ),
          selected = "in",
          inline = TRUE
        ),
        
        leafletOutput("map", height = "75vh"),
        
        br(),
        
        actionButton("undo", "Undo Last"),
        actionButton("reset", "Reset All"),
        actionButton("done", "Done")
      ),
      
      column(
        width = 4,
        valueBoxOutput("simple_box")
      )
    )
  )
  bb <- as.numeric(sf::st_bbox(res))
  # -----------------------------
  # SERVER
  # -----------------------------
  
  server <- function(input, output, session){
    
    rv <- reactiveValues(
      points = data.frame(
        id = integer(),
        lon = numeric(),
        lat = numeric(),
        type = character(),
        stringsAsFactors = FALSE
      ),
      history = list()
    )
    
    # -----------------------------
    # INITIAL MAP
    # -----------------------------
    
    output$map <- renderLeaflet({
      
      m <- leaflet(res) %>%
        addProviderTiles(providers$OpenTopoMap) %>%
        addPolygons(
          color = "red",
          weight = 2,
          fillOpacity = 0.15,
          group = "reservoir"
        )
      
      # -----------------------------
      # STREAM LAYER (SAFE VERSION)
      # -----------------------------
      
      if (!is.null(streams)) {
        
        m <- m %>%
          addRasterImage(
            streams,
            opacity = 0.6
          )
      }
      
      m %>%
        fitBounds(
          lng1 = bb[1],
          lat1 = bb[2],
          lng2 = bb[3],
          lat2 = bb[4]
        )
    })
    
    # -----------------------------
    # REDRAW MARKERS
    # -----------------------------
    
    redraw_map <- function(){
      
      proxy <- leafletProxy("map") %>%
        clearGroup("in") %>%
        clearGroup("out")
      
      in_pts <- rv$points[rv$points$type == "in", , drop = FALSE]
      out_pts <- rv$points[rv$points$type == "out", , drop = FALSE]
      
      if (nrow(in_pts) > 0) {
        
        proxy <- proxy %>%
          addCircleMarkers(
            data = in_pts,
            lng = ~lon,
            lat = ~lat,
            color = "blue",
            radius = 7,
            group = "in",
            label = ~paste0("Inflow ", id)
          )
      }
      
      if (nrow(out_pts) > 0) {
        
        proxy <- proxy %>%
          addCircleMarkers(
            data = out_pts,
            lng = ~lon,
            lat = ~lat,
            color = "red",
            radius = 9,
            group = "out",
            label = ~paste0("Outlet ", id)
          )
      }
    }
    
    # -----------------------------
    # CLICK HANDLER
    # -----------------------------
    
    observeEvent(input$map_click, {
      
      click <- input$map_click
      req(click)
      
      new_id <- if (nrow(rv$points) == 0) 1L else max(rv$points$id) + 1L
      
      # enforce single outlet
      if (input$mode == "out") {
        rv$points <- rv$points[rv$points$type != "out", , drop = FALSE]
      }
      
      rv$points <- rbind(
        rv$points,
        data.frame(
          id = new_id,
          lon = click$lng,
          lat = click$lat,
          type = input$mode,
          stringsAsFactors = FALSE
        )
      )
      
      rv$history[[length(rv$history) + 1]] <- new_id
      
      redraw_map()
    })
    
    # -----------------------------
    # UNDO
    # -----------------------------
    
    observeEvent(input$undo, {
      
      if (length(rv$history) == 0) return()
      
      last_id <- tail(rv$history, 1)[[1]]
      rv$history <- head(rv$history, -1)
      
      rv$points <- rv$points[rv$points$id != last_id, , drop = FALSE]
      
      redraw_map()
    })
    
    # -----------------------------
    # RESET
    # -----------------------------
    
    observeEvent(input$reset, {
      
      rv$points <- rv$points[0, ]
      rv$history <- list()
      
      redraw_map()
    })
    
    # -----------------------------
    # SUMMARY BOX
    # -----------------------------
    
    output$simple_box <- renderValueBox({
      
      n_in <- sum(rv$points$type == "in")
      n_out <- sum(rv$points$type == "out")
      
      valueBox(
        value = paste0(
          "Pour-ins: ", n_in,
          "\nPour-outs: ", n_out
        ),
        subtitle = if (n_in == 0 || n_out == 0)
          "Select at least one inflow and one outflow point."
        else
          "Selection complete. Click Done.",
        color = if (n_in == 0 || n_out == 0) "yellow" else "green"
      )
    })
    
    # -----------------------------
    # DONE
    # -----------------------------
    
    observeEvent(input$done, {
      
      validate(
        need(any(rv$points$type == "in"), "Need at least one inflow point"),
        need(any(rv$points$type == "out"), "Need one outflow point")
      )
      
      stopApp(
        sf::st_as_sf(
          rv$points,
          coords = c("lon", "lat"),
          crs = 4326
        )
      )
    })
    
  }
  
  list(ui = ui, server = server)
}
