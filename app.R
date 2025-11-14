# script loading required packages
# source(here::here('install.R'))
# renv::restore()
library(renv)
library(shiny)
library(bslib)
library(sf)
library(dplyr)
library(markdown)
library(leaflet.extras)
library(leafpop)
library(shinyWidgets)
library(plotly)
library(DT)
library(futile.logger)
sf_use_s2(FALSE)

# Log the loading of libraries
futile.logger::flog.info("All libraries loaded successfully.")
############################################################ DATA and FILTER ########################################################################################################################################################################
filepath <-"./data/dwc_melted.rds"
if(!file.exists(filepath)){
source("./data/bindDatasets.R")
}
futile.logger::flog.info("Load data")
data_dwc <- readRDS(filepath)
futile.logger::flog.info("Set default values for filters to be displayed by UI and used by server to filter and process data")
default_geom <- st_as_sfc(st_bbox(data_dwc))
default_wkt <- sf::st_as_text(default_geom)
current_geom <- reactiveVal(default_geom)
wkt <- reactiveVal(default_wkt) 
current_gbifID <- reactiveVal(1)

target_year <- unique(data_dwc$year)
# default_year <- NULL
default_year <- target_year[1]
default_year <- target_year

# default_species <- NULL
default_species <- c('Katsuwonus pelamis')
target_species <-  unique(data_dwc$scientificName)

# default_family <- NULL
target_family <- unique(data_dwc$family)
default_family <- target_family[1]

filters_combinations <- data_dwc %>% st_drop_geometry()  %>% distinct(family, scientificName) %>% arrange(family, scientificName)

################################################################ USER INTERFACE ###################################################################################################################################################

ui <- fluidPage(
  # theme = bs_theme(version = 5, bootswatch = "cerulean"),
  # titlePanel("Darwin Core viewer: map and plots"),
  tags$head(includeCSS("styles.css")),
  tags$style(HTML("
  ")),
  navbarPage(id = "navbarPageId",
             windowTitle="Data viewer for species occurences complying with Darwin Core data format",
             title = "Darwin Core Viewer",
             # title = div(img(src="logo_sardara.svg"), "Darwin Core Viewer"),
             tabPanel("Species occurences viewer",
                      modalDialog(
                        title = "Information",
                        # includeHTML("doc/ribbon_GH.html"),
                        includeMarkdown("doc/popup.md"),
                        size = "l",
                        easyClose = TRUE,
                        footer=modalButton("OK", icon =icon("check"))
                      ),
                      div(class="map",
                          # tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                          # shiny::tags$a(type ="", '<a href="https://github.com/you"><img decoding="async" width="149" height="149" src="https://github.blog/wp-content/uploads/2008/12/forkme_right_red_aa0000.png" class="attachment-full size-full" alt="Fork me on GitHub" loading="lazy"></a>'),
                          # shiny::tags$a(href="https://github.com/you", '<a ><img decoding="async" width="149" height="149" src="https://github.blog/wp-content/uploads/2008/12/forkme_right_red_aa0000.png" class="attachment-full size-full" alt="Fork me on GitHub" loading="lazy"></a>'),
                          leafletOutput("mymap", width="100%", height="100%"),
                          absolutePanel(id = "filters", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE,top = "12%",  left = "3%", width = "21%", height = "auto",
                                        # top = "12%", left = "1.5%", right="auto", width = "auto", height = "auto",
                                        tags$br(),
                                        h1("Select filters to customize the map and the plots"),
                                        tags$br(),
                                        # tags$br(),
                                        shinyWidgets::pickerInput(
                                          inputId = "year",
                                          label = "Year",
                                          choices = target_year,
                                          multiple = TRUE,
                                          selected= default_year,
                                          options = list(`actions-box` = TRUE),
                                          width = "98%"
                                        ),
                                        pickerInput(
                                          inputId = "family",
                                          label = "Family",
                                          choices = target_family,
                                          multiple = TRUE,
                                          selected= default_family,
                                          options = list(`actions-box` = TRUE),
                                          width = "98%"
                                        ),
                                        pickerInput(
                                          inputId = "species",
                                          label = "Scientific Name",
                                          choices = target_species,
                                          multiple = TRUE,
                                          selected= default_species,
                                          options = list(`actions-box` = TRUE),
                                          width = "98%"
                                        ),
                                        textInput(
                                          inputId="polygon",
                                          label="Edit WKT",
                                          value=default_wkt,
                                          width="98%"
                                          ),
                                        actionButton(
                                          inputId = "resetWkt",
                                          label = "Reset WKT",
                                          icon("sync"), 
                                          style="color: #fff; background-color: #63B061; border-color: #2e6da4;font-size: xx-large;font-weight: bold;"
                                          ),
                                        actionButton(
                                          inputId = "resetAllFilters",
                                          label = "Reset all filters",
                                          icon("sync"), 
                                          style="color: #fff; background-color: #63C5DA; border-color: #2e6da4;font-size: xx-large;font-weight: bold;"
                                          ),
                                        tags$br(),
                                        tags$br(),
                                        tags$br(),
                                        actionButton(
                                          inputId = "submit",
                                          label = "Apply current filters and display data",
                                          # icon = icon("refresh"),
                                          icon("play-circle"), 
                                          style="color: #fff; background-color: #F51D08; border-color: #2e6da4;font-size: xx-large;font-weight: bold;"
                                        ),
                                        tags$br(),
                                        tags$br()
                          ),
                          
                          absolutePanel(id = "plots", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = "12%", left = "auto", right="1%", width = "25%", height = "30%",
                                        # top = "12%", right = "1.5%", width = "auto", #fixed=TRUE,
                                        h2(paste0("Group identifier: ")),
                                        textOutput("idGroup"),
                                        # tags$br(),
                                        plotlyOutput("pie_map", height = '70%')
                                        ),
                          absolutePanel(id = "logo", class = "logo", bottom = "2%", left = "2%", width = "auto", fixed=FALSE, draggable = TRUE, height = "auto",
                                        tags$a(href='https://www.ird.fr/', tags$img(src='logo_IRD.svg',height='5%'))),
                          absolutePanel(id = "logo", class = "logo", top = "1.5%", right = "2%", width = "auto", fixed=FALSE, draggable = TRUE, height = "auto",
                                        tags$a(href='https://blue-cloud.d4science.org/', tags$img(src='logo_blue-cloud_2026.svg',height='5%')))
                          
                      )
             ),
             tabPanel("Explore current data table",
                      hr(),
                      DT::dataTableOutput("DT_within_WKT")
                      # downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
             ),
             navbarMenu("About",
                         tabPanel("About",
                                 fluidRow(
                                   column(2,
                                          markdown('
                                          [<img src="logo_VLab5.png" height="10%">](https://blue-cloud.d4science.org/group/globalfisheriesatlas)
                                                   ')
                                   ),
                                   column(6,
                                          includeMarkdown("doc/about.md"),
                                          ),
                                   column(2,
                                          markdown('
                                          [<img src="BET_YFT_SKJ_ALB.svg" height="10%">](https://blue-cloud.d4science.org/group/globalfisheriesatlas)
                                                   ')
                                   )
                                   )
                                 )
             )
  )
# )
)

################################################################ SERVER ###################################################################################################################################################

server <- function(input, output, session) {
  
  
  change <- reactive({
    unlist(strsplit(paste(input$family,collapse="|"),"|",fixed=TRUE))
  })
  
  
  observeEvent(input$family,{
    temp <- filters_combinations %>% filter(family %in% change()[])
    updateSelectInput(session,"species",choices = unique(temp$scientificName))
  }
  )
  
  
  observeEvent(input$resetWkt, {
    wkt(default_wkt)
    current_geom(default_geom)
    updateTextInput(session,"polygon", value = wkt())
  },
  ignoreInit = FALSE)
  
  
  observeEvent(input$resetAllFilters, {
    wkt(default_wkt)
    current_geom(default_geom)
    updateTextInput(session, "polygon", value = default_wkt)
    updatePickerInput(session,"year",choices = target_year$year, selected = NULL )
    updatePickerInput(session,"family",choices = target_family$family, selected = NULL )
    updatePickerInput(session,"species",choices = target_species$scientificName, selected = NULL)
  },
  ignoreInit = TRUE)
  
  
  data <- eventReactive(input$submit, {
    if(is.null(input$species)){filter_species=target_species$scientificName}else{filter_species=input$species}
    if(is.null(input$family)){filter_family=target_family$family}else{filter_family=input$family}
    if(is.null(input$year)){filter_year=target_year$year}else{filter_year=input$year}
    data_dwc %>% 
      filter(year %in% filter_year) %>%
      filter(family %in% filter_family) %>% 
      filter(scientificName %in% filter_species) %>%
      dplyr::filter(st_within(geom,st_as_sfc(input$polygon, crs = 4326), sparse = FALSE)) # %>% head(500)

        },ignoreNULL = FALSE)
  
  
  plot_df <- eventReactive(current_gbifID(), {
    sizeClassData <- strsplit(data()$sizeClasses[current_gbifID()], ",")[[1]]  %>% 
      data.frame() %>% 
        dplyr::mutate(class=gsub("=.*", "",.), count=as.numeric(gsub(".*=", "",.)))
      
      },ignoreInit = TRUE)    
    

############################################################# OUTPUTS   ############################################################# 
  output$idGroup <- renderText({
    current_gbifID()                   
  })

    output$DT_within_WKT <- renderDT({
      # data() %>%  dplyr::filter(st_within(st_as_sfc(input$polygon, crs = 4326), sparse = FALSE))  %>% st_drop_geometry()
      plot_df()
    }) 
    
    # output$WKT <- renderText({
    #   wkt()
    # }) 
    # 
    
  output$mymap <- renderLeaflet({
    
    shiny::validate(
      need(nrow(data())>0, 'Sorry no data with current filters !'),
      errorClass = "myClass"
    )
    
    
    # df <- data_dwc %>% st_centroid() filter(st_within(geometry,st_as_sfc(default_wkt, crs = 4326),sparse = FALSE)[, 1]) 
    df <- data()  %>% st_centroid()
    # current_selection <- st_sf(st_as_sfc(wkt(), crs = 4326))
    ongoing_geom <- current_geom()
    # all_points <- df %>% st_buffer(2000) %>% st_combine() #%>% st_convex_hull(
    combine_geom <- df %>% st_union()
    # plot(combine_geom)
    convex_hull <- combine_geom %>% st_convex_hull()
    all_points <- combine_geom %>% st_buffer(1)
    # plot(all_points)
    
    
    sizeClassData <- strsplit(df$sizeClasses[1], ",")[[1]] 
    
    testdd <-as.data.frame(sizeClassData)  %>% 
      dplyr::mutate(class=gsub("=.*", "",sizeClassData), count=as.numeric(gsub(".*=", "",sizeClassData)))
    fig <- plot_ly(testdd, x = ~class, y = ~count, type = 'bar', name = 'Number of lines')
    p2 <- fig %>% layout(yaxis = list(title = 'Count'))
    p2<-ggplot(data=testdd, aes(x=class, y=count)) +
      geom_bar(stat="identity")
    
    mymap <-leaflet::leaflet(data=df,options = leafletOptions(minZoom = 1, maxZoom = 40)) %>% 
      # clearPopups()  %>% 
      # clearShapes()   %>% 
      # https://leaflet-extras.github.io/leaflet-providers/preview/ 
      addProviderTiles("Esri.OceanBasemap", group = "bathymetry") %>%
      addProviderTiles("Esri.WorldImagery", group = "satellite") %>% 
      # addPolygons(data = ongoing_geom,color="red",fillColor = "transparent", group="current_selection") %>%
      addPolygons(data = convex_hull, color="blue",fillColor = "transparent", group="all_points") %>%
      addPolygons(data = all_points, color="blue",fillColor = "transparent", group="all_points") %>%
      clearBounds() %>%
      # addMarkers(~as_tibble(st_coordinates(geometry))$X,~as_tibble(st_coordinates(geometry))$Y,
      addMarkers(group = "pt",lat = df$decimalLatitude, lng= df$decimalLongitude,
                 #popup = ~as.character(sizeClasses),
                 #label = ~as.character(scientificName),
                 layerId = ~gbifID,
                 clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F)
                 ) %>% 
      # addPopupGraphs(list(p2), group = "pt", width = 300, height = 400)  %>% 
        addDrawToolbar(
        targetGroup = "draw",
        polylineOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(
          selectedPathOptions = selectedPathOptions()
        )
      ) %>%
      addLayersControl(
        position = "topleft",
        baseGroups = c("bathymetry","satellite"),
        overlayGroups = c("draw","current_selection","all_points"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%  
      addMiniMap(zoomLevelFixed = 1) %>%
      addScaleBar(
        position = "topright",
        options = scaleBarOptions(
          maxWidth = 10,
          metric = TRUE,
          imperial = FALSE
        )
      )
    # mymap

  })
  
  # observeEvent(input$mymap_marker_click, { 
  #   click <- input$map_marker_click
  #   if(is.null(click))
  #     return()
  #   print(p)
  # 
  #   # popup <- popupImage(phdata)
  #   # leafletProxy("map") %>% 
  #   #   addMarkers( lng=-81, lat=37,popup=popup)
  #   # 
  #   # output$click_text <- renderText({
  #   #   sizeClassData <- strsplit(data()$sizeClasses[1], ",")[[1]] 
  #   #   
  #   #   testdd <-as.data.frame(sizeClassData)  %>% 
  #   #     dplyr::mutate(class=gsub("=.*", "",sizeClassData), count=as.numeric(gsub(".*=", "",sizeClassData)))
  #   #   
  #   #   
  #   #   fig <- plot_ly(testdd, x = ~class, y = ~count, type = 'bar', name = 'Number of lines')
  #   #   fig <- fig %>% layout(yaxis = list(title = 'Count'))
  #   #   
  #   #   fig
  #   # })
  #   
  # })
  

  observe({
    click <- input$mymap_marker_click
    if (is.null(click))
      return()

    print(click)
    text <-
      paste("Lattitude ",
            click$lat,
            "Longtitude ",
            click$lng)
    current_gbifID(input$mymap_marker_click$id)

    sizeClassData <- strsplit(data()$sizeClasses[input$mymap_marker_click$id], ",")[[1]]
    # testdd <-as.data.frame(sizeClassData)  %>%
    #   dplyr::mutate(class=gsub("=.*", "",sizeClassData), count=as.numeric(gsub(".*=", "",sizeClassData)))

    # wkt(coord)
    # updateTextInput(session,"polygon", value = coord)
    testdd <-as.data.frame(sizeClassData)  %>% 
      dplyr::mutate(class=gsub("=.*", "",sizeClassData), count=as.numeric(gsub(".*=", "",sizeClassData)))
    fig <- plot_ly(testdd, x = ~class, y = ~count, type = 'bar', name = 'Number of lines')
    p2 <- fig %>% layout(yaxis = list(title = 'Count'))
    p2<-ggplot(data=testdd, aes(x=class, y=count)) +
      geom_bar(stat="identity")
    
    leafletProxy(mapId = "mymap") %>%
      clearPopups() %>%
      addPopupGraphs(list(p2), group = "pt", width = 300, height = 400)
      # addPopups(dat = click, lat = ~lat, lng = ~lng, popup = input$mymap_marker_click$layerId)

    # map$clearPopups()
    # map$showPopup(click$latitude, click$longtitude, text)
  })

  observeEvent(input$mymap_draw_stop,{
    # observe({
      #use the draw_stop event to detect when users finished drawing
    # req(input$mymap_draw_new_feature)
    req(input$mymap_draw_stop)
    feature <- input$mymap_draw_new_feature
    print(feature)
    polygon_coordinates <- input$mymap_draw_new_feature$geometry$coordinates[[1]]
    # see  https://rstudio.github.io/leaflet/shiny.html
    bb <- input$mymap_bounds 
    geom_polygon <- input$mymap_draw_new_feature$geometry
    # drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
    geoJson <- geojsonio::as.json(feature)
    # spdf <- geojsonio::geojson_sp(feature)
    geom <- st_read(geoJson)
    coord <- st_as_text(st_geometry(geom[1,]))
    wkt(coord)
    updateTextInput(session,"polygon", value = coord)
    new_geom <- st_sf(st_as_sfc(coord, crs = 4326))
    current_geom(new_geom)
    
    north <- polygon_coordinates[[1]][[1]]
    south <- polygon_coordinates[[2]][[1]]
    east <- polygon_coordinates[[1]][[2]]
    west <- polygon_coordinates[[2]][[2]]
    
    
    if(is.null(polygon_coordinates))
      return()
    text<-paste("North ", north, "South ", east)
    
    },ignoreInit = FALSE)

  output$pie_map <- renderPlotly({
    
    shiny::validate(
      need(nrow(plot_df())>0, 'Sorry no data with current filters !'),
      errorClass = "myClass"
    )
    # gbifId=1
    # sizeClassData <- strsplit(data()$sizeClasses[gbifId], ",")[[1]] 
    
    testdd <-plot_df()
    
    fig <- plot_ly(testdd, x = ~class, y = ~count, type = 'bar', name = 'Number of lines')
    fig <- fig %>% layout(yaxis = list(title = 'Count'))
    
    fig
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
