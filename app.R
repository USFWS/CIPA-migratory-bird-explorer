#########################################################

# Interactive application for the BOEM-funded project:

# Assessment and minimization of avian collision and
# displacement risk associated with energy infrastructure 
# in the Cook Inlet Planning Area, Alaska

#########################################################

####
##
# Clear environment, load packages, set working directory

library(fst); library(shiny); library(bslib); library(leaflet); library(shinyWidgets)
library(dplyr); library(viridisLite); library(shinybusy); library(shinyFeedback); library(leafgl)
library(shinyjs); library(adehabitatHR); library(sf); library(raster); library(DT)
library(tidyr); library(terra); library(plotly); library(ggplot2); library(markdown)
# library(); library(); library(); library(); library()
# library(); library(); library(); library(); library()

# rm(list=ls())
# z<-c("fst","shiny","bslib","leaflet","shinyWidgets","dplyr","viridisLite","shinybusy",
#      "shinyFeedback","leafgl","shinyjs","adehabitatHR","sf","raster","DT","tidyr",
#      "terra","plotly","ggplot2")
# lapply(z, library, character.only=TRUE)


####
##
# DATA --------------------------------------------------------------------
####
##
#

files <- list.files("Processed data objects/fast.tracks") # vector of file names
list.tracks <- setNames(vector("list", length(files)), # empty list of file names
                        c("Brant","Tundra Swan","King Eider", # reorder
                          "Black Scoter","Long-tailed Duck",
                          "Common Merganser","Hudsonian Whimbrel",
                          "Marbled Godwit","Tufted Puffin",
                          "Kittlitz's Murrelet","Marbled Murrelet",
                          "Common Murre","American Herring Gull",
                          "Glaucous-winged Gull","Cook Inlet Gull",
                          "Iceland Gull","Red-throated Loon",
                          "Yellow-billed Loon","Northern Fulmar"))
for (i in 1:length(files)){
  list.tracks[[i]] <- read.fst(paste("Processed data objects/fast.tracks/",paste0("",files[i],""),sep=""))} # tracks: dataframe
load("Processed data objects/list.shp.rda") # Spatial domains: shapefiles
load("Processed data objects/list.rst.rda") # UD grid: rasters
load("Processed data objects/list.track.plots.rda") # tracks: migratory timing: dataframe


# END: DATA ---------------------------------------------------------------------



####
##
# USER INTERFACE ----------------------------------------------------------
####
##
#

ui <- page_navbar(
  useShinyjs(), # this function causes the warning: "navigation containers expect..."
  title = tags$div(tags$span(style = "font-size: 18px; font-weight: bold;",
                             "Migratory Birds in the Cook Inlet Planning Area, Alaska"),
                   tags$br(),
                   tags$small(style = "font-size: 12px; font-style: italic;",
                              "US Fish & Wildlife Service, Division of Migratory Bird Management (Alaska Region)")),
  id = "page",
  navbar_options = navbar_options(position = "static-top", bg = "#703F2A", theme = "dark"),
  # Tab: Tracking data -------------------------------------------------------
  nav_panel(title = "Tracking data",
            useShinyFeedback(), # "
            add_busy_spinner(spin = "double-bounce",
                             position = "top-right",
                             color = "ivory",
                             timeout = 1000,
                             height = "100px",
                             width = "100px",
                             margins = c(10,10)),
            navset_card_tab(title = "",
                            # UI: Use areas -----------------------------------------------------------
                            nav_panel("Use areas",
                                      fluidRow(
                                        column(4, class = "no-gutter", wellPanel( #gray border
                                          pickerInput(inputId = "Species",
                                                        label = HTML("<b>Species:</b>"),
                                                        # choices = names(list.tracks),
                                                        # selected = character(0),
                                                        choices = c("none selected", names(list.tracks)),
                                                        selected = "none selected",
                                                        multiple = FALSE, # allow multiple selections
                                                        width = "100%",
                                                        option = pickerOptions(actionsBox = TRUE,
                                                                              selectedTextFormat = "count >1",
                                                                              size = 5,
                                                                              liveSearch = TRUE,
                                                                              noneSelectedText = "none selected")),
                                            pickerInput(inputId = "Month", 
                                                        label = HTML("<b>Months:</b>"),
                                                        choices = c("January","February","March","April",
                                                                    "May","June","July","August","September",
                                                                    "October","November","December"),
                                                        selected = c("January","February","March","April",
                                                                     "May","June","July","August","September",
                                                                     "October","November","December"),
                                                        multiple = TRUE, # allow multiple selections
                                                        width = "100%",
                                                        option = pickerOptions(actionsBox = TRUE,
                                                                               selectedTextFormat = "count >1",
                                                                               size = 5,
                                                                               liveSearch = FALSE,
                                                                               noneSelectedText = "none selected")),
                                            pickerInput(inputId = "individual.id", 
                                                        label = HTML("<b>Individuals:</b>"),
                                                        choices = character(0),
                                                        selected = character(0),
                                                        multiple = TRUE, # allow multiple selections
                                                        width = "100%",
                                                        option = pickerOptions(actionsBox = TRUE,
                                                                               selectedTextFormat = "count >1",
                                                                               size = 5,
                                                                               liveSearch = TRUE,
                                                                               noneSelectedText = "none selected")),
                                            pickerInput(inputId = "Year",
                                                        label = HTML("<b>Years:</b>"),
                                                        choices = character(0),
                                                        selected = character(0),
                                                        multiple = TRUE,
                                                        width = "100%",
                                                        option = pickerOptions(actionsBox = TRUE,
                                                                               selectedTextFormat = "count >1",
                                                                               size = 5,
                                                                               liveSearch = TRUE,
                                                                               noneSelectedText = "none selected")),
                                            actionButton(inputId = "update.map", icon=icon("refresh",lib="glyphicon"), "Update map", width='100%'),
                                            radioButtons(inputId = "UD",
                                                           label = HTML('<span style="color:#595959;">
                                                                            Click on a point to reveal its track.
                                                                            <span style="color:#black;">
                                                                            <span><br><br><b>Kernel density estimate (KDE):</b></span>'),
                                                           choices = c("Entire analysis area" = "kud.map.disc",
                                                                       "Offshore area" = "kud.off.map.disc"),
                                                           selected = character(0),
                                                           width = "100%"),
                                            radioButtons(inputId = "Spatial_layers",
                                                         label = HTML('<b>Spatial domains:</b>'),
                                                         choices = c(
                                                           "Cook Inlet Planning Area" = "area.CIP.map",
                                                           "Lease Sale Area" = "area.LSA.map",
                                                           "Analysis area" = "area.GCI.map"),
                                                         selected = c("Cook Inlet Planning Area"),
                                                         width = "100%"),
                                            HTML("<b>Attributes of the KDE:</b>"),
                                            verbatimTextOutput("Table_UD")
                                            )),
                                        column(8, class = "no-gutter", wellPanel( #gray border
                                              leafglOutput(outputId = "UD_map", width = "100%", height = "910")
                                              ))  
                                          )), # END: Use areas
                            # UI: Migratory timing ---------------------------------------------------------------------
                            nav_panel("Migratory timing",
                                      fluidRow(
                                        column(6, class = "no-gutter", wellPanel(
                                          useShinyFeedback(),
                                          pickerInput(inputId = "Species.timing",
                                                      label = HTML("<b>Species:</b>"),
                                                      choices = c("none selected", names(list.tracks)),
                                                      selected = "none selected",
                                                      multiple = FALSE, # allow multiple selections
                                                      width = "100%",
                                                      option = pickerOptions(actionsBox = TRUE,
                                                                             selectedTextFormat = "count >1",
                                                                             size = 5,
                                                                             liveSearch = TRUE,
                                                                             noneSelectedText = "none selected")),
                                          actionButton(inputId = "update.plots", icon=icon("refresh",lib="glyphicon"), "Update plots", width='100%'))),
                                        column(6, class = "no-gutter", wellPanel(
                                               verbatimTextOutput("Table_timing")))
                                        ),
                                      fluidRow(
                                        column(3, class = "no-gutter", #wellPanel(
                                          plotlyOutput("bar.track.first.loc", height = 250),
                                          plotlyOutput("bar.track.last.loc", height = 250),
                                          plotlyOutput("bar.track.los.loc", height = 250)
                                          #)
                                        ),
                                        column(6, class = "no-gutter", #wellPanel(
                                          plotlyOutput("bar.track.obs.freq", height = 500),
                                          plotlyOutput("bar.track.ava.freq", height = 250)
                                          #)
                                        ),
                                        column(3, class = "no-gutter", wellPanel(
                                          plotlyOutput("bar.ebird.obs.freq", height = 500),
                                          plotlyOutput("bar.ebird.ava.freq", height = 250)
                                          )))
                                      ), # END: Migratory timing
                            # UI: Methods / References ---------------------------------------------------------------------
                            nav_panel("User guide", includeMarkdown(path="www/README_Tracking_User guide.Rmd")),
                            nav_panel("References", includeMarkdown(path="www/README_Tracking_References.Rmd"))
            )),
  # Tab: Radar data ----------------------------------------------------------
  nav_panel(title = "Radar data",
            add_busy_spinner(spin = "double-bounce",
                             position = "top-right",
                             color = "ivory",
                             timeout = 1000,
                             height = "100px",
                             width = "100px",
                             margins = c(10,10)),
            navset_card_tab(title = "",
                            nav_panel("Map"), #, plotOutput()
                            nav_panel("Flight direction"), #, plotOutput()),
                            nav_panel("Flight altitude"), #, plotOutput()),
                            nav_panel("Summary statistics") #, plotOutput()),
            )), # END: Radar data
  # Tab: Survey data --------------------------------------------------------
  nav_panel(title = "Survey data",
            add_busy_spinner(spin = "double-bounce",
                             position = "top-right",
                             color = "ivory",
                             timeout = 1000,
                             height = "100px",
                             width = "100px",
                             margins = c(10,10)),
            navset_card_tab(title = "",
                            nav_panel("Map"), #, plotOutput()
                            nav_panel("Summary statistics") #, plotOutput()),
                            
            )), # END: Survey data
  # Tabs: Misc --------------------------------------------------------------
  nav_panel(title = "Contact us", 
            fluidRow(
              column(6, class = "no-gutter",
                     includeMarkdown(path="www/README_Contact us_pI.Rmd")),
              column(2, class = "no-gutter"),
              column(2, class = "no-gutter",
                     includeMarkdown(path="www/README_Contact us_pII.Rmd")),
              column(2, class = "no-gutter"))),
  nav_spacer(),
  nav_menu(
    title = "Species accounts",
    align = "right",
    nav_item(tags$a("Birds of the World", href = "https://birdsoftheworld.org/bow/home")),
    nav_item(tags$a("eBird species accounts", href = "https://ebird.org/species/hudgod"))
  )
)


# END: USER INTERFACE -----------------------------------------------------



####
##
# SERVER ------------------------------------------------------------------
####
##
#

server <- function(input, output, session){
  ###
  # Use areas ---------------------------------------------------------------
  ###
  observe( ### toggle on and off action button and UD radio buttons, with feedback
          if (input$Species=="none selected"){disable("update.map")
                                      showFeedbackWarning("Species",text="Select a species",color="#595959",icon=NULL)
                                      disable("UD")}
          else {enable("update.map")
                hideFeedback("Species")})
  observeEvent(c(input$Species,input$Month,input$individual.id,input$Year),{disable("UD")})
  ###
  Sel_birds_1 <- reactive( ### subset list.tracks by species selection
                          bind_rows(list.tracks[c(input$Species)])
  )
  ###
  Tag_pal <- reactive( ### color palette for "Individuals" of "Species"
                      colorFactor(palette = turbo(length(unique(Sel_birds_1()$individual.id))), 
                                  domain = as.factor(Sel_birds_1()$individual.id))
  )
  ###
  Sel_birds_fin <- reactive( ### subset species selection by month, individual, and year
                            filter(Sel_birds_1(), Month %in% input$Month,
                                   individual.id %in% input$individual.id,
                                   Year %in% input$Year) %>%
                              mutate(pnt.id = row_number()) %>%
                              mutate(Tag.pal = Tag_pal()(as.factor(individual.id))) %>%
                              mutate(Loc.long.sf = location.long.dateline) %>%
                              mutate(Loc.lat.sf = location.lat.dateline) %>%
                              st_as_sf(coords = c("Loc.long.sf", "Loc.lat.sf"), crs = 4326)
  )
  ###
  Sel_birds_fin2 <- reactive({ # subset points by Greater Cook Inlet Planning Area
                              Sel_birds_fin() %>%
                                st_drop_geometry() %>%
                                mutate(Loc.long.sf = location.long) %>%
                                mutate(Loc.lat.sf = location.lat) %>%
                                st_as_sf(coords = c("Loc.long.sf", "Loc.lat.sf"), crs = 4326) %>%
                                st_transform(crs = "+proj=laea +lat_0=59.6 +lon_0=-152.6 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") %>%
                                st_filter(list.shp[["area.GCI"]]) %>% # subset those in Great CIPA, using equal area projection
                                mutate(individual.id = factor(individual.id)) %>% # set individual.id as factor
                                mutate(across(where(is.factor), ~ {counts <- table(individual.id) # count each factor level
                                droplevels(factor(., levels = names(counts[counts >= 5]))) # keep only those with at least 5 locations
                                })) %>%
                                drop_na(individual.id)
    })
  ###
  list.spat <- reactive({ ### list of ud rasters ###
    ###
    spdf.POIN <- data.frame(individual.id = as.factor(Sel_birds_fin()$individual.id), # SpatialPointsDF
                            location.long = Sel_birds_fin()$location.long,
                            location.lat = Sel_birds_fin()$location.lat) %>%
      st_as_sf(coords = c("location.long", "location.lat"), crs = 4326) %>%
      st_transform(crs = "+proj=laea +lat_0=59.6 +lon_0=-152.6 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") %>%
      st_filter(list.shp[["area.GCI"]]) %>% # subset those in Great CIPA, using equal area projection
      mutate(across(where(is.factor), ~ {counts <- table(.) # count each factor level
      droplevels(factor(., levels = names(counts[counts >= 5]))) # keep only those with at least 5 locations
      })) %>% 
      as("Spatial")
    ###
    # Estimating and averaging
    kud <- kernelUD(spdf.POIN, grid = list.rst[["spdf.area.GCI"]], h="href")
    
    xbar <- rep(0, dim(kud[[1]]@data)[1]) # string of 0's equal to num of pixels
    for (i in 1:length(kud)){ # cycle through individuals
      for (j in 1:dim(kud[[i]]@data)[1]){ # cycle through pixels
        xbar[j] <- xbar[j] + kud[[i]]@data[["ud"]][j] # sum each pixel
      }
    }
    xbar <- xbar / length(kud)
    kud[[length(kud)+1]] <- kud[[length(kud)]]
    kud[[length(kud)]]@data[["ud"]] <- xbar
    ###
    # Mapping continuous UD
    kud.spdf <- estUDm2spixdf(kud)
    kud.map <- rast(kud.spdf[(dim(kud.spdf@data)[2])]) # raster
    off.mask <- rast(list.rst[["spdf.area.GCI.off"]]) # offshore mask
    off.mask <- crop(off.mask,kud.map)
    off.mask <- resample(off.mask, kud.map, method = "near")
    kud.off.map <- mask(kud.map, off.mask)
    #
    kud.map <- project(kud.map,"EPSG:3857") # Reproject for leaflet
    kud.off.map <- project(kud.off.map,"EPSG:3857") # Reproject for leaflet
    #
    tot <- sum(kud.map[,,1],na.rm=T) # recalculate to sum to 1
    kud.map <- kud.map / tot
    kud.off.map <- kud.off.map / tot
    ###
    # Converting to discrete
    thresholds <- rep(NA,4)
    counter <- 0
    vals <- sort(as.numeric(values(kud.map)), decreasing = TRUE)
    for (i in 1:length(vals)){
      counter <- counter + vals[i]
      if (is.na(thresholds[4]) && counter >= 0.25){
        thresholds[4] <- vals[i]}
      if (is.na(thresholds[3]) && counter >= 0.50){
        thresholds[3] <- vals[i]}
      if (is.na(thresholds[2]) && counter >= 0.75){
        thresholds[2] <- vals[i]}
      if (is.na(thresholds[1]) && counter >= 0.90){
        thresholds[1] <- vals[i]}
      if (counter >= 0.90) {break}}
    kud.map.disc <- classify(kud.map, rcl = matrix(c(-Inf, thresholds[1], NA,
                                                     thresholds[4], Inf, 0.25,
                                                     thresholds[3], Inf, 0.50,
                                                     thresholds[2], Inf, 0.75,
                                                     thresholds[1], Inf, 0.90), ncol=3, byrow=TRUE))
    kud.off.map.disc <- classify(kud.off.map, rcl = matrix(c(-Inf, thresholds[1], NA,
                                                             thresholds[4], Inf, 0.25,
                                                             thresholds[3], Inf, 0.50,
                                                             thresholds[2], Inf, 0.75,
                                                             thresholds[1], Inf, 0.90), ncol=3, byrow=TRUE))
   list(kud.map.disc = kud.map.disc,
         kud.off.map.disc = kud.off.map.disc)
  })
  ###
  plotfunc <- eventReactive(input$update.map, {
                                                if (dim(Sel_birds_fin())[1] > 0){
                                                  leafletProxy("UD_map") %>%
                                                    clearGlLayers() %>% # remove points
                                                    clearShapes() %>% # remove polygons
                                                    clearControls() %>% # remove legend
                                                    clearImages() %>% # remove UD
                                                    addGlPoints(data = Sel_birds_fin(),
                                                                fillColor = Sel_birds_fin()$Tag.pal,
                                                                radius = 10,
                                                                fillOpacity = 1,
                                                                popup = Sel_birds_fin()$location.label,
                                                                layerId = "LCI_pnts")}
                                                else {
                                                  leafletProxy("UD_map") %>%
                                                    clearGlLayers() %>% # remove points
                                                    clearShapes() %>% # remove polygons
                                                    clearControls() %>% # remove legend
                                                    clearImages()} 
  })
  ###
  observeEvent(input$Species, { ### update "Individuals" and "Years" by species selection
                                updatePickerInput(session,  
                                                  "individual.id",
                                                  choices = unique(Sel_birds_1()$individual.id),
                                                  selected = unique(Sel_birds_1()$individual.id))
                                updatePickerInput(session, 
                                                  "Year",
                                                  choices = sort(unique(Sel_birds_1()$Year)),
                                                  selected = sort(unique(Sel_birds_1()$Year)))
  })
  ###
  observeEvent(input$update.map, { #when action button, update map and radio buttons
                                  ###
                                  plotfunc()
                                  ###
                                  updateRadioButtons(session,
                                                     "Spatial_layers",
                                                     choices = c(
                                                       "Cook Inlet Planning Area" = "area.CIP.map",
                                                       "Lease Sale Area" = "area.LSA.map",
                                                       "Analysis area" = "area.GCI.map"),
                                                     selected = character(0))
                                  updateRadioButtons(session,
                                                     "UD",
                                                     choices = c(
                                                       "Entire analysis area" = "kud.map.disc",
                                                       "Offshore area" = "kud.off.map.disc"),
                                                     selected = character(0))
  })
  ###
  observeEvent(input$Spatial_layers, { ### regional polygons ###
                                      leafletProxy("UD_map") %>%
                                        clearShapes() %>% # on: 1 poly at a time; off: polys accumulate as they're clicked
                                        addPolygons(data = list.shp[[input$Spatial_layers]],
                                                    color = "black",
                                                    stroke = TRUE,
                                                    weight = 2,
                                                    opacity = 1,
                                                    fillOpacity = 0,
                                                    highlightOptions = highlightOptions(color = "ivory"))
  })
  ###
  observeEvent(input$UD, { ### mapping UD's ###
                          if (dim(Sel_birds_fin2())[1] > 0){
                            # Create a color palette function
                            pal <- colorFactor(
                              palette = viridis(256),  # viridis colors
                              levels = c(0.25,0.50,0.75,0.90),
                              na.color = "#00000000",
                              reverse = TRUE)
                            
                            leafletProxy("UD_map") %>%
                              clearGlLayers() %>% # remove points
                              clearShapes() %>% # remove polygons
                              clearControls() %>% # remove legend
                              clearImages() %>% # on: 1 UD at a time
                              addRasterImage(x = list.spat()[[input$UD]],
                                             colors = pal,
                                             opacity = 1,
                                             project = FALSE) %>%
                              addLegend(position = "topright",
                                        colors = pal(c(0.25,0.50,0.75,0.90)),
                                        labels = c("25%","50%","75%","90%"),
                                        title = "Core areas",
                                        values = values(list.spat()[[input$UD]])) %>%
                              setView(lng = 207.40, lat = 58.00, zoom = 6)}
                          else {leafletProxy("UD_map") %>%
                              clearGlLayers() %>% # remove points
                              clearShapes() %>% # remove polygons
                              clearControls() %>% # remove legend
                              clearImages() %>% # on: 1 UD at a time
                              setView(lng = 207.40, lat = 58.00, zoom = 6)
                            }
                        ###
                        updateRadioButtons(session,
                                           "Spatial_layers",
                                           choices = c(
                                             "Cook Inlet Planning Area" = "area.CIP.map",
                                             "Lease Sale Area" = "area.LSA.map",
                                             "Analysis area" = "area.GCI.map"),
                                           selected = character(0))
  })
  ###
  observeEvent(input$UD_map_glify_click,{ ### mapping track lines ###
                                        click_data <- input$UD_map_glify_click
                                        ind.id <- Sel_birds_fin()[Sel_birds_fin()$location.label == click_data$data,]$individual.id
                                        yr <- Sel_birds_fin()[Sel_birds_fin()$location.label == click_data$data,]$Year
                                        df.track <- Sel_birds_fin()[(Sel_birds_fin()$individual.id == ind.id) &
                                                                      (Sel_birds_fin()$Year == yr),]
                                        
                                        leafletProxy("UD_map") %>%
                                          #clearShapes() %>% # on: 1 track at a time; off: tracks accumulate as they're clicked
                                          addPolylines(data = df.track,
                                                       lat = df.track$location.lat.dateline,
                                                       lng = df.track$location.long.dateline,
                                                       color = "black",
                                                       weight = 3,
                                                       opacity = 1)
  })
  ###
  Dt_UD <- eventReactive(input$update.map, {
                                                  Sp <- unique(Sel_birds_fin2()$Species)
                                                  Tot.indi <- length(unique(Sel_birds_fin2()$individual.id))
                                                  Tot.loc <- dim(Sel_birds_fin2())[1]
                                                  IQR.loc <- round(quantile(Sel_birds_fin2() %>%
                                                                              group_by(individual.id) %>%
                                                                              summarise(locs = length(Year), .groups = "drop") %>%
                                                                              pull(locs)))
                                                  Yr <- sort(unique(Sel_birds_fin2()$Year))
                                                  
                                                  list(Sp = Sp,
                                                       Tot.indi = Tot.indi,
                                                       Tot.loc = Tot.loc,
                                                       IQR.loc = IQR.loc,
                                                       Yr = Yr)
  })
  ###
  output$Table_UD <- renderPrint({
                                    validate(need(!is.na(Dt_UD()[["Sp"]]), ""))
                                    cat("Species:", Dt_UD()[["Sp"]], "\n")                                
                                    cat("Total individuals:", Dt_UD()[["Tot.indi"]], "\n")
                                    cat("Total locations:", Dt_UD()[["Tot.loc"]], "\n")
                                    cat("Locations per individual\n(min, 25%, median, 75%, max):", Dt_UD()[["IQR.loc"]], "\n")
                                    cat("Years with locations in the Cook Inlet:", Dt_UD()[["Yr"]], "\n")
  })
  ###
  output$UD_map <- renderLeaflet( ### render the map ###
                                leaflet() %>%
                                addProviderTiles("Esri.OceanBasemap", group = "Ocean Map") %>%
                                addProviderTiles("Esri.WorldStreetMap", group = "Street Map") %>%
                                addProviderTiles("Esri.WorldImagery", group = "Satellite Map") %>%
                                setView(lng = 207.40, lat = 58.00, zoom = 6) %>% # Center on the LCI
                                addPolygons(data = list.shp[["area.CIP.map"]],
                                            color = "black",
                                            stroke = TRUE,
                                            weight = 2,
                                            opacity = 1,
                                            fillOpacity = 0,
                                            highlightOptions = highlightOptions(color = "ivory")) %>%
                                addLayersControl(baseGroups = c("Ocean Map","Street Map","Satellite Map"))%>%
                                addScaleBar(position = "topleft", scaleBarOptions(imperial = F))
  )
  ###
  # Migratory timing --------------------------------------------------
  ###
  observe( ### toggle on and off action button, with feedback
    if (input$Species.timing == "none selected"){disable("update.plots")}
    else {enable("update.plots")})
  ###
  Sel_birds_time <- reactive( ### subset list.tracks by species selection
                          bind_rows(list.tracks[c(input$Species.timing)]))
  Sel_birds_time_CI <- reactive( ### subset those in Great CIPA, using equal area projection
                              Sel_birds_time() %>%
                                mutate(Loc.long.sf = location.long) %>%
                                mutate(Loc.lat.sf = location.lat) %>%
                                st_as_sf(coords = c("Loc.long.sf", "Loc.lat.sf"), crs = 4326) %>%
                                st_transform(crs = "+proj=laea +lat_0=59.6 +lon_0=-152.6 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") %>%
                                st_filter(list.shp[["area.GCI"]]) 
  )
  ###
  df.los <- eventReactive(input$update.plots, {list.track.plots[[input$Species.timing]][["length.of.stay"]]})
  df.timing <- eventReactive(input$update.plots, {list.track.plots[[input$Species.timing]][["timing.data"]]})
  Dt_timing <- eventReactive(input$update.plots, {
                                                Sp <- unique(Sel_birds_time_CI()$Species)
                                                Tot.indi.CI <- paste(length(unique(Sel_birds_time_CI()$individual.id)),
                                                                     paste0("of"),
                                                                     length(unique(Sel_birds_time()$individual.id)),
                                                                     paste0("individuals (","",round(length(unique(Sel_birds_time_CI()$individual.id)) / 
                                                                                            length(unique(Sel_birds_time()$individual.id)) * 100),"%)"))
                                                Est.los <- quantile(df.los()$Min.los) # grab string from pre-constructed data frame
                                                Yr <- sort(unique(Sel_birds_time_CI()$Year))
                                                #
                                                list(Sp = Sp,
                                                     Tot.indi.CI = Tot.indi.CI,
                                                     Est.los = Est.los,
                                                     Yr = Yr)
  })
  ###
  output$Table_timing <- renderPrint({
                                  validate(need(!is.na(Dt_timing()[["Sp"]]), ""))
                                  cat("Species:", Dt_timing()[["Sp"]], "\n")                              
                                  cat("Individuals with locations in the Cook Inlet:", Dt_timing()[["Tot.indi.CI"]], "\n")
                                  cat("Minimum length of stay per visit (days)\n(min, 25%, median, 75%, max):", Dt_timing()[["Est.los"]], "\n")
                                  cat("Years with locations in the Cook Inlet:", Dt_timing()[["Yr"]], "\n")
  })
  ###
  output$bar.track.first.loc <- renderPlotly({
                                              fig <- ggplot(df.timing(), aes(x = Week, y = Num.first.loc, 
                                                                             text = Dates)) +
                                                geom_col(fill="gray55", color="black") +
                                                scale_x_continuous(limits = c(0,49),
                                                                   breaks=c(1,5,9,13,17,21,25,29,33,37,41,45),
                                                                   labels=c("","Feb","","Apr","","Jun","","Aug",
                                                                            "","Oct","","Dec")) +
                                                scale_y_continuous(limits = c(0,max(c(df.timing()$Num.first.loc,
                                                                                      df.timing()$Num.last.loc)))) +
                                                labs(x = NULL, 
                                                     y = "Count (tracks)",
                                                     title = "First location in the Cook Inlet")+
                                                theme_bw()
                                              #
                                              ggplotly(fig, tooltip = c("text")) %>%
                                                config(displayModeBar = FALSE) %>%
                                                layout(hoverlabel = list(bgcolor = "ivory", bordercolor = "black"),
                                                       xaxis = list(fixedrange = TRUE),
                                                       yaxis = list(fixedrange = TRUE))
    
  })
  output$bar.track.last.loc <- renderPlotly({
                                              fig <- ggplot(df.timing(), aes(x = Week, y = Num.last.loc, 
                                                                             text = Dates)) +
                                                geom_col(fill="gray55", color="black") +
                                                scale_x_continuous(limits = c(0,49),
                                                                   breaks=c(1,5,9,13,17,21,25,29,33,37,41,45),
                                                                   labels=c("","Feb","","Apr","","Jun","","Aug",
                                                                            "","Oct","","Dec")) +
                                                scale_y_continuous(limits = c(0,max(c(df.timing()$Num.first.loc,
                                                                                      df.timing()$Num.last.loc)))) +
                                                labs(x = NULL, 
                                                     y = "Count (tracks)",
                                                     title = "Last location in the Cook Inlet")+
                                                theme_bw()
                                              #
                                              ggplotly(fig, tooltip = c("text")) %>%
                                                config(displayModeBar = FALSE) %>%
                                                layout(hoverlabel = list(bgcolor = "ivory", bordercolor = "black"),
                                                       xaxis = list(fixedrange = TRUE),
                                                       yaxis = list(fixedrange = TRUE))
    
  })
  output$bar.track.los.loc <- renderPlotly({
                                              fig <- ggplot(df.los(), aes(x = Min.los, 
                                                                          text = paste(Min.los,"days"))) +
                                                geom_histogram(fill="gray55", color="black") +
                                                labs(x = "Days", 
                                                     y = "Count (tracks)",
                                                     title = "Minimum length of stay")+
                                                theme_bw()                                              
                                              #
                                              ggplotly(fig, tooltip = c("text")) %>%
                                                config(displayModeBar = FALSE) %>%
                                                layout(hoverlabel = list(bgcolor = "ivory", bordercolor = "black"),
                                                       xaxis = list(fixedrange = TRUE),
                                                       yaxis = list(fixedrange = TRUE))
  })
  ###
  output$bar.track.obs.freq <- renderPlotly({
                                              fig <- ggplot(df.timing(), aes(x = Week, y = Perc.in.CI, 
                                                                             text = paste(Dates,"\n",
                                                                                          Perc.in.CI,"%\n",
                                                                                          Num.in.CI,"of",Num.tracks,"tracks"))) +
                                                geom_col(fill="gray55", color="black") +
                                                scale_x_continuous(limits = c(0,49),
                                                                   breaks=c(1,5,9,13,17,21,25,29,33,37,41,45),
                                                                   labels=c("","Feb","","Apr","","Jun","","Aug",
                                                                            "","Oct","","Dec")) +
                                                scale_y_continuous(limits = c(0,max(df.timing()$Perc.in.CI))) +
                                                labs(x = NULL, 
                                                     y = "% in the Cook Inlet (tracks)",
                                                     title = "Tracking data in the Cook Inlet")+
                                                theme_bw()                                              
                                              #
                                              ggplotly(fig, tooltip = c("text")) %>%
                                                config(displayModeBar = FALSE) %>%
                                                layout(hoverlabel = list(bgcolor = "ivory", bordercolor = "black"),
                                                       xaxis = list(fixedrange = TRUE),
                                                       yaxis = list(fixedrange = TRUE))
  })
  output$bar.track.ava.freq <- renderPlotly({
                                              fig <- ggplot(df.timing(), aes(x = Week, y = Num.tracks, 
                                                                             text = paste(Dates,"\n",
                                                                                          Num.tracks,"tracks\n",
                                                                                          Num.indi,"individuals\n",
                                                                                          Num.years,"years of data"))) +
                                                geom_col(fill="gray55", color="black") +
                                                scale_x_continuous(limits = c(0,49),
                                                                   breaks=c(1,5,9,13,17,21,25,29,33,37,41,45),
                                                                   labels=c("","Feb","","Apr","","Jun","","Aug",
                                                                            "","Oct","","Dec")) +
                                                scale_y_continuous(limits = c(0,max(df.timing()$Num.tracks))) +
                                                labs(x = "Week", 
                                                     y = "Count (tracks)",
                                                     title = "Total available tracks")+
                                                theme_bw()
                                              #
                                              ggplotly(fig, tooltip = c("text")) %>%
                                                config(displayModeBar = FALSE) %>%
                                                layout(hoverlabel = list(bgcolor = "ivory", bordercolor = "black"),
                                                       xaxis = list(fixedrange = TRUE),
                                                       yaxis = list(fixedrange = TRUE))
  })
  output$bar.ebird.obs.freq <- renderPlotly({
                                              fig <- ggplot(df.timing(), aes(x = Week, y = Perc.ebird, 
                                                                             text = paste(Dates,"\n",
                                                                                          Perc.ebird,"% of\n",
                                                                                          Tot.ebird.lists,"checklists"))) +
                                                geom_col(fill="gray95", color="black") +
                                                scale_x_continuous(limits = c(0,49),
                                                                   breaks=c(1,5,9,13,17,21,25,29,33,37,41,45),
                                                                   labels=c("","Feb","","Apr","","Jun","","Aug",
                                                                            "","Oct","","Dec")) +
                                                scale_y_continuous(limits = c(0,max(df.timing()$Perc.ebird))) +
                                                labs(x = NULL,
                                                     y = paste("% that report ",paste0("",input$Species.timing,"")," (checklists)",sep=""),
                                                     #y = "% that report sp. (checklists)",
                                                     title = "eBird data in the Cook Inlet")+
                                                theme_bw()
                                              #
                                              ggplotly(fig, tooltip = c("text")) %>%
                                                config(displayModeBar = FALSE) %>%
                                                layout(hoverlabel = list(bgcolor = "ivory", bordercolor = "black"),
                                                       xaxis = list(fixedrange = TRUE),
                                                       yaxis = list(fixedrange = TRUE))
  })
  output$bar.ebird.ava.freq <- renderPlotly({
                                              fig <- ggplot(df.timing(), aes(x = Week, y = Tot.ebird.lists, 
                                                                             text = paste(Dates,"\n",
                                                                                          Tot.ebird.lists,"checklists"))) +
                                                geom_col(fill="gray95", color="black") +
                                                scale_x_continuous(limits = c(0,49),
                                                                   breaks=c(1,5,9,13,17,21,25,29,33,37,41,45),
                                                                   labels=c("","Feb","","Apr","","Jun","","Aug",
                                                                            "","Oct","","Dec")) +
                                                scale_y_continuous(limits = c(0,max(df.timing()$Tot.ebird.lists))) +
                                                labs(x = "Week", 
                                                     y = "Count (checklists)",
                                                     title = "Total checklists in the CI")+
                                                theme_bw()
                                              #
                                              ggplotly(fig, tooltip = c("text")) %>%
                                                config(displayModeBar = FALSE) %>%
                                                layout(hoverlabel = list(bgcolor = "ivory", bordercolor = "black"),
                                                       xaxis = list(fixedrange = TRUE),
                                                       yaxis = list(fixedrange = TRUE))
  })
  ###
}


# END: SERVER -------------------------------------------------------------



####
##
# CALL FUNCTION -----------------------------------------------------------
####
##
#

# The call function
shinyApp(ui = ui, server = server)


# END: CALL FUNCTION ------------------------------------------------------




