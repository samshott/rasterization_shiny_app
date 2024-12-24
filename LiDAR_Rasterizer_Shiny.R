# app.R
library(shiny)
library(lidR)
library(raster)
library(shinydashboard)
library(DT)
library(plotly)

# Global options for file size
options(shiny.maxRequestSize = 30000 * 1024^2)  # 30GB

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "LiDAR Data Processor"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Input", tabName = "input", icon = icon("upload")),
      menuItem("Ground Classification", tabName = "ground", icon = icon("layer-group")),
      menuItem("Raster Generation", tabName = "raster", icon = icon("image")),
      menuItem("Results", tabName = "results", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Data Input Tab
      tabItem(tabName = "input",
              fluidRow(
                box(
                  title = "Upload LiDAR Data",
                  width = 12,
                  fileInput("lasFile", "Choose .las/.laz file",
                            multiple = TRUE,
                            accept = c(".las", ".laz")
                  ),
                  textInput("dirPath", "Or enter directory path containing .las/.laz files"),
                  actionButton("loadDir", "Load Directory"),
                  verbatimTextOutput("fileInfo")
                )
              )
      ),
      
      # Ground Classification Tab
      tabItem(tabName = "ground",
              fluidRow(
                box(
                  title = "CSF Parameters",
                  width = 6,
                  checkboxInput("sloop_smooth", "Slope Smoothing", value = TRUE),
                  numericInput("class_threshold", "Classification Threshold", 
                               value = 0.5, min = 0, max = 2, step = 0.1),
                  numericInput("cloth_resolution", "Cloth Resolution",
                               value = 0.5, min = 0.1, max = 5, step = 0.1),
                  numericInput("rigidness", "Rigidness",
                               value = 1, min = 0, max = 3, step = 0.1),
                  numericInput("iterations", "Iterations",
                               value = 500, min = 100, max = 1000, step = 50),
                  numericInput("time_step", "Time Step",
                               value = 0.65, min = 0.1, max = 1, step = 0.05),
                  actionButton("runClassification", "Run Ground Classification")
                ),
                box(
                  title = "Classification Results",
                  width = 6,
                  verbatimTextOutput("classificationInfo"),
                  plotOutput("classificationPlot")
                )
              )
      ),
      
      # Raster Generation Tab
      tabItem(tabName = "raster",
              fluidRow(
                box(
                  title = "Terrain Model Parameters",
                  width = 6,
                  numericInput("resolution", "Raster Resolution (m)",
                               value = 1, min = 0.1, max = 10, step = 0.1),
                  selectInput("terrain_algorithm", "DTM Algorithm",
                              choices = c("TIN" = "tin", 
                                          "IDW" = "knnidw",
                                          "Kriging" = "kriging"),
                              selected = "tin"),
                  # Additional parameters for IDW
                  conditionalPanel(
                    condition = "input.terrain_algorithm == 'knnidw'",
                    numericInput("idw_k", "Number of neighbors (k)",
                                 value = 6, min = 3, max = 20),
                    numericInput("idw_p", "Power (p)",
                                 value = 2, min = 1, max = 5)
                  ),
                  # Additional parameters for Kriging
                  conditionalPanel(
                    condition = "input.terrain_algorithm == 'kriging'",
                    numericInput("kriging_k", "Number of points",
                                 value = 10, min = 5, max = 30)
                  )
                ),
                box(
                  title = "Canopy Model Parameters",
                  width = 6,
                  selectInput("canopy_algorithm", "DSM/CHM Algorithm",
                              choices = c(
                                "Point-to-raster" = "p2r",
                                "TIN" = "dsmtin",
                                "Pitfree" = "pitfree"
                              ),
                              selected = "p2r"
                  ),
                  # Additional parameters for pitfree
                  conditionalPanel(
                    condition = "input.canopy_algorithm == 'pitfree'",
                    textInput("pitfree_thresholds", 
                              "Height thresholds (comma-separated)",
                              value = "0,2,5,10,15"),
                    textInput("pitfree_subcircle", 
                              "Subcircle radii (comma-separated)",
                              value = "0,1.5")
                  ),
                  # Additional parameters for p2r
                  conditionalPanel(
                    condition = "input.canopy_algorithm == 'p2r'",
                    numericInput("p2r_radius", "Point radius (m)",
                                 value = 0.2, min = 0, max = 1, step = 0.1)
                  )
                ),
                box(
                  title = "Generate Models",
                  width = 12,
                  actionButton("generateRasters", "Generate Raster Models"),
                  verbatimTextOutput("rasterStatus")
                )
              )
      ),
      
      # Results Tab
      tabItem(tabName = "results",
              fluidRow(
                box(
                  title = "Export Options",
                  width = 12,
                  textInput("exportPath", "Export Directory Path", 
                            value = getwd()),
                  selectInput("exportFormat", "Export Format",
                              choices = c("GeoTiff (.tif)" = "GTiff", 
                                          "ESRI Shapefile (.shp)" = "ESRI Shapefile",
                                          "ASCII Grid (.asc)" = "AAIGrid"),
                              selected = "GTiff"),
                  actionButton("exportRasters", "Export All Rasters"),
                  verbatimTextOutput("exportStatus")
                )
              ),
              fluidRow(
                box(
                  title = "Digital Terrain Model (DTM)",
                  width = 12,
                  plotOutput("dtmPlot")
                )
              ),
              fluidRow(
                box(
                  title = "Digital Surface Model (DSM)",
                  width = 6,
                  plotOutput("dsmPlot")
                ),
                box(
                  title = "Canopy Height Model (CHM)",
                  width = 6,
                  plotOutput("chmPlot")
                )
              ),
              fluidRow(
                box(
                  title = "Point Density",
                  width = 12,
                  plotOutput("densityPlot")
                )
              )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Reactive values to store data
  values <- reactiveValues(
    las = NULL,
    ground_classified = NULL,
    dtm = NULL,
    dsm = NULL,
    chm = NULL,
    density = NULL
  )
  
  # File Input Handler
  observeEvent(input$lasFile, {
    req(input$lasFile)
    withProgress(message = 'Loading LiDAR data...', {
      tryCatch({
        values$las <- readLAS(input$lasFile$datapath)
        output$fileInfo <- renderPrint({
          summary(values$las)
        })
      }, error = function(e) {
        showNotification(paste("Error loading file:", e$message), type = "error")
      })
    })
  })
  
  # Directory Input Handler
  observeEvent(input$loadDir, {
    req(input$dirPath)
    if (dir.exists(input$dirPath)) {
      withProgress(message = 'Loading directory...', {
        tryCatch({
          las_files <- list.files(input$dirPath, pattern = "\\.la[sz]$", 
                                  full.names = TRUE)
          values$las <- readLAS(las_files)
          output$fileInfo <- renderPrint({
            summary(values$las)
          })
        }, error = function(e) {
          showNotification(paste("Error loading directory:", e$message), 
                           type = "error")
        })
      })
    }
  })
  
  # Ground Classification Handler
  observeEvent(input$runClassification, {
    req(values$las)
    withProgress(message = 'Classifying ground points...', {
      tryCatch({
        csf <- csf(sloop_smooth = input$sloop_smooth,
                   class_threshold = input$class_threshold,
                   cloth_resolution = input$cloth_resolution,
                   rigidness = input$rigidness,
                   iterations = input$iterations,
                   time_step = input$time_step)
        
        values$ground_classified <- classify_ground(values$las, csf)
        
        output$classificationInfo <- renderPrint({
          paste("Ground points classified:",
                sum(values$ground_classified@data$Classification == 2),
                "of", nrow(values$ground_classified@data))
        })
        
        output$classificationPlot <- renderPlot({
          plot(values$ground_classified, color = "Classification")
        })
      }, error = function(e) {
        showNotification(paste("Error in ground classification:", e$message), 
                         type = "error")
      })
    })
  })
  
  # Raster Generation Handler
  observeEvent(input$generateRasters, {
    req(values$ground_classified)
    withProgress(message = 'Generating raster models...', {
      tryCatch({
        # Prepare algorithm for terrain model
        terrain_algo <- switch(input$terrain_algorithm,
                               "tin" = tin(),
                               "knnidw" = knnidw(k = input$idw_k, p = input$idw_p),
                               "kriging" = kriging(k = input$kriging_k)
        )
        
        # Generate DTM
        values$dtm <- rasterize_terrain(
          values$ground_classified,
          res = input$resolution,
          algorithm = terrain_algo
        )
        
        # Prepare algorithm for canopy model
        canopy_algo <- switch(input$canopy_algorithm,
                              "p2r" = p2r(input$p2r_radius),
                              "dsmtin" = dsmtin(),
                              "pitfree" = {
                                thresholds <- as.numeric(strsplit(input$pitfree_thresholds, ",")[[1]])
                                subcircle <- as.numeric(strsplit(input$pitfree_subcircle, ",")[[1]])
                                pitfree(thresholds, subcircle)
                              }
        )
        
        # Generate DSM
        values$dsm <- rasterize_canopy(
          values$ground_classified,
          res = input$resolution,
          algorithm = canopy_algo
        )
        
        # Generate CHM (DSM - DTM)
        values$chm <- values$dsm - values$dtm
        
        # Generate density map
        values$density <- rasterize_density(
          values$ground_classified,
          res = input$resolution
        )
        
        # Update plots
        output$dtmPlot <- renderPlot({
          plot(values$dtm, main = "Digital Terrain Model")
        })
        
        output$dsmPlot <- renderPlot({
          plot(values$dsm, main = "Digital Surface Model")
        })
        
        output$chmPlot <- renderPlot({
          plot(values$chm, main = "Canopy Height Model")
        })
        
        output$densityPlot <- renderPlot({
          plot(values$density, main = "Point Density")
        })
        
        output$rasterStatus <- renderPrint({
          "Raster models generated successfully!"
        })
      }, error = function(e) {
        showNotification(paste("Error generating rasters:", e$message), 
                         type = "error")
      })
    })
  })
}

# Export Handler
observeEvent(input$exportRasters, {
  req(values$dtm, values$dsm, values$chm, values$density)
  
  # Validate export path
  if (!dir.exists(input$exportPath)) {
    showNotification("Export directory does not exist", type = "error")
    return()
  }
  
  withProgress(message = 'Exporting raster files...', {
    tryCatch({
      # Create timestamp for unique filenames
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      
      # Export DTM
      dtm_path <- file.path(input$exportPath, 
                            sprintf("dtm_%s.%s", 
                                    timestamp,
                                    if(input$exportFormat == "GTiff") "tif" else 
                                      if(input$exportFormat == "AAIGrid") "asc" else "shp"))
      writeRaster(values$dtm, 
                  filename = dtm_path,
                  format = input$exportFormat,
                  overwrite = TRUE)
      
      # Export DSM
      dsm_path <- file.path(input$exportPath, 
                            sprintf("dsm_%s.%s", 
                                    timestamp,
                                    if(input$exportFormat == "GTiff") "tif" else 
                                      if(input$exportFormat == "AAIGrid") "asc" else "shp"))
      writeRaster(values$dsm,
                  filename = dsm_path,
                  format = input$exportFormat,
                  overwrite = TRUE)
      
      # Export CHM
      chm_path <- file.path(input$exportPath,
                            sprintf("chm_%s.%s", 
                                    timestamp,
                                    if(input$exportFormat == "GTiff") "tif" else 
                                      if(input$exportFormat == "AAIGrid") "asc" else "shp"))
      writeRaster(values$chm,
                  filename = chm_path,
                  format = input$exportFormat,
                  overwrite = TRUE)
      
      # Export density map
      density_path <- file.path(input$exportPath,
                                sprintf("density_%s.%s", 
                                        timestamp,
                                        if(input$exportFormat == "GTiff") "tif" else 
                                          if(input$exportFormat == "AAIGrid") "asc" else "shp"))
      writeRaster(values$density,
                  filename = density_path,
                  format = input$exportFormat,
                  overwrite = TRUE)
      
      output$exportStatus <- renderPrint({
        paste("Rasters exported successfully to:", input$exportPath,
              "\nFiles:",
              "\n - ", basename(dtm_path),
              "\n - ", basename(dsm_path),
              "\n - ", basename(chm_path),
              "\n - ", basename(density_path))
      })
      
      showNotification("Rasters exported successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error exporting rasters:", e$message), 
                       type = "error")
    })
  })
})

# Run the application
shinyApp(ui = ui, server = server)