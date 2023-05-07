library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# 读取数据
pop_density_data <- read.csv("path_to_population_density_data.csv")
forest_coverage_data <- read.csv("path_to_forest_coverage_data.csv")
factory_data <- read.csv("path_to_oil_and_palm_factory_data.csv")

# 读取各省的边界数据
province_shapefiles <- list.files("\\shp_files", pattern = "\\.shp$", full.names = TRUE)
province_boundaries <- lapply(province_shapefiles, function(x) {
  st_read(x)
})

# 合并省边界数据
all_provinces <- do.call(rbind, province_boundaries)

# 根据需要，将 province_name 替换为你的 CSV 文件中的省份名称列
pop_density_data <- left_join(all_provinces, pop_density_data, by = "province_name")
forest_coverage_data <- left_join(all_provinces, forest_coverage_data, by = "province_name")

ui <- fluidPage(
  titlePanel("Sumatra"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "layer_selector",
        "选择一个涂层:",
        choices = c("population" = "pop_density",
                    "forest loss" = "forest_coverage")
      ),
      sliderInput(
        "year_selector",
        "Select Year:",
        min = 2000, # 根据你的数据设置最小年份
        max = 2021, # 根据你的数据设置最大年份
        value = 2000,
        step = 1,
        animate = TRUE
      )
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 102.5, lat = -2.5, zoom = 5)
  })
  
  observeEvent(c(input$layer_selector, input$year_selector), {
    selected_data <- switch(input$layer_selector,
                            "pop_density" = pop_density_data,
                            "forest_coverage" = forest_coverage_data)

    selected_data <- selected_data %>% filter(year == input$year_selector)
    
    selected_data_sf <- st_as_sf(selected_data)
    
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = selected_data_sf, fillColor = "blue", fillOpacity = 0.5, weight = 1) %>%
      addPolygons(data = all_provinces, fillOpacity = 0, color = "black", weight = 2) %>%
      addMarkers(data = factory_data, lng = ~longitude, lat = ~latitude, popup = ~name)
  })

  shinyApp(ui, server)