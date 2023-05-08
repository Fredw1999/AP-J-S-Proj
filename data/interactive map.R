library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(tidyverse)
library(htmltools)

# 读取数据
pop_density_data <- read.csv("net_migrant_long.csv")
forest_coverage_data <- read.csv("yearly_forest_loss_density_long.csv")
factory_data <- read.csv("RSPO_Palm_Oil_Mills.csv")
language_data<-read.csv("language_freq.csv")

language_data_sf <- st_as_sf(language_data, coords = c("longitude", "latitude"), crs = 4326) 
# 读取各省的边界数据
# 读入8个省份的边界数据
Aceh_boundary <- st_read("/Users/weijiahao/Dropbox/PC/Desktop/统计作业/5293/AP-J-S-Proj/data/shp_files/Aceh_boundary.shp")
Bengkulu_boundary <- st_read("/Users/weijiahao/Dropbox/PC/Desktop/统计作业/5293/AP-J-S-Proj/data/shp_files/Bengkulu_boundary.shp")
Jambi_boundary <- st_read("/Users/weijiahao/Dropbox/PC/Desktop/统计作业/5293/AP-J-S-Proj/data/shp_files/Jambi_boundary.shp")
Lampung_boundary <- st_read("/Users/weijiahao/Dropbox/PC/Desktop/统计作业/5293/AP-J-S-Proj/data/shp_files/Lampung_boundary.shp")
Riau_boundary <- st_read("/Users/weijiahao/Dropbox/PC/Desktop/统计作业/5293/AP-J-S-Proj/data/shp_files/Riau_boundary.shp")
SumateraBarat_boundary <- st_read("/Users/weijiahao/Dropbox/PC/Desktop/统计作业/5293/AP-J-S-Proj/data/shp_files/SumateraBarat_boundary.shp")
SumateraSelatan_boundary <- st_read("/Users/weijiahao/Dropbox/PC/Desktop/统计作业/5293/AP-J-S-Proj/data/shp_files/SumateraSelatan_boundary.shp")
SumateraUtara_boundary <- st_read("/Users/weijiahao/Dropbox/PC/Desktop/统计作业/5293/AP-J-S-Proj/data/shp_files/SumateraUtara_boundary.shp")



Aceh_boundary$province_name <- "Aceh"
Bengkulu_boundary$province_name <- "Bengkulu"
Jambi_boundary$province_name <- "Jambi"
Lampung_boundary$province_name <- "Lampung"
Riau_boundary$province_name <- "Riau"
SumateraBarat_boundary$province_name <- "SumateraBarat"
SumateraSelatan_boundary$province_name <- "SumateraSelatan"
SumateraUtara_boundary$province_name <- "SumateraUtara"

# 合并所有省份的边界数据
all_provinces_boundary <- rbind(Aceh_boundary, Bengkulu_boundary, Jambi_boundary, Lampung_boundary,
                                Riau_boundary, SumateraBarat_boundary, SumateraSelatan_boundary,
                                SumateraUtara_boundary)

all_provinces_boundary_with_center <- all_provinces_boundary %>%
  group_by(province_name) %>%
  summarise() %>%
  st_centroid() %>%
  left_join(language_data_sf, by = "province_name") 
# 将数据框与 all_provinces_boundary 数据框合

pop_density_data <- left_join(pop_density_data, all_provinces_boundary, by = "province_name")
forest_coverage_data <- left_join(forest_coverage_data, all_provinces_boundary, by = "province_name")

factory_icon <- makeIcon(
  iconUrl = "icons8-factory-48.png",
  iconWidth = 30, iconHeight = 30,
  iconAnchorX = 15, iconAnchorY = 15
)

ui <- fluidPage(
  titlePanel("Sumatra"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "layer_selector",
        "Choose Type of Data:",
        choices = c("population" = "pop_density",
                    "forest loss" = "forest_coverage")
      ),
      conditionalPanel(
        condition = "input.layer_selector == 'pop_density'",
        sliderInput(
          "year_selector_pop",
          "Select Year for Population Density:",
          min = 1980,
          max = 2015,
          value = 1980,
          step = 5,
          animate = TRUE
        )
      ),
      conditionalPanel(
        condition = "input.layer_selector == 'forest_coverage'",
        sliderInput(
          "year_selector_forest",
          "Select Year for Forest Loss Density:",
          min = 2001,
          max = 2017,
          value = 2001,
          step = 1,
          animate = TRUE
        )
      )
    ),
    mainPanel(
      leafletOutput("map", width = "100%",height = 800),# 设置宽度为 100%，使地图占据更大的空间
      width = 9 # 调整 mainPanel 宽度
    )
  )
)
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 102.5, lat = -2.5, zoom = 5) %>%
      # 添加图例（初始状态）
      addLegend(
        position = "bottomright",
        pal = colorBin(palette = "YlOrRd", domain = NULL, na.color = "transparent", bins = 10),
        values = NULL,
        title = "Data Value",
        opacity = 1,
        group = "map_legend"
      )
  })
  
  observeEvent(c(input$layer_selector, input$year_selector_pop, input$year_selector_forest), {
    selected_data <- switch(input$layer_selector,
                            "pop_density" = {
                              pop_density_data %>% dplyr::filter(year == input$year_selector_pop)
                            },
                            "forest_coverage" = {
                              forest_coverage_data %>% dplyr::filter(year == input$year_selector_forest)
                            })
    
    selected_data_sf <- st_as_sf(selected_data)
    
    # 创建颜色映射
    color_map <- colorBin(palette = "YlOrRd", domain = selected_data$value, na.color = "transparent", bins = 10)
    
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(
        data = selected_data_sf,
        fillColor = ~color_map(value), 
        fillOpacity = 0.5,
        weight = 1
      ) %>%
      addPolygons(data = all_provinces_boundary, fillOpacity = 0, color = "black", weight = 2) %>%
      addMarkers(data = factory_data, lng = ~longitude, lat = ~latitude, popup = 'palm',icon = factory_icon, clusterOptions = markerClusterOptions()) %>%
      removeControl(layerId="map_legend") %>%
      addMarkers(
        data = all_provinces_boundary_with_center,
        lat = ~y,
        lng = ~x,
        popup = ~as.character(value),
        label = ~as.character(value),
        labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "auto"),
        group = "language_data_markers"
      )%>%
      addLegend(
        position = "bottomright",
        pal = color_map,
        values = selected_data$value,
        title = "Data Value",
        opacity = 1,
        group = "map_legend",
        layerId ="map_legend"
      )})
}

shinyApp(ui, server)