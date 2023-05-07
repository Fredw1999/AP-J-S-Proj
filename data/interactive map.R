library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# 读取数据
pop_density_data <- read.csv("net_migrants.csv")
forest_coverage_data <- read.csv("yearly_forest_loss_density.csv")
#factory_data <- read.csv("path_to_oil_and_palm_factory_data.csv")
#language_data
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

# 新建数据框，包含所有省份的名称
all_provinces <- data.frame(province_name = c("Aceh", "Bengkulu", "Jambi", "Lampung", 
                                              "Riau", "SumateraBarat", "SumateraSelatan", "SumateraUtara"))

# 将所有省份的名称与边界数据合并
all_provinces_boundary <- all_provinces %>% 
  left_join(Aceh_boundary, by = c("province_name" = "PROVINSI"), all = TRUE, fill = list(geometry = st_sfc()))
all_provinces_boundary <- all_provinces_boundary %>% 
  left_join(Bengkulu_boundary, by = c("province_name" = "PROVINSI"), all = TRUE, fill = list(geometry = st_sfc()))
all_provinces_boundary <- all_provinces_boundary %>% 
  left_join(Jambi_boundary, by = c("province_name" = "PROVINSI"), all = TRUE, fill = list(geometry = st_sfc()))
all_provinces_boundary <- all_provinces_boundary %>% 
  left_join(Lampung_boundary, by = c("province_name" = "PROVINSI"), all = TRUE, fill = list(geometry = st_sfc()))
all_provinces_boundary <- all_provinces_boundary %>% 
  left_join(Riau_boundary, by = c("province_name" = "PROVINSI"), all = TRUE, fill = list(geometry = st_sfc()))
all_provinces_boundary <- all_provinces_boundary %>% 
  left_join(SumateraBarat_boundary, by = c("province_name" = "PROVINSI"), all = TRUE, fill = list(geometry = st_sfc()))
all_provinces_boundary <- all_provinces_boundary %>% 
  left_join(SumateraSelatan_boundary, by = c("province_name" = "PROVINSI"), all = TRUE, fill = list(geometry = st_sfc()))
all_provinces_boundary <- all_provinces_boundary %>% 
  left_join(SumateraUtara_boundary, by = c("province_name" = "PROVINSI"), all = TRUE, fill = list(geometry = st_sfc()))

# 根据需要，将 province_name 替换为你的 CSV 文件中的省份名称列
pop_density_data <- left_join(all_provinces, pop_density_data, by = "province_name")
forest_coverage_data <- left_join(all_provinces, forest_coverage_data, by = "province_name")


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
  })}
shinyApp(ui, server)