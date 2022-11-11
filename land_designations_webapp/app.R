library(shiny)
library(tidyverse)
# library(magick)
# library(ggimage)
# library(ggtext)
# library(slickR)
library(sf)
library(bslib)
library(DT)

# setwd("C:/Users/CMADSEN/Downloads/LocalR/land-designations-indicator")

rm(list = ls())

prov_fig_dat = data.frame(industry = c('Forestry','Mining','Oil and Gas'),
                          prov_image_path = c('forest_restriction_plot.jpeg',
                                              'mine_restriction_plot.jpeg',
                                              'og_restriction_plot.jpeg'),
                          bar_image_path = c('forestry_barplot.png',
                                             'mining_barplot.png',
                                             'og_barplot.png'))

### Input Filters
industry_filter = selectInput(
  inputId = 'industry_filter',
  label = "Select Industry",
  choices = prov_fig_dat$industry,
  selected = "Forestry"
)

### Horizontal Orientation ###
ui <- bslib::page_fluid(
  titlePanel(
    title = "Land Designation Indicator"
  ),
  sidebarLayout(
    sidebarPanel(
      width = 5,
      height = 1000,
      industry_filter,
      uiOutput('spatial_scale_selector'),
      plotOutput('bar_fig'#, height = 500)
      )
    ),
    mainPanel(
      width = 7,
      card(
        card_header(),
        card_body(
          # industry_filter,
          uiOutput('jpeg_fig', width = '100px'),
          plotOutput('map_insert', height = '200px')
        ),
        width = '100%'
      )
    )
  )
)

### Vertical organization ###
# ui <- bslib::page_fluid(
#   titlePanel(
#     title = "Land Designation Indicator"
#   ),
#   card(
#     card_header(h5('Controls')),
#     card_body(
#       crosstalk::bscols(
#         industry_filter,
#         uiOutput('spatial_scale_selector')
#       )
#     )
#   ),
#   card(
#     card_header(h5("Output"), class = 'bg-success'),
#     card_body(
#       uiOutput('jpeg_fig'),
#       plotOutput('bar_fig'),
#       plotOutput('map_insert', height = '200px')
#     )
#   )
# )


server <- function(input, output) {

  ### Static entities
  bc_size = 941944305881

  bc_regs = read_sf('www/bc_regdists.gpkg') %>%
    mutate(industry_name_key = industry_name) %>%
    mutate(industry_name = case_when(
      str_detect(industry_name, 'forest_') ~ "Forestry",
      str_detect(industry_name, 'mine_') ~ "Mining",
      str_detect(industry_name, 'og_') ~ "Oil and Gas"
    )) %>%
    mutate(max_rlevel = max_restriction_value) %>%
    mutate(max_restriction_value = case_when(
      max_restriction_value == 0 ~ "NA",
      max_restriction_value == 1 ~ "Other (Very Low)",
      max_restriction_value == 2 ~ "Managed Areas (Low)",
      max_restriction_value == 3 ~ "Resource Exclusion Area (Medium)",
      max_restriction_value == 4 ~ "Other Protected Lands (High)",
      max_restriction_value == 5 ~ "Parks & Protected Areas (Full)"
    )) %>%
    mutate(max_restriction_value = replace(max_restriction_value, max_rlevel == "NA", NA))

  regdist_sum_dat = read.csv('www/ld_choro.csv') %>%
    as_tibble() %>%
    mutate(industry_name_key = industry_name) %>%
    mutate(industry_name = case_when(
      str_detect(industry_name, 'forest_') ~ "Forestry",
      str_detect(industry_name, 'mine_') ~ "Mining",
      str_detect(industry_name, 'og_') ~ "Oil and Gas"
    )) %>%
    mutate(max_rlevel = max_restriction_value) %>%
    mutate(max_restriction_value = case_when(
      max_restriction_value == 0 ~ "NA",
      max_restriction_value == 1 ~ "Other (Very Low)",
      max_restriction_value == 2 ~ "Managed Areas (Low)",
      max_restriction_value == 3 ~ "Resource Exclusion Area (Medium)",
      max_restriction_value == 4 ~ "Other Protected Lands (High)",
      max_restriction_value == 5 ~ "Parks & Protected Areas (Full)"
    )) %>%
    mutate(max_restriction_value = replace(max_restriction_value, max_rlevel == "NA", NA))

  #Add a summary row for the whole province.
  regdist_sum_dat = regdist_sum_dat %>%
    bind_rows(
      regdist_sum_dat %>%
        group_by(industry_name,industry_name_key,max_restriction_value,max_rlevel) %>%
        summarise(area = sum(area,na.rm=T)) %>%
        mutate(ADMIN_AREA_NAME = "Provincial")
    )

  # #Replace character "NA" with actual NA.
  # regdist_sum_dat[regdist_sum_dat$max_restriction_value == 'NA',] = NA

  ### Reactive entities
  province_fig_dat_filtered = reactive({
    prov_fig_dat %>%
      filter(industry %in% input$industry_filter)
  })

  bcRegsFiltered = reactive({
    bc_regs %>%
      filter(industry_name %in% input$industry_filter) %>%
      filter(ADMIN_AREA_NAME %in% input$spatial_scale_selector)
  })

  regDistSumms = reactive({
    regdist_sum_dat %>%
      filter(industry_name %in% input$industry_filter) %>%
      filter(ADMIN_AREA_NAME %in% input$spatial_scale_selector)
  })

  ### Render Visuals and UI Elements
  output$spatial_scale_selector = renderUI({
    selectInput(
      inputId = 'spatial_scale_selector',
      label = "Select Spatial Scale",
      choices = c("Provincial",all_of(bc_regs$ADMIN_AREA_NAME)),
      selected = 'Provincial',
      selectize = T
    )
  })

  output$jpeg_fig = renderUI({
    if(input$spatial_scale_selector == "Provincial"){
      tags$img(src = province_fig_dat_filtered()$prov_image_path,
               width = '100%')
    } else {
      tags$img(src = paste0("regdist_figs/",
                            regDistSumms()$industry_name_key[1],
                            "_",
                            regDistSumms()$ADMIN_AREA_NAME[1],
                            ".jpeg"),
               width = '90%')
    }
  })

  output$map_insert = renderPlot({
    if(input$spatial_scale_selector == "Provincial")return(NULL)
    ggplot() +
      geom_sf(data = bc_regs, fill = 'antiquewhite', col = 'grey') +
      geom_sf(data = bcRegsFiltered(), fill = "red", col = 'black') +
      ggthemes::theme_map()
  })

  output$bar_fig = renderPlot({

    if(input$spatial_scale_selector == "Provincial"){
      dat = regDistSumms() %>%
        group_by(max_restriction_value,max_rlevel) %>%
        summarise(area_summed = sum(area,na.rm=T),.groups = "drop") %>%
        mutate(area_prop = area_summed / bc_size) %>%
        arrange(max_rlevel)
    } else {
      dat = regDistSumms() %>%
        rename(area_prop = prop_regdist_area)
    }

    p = dat %>%
      mutate(max_restriction_value = replace(max_restriction_value,
                                             max_restriction_value == "NA",
                                             NA)) %>%
      arrange(max_rlevel) %>%
      mutate(max_restriction_value = fct_inorder(max_restriction_value)) %>%
      # mutate(area_prop = 100*round(area_prop,4)) %>%
      ggplot() +
      geom_col(aes(x = max_restriction_value,
                   y = area_prop,
                   fill = max_restriction_value)) +
      coord_flip() +
      scale_x_discrete(labels = scales::label_wrap(width = 16)) +
      scale_y_continuous(labels = scales::percent_format()) +
      envreportutils::theme_soe() +
      # theme_bw() +
      theme(legend.position = 'none',
            text = element_text(size = 16))

    if(input$spatial_scale_selector == "Provincial"){
      p = p + labs(x = "",
           y = "Proportion of Province (m^2)",
           fill = "")
    } else {
      p = p + labs(x = "",
               y = "Proportion of Regional District (m^2)",
               fill = "")
    }

    if(input$industry_filter == "Forestry") p = p+scale_fill_brewer(palette = 'Greens', na.value = "grey")
    if(input$industry_filter == "Mining") p = p+scale_fill_brewer(palette = 'Purples', na.value = "grey")
    if(input$industry_filter == "Oil and Gas") p = p+scale_fill_brewer(palette = 'Oranges', na.value = "grey")

    p
  })

  output$regdist_fig = renderUI({

  })
  output$test_text = renderText({
    paste0("regdist_figs/",
           regDistSumms()$industry_name_key,
           "_",
           regDistSumms()$ADMIN_AREA_NAME,
           ".jpeg")
  })
}

shinyApp(ui = ui, server = server)
