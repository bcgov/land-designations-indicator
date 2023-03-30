library(tidyverse)
library(crosstalk)
library(shiny)
library(tidyr)
library(DT)
library(sf)
library(leaflet)
library(ggtext)
library(bs4Dash)
library(fresh)
library(plotly)
library(shinyjs)
library(RColorBrewer)
library(scales)

rm(list = ls())

### Static layout settings ###

# Row width and height
my_row_width_minimized = '235px'
my_row_width_maximized = '300px'
my_row_height_minimized = paste0((6/7)*as.numeric(str_remove(my_row_width_minimized,'px')),"px")
my_row_height_maximized = paste0((6/7)*as.numeric(str_remove(my_row_width_maximized,'px')),"px")

# Sidebar width
my_sidebar_width = '35%'
my_box_content_width = '115%'

# Plotting function

regdist_barplot = function(dat, industry = NULL, reactive_height = NULL, number_ticks = 4){

  mapping_dat = dat %>%
    filter(industry_name == industry) %>%
    mutate(Proportion = paste0(round(100*proportional_area,1),"%"))

  g = ggplot(mapping_dat,
             aes(label = Proportion)) +
    geom_col(aes(x = max_restriction_value,
                 y = proportional_area,
                 fill = max_restriction_value,
                 col = max_restriction_value),
             linewidth = 1/5) +
    coord_flip() +
    scale_y_continuous(labels = scales::label_percent(),
                       breaks = scales::breaks_pretty(n = number_ticks)) +
    labs(x = "Restriction Level",
         y = "Percent of <br>Area") +
    theme_bw() +
    theme(axis.text = element_text(size = 10),
          axis.title.y = element_text(size = 12),
          axis.title.x = element_markdown(size = 12),
          legend.position = 'none')

  # Number of restriction levels (this varies!)
  num_levels = length(unique(mapping_dat$max_restriction_value))

  # Colour palette (conditional)
  my_palette = case_when(
    str_detect(industry, 'forest.*') ~ 'Greens',
    str_detect(industry, 'mine_.*') ~ 'Purples',
    str_detect(industry, 'og_.*') ~ 'Oranges'
  )

  my_fill_scale = rev(RColorBrewer::brewer.pal(n = num_levels, my_palette))
  my_fill_scale[num_levels] = 'white'
  my_color_scale = c(rep('white',num_levels-1),'black')

  g = g + scale_fill_manual(values = my_fill_scale) +
    scale_color_manual(values = my_color_scale)

  ggplotly(g,
           tooltip = 'label',
           # height = '100%') %>%
           height = 1.40*as.numeric(str_extract(reactive_height, '[0-9]*'))) %>%
    config(displayModeBar = F)
}


## Buttons and inputs

reset_focus_button = div(
  actionButton(
    inputId = 'reset_to_province',
    label = "Reset Selection to Province"
  ),
  style = 'display:inline;'
)

## Body design:

# Three vertical boxes that each display one of the industries (static map) and a plotly barchart.
three_vertical_boxes = fluidRow(
  column(width = 4,
         box(width = 12,
             title = "Forestry Restrictions",
             status = 'success',
             style = '.crosstalk-bscols {box-sizing:content-box}',
             maximizable = F,
             collapsible = F,
             div(
               crosstalk::bscols(
                 list(
                   uiOutput('jpeg_fig_forest'),
                   plotlyOutput('barplot_forest', height = 'auto', width = my_box_content_width)
                 )
               ),
               style = '{box-sizing:content-box}',
             )
         )
  ),
  column(width = 4,
         box(width = 12,
             title = 'Mining Restrictions',
             status = 'info',
             maximizable = F,
             collapsible = F,
             crosstalk::bscols(
               list(
                 uiOutput('jpeg_fig_mine'),
                 plotlyOutput('barplot_mine', height = 'auto', width = my_box_content_width)
               )
             )
         )
  ),
  column(width = 4,
         box(width = 12,
             title = "Oil/Gas Restrictions",
             status = 'danger',
             maximizable = F,
             collapsible = F,
             crosstalk::bscols(
               list(
                 uiOutput('jpeg_fig_og'),
                 plotlyOutput('barplot_og', height = 'auto', width = my_box_content_width)
               )
             )
         )
  )
)

# Table of specific designation areas

designation_table = div(box(width = 12,
                        title = "10 Largest Designations in Area of Focus",
                        maximizable = F,
                        collapsible = T,
                        collapsed = T,
                        div(
                          DTOutput('desig_table'))
                       )
)

# Body of app using {bs4Dash} (hotdog style)
boxes_body_long = bs4DashBody(width = 5,
                              useShinyjs(),
                              # The following script allows us to manually change the input value
                              # that tracks whether the sidebar is open or closed. Useful!
                              tags$script("
    Shiny.addCustomMessageHandler('leaflet_sidebar', function(value) {
    Shiny.setInputValue('leaflet_sidebar', value);
    });"),
                              div(textOutput('region_selected_text'),
                                  style = 'text-align:center;font-size:26px;font-family:bold;padding:10px;'),
                              # 3 rows, one per industry, with regional jpeg on left, barplot on right. Table below.
                              three_vertical_boxes,
                              designation_table
)

my_theme = create_theme(
  bs4dash_status(
    success = '#28a745',
    info = '#6f42c1',
    danger = '#F77408'
  ),
  bs4dash_layout(
    sidebar_width = my_sidebar_width,
    screen_header_collapse = 1
  ),
  bs4dash_button(
    default_background_color = '#e6e9ed'
  )
)

ui <- bs4Dash::bs4DashPage(
  freshTheme = my_theme,
  dark = NULL,
  header = bs4DashNavbar(
    title = div(
      h5("Click a District to Refine Results",style = "color:white;text-align:center;padding-top:15px")
    ),
    sidebarIcon = div(
      img(src = 'bc_silhouette2.png', style = 'width:35px;'),
      h5("Click to Show/Hide District Map", style = 'display:inline;')
    ),
    rightUi = tags$li(class = 'dropdown',
                      reset_focus_button
    )
  ),
  # HTML('<br>'), #class = 'nav-item'),
  sidebar = bs4DashSidebar(
    id = 'leaflet_sidebar',
    minified = F,
    leafletOutput('sel_reg_map', height = "650px"),
    h5("Click and Drag to Move Map",style = "color:white;text-align:center;padding-top:15px"),
    column(width = 10,offset = 2)
  ),
  body = boxes_body_long,
  title = NULL
)

server <- function(input, output, session) {

  ### Static objects and values

  # Area of largest 10 designations for each natural resource district-restriction type-restriction level combination.
  area_per_des = read_csv('www/overlapping_land_designations_with_district.csv') %>%
    mutate(Designation = name) %>%
    dplyr::select(-name)

  # Dataframe of file paths to provincial and district images.
  jpeg_filepaths_df = data.frame(image_path = list.files(path = 'www/regdist_figs',
                                                         pattern = '^forest.*')) %>%
    mutate(image_path = str_remove(image_path, '^forest_')) %>%
    mutate(regdist = str_extract(image_path, "(?<=max_).*(?=\\.jpeg)")) %>%
    #Add 3 rows to table, one for each provincial-scale jpeg.
    bind_rows(
      data.frame(
        image_path = "restriction_plot.jpeg",
        regdist = 'Provincial'
      )
    ) %>%
    as_tibble()

  # Geopackage (spatial file) of districts of BC, with max restriction level data
  # included.
  bc_regs = read_sf('www/bc_regdists.gpkg') %>%
    dplyr::select(DISTRICT_NAME) %>%
    st_transform(crs = 4326)

  bc_reg_dat = read_csv('www/bc_reg_dat.csv') %>%
    mutate(max_restriction_value = factor(max_restriction_value,
                                          levels = c("Full","High","Medium","Low","None")))

  ### Reactive entities

  # Status of the sidebar. This is a workaround, as we are not able to directly
  # set the input$leaflet_sidebar. So, this reactive entity will keep track for us.
  manual_sidebar_tracker = reactiveVal(value = 'open')

  # Reactive number of x-axis ticks and labels for plotly figures;
  # responds to whether or not the sidebar is open.
  numberTicks = reactive({
    if(input$leaflet_sidebar){
      2
    }
    else {
      4
    }
  })

  # Leaflet map to select districts

  output$sel_reg_map <- renderLeaflet({

    leaflet(bc_regs) %>%
      addTiles() %>%
      addPolygons(
        layerId = ~DISTRICT_NAME,
        fillColor = ~"Green",
        weight = 2,
        opacity = 0.5,
        color = 'black',
        dashArray = '2',
        fillOpacity = 0.25,
        highlightOptions = highlightOptions(color = "red", weight = 3,
                                            bringToFront = TRUE),
        label = ~DISTRICT_NAME,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "4px 8px"),
          textsize = "15px",
          direction = 'auto')) %>%
      envreportutils::add_bc_home_button() %>%
      envreportutils::set_bc_view(zoom = 5)
  })

  # Set up a reactive value that stores a district's name upon user's click
  click_regdist <- reactiveVal('Provincial')

  # Watch for a click on the leaflet map. Once clicked...

  # 1. Update Leaflet map.
  observeEvent(input$sel_reg_map_shape_click, {
    # Capture the info of the clicked polygon. We use this for filtering.
    click_regdist(input$sel_reg_map_shape_click$id)
    # Toggle the sidebar to be closed. Note: this skips the actual input$leaflet_sidebar variable.
    addClass(selector = "body", class = "sidebar-collapse")
    session$sendCustomMessage("leaflet_sidebar", 'FALSE')
    manual_sidebar_tracker('closed')
  })

  # 2. (Create and) update table.
  designations_for_table = reactive({
    if(click_regdist() == "Provincial"){
      #When the focus is provincial, only keep the 10 largest designations
      # by industry type. This step already done for each district in analysis script.
      area_per_des %>%
        filter(`District Name` == "Provincial") %>%
        ungroup() %>%
        distinct() %>%
        arrange(desc(`Area (km²)`)) %>%
        dplyr::select(-`District Name`,-`Area (km²)`) %>%
        slice(1:10)
    } else if(click_regdist() != "Provincial"){
      area_per_des %>%
        filter(`District Name` %in% click_regdist()) %>%
        ungroup() %>%
        distinct() %>%
        arrange(desc(`Area (km²)`)) %>%
        dplyr::select(-`District Name`,-`Area (km²)`) %>%
        slice(1:10)
    }
  })

  output$desig_table <- renderDT(
    designations_for_table()
  )

  # If user clicks on 'reset to province' button, reset the selection to 'Provincial'
  observeEvent(input$reset_to_province, {
    click_regdist('Provincial')
  })

  # Make a label out of the spatial focus (either provincial or some district's name)
  output$region_selected_text = renderText({
    spatial_focus = click_regdist()
    #If the spatial focus is 'Provincial', change to 'Province'
    if(spatial_focus == "Provincial")spatial_focus = 'British Columbia'
    else spatial_focus
  })

  # A subset of the bc_regs object, just with selected district.
  bc_reg_dat_selected = reactive({
    # Use the leaflet map to respond to user clicking a region
    # (or setting focus to whole provice) and then filter the data table.
    bc_reg_dat %>%
      filter(DISTRICT_NAME %in% click_regdist())
  })

  # A subset of the jpeg_filepaths_df dataframe, filtered for either provincial-scale
  # figure, or for just the selected district (selected with leaflet map)

  jpeg_filepaths_df_selected = reactive({
    jpeg_filepaths_df %>%
      filter(regdist %in% click_regdist())
  })

  output$jpeg_fig_forest = renderUI({
    if(click_regdist() == "Provincial"){
      tags$img(src = paste0('forest_',jpeg_filepaths_df_selected()$image_path),
               width = my_box_content_width)
    } else {
      tags$img(src = paste0("regdist_figs/forest_",
                            jpeg_filepaths_df_selected()$image_path),
               width = my_box_content_width)
    }
  })

  output$jpeg_fig_mine = renderUI({
    if(click_regdist() == "Provincial"){
      tags$img(src = paste0('mine_',jpeg_filepaths_df_selected()$image_path),
               width = my_box_content_width)
    } else {
      tags$img(src = paste0("regdist_figs/mine_",
                            jpeg_filepaths_df_selected()$image_path),
               width = my_box_content_width)
    }
  })

  output$jpeg_fig_og = renderUI({
    if(click_regdist() == "Provincial"){
      tags$img(src = paste0('og_',jpeg_filepaths_df_selected()$image_path),
               width = my_box_content_width)
    } else {
      tags$img(src = paste0("regdist_figs/og_",
                            jpeg_filepaths_df_selected()$image_path),
               width = my_box_content_width)
    }
  })

  output$barplot_forest = renderPlotly({

    regdist_barplot(bc_reg_dat_selected(),
                    industry = 'forest_restriction_max',
                    reactive_height = my_row_height_minimized,
                    number_ticks = numberTicks())
  })

  output$barplot_mine = renderPlotly({

    regdist_barplot(bc_reg_dat_selected(),
                    industry = 'mine_restriction_max',
                    reactive_height = my_row_height_minimized,
                    number_ticks = numberTicks())
  })

  output$barplot_og = renderPlotly({

    regdist_barplot(bc_reg_dat_selected(),
                    industry = 'og_restriction_max',
                    reactive_height = my_row_height_minimized,
                    number_ticks = numberTicks())
  })
}

shinyApp(ui = ui, server = server)
