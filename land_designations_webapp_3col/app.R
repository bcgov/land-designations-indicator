library(crosstalk)
library(shiny)
library(tidyverse)
library(leaflet)
library(ggtext)
library(sf)
library(bslib)
library(bs4Dash)
library(fresh)
library(plotly)
library(shinyjs)

rm(list = ls())

# Plotting function

regdist_barplot = function(dat, industry = NULL, reactive_height = NULL){

  mapping_dat = dat %>%
    filter(industry_name == industry) %>%
    mutate(Proportion = paste0(round(100*proportional_area,1),"%"))

  g = ggplot(mapping_dat,
             aes(label = Proportion)) +
    geom_col(aes(x = max_restriction_value,
                 y = proportional_area,
                 fill = max_restriction_value,
                 col = max_restriction_value),
             size = 1/5) +
    coord_flip() +
    scale_y_continuous(labels = scales::label_percent(),
                       breaks = scales::breaks_pretty(4)) +
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
           height = 1.50*as.numeric(str_extract(reactive_height, '[0-9]*'))) %>%
    config(displayModeBar = F)

}
# ### Box dimensions
# # Tailored to 'wide' layout
# my_row_width_minimized = '350px'
# my_row_width_maximized = '550px'
# my_row_height_minimized = '300px'
# my_row_height_maximized = '500px'


# Tailored to 'long' layout
my_row_width_minimized = '235px'
my_row_width_maximized = '300px'
my_row_height_minimized = paste0((6/7)*as.numeric(str_remove(my_row_width_minimized,'px')),"px")
my_row_height_maximized = paste0((6/7)*as.numeric(str_remove(my_row_width_maximized,'px')),"px")

# Sidebar width
my_sidebar_width = '30%'
my_box_content_width = '115%'

# Buttons and inputs
reset_focus_button = div(
  actionButton(
    inputId = 'reset_to_province',
    label = "Reset Selection to Province"
  ),
  style = 'display:inline;'
)

## Body design:

# Boxes from {bs4Dash} (hamburger style)
boxes_body_wide = bs4DashBody(width = 5,
                              useShinyjs(),
                              div(textOutput('region_selected_text'),
                                  style = 'text-align:center;font-size:20px;height:20px'),
                              #3 rows, one per industry, with regional jpeg on left, barplot on right.
                              fluidRow(
                                box(width = 12,
                                    title = "Forestry Restrictions",
                                    status = 'success',
                                    maximizable = T,
                                    crosstalk::bscols(
                                      uiOutput('jpeg_fig_forest'),
                                      plotlyOutput('barplot_forest')
                                    )
                                )
                              ),
                              fluidRow(
                                box(width = 12,
                                    title = 'Mining Restrictions',
                                    status = 'info',
                                    maximizable = T,
                                    crosstalk::bscols(
                                      uiOutput('jpeg_fig_mine'),
                                      plotlyOutput('barplot_mine', height = 'auto')
                                    )
                                )
                              ),
                              fluidRow(
                                box(width = 12,
                                    title = "Oil/Gas Restrictions",
                                    status = 'danger',
                                    maximizable = T,
                                    crosstalk::bscols(
                                      uiOutput('jpeg_fig_og'),
                                      plotlyOutput('barplot_og', height = 'auto')
                                    )
                                )
                              )
)


# Boxes from {bs4Dash} (hotdog style)
boxes_body_long = bs4DashBody(width = 5,
                              style = 'div.col-sm-6 {padding:1px}',
                              useShinyjs(),
                              div(textOutput('region_selected_text'),
                                  style = 'text-align:center;font-size:26px;font-family:bold;padding:10px;'),
                              #3 rows, one per industry, with regional jpeg on left, barplot on right.
                              column(width = 4,
                                     box(width = 12,
                                         title = "Forestry Restrictions",
                                         status = 'success',
                                         maximizable = F,
                                         collapsible = F,
                                         crosstalk::bscols(
                                           list(
                                             uiOutput('jpeg_fig_forest'),
                                             plotlyOutput('barplot_forest', height = 'auto', width = my_box_content_width)
                                           )
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
                                         crosstalk::bscols(
                                           list(
                                             uiOutput('jpeg_fig_og'),
                                             plotlyOutput('barplot_og', height = 'auto', width = my_box_content_width)
                                           )
                                         )
                                     )
                              )
)

my_theme = create_theme(
  bs4dash_status(
    success = '#28a745',
    info = '#6f42c1',
    danger = '#F77408'
    # lightblue = '#428bca'
  ),
  bs4dash_layout(
    sidebar_width = my_sidebar_width,
    screen_header_collapse = 1
  ),
  bs4dash_button(
    # default_background_color = '#428bca'
    default_background_color = '#3c8dbc'
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
      h5("Click to show/hide district map", style = 'display:inline;')
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
    h5("Click and drag to move map",style = "color:white;text-align:center;padding-top:15px"),
    column(width = 10,offset = 2)
  ),
  body = boxes_body_long,
  title = NULL
)

server <- function(input, output) {

  ### Static objects and values

  # Number of unique designations per region.
  num_des_per_reg = read_csv('www/number_designations_per_region.csv')

  # Dataframe of file paths to provincial and district images.
  jpeg_filepaths_df = data.frame(image_path = list.files(path = 'www/regdist_figs',
                                                         pattern = '^forest.*')) %>%
    mutate(image_path = str_remove(image_path, '^forest_')) %>%
    mutate(regdist = str_extract(image_path, "(?<=max_).*(?=\\.jpeg)")) %>%
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

  # Add number of unique designations to the bc_regs spatial object.
  bc_regs = bc_regs %>%
    left_join(num_des_per_reg)

  bc_reg_dat = read_csv('www/bc_reg_dat.csv') %>%
    mutate(max_restriction_value = factor(max_restriction_value,
                                          levels = c("Full","High","Medium","Low","None")))

  ### Reactive entities

  # Figure dimensions, determined in part by whether or not the sidebar is collapsed.
  figureHeight = reactive({
    if(input$leaflet_sidebar){
      my_row_height_minimized
    } else {my_row_height_maximized}
  })

  figureWidth = reactive({
    if(input$leaflet_sidebar){
      my_row_width_minimized
    } else {my_row_width_maximized}
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
      envreportutils::set_bc_view()

  })

  click_regdist <- reactiveVal('Provincial')

  # Watch for a click on the leaflet map. Once clicked...
  observeEvent(input$sel_reg_map_shape_click, {
    # Capture the info of the clicked polygon. We use this for filtering.
    click_regdist(input$sel_reg_map_shape_click$id)
    # Toggle the sidebar to be closed.
    addClass(selector = "body", class = "sidebar-collapse")
  })

  # If user clicks on 'reset to province' button, reset the selection to 'Provincial'
  observeEvent(input$reset_to_province, {
    click_regdist('Provincial')
  })

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

  # output$test_text = renderText(jpeg_filepaths_df_selected()$regdist)

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
                    reactive_height = figureHeight())
  })

  output$barplot_mine = renderPlotly({

    regdist_barplot(bc_reg_dat_selected(),
                    industry = 'mine_restriction_max',
                    reactive_height = figureHeight())
  })

  output$barplot_og = renderPlotly({

    regdist_barplot(bc_reg_dat_selected(),
                    industry = 'og_restriction_max',
                    reactive_height = figureHeight())
  })
}

shinyApp(ui = ui, server = server)




