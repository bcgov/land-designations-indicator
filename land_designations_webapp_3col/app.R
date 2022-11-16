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

rm(list = ls())

# Row heights
# my_row_height = '300px'
my_row_height_minimized = '300px'
my_row_height_maximized = '500px'
my_row_width_minimized = '350px'
my_row_width_maximized = '550px'

# Sidebar width
my_sidebar_width = '40%'

## Body designs:

### i. Basic fluidRow and columns.
basic_body = bs4DashBody(width = 5,
                         #3 rows, one per industry, with regional jpeg on left, barplot on right.
                         fluidRow(
                           column(width = 6,
                                  uiOutput('jpeg_fig_forest', height = 'auto')
                           ),
                           column(width = 6,
                                  plotlyOutput('barplot_forest', height = 'auto'))
                         ),
                         fluidRow(
                           column(width = 6,
                                  uiOutput('jpeg_fig_mine', height = 'auto')
                           ),
                           column(width = 6,
                                  plotlyOutput('barplot_mine', height = 'auto'))
                         ),
                         fluidRow(
                           column(width = 6,
                                  uiOutput('jpeg_fig_og', height = 'auto')
                           ),
                           column(width = 6,
                                  plotlyOutput('barplot_og', height = 'auto'))
                         )
)

### ii. Accordion
accordion_body = bs4DashBody(width = 5,
                             #3 rows, one per industry, with regional jpeg on left, barplot on right.
                             bs4Dash::bs4Accordion(
                               id = 'my_acc',
                               bs4Dash::bs4AccordionItem(
                                 title = "Forestry",
                                 crosstalk::bscols(
                                   uiOutput('jpeg_fig_forest'),
                                   plotlyOutput('barplot_forest', height = 'auto')
                                 ),
                                 collapsed = F
                               ),
                               bs4Dash::bs4AccordionItem(
                                 title = "Mining",
                                 crosstalk::bscols(
                                   uiOutput('jpeg_fig_mine'),
                                   plotlyOutput('barplot_mine', height = 'auto')
                                 ),
                                 collapsed = F
                               ),
                               bs4Dash::bs4AccordionItem(
                                 title = "Oil and Gas",
                                 crosstalk::bscols(
                                   uiOutput('jpeg_fig_og'),
                                   plotlyOutput('barplot_og', height = 'auto')
                                 ),
                                 collapsed = F
                               )
                             )
)

### iii. Cards
cards_body = bs4DashBody(width = 5,
                         #3 rows, one per industry, with regional jpeg on left, barplot on right.
                         fluidRow(
                           card(width = '100%',
                                card_header('Forestry Restrictions', class = 'bg-success',
                                            style = 'font-size:25px;'),
                                card_body(
                                  crosstalk::bscols(
                                    uiOutput('jpeg_fig_forest'),
                                    plotlyOutput('barplot_forest', height = 'auto')
                                  )
                                )
                           )
                         ),
                         fluidRow(
                           card(width = '100%',
                                card_header('Mining Restrictions', class = 'bg-info',
                                            style = 'font-size:25px;'),
                                card_body(
                                  crosstalk::bscols(
                                    uiOutput('jpeg_fig_mine'),
                                    plotlyOutput('barplot_mine', height = 'auto')
                                  )
                                )
                           )
                         ),
                         fluidRow(
                           card(width = '100%',
                                card_header("Oil and Gas Restrictions", class = 'bg-danger',
                                            style = 'font-size:25px;'),
                                card_body(
                                  crosstalk::bscols(
                                    uiOutput('jpeg_fig_og'),
                                    plotlyOutput('barplot_og', height = 'auto')
                                  )
                                )
                           )
                         )
)

### iv. bscols from {crosstalk}
bscols_body = bs4DashBody(width = 5,
                          #3 rows, one per industry, with regional jpeg on left, barplot on right.
                          fluidRow(
                            bscols(
                              uiOutput('jpeg_fig_forest', height = 'auto'),
                              plotlyOutput('barplot_forest', height = 'auto')
                            )
                          ),
                          fluidRow(
                            bscols(
                              uiOutput('jpeg_fig_mine', height = 'auto'),
                              plotlyOutput('barplot_mine', height = 'auto')
                            )
                          ),
                          fluidRow(
                            bscols(
                              uiOutput('jpeg_fig_og', height = 'auto'),
                              plotlyOutput('barplot_og', height = 'auto')
                            )
                          )
)

# v. boxes from {bs4Dash}
boxes_body = bs4DashBody(width = 5,
                         #3 rows, one per industry, with regional jpeg on left, barplot on right.
                         fluidRow(
                           box(width = 12,
                               title = "Forestry Restrictions",
                               status = 'success',
                               maximizable = T,
                               crosstalk::bscols(
                                 uiOutput('jpeg_fig_forest'),
                                 plotlyOutput('barplot_forest', height = 'auto')
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
                               title = "Oil and Gas Restrictions",
                               status = 'danger',
                               maximizable = T,
                               crosstalk::bscols(
                                 uiOutput('jpeg_fig_og'),
                                 plotlyOutput('barplot_og', height = 'auto')
                               )
                           )
                         )
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
  )
)

reset_focus_button = div(
  actionButton(
    inputId = 'reset_to_province',
    label = "Reset Selection to Province"
  ),
  style = 'text-align:center;'
)

ui <- bs4Dash::bs4DashPage(
  freshTheme = my_theme,
  header = bs4DashNavbar(
    title = div(
      h4("Regional Districts of BC",style = "color:white;text-align:center;padding-top:20px"),
      h5("Click a Regional District Refine Results",style = "color:white;text-align:center;")
    ),
    sidebarIcon = div(
      tags$i(
        class = "fa-solid fa-magnifying-glass",
        style = "color:#0072B2;font-size:25px",
        title = "Show or hide Regional District map"
      )
    )
  ),
  sidebar = bs4DashSidebar(
    id = 'leaflet_sidebar',
    minified = F,
    leafletOutput('sel_reg_map', height = "650px"),
    column(width = 4, offset = 4,
    reset_focus_button
    )
  ),
  body = boxes_body,
  title = NULL
)

server <- function(input, output) {

  ### Static objects and values

  # Size of BC surface area in square meters.
  bc_size = 941944305881

  # Dataframe of file paths to provincial and regional district images.
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

  # Geopackage (spatial file) of regional districts of BC, with max restriction level data
  # included.
  bc_regs = read_sf('www/bc_regdists.gpkg') %>%
    dplyr::select(ADMIN_AREA_NAME) %>%
    st_transform(crs = 4326)

  bc_reg_dat = read_csv('www/bc_reg_dat.csv')

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
  # Leaflet map to select regional districts

  output$sel_reg_map <- renderLeaflet({

    leaflet(bc_regs) %>%
      addTiles() %>%
      addPolygons(
        layerId = ~ADMIN_AREA_NAME,
        fillColor = ~"Green",
        weight = 2,
        opacity = 0.5,
        color = 'black',
        dashArray = '2',
        fillOpacity = 0.25,
        highlightOptions = highlightOptions(color = "red", weight = 3,
                                            bringToFront = TRUE),
        label = ~ADMIN_AREA_NAME,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "4px 8px"),
          textsize = "15px",
          direction = 'auto')) %>%
      envreportutils::add_bc_home_button() %>%
      envreportutils::set_bc_view()

  })

  click_regdist <- reactiveVal('Provincial')

  observeEvent(input$sel_reg_map_shape_click, {
    # Capture the info of the clicked polygon
    click_regdist(input$sel_reg_map_shape_click$id)
  })

  # If user clicks on 'reset to province' button, reset the selection to 'Provincial'
  observeEvent(input$reset_to_province, {
    click_regdist('Provincial')
  })

  # A subset of the bc_regs object, just with selected regional district.
  bc_reg_dat_selected = reactive({
    # Use the leaflet map to respond to user clicking a region
    # (or setting focus to whole provice) and then filter the data table.
    bc_reg_dat %>%
      filter(ADMIN_AREA_NAME %in% click_regdist())
  })

  # A subset of the jpeg_filepaths_df dataframe, filtered for either provincial-scale
  # figure, or for just the selected regional district (selected with leaflet map)

  jpeg_filepaths_df_selected = reactive({
    jpeg_filepaths_df %>%
      filter(regdist %in% click_regdist())
  })

  # output$test_text = renderText(jpeg_filepaths_df_selected()$regdist)

  output$jpeg_fig_forest = renderUI({
    if(click_regdist() == "Provincial"){
      tags$img(src = paste0('forest_',jpeg_filepaths_df_selected()$image_path),
               height = figureHeight(),
               width = figureWidth())
    } else {
      tags$img(src = paste0("regdist_figs/forest_",
                            jpeg_filepaths_df_selected()$image_path),
               height = figureHeight(),
               width = figureWidth())
    }
  })

  output$jpeg_fig_mine = renderUI({
    if(click_regdist() == "Provincial"){
      tags$img(src = paste0('mine_',jpeg_filepaths_df_selected()$image_path),
               height = figureHeight(),
               width = figureWidth())
    } else {
      tags$img(src = paste0("regdist_figs/mine_",
                            jpeg_filepaths_df_selected()$image_path),
               height = figureHeight(),
               width = figureWidth())
    }
  })

  output$jpeg_fig_og = renderUI({
    if(click_regdist() == "Provincial"){
      tags$img(src = paste0('og_',jpeg_filepaths_df_selected()$image_path),
               height = figureHeight(),
               width = figureWidth())
    } else {
      tags$img(src = paste0("regdist_figs/og_",
                            jpeg_filepaths_df_selected()$image_path),
               height = figureHeight(),
               width = figureWidth())
    }
  })

  output$barplot_forest = renderPlotly({
    ggplotly(
      ggplot(bc_reg_dat_selected() %>%
               filter(industry_name == "forest_restriction_max") %>%
               mutate(Proportion = paste0(round(100*proportional_area,1),"%")),
             aes(label = Proportion)) +
        geom_col(aes(x = max_restriction_value,
                     y = proportional_area,
                     fill = max_restriction_value)) +
        coord_flip() +
        scale_fill_brewer(palette = 'Greens', direction = -1) +
        scale_y_continuous(labels = scales::label_percent()) +
        labs(x = "Restriction Level",
             y = "Proportional Area (m<sup>2</sup>)") +
        theme_bw() +
        theme(axis.text = element_text(size = 12),
              axis.title.y = element_text(size = 16),
              legend.position = 'none',
              axis.title.x = element_markdown(size = 16)),
      tooltip = 'label',
      height = as.numeric(str_extract(figureHeight(), '[0-9]*')))
  })

  output$barplot_mine = renderPlotly({
    ggplotly(
      ggplot(bc_reg_dat_selected() %>%
               filter(industry_name == "mine_restriction_max")  %>%
               mutate(Proportion = paste0(round(100*proportional_area,1),"%")),
             aes(label = Proportion)) +
        geom_col(aes(x = max_restriction_value,
                     y = proportional_area,
                     fill = max_restriction_value)) +
        coord_flip() +
        scale_fill_brewer(palette = 'Purples', direction = -1) +
        labs(x = "Restriction Level",
             y = "Proportional Area (m<sup>2</sup>)") +
        theme_bw() +
        theme(axis.text = element_text(size = 16),
              axis.title.y = element_text(size = 16),
              legend.position = 'none',
              axis.title.x = element_markdown(size = 16)),
      tooltip = 'label',
      height = as.numeric(str_extract(figureHeight(), '[0-9]*')))
  })

  output$barplot_og = renderPlotly({
    ggplotly(
      ggplot(bc_reg_dat_selected() %>%
               filter(industry_name == "og_restriction_max") %>%
               mutate(Proportion = paste0(round(100*proportional_area,1),"%")),
             aes(label = Proportion)) +
        geom_col(aes(x = max_restriction_value,
                     y = proportional_area,
                     fill = max_restriction_value)) +
        coord_flip() +
        scale_fill_brewer(palette = 'Oranges', direction = -1) +
        labs(x = "Restriction Level",
             y = "Proportional Area (m<sup>2</sup>)") +
        theme_bw() +
        theme(axis.text = element_text(size = 16),
              axis.title.y = element_text(size = 16),
              legend.position = 'none',
              axis.title.x = element_markdown(size = 16)),
      tooltip = 'label',
      height = as.numeric(str_extract(figureHeight(), '[0-9]*')))
  })
}

shinyApp(ui = ui, server = server)




