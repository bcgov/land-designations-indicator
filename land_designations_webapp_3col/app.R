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
# library(DT)

rm(list = ls())

# Row heights
my_row_height = '300px'

# Sidebar width
my_sidebar_width = '35%'

## Body designs:

### i. Basic fluidRow and columns.
basic_body = bs4DashBody(width = 5,
                         #3 rows, one per industry, with regional jpeg on left, barplot on right.
                         fluidRow(
                           column(width = 6,
                                  uiOutput('jpeg_fig_forest', height = my_row_height)
                           ),
                           column(width = 6,
                                  plotlyOutput('barplot_forest', height = my_row_height))
                         ),
                         fluidRow(
                           column(width = 6,
                                  uiOutput('jpeg_fig_mine', height = my_row_height)
                           ),
                           column(width = 6,
                                  plotlyOutput('barplot_mine', height = my_row_height))
                         ),
                         fluidRow(
                           column(width = 6,
                                  uiOutput('jpeg_fig_og', height = my_row_height)
                           ),
                           column(width = 6,
                                  plotlyOutput('barplot_og', height = my_row_height))
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
                                   plotlyOutput('barplot_forest', height = my_row_height)
                                 ),
                                 collapsed = F
                               ),
                               bs4Dash::bs4AccordionItem(
                                 title = "Mining",
                                 crosstalk::bscols(
                                   uiOutput('jpeg_fig_mine'),
                                   plotlyOutput('barplot_mine', height = my_row_height)
                                 ),
                                 collapsed = F
                               ),
                               bs4Dash::bs4AccordionItem(
                                 title = "Oil and Gas",
                                 crosstalk::bscols(
                                   uiOutput('jpeg_fig_og'),
                                   plotlyOutput('barplot_og', height = my_row_height)
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
                                    plotlyOutput('barplot_forest', height = my_row_height)
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
                                    plotlyOutput('barplot_mine', height = my_row_height)
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
                                    plotlyOutput('barplot_og', height = my_row_height)
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
                              uiOutput('jpeg_fig_forest', height = my_row_height),
                              plotlyOutput('barplot_forest', height = my_row_height)
                            )
                          ),
                          fluidRow(
                            bscols(
                              uiOutput('jpeg_fig_mine', height = my_row_height),
                              plotlyOutput('barplot_mine', height = my_row_height)
                            )
                          ),
                          fluidRow(
                            bscols(
                              uiOutput('jpeg_fig_og', height = my_row_height),
                              plotlyOutput('barplot_og', height = my_row_height)
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
                               crosstalk::bscols(
                                 uiOutput('jpeg_fig_forest'),
                                 plotlyOutput('barplot_forest', height = my_row_height)
                             )
                           )
                         ),
                         fluidRow(
                           box(width = 12,
                               title = 'Mining Restrictions',
                               status = 'info',
                               crosstalk::bscols(
                                 uiOutput('jpeg_fig_mine'),
                                 plotlyOutput('barplot_mine', height = my_row_height)
                             )
                           )
                         ),
                         fluidRow(
                           box(width = 12,
                             title = "Oil and Gas Restrictions",
                             status = 'danger',
                               crosstalk::bscols(
                                 uiOutput('jpeg_fig_og'),
                                 plotlyOutput('barplot_og', height = my_row_height)
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
    sidebar_width = my_sidebar_width
  )
)

ui <- bs4Dash::bs4DashPage(
  freshTheme = my_theme,
  header = bs4DashNavbar(h4("Show/Hide Province Map")),
  sidebar = bs4DashSidebar(
    # height = 1000,
    minified = F,
    leafletOutput('sel_reg_map'),
    actionButton(
      inputId = 'reset_to_province',
      label = "Reset Selection to Province"
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

  # Leaflet map to select regional districts

  output$sel_reg_map <- renderLeaflet({

    leaflet(bc_regs) %>%
      addTiles() %>%
      addPolygons(
        layerId = ~ADMIN_AREA_NAME,
        fillColor = ~"Green",
        weight = 2,
        opacity = 0.75,
        color = 'black',
        dashArray = '2',
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(color = "red", weight = 3,
                                            bringToFront = TRUE),
        label = ~ADMIN_AREA_NAME,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "4px 8px"),
          textsize = "15px",
          direction = 'auto')) %>%
      envreportutils::add_bc_home_button()
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
               height = my_row_height)
    } else {
      tags$img(src = paste0("regdist_figs/forest_",
                            jpeg_filepaths_df_selected()$image_path),
               width = my_row_height)
    }
  })

  output$jpeg_fig_mine = renderUI({
    if(click_regdist() == "Provincial"){
      tags$img(src = paste0('mine_',jpeg_filepaths_df_selected()$image_path),
               height = my_row_height)
    } else {
      tags$img(src = paste0("regdist_figs/mine_",
                            jpeg_filepaths_df_selected()$image_path),
               width = my_row_height)
    }
  })

  output$jpeg_fig_og = renderUI({
    if(click_regdist() == "Provincial"){
      tags$img(src = paste0('og_',jpeg_filepaths_df_selected()$image_path),
               height = my_row_height)
    } else {
      tags$img(src = paste0("regdist_figs/og_",
                            jpeg_filepaths_df_selected()$image_path),
               width = my_row_height)
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
    tooltip = 'label')
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
    tooltip = 'label')
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
    tooltip = 'label')
  })
}

shinyApp(ui = ui, server = server)




