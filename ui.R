#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# library(shiny)

# Define UI for application that draws a histogram
# shinyUI(fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#             plotOutput("distPlot")
#         )
#     )
# ))


shinyUI(dashboardPage(
    dashboardHeader(title = 'Murder Statistics'),
    dashboardSidebar(
        sidebarUserPanel(name="Question"),
        sidebarMenu(
            menuItem(text = 'Where?',tabName = 'where', icon = icon('map')),
            menuItem(text = 'When?',tabName = 'when', icon = icon('database')),
            menuItem(text = 'Who?',tabName = 'who', icon = icon('database')),
            menuItem(text = 'What Used?',tabName = 'what', icon = icon('database')),
            menuItem(text = 'Who killed whom?',tabName = 'whokilled', icon = icon('database'))
        )
       
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = 'where',
                    # fluidRow(infoBoxOutput("maxBox")),
                    # fluidRow(
                    #     sliderInput(inputId = "year", label = "Set the year:", 
                    #                 min=1980, max=2014, value=c(1980,2014),  step=1,
                    #                 format="###0",animate=TRUE)
                    # ),
                    fluidRow(
                        box(status = "primary", width=3,title = "Inputs",
                            selectizeInput(
                                inputId = 'sel_crime_solved',label='Select Crime solved Item', choices = choice_crime_solved)
                            ), 
                        box(status = "primary",width=5,title="Murders by state",
                            htmlOutput('map')
                            )
                    )
            ),
            tabItem(tabName = 'when',
                    # fluidRow(box(DT::dataTableOutput('table'),
                    #              width=12))
                    fluidRow(
                        box(status = "warning",width=8,
                            htmlOutput('line_peryear')
                            )
                        # box(status = "primary", width=3,
                        #     selectizeInput(
                        #         inputId = 'sel_rank_item',label='Select Item', choices = choice_rank_item)
                        # ) 
                    ),
                    fluidRow(
                        box(status = "warning",width=8,
                            htmlOutput('bar_peryear_crime_solved')
                        )
                    )
            ),
            tabItem(tabName = 'who',
                    # fluidRow(infoBoxOutput("maxBox")),
                    # fluidRow(
                    #     sliderInput(inputId = "year", label = "Set the year:", 
                    #                 min=1980, max=2014, value=c(1980,2014),  step=1,
                    #                 format="###0",animate=TRUE)
                    # ),
                    fluidRow(
                        box(status = "primary",width=6,
                            htmlOutput('pie_victim_perpetrator_sex')
                        )
                    ),
                    
                    fluidRow(
                        box(status = "primary",width=2, title = "Inputs",
                        #     selectizeInput(
                        #         inputId = 'sel_perpetrator_gender',label='Please Select the Perpetrator Gender', choices = choice_gender)
                        # ) 
                            radioButtons(inputId="radio_perpetrator_victim", label = h5("select Perpetrator or Victim"),
                                         choices = list("Perpetrator" = 1, "Victim" = 2), 
                                         selected = 1)
                        ),
                        box(status = "primary",width=7,
                            htmlOutput('box_victim_perpetrator_sex_age2')
                        )
                    )
                    # fluidRow(
                    #     box(status = "primary",width=10,
                    #         plotOutput("box_victim_perpetrator_sex_age")
                    #         #htmlOutput('box_victim_perpetrator_sex_age')
                    #     )
                    # )
            ),
            tabItem(tabName = 'what',
                    # fluidRow(infoBoxOutput("maxBox")),
                    # fluidRow(
                    #     sliderInput(inputId = "year", label = "Set the year:", 
                    #                 min=1980, max=2014, value=c(1980,2014),  step=1,
                    #                 format="###0",animate=TRUE)
                    # ),
                    fluidRow(
                        box(status = "warning", width=3,title = "Inputs",
                             selectizeInput(
                                 inputId = 'sel_perpetrator_gender',label='Please Select the Perpetrator Gender', choices = choice_gender)
                         ) 
                        # , box(status = "primary",width=12,
                        #     htmlOutput('pie_weapon')
                        # )
                    ),
                    fluidRow(
                        box(status = "warning",width=8,
                            htmlOutput('pie_weapon_2')
                        )
                    ),
                    fluidRow(
                        box(status = "warning",width=8,
                            htmlOutput('area_weapon_year')
                        )
                    )
            ),
            tabItem(tabName = 'whokilled',
                    fluidRow(
                        box(status = "primary", width=3,title = "Inputs",
                            selectizeInput(
                                inputId = 'sel_rel_category',label='Please Select the Relationship Category', choices = choice_rel_category)
                        )
                    ),
                    fluidRow(
                          box(status = "primary",width=8,
                             htmlOutput('pie_rel_category')
                         )
                     ),
                    fluidRow(
                         box(status = "primary",width=8,
                             htmlOutput('line_rel_detail')
                         )
                     ),
                    fluidRow(
                        box(status = "primary",width=8,
                            htmlOutput('bar_rel_detail')
                        )
                    )
                    # fluidRow(
                    #     box(status = "primary",width=12,
                    #         htmlOutput('area_weapon_year')
                    #     )
                    # )
            )
        )
    )
))


# shinyUI(dashboardPage(
#     dashboardHeader(title = 'Dashboard'),
#     dashboardSidebar(
#         sidebarUserPanel(name = 'homicide'),
#         sidebarMenu(
#             menuItem(text = 'Map',tabName = 'map', icon = icon('map')),
#             menuItem(text = 'Data',tabName = 'data', icon = icon('database'))
#         ),
#         selectizeInput('selected','Select Item to Display', choice)
#     ),
#     dashboardBody(
#         tabItems(
#             tabItem(tabName = 'map',
#                     # fluidRow(infoBoxOutput("maxBox")),
#                     fluidRow(box(htmlOutput('map'),
#                                  height=300),
#                              box(htmlOutput('hist'),
#                                  height=300))),
#             tabItem(tabName = 'data',
#                     fluidRow(box(DT::dataTableOutput('table'),
#                                  width=12))
#             )
#         )
#     )
