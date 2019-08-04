#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# library(shiny)
# 
# # Define server logic required to draw a histogram
# shinyServer(function(input, output) {
# 
#     output$distPlot <- renderPlot({
# 
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
# 
#     })
# 
# })

shinyServer(
    function(input, output){
        
        ### 1st tabName : where -> state #### 
        selected_crime_solved <- reactive(
            input$sel_crime_solved
            #df_state_homicide[df_state_homicide$crime.solved == input$sel_crime_solved, ] 
        )
        # selected_year <- reactive({
        #     input$year[1]
        #     input$year[2]
        # })
        
            
       
        output$map = renderGvis({
            # if(grep(selected_crime_solved(),'ratio')){
            if(!selected_crime_solved() %in% c('Yes','No','both')){    
                df_state_homicide_tmp <- df_state_ratio_homicide
                colorvar = "ratio"
                colors = "['#FFFFFF','#1857D5']"
            }else{
                df_state_homicide_tmp <- df_state_homicide[df_state_homicide$crime.solved == selected_crime_solved(), ]
                colorvar = "count"
                colors =  "['#EEE4E4','#F29D9D','#9E0A0A']"
                
            }
            gvisGeoChart(data = df_state_homicide_tmp, locationvar = "state",  colorvar = colorvar,
                                            options=list(region="US", displayMode="regions",
                                       resolution="provinces", title="Tallest Building Count by Country"
                                       #, colors="['#cbb69d', '#603913', '#c69c6e']"
                                       #, colors= "[\'white\',\'pink\',\'red']"
                                       , colors= colors
                                       # ,width="600", height="400"
                                       )
             )
         })
        # Merge 합치기
        # G <- gvisGeoChart(Exports, "Country", "Profit", 
        #                   options=list(width=300, height=300))
        # T <- gvisTable(Exports, 
        #                options=list(width=220, height=300))
        # 
        # GT <- gvisMerge(G,T, horizontal=TRUE) 
        # plot(GT)
        
        
        ### 2nd tabName : when -> year #### 
       
        output$line_peryear = renderGvis({
            gvisLineChart(data = df_year_all, xvar="year", yvar="count"
                         , options=list( title = 'Number of homicide in USA per year ',
                                         chartArea= "{width: '70%'}",
                                         hAxis = "{
                                             title: 'Year',
                                               format: '####',
                                          }",
                                         vAxis  = "{
                                              title: 'Number of incident',
                                              minValue: 0
                                          }",
                                         pointSize= 5
                                         #                series="[{color:'green', targetAxisIndex: 0, 
                                         # lineWidth: 1, lineDashStyle: [2, 2, 20, 2, 20, 2]}, 
                                         # {color: 'blue',targetAxisIndex: 1, 
                                         # lineWidth: 2, lineDashStyle: [4, 1]}]",
                                         #                vAxes="[{title:'val1'}, {title:'val2'}]"
                         )
            )
        })
        
        output$bar_peryear_crime_solved = renderGvis({
            # if(grepl("Top",selected_rank_item())){    
            #     df_peryear_homicide_tmp <- df_year_state_top5_homicide
            # }else if(grepl("Bottom",selected_rank_item())){
            #     df_state_homicide_tmp <- df_year_state_bottom5_homicide
            # }else{
            #     df_peryear_homicide_tmp <- df_year_all_homicide
            # }
            gvisColumnChart(data = df_year_crime_solved, xvar="year", yvar=c("solved","notSolved")
                         , options=list( isStacked=TRUE , 
                                         title = 'Number of homicide in USA per year ',
                                         chartArea= "{width: '70%'}",
                                         colors="['#9575cd', '#33ac71']",
                                         hAxis= "{
                                          title: 'Year',
                                               format: '####'
                                          }",
                                         vAxis = "{
                                              title: 'Number of incident',
                                              minValue: 0
                                          }",
                                         bar="{groupWidth:'70%'}",
                                         height = 400
                                         #                series="[{color:'green', targetAxisIndex: 0, 
                                         # lineWidth: 1, lineDashStyle: [2, 2, 20, 2, 20, 2]}, 
                                         # {color: 'blue',targetAxisIndex: 1, 
                                         # lineWidth: 2, lineDashStyle: [4, 1]}]",
                                         #                vAxes="[{title:'val1'}, {title:'val2'}]"
                         )
            )
        })
        ### 3rd tabName : who  #### 
        
        output$pie_victim_perpetrator_sex = renderGvis({
            
            # if(grepl("Top",selected_rank_item())){    
            #     df_peryear_homicide_tmp <- df_year_state_top5_homicide
            # }else if(grepl("Bottom",selected_rank_item())){
            #     df_state_homicide_tmp <- df_year_state_bottom5_homicide
            # }else{
            #     df_peryear_homicide_tmp <- df_year_all_homicide
            # }
            #gvisPieChart(data, labelvar = "", numvar = "", options = list(), chartid)
            gvisPieChart(data = df_victim_perpetrator_sex, labelvar="who", numvar="count"
                         , options=list( title = 'Who killed by who (Gender)?'
                                         #,is3D= TRUE
                                         ,width="600", height="300",
                                         pieHole= 0.3
                                         # vAxes="[{title:'val1'}, {title:'val2'}]"
                                         # colors="['#cbb69d', '#603913', '#c69c6e']"
                         )
            )
        })
        selected_radio_perpetrator_victim <- reactive(
            input$radio_perpetrator_victim
            #df_state_homicide[df_state_homicide$crime.solved == input$sel_crime_solved, ] 
        )
        
        
        #gvisCandlestickChart(data, xvar = "", low = "", open = "", close = "", high = "", options = list(), chartid)
        output$box_victim_perpetrator_sex_age2 = renderGvis({
            if(selected_radio_perpetrator_victim() == 1){ #perpetrator 
                colnum =  c("Male Perpetrator","Female Perpetrator")
                seriesVal = "[{color: 'green', lineWidth: 2, lineDashStyle: [2, 2, 20, 2, 20, 2]}, 
                                                  {color: 'purple',
                                                   lineWidth: 2, lineDashStyle: [4, 1]}]"
            }else{ #victim
                colnum = c("Male Victim","Female Victim")
                seriesVal = "[{color: 'blue', lineWidth: 2, lineDashStyle: [2, 2, 20, 2, 20, 2]}, 
                                                  {color: 'red',
                                                   lineWidth: 2, lineDashStyle: [4, 1]}]"
            }
            gvisLineChart(data = df_victim_perpetrator_age_linegraph, xvar="age", yvar=colnum
                          , options=list( title = 'Number of male, female by age ',
                                          chartArea= "{width: '70%'}",
                                          hAxis = "{
                                             title: 'Age',
                                               format: '###',
                                          }",
                                          vAxis  = "{
                                              title: 'Count',
                                              minValue: 0
                                          }"
                                          , height = 300
                                          , series= seriesVal 
                                          #                vAxes="[{title:'val1'}, {title:'val2'}]"
                          )
            )
        })
        #box plot ....
        # output$box_victim_perpetrator_sex_age = renderPlot({
        #     ggplot(data = df_victim_perpetrator_age, aes(x = gender_victim_perpetrator, y = age)) + geom_boxplot()
        # })
        
        ### 4th tabName : what -> weapon #### 
        
        selected_perpetrator_gender <- reactive(
            input$sel_perpetrator_gender
        )
        # output$pie_weapon = renderGvis({
        #     df_weapon_perpetrator_tmp <-  
        #         df_weapon_perpetrator_all[df_weapon_perpetrator_all$perpetrator.sex ==  selected_perpetrator_gender(), ] %>%  
        #         select(weapon,count)
        #     
        #     print(df_weapon_perpetrator_tmp)
        #     # if(grepl("Top",selected_rank_item())){    
        #     #     df_peryear_homicide_tmp <- df_year_state_top5_homicide
        #     # }else if(grepl("Bottom",selected_rank_item())){
        #     #     df_state_homicide_tmp <- df_year_state_bottom5_homicide
        #     # }else{
        #     #     df_peryear_homicide_tmp <- df_year_all_homicide
        #     # }
        #     #gvisPieChart(data, labelvar = "", numvar = "", options = list(), chartid)
        #     gvisPieChart(data = df_weapon_perpetrator_tmp, labelvar="weapon", numvar="count"
        #                   , options=list( title = 'What kind of weapon does the perpetrator use?'
        #                                   ,is3D= TRUE,
        #                                   width="800", height="400"
        #                                   # vAxes="[{title:'val1'}, {title:'val2'}]"
        #                                   # colors="['#cbb69d', '#603913', '#c69c6e']"
        #                   )
        #     )
        # })
        # 
        
      
            
        output$pie_weapon_2 = renderGvis({
            
            df_weapon_perpetrator_tmp2 <-  
                df_weapon_perpetrator_all_gunGroup[df_weapon_perpetrator_all_gunGroup$perpetrator.sex ==  selected_perpetrator_gender(), ] %>%  
                select(weapon,count)
            
            
            print(df_weapon_perpetrator_tmp2)
            # if(grepl("Top",selected_rank_item())){    
            #     df_peryear_homicide_tmp <- df_year_state_top5_homicide
            # }else if(grepl("Bottom",selected_rank_item())){
            #     df_state_homicide_tmp <- df_year_state_bottom5_homicide
            # }else{
            #     df_peryear_homicide_tmp <- df_year_all_homicide
            # }
            #gvisPieChart(data, labelvar = "", numvar = "", options = list(), chartid)
            gvisPieChart(data = df_weapon_perpetrator_tmp2, labelvar="weapon", numvar="count"
                         , options=list( title = 'What kind of weapon does the perpetrator use?'
                                         ,is3D= TRUE,
                                         width="800", height="400"
                                         # vAxes="[{title:'val1'}, {title:'val2'}]"
                                         # colors="['#cbb69d', '#603913', '#c69c6e']"
                         )
            )
        })
        
        
        #Area <- gvisAreaChart(df)
        #plot(Area)
        output$area_weapon_year = renderGvis({
            
            df_weapon_year_perpetrator_tmp <-  
                df_weapon_year_all_graph[df_weapon_year_all_graph$perpetrator.sex ==  selected_perpetrator_gender(), ] 
            print(df_weapon_year_perpetrator_tmp)
           
            #gvisAreaChart(data, xvar = "", yvar = "", options = list(), chartid)
            gvisAreaChart(data = df_weapon_year_perpetrator_tmp, xvar="year", yvar=c("Blunt Object", "Drowning", "Drugs", "Explosives", "Fall", "Fire", "Gun", "Knife", "Poison", "Strangulation", "Suffocation", "Unknown")
                         , options=list( title = 'Use of perpetrator weapons by year'
                                         ,is3D= TRUE,
                                         width="800", height="400",
                                         hAxis= "{
                                          title: 'Year',
                                               format: '####'
                                          }",
                                         vAxis = "{
                                              title: 'Number of weapon',
                                              minValue: 0
                                          }",
                                         with=600
                                         # vAxes="[{title:'val1'}, {title:'val2'}]"
                                         # colors="['#cbb69d', '#603913', '#c69c6e']"
                         )
            )
        })
        
        
        ### 5th tabName : who killed who? -> realationship ####
        
        selected_rel_category <- reactive(
            input$sel_rel_category
        )
        
        output$pie_rel_category = renderGvis({
            
            # df_weapon_perpetrator_tmp2 <-  
            #     df_weapon_perpetrator_all_gunGroup[df_weapon_perpetrator_all_gunGroup$perpetrator.sex ==  selected_perpetrator_gender(), ] %>%  
            #     select(weapon,count)
            if(grepl("All",selected_rel_category())){    
                df_rel_category_tmp <- df_rel_category_groupby
                labelvarVal = "relationship.category"
            }else{
                df_rel_category_tmp <- 
                    df_rel_detail_groupby[df_rel_detail_groupby$relationship.category == selected_rel_category(), ] %>% 
                    select(relationship.detail,count) 
                labelvarVal="relationship.detail"
            }
            
            #print(df_rel_category_tmp)
            # if(grepl("Top",selected_rank_item())){    
            #     df_peryear_homicide_tmp <- df_year_state_top5_homicide
            # }else if(grepl("Bottom",selected_rank_item())){
            #     df_state_homicide_tmp <- df_year_state_bottom5_homicide
            # }else{
            #     df_peryear_homicide_tmp <- df_year_all_homicide
            # }
            #gvisPieChart(data, labelvar = "", numvar = "", options = list(), chartid)
            gvisPieChart(data = df_rel_category_tmp, labelvar=labelvarVal, numvar="count"
                         , options=list( title = 'Who killed who (Relationship)?'
                                         #is3D= TRUE,
                                         , height="400"
                                         # vAxes="[{title:'val1'}, {title:'val2'}]"
                                         # colors="['#cbb69d', '#603913', '#c69c6e']"
                         )
            )
        })
        
        output$bar_rel_detail = renderGvis({
            
             if(grepl("All",selected_rel_category())){    
                 df_rel_detail_tmp <- df_rel_groupby
                 xvarVal = "relationship"
             }else{
                 df_rel_detail_tmp <- df_rel_detail_groupby[df_rel_detail_groupby$relationship.category == selected_rel_category(), ]
                 xvarVal="relationship.detail"
            }
            #else if(grepl("Bottom",selected_rank_item())){
            #     df_state_homicide_tmp <- df_year_state_bottom5_homicide
            # }else{
            #     df_peryear_homicide_tmp <- df_year_all_homicide
            # }
            gvisColumnChart(data = df_rel_detail_tmp, xvar=xvarVal, yvar="count"
                            , options=list( #isStacked=TRUE , 
                                            title = 'Number of incident by Relationship detail ',
                                            chartArea= "{width: '70%'}",
                                            #colors="['#9575cd', '#33ac71']",
                                            
                                            hAxis= "{
                                          title: 'Relationship Datail'
                                          
                                               
                                          }",
                                            vAxis = "{
                                              title: 'Number of incident',
                                              minValue: 0
                                          }",
                                            bar="{groupWidth:'70%'}",
                                            height = 400,
                                            fontSize= 11
                                            #                series="[{color:'green', targetAxisIndex: 0, 
                                            # lineWidth: 1, lineDashStyle: [2, 2, 20, 2, 20, 2]}, 
                                            # {color: 'blue',targetAxisIndex: 1, 
                                            # lineWidth: 2, lineDashStyle: [4, 1]}]",
                                            #                vAxes="[{title:'val1'}, {title:'val2'}]"
                            )
            )
        })
        
        output$line_rel_detail = renderGvis({
            
            if(grepl("All",selected_rel_category())){    
                df_rel_detail_year_tmp <- df_rel_detail_groupby_year_all
                yvarVal = c(names(df_rel_detail_year_tmp)[-1])
            }else{
                df_rel_detail_year_tmp <- df_rel_detail_groupby_year %>% 
                    filter(relationship.category == selected_rel_category()) %>% 
                    spread(relationship.detail,count,fill = 0, convert = FALSE, drop = TRUE,sep = NULL) 
                
                yvarVal = c(names(df_rel_detail_year_tmp)[-1:-2])
            }
            
            print(df_rel_detail_year_tmp)
            gvisLineChart(data = df_rel_detail_year_tmp, xvar="year", yvar=yvarVal
                          , options=list( title = 'Number of incident by Relationship detail, Year ',
                                          chartArea= "{width: '70%'}",
                                          hAxis = "{
                                             title: 'Year',
                                               format: '####',
                                          }",
                                          vAxis  = "{
                                              title: 'Number of incident',
                                              minValue: 0
                                          }",
                                          pointSize= 1,
                                          height = 400
                                          #                series="[{color:'green', targetAxisIndex: 0, 
                                          # lineWidth: 1, lineDashStyle: [2, 2, 20, 2, 20, 2]}, 
                                          # {color: 'blue',targetAxisIndex: 1, 
                                          # lineWidth: 2, lineDashStyle: [4, 1]}]",
                                          #                vAxes="[{title:'val1'}, {title:'val2'}]"
                          )
            )
        })
        
        
        # output$?? = renderGvis({
        #     
        #     df_weapon_year_perpetrator_tmp <-  
        #         df_weapon_year_all_graph[df_weapon_year_all_graph$perpetrator.sex ==  selected_perpetrator_gender(), ] 
        #     print(df_weapon_year_perpetrator_tmp)
        #     
        #     #gvisAreaChart(data, xvar = "", yvar = "", options = list(), chartid)
        #     gvisAreaChart(data = df_weapon_year_perpetrator_tmp, xvar="year", yvar=c("Blunt Object", "Drowning", "Drugs", "Explosives", "Fall", "Fire", "Gun", "Knife", "Poison", "Strangulation", "Suffocation", "Unknown")
        #                   , options=list( title = 'Use of perpetrator weapons by year'
        #                                   ,is3D= TRUE,
        #                                   width="800", height="400",
        #                                   hAxis= "{
        #                                   title: 'Year',
        #                                        format: '####'
        #                                   }",
        #                                   vAxis = "{
        #                                       title: 'Number of weapon',
        #                                       minValue: 0
        #                                   }"
        #                                   # vAxes="[{title:'val1'}, {title:'val2'}]"
        #                                   # colors="['#cbb69d', '#603913', '#c69c6e']"
        #                   )
        #     )
        # })
    }
)
