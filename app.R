
theme_reach <- function() {
    theme_fivethirtyeight() +
        theme(
            legend.position = "none",
            plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(size = 10, hjust = 0.5),
            axis.title.x = element_text(size=14),
            axis.title.y = element_text(size=14)
        )
}

#regular libraries
library(tidyverse) 
library(teamcolors) 
library(ggimage)
library(ggrepel)
library(ggthemes)
library(ggplot2)
library(knitr)
library(stringr)
library(shiny)
library(DT)


OffWarCollege <- read_rds("data/OffWarCollege.rds")

OffWar <- read_rds("data/OffWar.rds")

CGameData <- read_rds("data/GameData.rds") %>% 
    arrange(player) %>% 
    arrange(season)

Players_toInclude <- read_rds("data/Players_toInclude.rds")


GameData <- read_rds("data/Rolling_Games.rds") %>% 
    arrange(player) %>% 
    arrange(season) %>% 
    group_by(player) %>% 
    mutate(SeasonCount = length(unique((season)))) %>% 
    group_by()



NFLEndSmooth <- 8


QBFill <- GameData %>% 
    group_by(player) %>% 
    mutate(Count = n()) %>% 
    filter(Count >= 4) %>% 
    arrange(player)


CQBFill <- CGameData %>% 
    group_by(player) %>% 
    mutate(Count = n()) %>%
    filter(Count >= 4) %>% 
    arrange(player)


QBEPA <- read_rds("data/QBEPA.rds") %>% 
  arrange(QBname)%>% 
  mutate(QBname = case_when(QBname == "D. Brees" ~ "D.Brees",
                            T ~ QBname)) %>% 
  filter(posteam != "CLE*",
         QBname != "D.Carr" | QBname == "D.Carr" & season >=2014)

QBname <- QBEPA %>% 
  group_by(QBname) %>% 
  mutate(LastSeason = max(season)) %>% 
  filter(Count >= 600,
         LastSeason >= 2015)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Quarterback Bayesian Updating"),
    
    fluidRow(
        tabsetPanel(
          tabPanel("Intro Page",
                   fluidRow(
                     column(width = 10, offset = 1,
                            helpText(HTML("This shiny app is designed for evaluating quarterbacks using bayesian updating. There is a video explaining my methodology <a href='https://www.youtube.com/watch?v=Eg-k69IlnTc&feature=youtu.be'>HERE</a>. This includes values for both Pro and College, including their careers and just the most recent season. Additionally, it includes a measure of their EPA in both total and rate form over their careers.</br>
                        </br>
                        
                                   To check out my other shiny apps, follow one of the following links.</br>
                                   </br>

        <a href='https://seththedatascientist.shinyapps.io/QB_Bayesian_Updating/'>Bayesian Updating of composite metrics for Quarterback play for NFL and College QBs</a></br>

        This shiny app displays both college and pro QBs in two composite metrics that show not only their relative playstyles, but also how those values change over their careers. These values have a high correlation from college to pro for predicting playstyle once in the NFL</br>
        </br>

<a href='https://seththedatascientist.shinyapps.io/General_Manager_Toolbox/'>General Manager's Toolbox: A collection of tools to help analyze an NFL Team's Offseason moves.</a></br>

        This shiny app goes over a handful of useful data points that I have found very helpful for analyzing a team's offseason moves, including draft trade calculators (with some linear programming to try and ensure extra value by comparing the Jimmy Johnson trade chart to the Wins Above Replacement values), created metrics to analyze draft prospects in further detail, and team breakdowns of their effective cap and team structure over the coming years.</br>
        </br>

<a href='https://seththedatascientist.shinyapps.io/Offense_And_Defense_Metrics/'>Collection of Offense & Defense efficiency and playstyle metrics for the NFL</a></br>

        This shiny app includes a number of metrics used to understand Offense and Defense in further detail including down conversion values of how often you are allowing a first down or touchdown based on what down it currently is, explosive play rates, big throws and misses by quarterbacks, and more. Most metrics include a feature to isolate a playcaller's history of that metric across all teams they were the playcaller for.</br>
        </br>

<a href='https://seththedatascientist.shinyapps.io/Season_EPA_Tracker/'>Timeline of play measuring efficiency metrics, team season-long rankings, and team tier plots</a></br>

        This shiny app includes many iterations of breaking down expected points added (EPA) adjusted based on opponent strength and situation. Season long graphs to see individual team or starting quarterback trends, team plots for offense and defense including splits for passing and rushing, and a metric for team strength based on the relative margin of domination during the game as well as opponent strength.</br>
        </br>

<a href='https://seththedatascientist.shinyapps.io/WAR_positiongroup/'>Position Wins Above Replacement graphs by team and Watch Index</a></br>

        This shiny app shows Wins Above Replacement (WAR) values and plots for both college and pro broken down into many useful facets like by position group, team, and individual player. Includes receiver custom metrics plotted to compare players both within college and pro, as well as a customizable Watch Index which assigns a values based on relative values of excitement and closeness.</br>
        </br>
                  
                                       To check some of my other work, check out my <a href='https://twitter.com/SethDataScience'>Twitter</a>, <a href='https://www.linkedin.com/in/sethlanza/'>Linkedin</a>, or <a href='https://sites.google.com/view/seth-lanza-portfolio'>Portfolio</a>")),
                     )
                   ),
                   fluidRow(
                     column(width = 10, offset = 1,
                            textOutput("lastDeploymentTime")
                     )
                   )
          ),
            tabPanel("NFL Quarterbacks",
                             fluidRow(
                                 column(width = 4,
                                        selectizeInput(inputId = "QB1",
                                                       label = "QB Selection",
                                                       choices = unique(QBFill$player),
                                                       selected = "Josh Allen"),
                                        
                                        uiOutput("select_var1")
                                        ),
                                 
                                 column(width = 4,
                             
                                 submitButton(),
                                 helpText(HTML("Click on Startup <br/>    and Click Twice to Refresh")),
                             )),
                                 
                                 
                                 # Show a plot of the generated distribution
                                 fluidRow(
                                     column(
                                         plotOutput("Plot1", height = 700),
                                         width = 10, offset = 1)),
                                 fluidRow(
                                     column(
                                         plotOutput("Plot2", height = 700),
                                         width = 10, offset = 1)),
                     fluidRow(
                       column(
                         plotOutput("Plot3", height = 700),
                         width = 10, offset = 1)),
                     fluidRow(
                         column(
                             width = 10, offset = 1,
                             helpText(HTML("All Values are Normalized Percentiles.<br/>
                                      Diff is (EPA - WAR). Typically more Mobile QBs have Higher EPA than WAR.<br/>
                                      I would Rather have my Offense move the ball better than my QB is Grading. <br/>
                                      Support is a Metric based on Offensive Team WAR Values and Pre-snap Win Probability. <br/>
                                      Difficulty refers to Difficulty of passing assignment. Includes Passing Over Expected, Third and Long passing attempts, and Expected Completion Percentage. Rewards QBs that are trusted to run the offense. <br/>
                                      Downweights QBs that get carried by their run game. Adjustment made if you are contributing to the rushing success with mobile QB."))
                         )
                     ),
                     fluidRow(
                         column(
                             dataTableOutput("table"),
                             width = 10, offset = 1))
                     ,
                     fluidRow(
                       column(
                         width = 10, offset = 1,
                         helpText(HTML("This table shows the values if we only had the current season of data for active players. It is still comparing against all the players in the dataset, but if they played this season, it only considers this season of data for them."))
                       )
                     ),
                     fluidRow(
                       column(
                         dataTableOutput("table_current"),
                         width = 10, offset = 1))
                     ),
            tabPanel("College Quarterbacks",
                     fluidRow(
        column(width = 4,
               selectizeInput(inputId = "QB2",
                              label = "QB Selection",
                              choices = unique(CQBFill$player),
                              selected = "Baker Mayfield")
        ),
        submitButton(),
               ),
        
        
        # Show a plot of the generated distribution
        fluidRow(
            column(
                plotOutput("CPlot1", height = 700),
                width = 10, offset = 1)),
        fluidRow(
            column(
                plotOutput("CPlot2", height = 700),
                width = 10, offset = 1)),
        fluidRow(
          column(
            plotOutput("CPlot3", height = 700),
            width = 10, offset = 1)),
        fluidRow(
                    column(
                        width = 10, offset = 1,
                        helpText(HTML("All Values are Normalized Percentiles.<br/>
                                      Diff is (EPA - WAR). Typically more Mobile QBs have Higher EPA than WAR.<br/>
                                      I would Rather have my Offense move the ball better than my QB is Grading.<br/>
                                      Support is a Metric based on Offensive Team WAR Values and Pre-snap Win Probability. <br/>
                                      Difficulty refers to Difficulty of passing assignment. Includes Passing Over Expected, Third and Long passing attempts, and Expected Completion Percentage. Rewards QBs that are trusted to run the offense. <br/>
                                      Downweights QBs that get carried by their run game. Adjustment made if you are contributing to the rushing success with mobile QB."))
                    )
                ),
        fluidRow(
            column(
                dataTableOutput("Ctable"),
                width = 10, offset = 1))
            ),
        
        tabPanel(title = "Cumulative EPA QB Season Plots",
                 fluidRow(
                   column(width = 4,
                          
                          selectInput(inputId = "QB",
                                      label = "Quarterback Selection",
                                      choices = unique(QBname$QBname),
                                      selected = "P.Mahomes")
                   ),
                   
                   column(width = 3, submitButton())
                 ),
                 
                 
                 fluidRow(
                   column(
                     plotOutput("QBPlot", height = 650),
                     width = 10, offset = 1
                   )
                 ),
                 
                 fluidRow(
                   column(
                     plotOutput("QBPlot2", height = 650),
                     width = 10, offset = 1
                   )
                 ), 
                 fluidRow(
                   column(width = 3,
                          sliderInput(inputId = "QBSeason",
                                      label = "Season Selection",
                                      min = 1999,
                                      max = max(QBEPA$season),
                                      value = c(1999,max(QBEPA$season)),
                                      step = 1,
                                      round = T,
                                      sep = "")),
                   column(width = 3, submitButton()),
                   column(
                     dataTableOutput("QBtable"),
                     width = 10, offset = 1
                   )
                 )
        )
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  getLastDeploymentTime <- function() {
    timestamp <- tryCatch(
      readLines("deployment_timestamp.txt"),
      error = function(e) NA
    )
    if (!is.na(timestamp)) {
      as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    } else {
      NA
    }
  }
  
  # Display the last deployment time
  output$lastDeploymentTime <- renderText({
    lastDeploymentTime <- getLastDeploymentTime()
    if (!is.na(lastDeploymentTime)) {
      paste("Last Deployment Time: ", format(lastDeploymentTime, "%Y-%m-%d %H:%M:%S"))
    } else {
      "Deployment time not available."
    }
  })
  
  js <- c(
    "table.on('draw.dt', function(){",
    "  var PageInfo = table.page.info();",
    "  table.column(0, {page: 'current'}).nodes().each(function(cell,i){", 
    "    cell.innerHTML = i + 1 + PageInfo.start;",
    "  });",
    "})")
  
  
  
  output$select_var1 <- renderUI({
    checkboxGroupInput(inputId = "seasons1",
                       label = "Season(s) Selection",
                       inline = TRUE,
                       choices = unique(GameData$season[between(GameData$season,
                                                                suppressWarnings(min(GameData$season[GameData$player == input$QB1], na.rm = T)),
                                                                suppressWarnings(max(GameData$season[GameData$player == input$QB1], na.rm = T)))]),
                       selected = unique(GameData$season[GameData$player == input$QB1])
    )
  })
    
    output$CPlot1 <- renderPlot({
        
        
        
        CGameDataLabel <- CGameData %>% 
          group_by(player) %>% 
          mutate(MaxWAR = max(SeasonWAR)) %>% 
          group_by() %>% 
          filter( player %in% Players_toInclude |
                       player == input$QB2) %>% 
          # mutate(PRWar = percent_rank(MeanWAR),
          #        PREPA = percent_rank(MeanEPA),
          #        Diff = PREPA - PRWar,
          #        PRFloor = percent_rank(MeanFloor),
          #        PRCeiling = percent_rank(MeanCeiling))   %>% 
          group_by(player) %>% 
          slice_tail(n = 1)
        
        
        
        CGameData %>% 
          group_by(player) %>% 
          mutate(MaxWAR = max(SeasonWAR)) %>% 
          group_by() %>% 
          filter( player %in% Players_toInclude|
                       player == input$QB2) %>% 
          # mutate(PRWar = percent_rank(MeanWAR),
          #        PREPA = percent_rank(MeanEPA),
          #        Diff = PREPA - PRWar,
          #        PRFloor = percent_rank(MeanFloor),
          #        PRCeiling = percent_rank(MeanCeiling))   %>% 
            filter(player == input$QB2) %>%
            ggplot(aes(x = Time, y = PRFloor, color = "blue")) +
            geom_point()+
            geom_line()+
            geom_smooth()+
            geom_point(aes(y = PRCeiling, color = "darkgreen"))+
            geom_line(aes(y = PRCeiling, color = "darkgreen"))+
            geom_smooth(aes(y = PRCeiling, color = "darkgreen"))+
            geom_label_repel(data = CGameDataLabel %>% filter(player == input$QB2),
                             aes(x = Time, y = PRFloor, color = "blue",
                                 label = paste0((100*round(PRFloor,4)),"%")),
                             min.segment.length = 0, force = 5)+
            geom_label_repel(data = CGameDataLabel %>% filter(player == input$QB2),
                             aes(x = Time, y = PRCeiling, color = "darkgreen",
                                 label = paste0((100*round(PRCeiling,4)),"%")),
                             min.segment.length = 0, force = 5)+
            scale_y_continuous(breaks = seq(0,1,0.25), limits = c(0,1),
                               labels = scales::percent_format(accuracy = 1))+
            scale_x_continuous(breaks = seq(0,200,10))+
            geom_hline(yintercept = 0.75, linetype = "dashed")+
            geom_hline(yintercept = 0.5, linetype = "dashed")+
            geom_hline(yintercept = 0.25, linetype = "dashed")+
            scale_color_identity(aesthetics = c("color", "fill"))+
            labs(title = paste0(
                "Percentile Normalized Career Bayesian Updated Structure vs Playmaker ",
                "from ",
                min(CGameData$season[CGameData$player == input$QB2]), "-",
                max(CGameData$season[CGameData$player == input$QB2]), " "),
                subtitle = "Weighted towards 50% for the first 8 Games, Blue Line is Structure/Green Line is Playmaker,
Vertical Black Lines Represent Season Changes",
                y = "Career Bayesian Updated Per-Game Structure vs Playmaker",
                x = "Number of Games",
                caption = "Data includes: 2014-2021, @SethDataScience")+
            theme_reach()+
            facet_wrap(~player) +
            geom_vline(aes(xintercept = Season_Change_variable,
                           color = "black"))
        
    })
    
    output$CPlot2 <- renderPlot({
        
        
        CGameDataLabel <- CGameData %>% 
          group_by(player) %>% 
          mutate(MaxWAR = max(SeasonWAR)) %>% 
          group_by() %>% 
          filter(player %in% Players_toInclude|
                       player == input$QB2) %>% 
            # mutate(PRWar = percent_rank(MeanWAR),
            #        PREPA = percent_rank(MeanEPA),
            #        Diff = PREPA - PRWar,
            #        PRFloor = percent_rank(MeanFloor),
            #        PRCeiling = percent_rank(MeanCeiling)) %>% 
            group_by(player) %>% 
            slice_tail(n = 1)
        
        
        CGameData %>% 
          group_by(player) %>% 
          mutate(MaxWAR = max(SeasonWAR)) %>% 
          group_by() %>% 
          filter(player %in% Players_toInclude|
                       player == input$QB2) %>% 
            # mutate(PRWar = percent_rank(MeanWAR),
            #        PREPA = percent_rank(MeanEPA),
            #        Diff = PREPA - PRWar,
            #        PRFloor = percent_rank(MeanFloor),
            #        PRCeiling = percent_rank(MeanCeiling)) %>% 
            filter(player == input$QB2) %>% 
            ggplot(aes(x = Time, y = PRWar)) +
            geom_point()+
            geom_line()+
            geom_smooth()+
            geom_point(aes(y = PREPA, color = "red"))+
            geom_line(aes(y = PREPA, color = "red"))+
            geom_smooth(aes(y = PREPA, color = "red"))+
            geom_label_repel(data = CGameDataLabel %>% filter(player == input$QB2),
                             aes(x = Time, y = PRWar,
                                 label = paste0((100*round(PRWar,4)),"%")),
                             min.segment.length = 0, force = 5)+
            geom_label_repel(data = CGameDataLabel %>% filter(player == input$QB2),
                             aes(x = Time, y = PREPA, color = "red",
                                 label = paste0((100*round(PREPA,4)),"%")),
                             min.segment.length = 0, force = 5)+
            scale_y_continuous(breaks = seq(0,1,0.25), limits = c(0,1),
                               labels = scales::percent_format(accuracy = 1))+
            scale_x_continuous(breaks = seq(0,200,10))+
            geom_hline(yintercept = 0.75, linetype = "dashed")+
            geom_hline(yintercept = 0.5, linetype = "dashed")+
            geom_hline(yintercept = 0.25, linetype = "dashed")+
            scale_color_identity(aesthetics = c("color", "fill"))+
            labs(title = paste0(
                "Percentile Normalized Career Bayesian Updated Per-Game WAR and EPA ",
                "from ",
                min(CGameData$season[CGameData$player == input$QB2]), "-",
                max(CGameData$season[CGameData$player == input$QB2]), " "),
                subtitle = "Weighted towards 50% for the first 8 Games, Red Line is Opp/WP Adjusted EPA/Play,
Vertical Black Lines Represent Season Changes",
                y = "Career Bayesian Updated Per-Game WAR and EPA",
                x = "Number of Games",
                caption = "Data includes: 2014-2021, @SethDataScience")+
            theme_reach()+
            facet_wrap(~player) +
            geom_vline(aes(xintercept = Season_Change_variable,
                           color = "black"))
    })
    
    output$CPlot3 <- renderPlot({
      
      CGameDataFull <- CGameData %>% 
        group_by(player) %>% 
        mutate(MaxWAR = max(SeasonWAR)) %>% 
        group_by() %>% 
        filter( player %in% Players_toInclude |
                 player == input$QB2)%>% 
        mutate(PRWar =PRWar* 100,
               PREPA = PREPA * 100,
               PRFloor = PRFloor * 100,
               PRCeiling = PRCeiling * 100) 
      
      CGameDataDistro <- CGameDataFull %>% 
        filter(player == input$QB2) %>% 
        group_by(player) %>% 
        mutate(
          posterior_variance_HighFloor = 1 / (1 / var(CGameDataFull$PRFloor, na.rm = T)  + cumsum(Count) / var(PRCeiling, na.rm = T)),
          posterior_variance_HighCeiling = 1 / (1 / var(CGameDataFull$PRCeiling, na.rm = T)   + cumsum(Count) / var(PRCeiling, na.rm = T)),
          posterior_variance_WAR = 1 / (1 / var(CGameDataFull$PRWar, na.rm = T)   + cumsum(Count) / var(PRWar, na.rm = T)),
          posterior_variance_EPA = 1 / (1 / var(CGameDataFull$PREPA, na.rm = T)   + cumsum(Count) / var(PREPA, na.rm = T))
        )%>%
        slice_tail(n = 1)%>% 
        mutate(xmin = case_when(min(PREPA, PRFloor, PRCeiling, PRWar) <= 20 ~ 0,
                                min(PREPA, PRFloor, PRCeiling, PRWar) >= 50 ~ 50,
                                T ~ min(PREPA, PRFloor, PRCeiling, PRWar) - 5),
               xmax = case_when(max(PREPA, PRFloor, PRCeiling, PRWar) >=80 ~ 100,
                                max(PREPA, PRFloor, PRCeiling, PRWar) <= 50 ~ 50,
                                T ~ max(PREPA, PRFloor, PRCeiling, PRWar) + 5))
      
      ggplot(CGameDataDistro) +
        stat_function(
          fun = dnorm,
          args = with(CGameDataDistro, c(mean = tail(PRFloor,1), sd = sqrt(tail(posterior_variance_HighFloor, 1)))),
          geom = "area",
          fill = "blue",
          alpha = 0.2,
          aes(color = "blue")) +
        geom_vline(aes(xintercept = tail(PRFloor, 1), color = "blue"), linetype = "dashed")+
        stat_function(
          fun = dnorm,
          args = with(CGameDataDistro, c(mean = tail(PRCeiling,1), sd = sqrt(tail(posterior_variance_HighCeiling, 1)))),
          geom = "area",
          fill = "darkgreen",
          alpha = 0.2,
          aes(color = "darkgreen")) +
        geom_vline(aes(xintercept = tail(PRCeiling, 1), color = "darkgreen"), linetype = "dashed")+
        stat_function(
          fun = dnorm,
          args = with(CGameDataDistro, c(mean = tail(PREPA,1), sd = sqrt(tail(posterior_variance_EPA, 1)))),
          geom = "area",
          fill = "red",
          alpha = 0.2,
          aes(color = "red")) +
        geom_vline(aes(xintercept = tail(PREPA, 1), color = "red"), linetype = "dashed")+
        stat_function(
          fun = dnorm,
          args = with(CGameDataDistro, c(mean = tail(PRWar,1), sd = sqrt(tail(posterior_variance_WAR, 1)))),
          geom = "area",
          fill = "black",
          alpha = 0.2,
          aes(color = "black")) +
        geom_vline(aes(xintercept = tail(PRWar, 1), color = "black"), linetype = "dashed")+
        geom_vline(aes(xintercept = 50), linewidth = 1)+
        geom_hline(aes(yintercept = 0))+
        scale_x_continuous(breaks = seq(0,100,10), limits = c(CGameDataDistro$xmin, CGameDataDistro$xmax))+
        scale_color_identity(aesthetics = c("color", "fill"))+
        labs(title = paste0(
          "Career Bayesian Updated Floor and Ceiling Distributions"),
          subtitle = "Distributions generated from the Bayesian Updated Mean (dashed line) and Variance (normal curve). Solid black line is the 50th Percentile.",
          y = "Probability Density Function",
          x = "Percentile",
          caption = "Data includes: 2011+, @SethDataScience")+
        theme_reach()+
        facet_wrap(~player)
      
    })
    
    
    output$Ctable = DT::renderDataTable({
        
        CGameData1 <- CGameData %>% 
          group_by(player) %>% 
          mutate(MaxWAR = max(SeasonWAR)) %>% 
          group_by()  %>% 
          filter( player %in% Players_toInclude |
                    player == input$QB2)%>% 
          mutate(PRWar =PRWar* 100,
                 PREPA = PREPA * 100,
                 PRFloor = PRFloor * 100,
                 PRCeiling = PRCeiling * 100,
                 Difficulty = Difficulty * 100) %>% 
            mutate(CurrentYear = max(season)) %>% 
            group_by(player) %>% 
            mutate(FinalSeason = max(season)) %>%
            group_by(season, player) %>% 
            mutate(LastYear = case_when(season == FinalSeason ~ "Last Year",
                                           T ~ ""),
                   Selection = case_when(player == input$QB2 ~ "Selection",
                                         T ~ "")) %>% 
            slice_tail(n = 1) %>% 
            group_by(player) %>% 
            mutate(Year = seq(1,n(),1)) %>% 
          group_by() %>% 
          mutate(Cluster = case_when(
            PRFloor >= 85 & PRCeiling >= 85 & DifficultyAdj >= 1 ~ "Elite",
            PRFloor >= 85 & PRCeiling >= 85 ~ "Elite / Low Volume",
            PRFloor >= 75 & PRCeiling >= 75 & DifficultyAdj >= 1 ~ "SubElite",
            PRFloor >= 75 & PRCeiling >= 75 ~ "SubElite / Low Volume",
            PRFloor >= 85  & DifficultyAdj >= 1 ~ "High Floor / High Volume",
            PRFloor >= 85  ~ "High Floor / Low Volume",
            PRCeiling >= 85 & DifficultyAdj >= 1 ~ "High Ceiling / High Volume",
            PRCeiling >= 85 ~ "High Ceiling / Low Volume",
            PRFloor <= 50 & PRCeiling <= 50 & DifficultyAdj >= 1 ~ "Bad / High Volume",
            PRFloor <= 50 & PRCeiling <= 50 ~ "Bad / Low Volume",
            PRFloor >= PRCeiling & DifficultyAdj >= 1 ~ "Mid Floor / High Volume",
            PRFloor >= PRCeiling ~ "Mid Floor / Low Volume",
            PRFloor <= PRCeiling & DifficultyAdj >= 1 ~ "Mid Ceiling / High Volume",
            PRFloor <= PRCeiling ~ "Mid Ceiling / Low Volume",
            T ~ "Unknown"
          ),
          Cluster = factor(Cluster, levels = c("Elite", "Elite / Low Volume", "SubElite", "SubElite / Low Volume", "High Floor / High Volume", "High Floor / Low Volume", "High Ceiling / High Volume", "High Ceiling / Low Volume" , "Mid Floor / High Volume", "Mid Floor / Low Volume",  "Mid Ceiling / High Volume", "Mid Ceiling / Low Volume", "Bad / High Volume", "Bad / Low Volume", "Unknown")),
          Value = (PRFloor * 2 + PRCeiling)/3) %>% 
          arrange(desc(Value)) %>% 
          arrange(desc(season)) %>% 
          arrange(desc(Selection)) %>%
            mutate(season = as.factor(season),
                   team_name = as.factor(team_name),
                   Selection = as.factor(Selection),
                   LastYear = as.factor(LastYear),
                   Year = as.integer(Year))  %>% 
          mutate(PRWar = round(PRWar,2),
                 PREPA = round(PREPA,2),
                 Diff = round(Diff,2),
                 PRFloor = round(PRFloor,2),
                 PRCeiling = round(PRCeiling,2),
                 Difficulty = round(Difficulty, 2),
                 Support = round(Support, 2)) %>% 
            select(season, Selection, LastYear, Year, player, team_name,
                   PRFloor, PRCeiling, PREPA, PRWar, Diff, Support,
                   Difficulty, Cluster)
        
        
        PRWarbrks <- quantile(CGameData1$PRWar, probs = seq(.05, .95, .01), na.rm = TRUE)
        PRWarbrksy <- round(seq(255, 40, length.out = length(PRWarbrks) + 1), 0)
        PRWarbrksclrs <- paste0("rgb(", PRWarbrksy, "," , 255-PRWarbrksy , ",", 0, ")")
        
        PREPAbrks <- quantile(CGameData1$PREPA, probs = seq(.05, .95, .01), na.rm = TRUE)
        PREPAbrksy <- round(seq(255, 40, length.out = length(PREPAbrks) + 1), 0)
        PREPAbrksclrs <- paste0("rgb(", PREPAbrksy, "," , 255-PREPAbrksy , ",", 0, ")")
        
        PRCeilingbrks <- quantile(CGameData1$PRCeiling, probs = seq(.05, .95, .01), na.rm = TRUE)
        PRCeilingbrksy <- round(seq(255, 40, length.out = length(PRCeilingbrks) + 1), 0)
        PRCeilingbrksclrs <- paste0("rgb(", PRCeilingbrksy, "," , 255-PRCeilingbrksy , ",", 0, ")")
        
        PRFloorbrks <- quantile(CGameData1$PRFloor, probs = seq(.05, .95, .01), na.rm = TRUE)
        PRFloorbrksy <- round(seq(255, 40, length.out = length(PRFloorbrks) + 1), 0)
        PRFloorbrksclrs <- paste0("rgb(", PRFloorbrksy, "," , 255-PRFloorbrksy , ",", 0, ")")
        
        
        Diffbrks <- quantile(CGameData1$Diff, probs = seq(.05, .95, .01), na.rm = TRUE)
        Diffbrksy <- round(seq(255, 40, length.out = length(Diffbrks) + 1), 0)
        Diffbrksclrs <- paste0("rgb(", Diffbrksy, "," , 255-Diffbrksy , ",", 0, ")")
        
        
        Supportbrks <- quantile(CGameData1$Support, probs = seq(.05, .95, .01), na.rm = TRUE)
        Supportbrksy <- round(seq(255, 40, length.out = length(Supportbrks) + 1), 0)
        Supportbrksclrs <- paste0("rgb(", Supportbrksy, "," , 255-Supportbrksy , ",", 0, ")")
        
        
        Difficultybrks <- quantile(CGameData1$Difficulty, probs = seq(.05, .95, .01), na.rm = TRUE)
        Difficultybrksy <- round(seq(255, 40, length.out = length(Difficultybrks) + 1), 0)
        Difficultybrksclrs <- paste0("rgb(", Difficultybrksy, "," , 255-Difficultybrksy , ",", 0, ")")
        
        
        
        
        datatable(CGameData1, rownames = F, filter = "top",
                  extensions = 'Buttons', options = list(pageLength = 15,
                                                         dom = 'Bfrtip',                      
                                                         buttons = c('csv', 'excel')))%>% 
            formatStyle("Selection",
                        backgroundColor = styleEqual(c("", "Selection"),
                                                     c("","gold"))
            )%>%
            formatStyle("PRWar", 
                              backgroundColor = styleInterval(PRWarbrks,PRWarbrksclrs)
            ) %>%
            formatStyle("PREPA", 
                        backgroundColor = styleInterval(PREPAbrks,PREPAbrksclrs)
            ) %>%
            formatStyle("PRCeiling", 
                        backgroundColor = styleInterval(PRCeilingbrks,PRCeilingbrksclrs)
            ) %>%
            formatStyle("PRFloor", 
                        backgroundColor = styleInterval(PRFloorbrks,PRFloorbrksclrs)
            )  %>%
            formatStyle("Diff", 
                        backgroundColor = styleInterval(Diffbrks,Diffbrksclrs)
            )  %>%
          formatStyle("Support", 
                      backgroundColor = styleInterval(Supportbrks,Supportbrksclrs)
          ) %>%
          formatStyle("Difficulty", 
                      backgroundColor = styleInterval(Difficultybrks,Difficultybrksclrs)
          )
        
    
    })
    
    
    output$Plot1 <- renderPlot({
      
      req(input$seasons1)
      
      GameDataFull <- GameData %>% 
        filter((player == input$QB1 & season %in% input$seasons1) |
                 player != input$QB1) %>% 
        group_by(QBname) %>% 
        mutate(QBTime = tryCatch(
          #try to do this
          {
            seq(1,n(),1)
          },
          #if an error occurs, tell me the error
          error=function(e) {
            message('An Error Occurred')
            print(e)
            return(0)
          },
          #if a warning occurs, tell me the warning
          warning=function(w) {
            message('A Warning Occurred')
            print(w)
            return(0)
          },
          finally = 0
        ),
        Season_Change_variable = case_when(lag(season, 1) != season ~ Time-0.5),
        Playoffs_variable = case_when(season < 2021 & week > 17 & between(lag(week, 1), 15, 17) ~ Time-0.5,
                                      season >= 2021 & week > 18 & between(lag(week, 1), 15, 18) ~ Time - 0.5),
        QBSeason_Change_variable = case_when(is.na(Season_Change_variable) == 0 ~ (QBTime-0.5)),
        QBPlayoffs_variable = case_when(is.na(Playoffs_variable) == 0 ~ (QBTime-0.5))) %>%
        group_by(QBname) %>% 
        mutate(
          # Calculate the posterior parameters
          posterior_mean_HighFloor = (mean(GameData$HighFloor) / var(GameData$HighFloor)   + cumsum(HighFloor) / var(HighFloor)) / (1 / var(GameData$HighFloor)   + cumsum(Count) / var(HighFloor)),
          posterior_variance_HighFloor = 1 / (1 / var(GameData$HighFloor)   + cumsum(Count) / var(HighFloor)),
          
          posterior_mean_HighCeiling = (mean(GameData$HighCeiling) / var(GameData$HighCeiling)   + cumsum(HighCeiling) / var(HighCeiling)) / (1 / var(GameData$HighCeiling)   + cumsum(Count) / var(HighCeiling)),
          posterior_variance_HighCeiling = 1 / (1 / var(GameData$HighCeiling)   + cumsum(Count) / var(HighCeiling)),
          
          posterior_mean_WAR = (mean(GameData$WAR) / var(GameData$WAR)   + cumsum(WAR) / var(WAR)) / (1 / var(GameData$WAR)   + cumsum(Count) / var(WAR)),
          posterior_variance_WAR = 1 / (1 / var(GameData$WAR)   + cumsum(Count) / var(WAR)),
          
          posterior_mean_FinEPA = (mean(GameData$FinEPA) / var(GameData$FinEPA)   + cumsum(FinEPA) / var(FinEPA)) / (1 / var(GameData$FinEPA)   + cumsum(Count) / var(FinEPA)),
          posterior_variance_FinEPA = 1 / (1 / var(GameData$FinEPA)   + cumsum(Count) / var(FinEPA)),
          
          MeanFloor = case_when(Time <= (NFLEndSmooth) ~ (posterior_mean_HighFloor + (((NFLEndSmooth+1)-Time)*mean(GameData$HighFloor)))/((NFLEndSmooth+2)-Time),
                                T ~ posterior_mean_HighFloor),
          
          MeanCeiling = case_when(Time <= (NFLEndSmooth) ~ (posterior_mean_HighCeiling + (((NFLEndSmooth+1)-Time)*mean(GameData$HighCeiling)))/((NFLEndSmooth+2)-Time),
                                  T ~ posterior_mean_HighCeiling),
          
          MeanWAR = case_when(Time <= (NFLEndSmooth) ~ (posterior_mean_WAR + (((NFLEndSmooth+1)-Time)*mean(GameData$WAR)))/((NFLEndSmooth+2)-Time),
                              T ~ posterior_mean_WAR),
          
          MeanEPA = case_when(Time <= (NFLEndSmooth) ~ (posterior_mean_FinEPA + (((NFLEndSmooth+1)-Time)*mean(GameData$FinEPA)))/((NFLEndSmooth+2)-Time),
                              T ~ posterior_mean_FinEPA),
          
        ) %>% 
        group_by() %>%   
        filter(!is.na(MeanFloor), !is.na(MeanWAR)) %>% 
        mutate(PRWar = percent_rank(MeanWAR),
               PREPA = percent_rank(MeanEPA),
               Diff = PREPA - PRWar,
               PRFloor = percent_rank(MeanFloor),
               PRCeiling = percent_rank(MeanCeiling)) 
      
      GameDataLabel <- GameDataFull %>% 
        filter(player == input$QB1,
               season %in% input$seasons1) %>% 
        group_by(QBname) %>% 
        reframe(FloorLabel = tail(PRFloor, 1),
               CeilingLabel = tail(PRCeiling, 1),
               WarLabel = tail(PRWar, 1),
               EPALabel = tail(PREPA, 1),
               MaxQBTime = tail(QBTime, 1))
      
      
      GameDataFull %>% 
          filter(player == input$QB1,
                 season %in% input$seasons1) %>% 
          ggplot(aes(x = QBTime, 
                     y = PRFloor, color = "blue")) +
          geom_point()+
          geom_line()+
          geom_smooth()+
          geom_point(aes(x = QBTime,
                         y = PRCeiling, color = "darkgreen"))+
          geom_line(aes(x = QBTime,
                        y = PRCeiling, color = "darkgreen"))+
          geom_smooth(aes(x = QBTime,
                          y = PRCeiling, color = "darkgreen", fill = "darkgreen"))+
          scale_y_continuous(breaks = seq(0,1,0.25), limits = c(0,1),
                             labels = scales::percent_format(accuracy = 1))+
          scale_x_continuous(breaks = seq(0,200,10))+
          geom_hline(yintercept = 0.75, linetype = "dashed")+
          geom_hline(yintercept = 0.5, linetype = "dashed")+
          geom_hline(yintercept = 0.25, linetype = "dashed")+
          scale_color_identity(aesthetics = c("color", "fill"))+
          labs(title = paste0(
            "Percentile Normalized Career Bayesian Updated Structure vs Playmaker ",
            "from ",
            suppressWarnings(min(GameData$season[GameData$season %in% input$seasons1])),
            "-",
            suppressWarnings(max(GameData$season[GameData$season %in% input$seasons1])),
            " "),
            subtitle = "Weighted towards 50% for the first 8 Games (Not if Selecting Seasons), Blue Line is Structure/Green Line is Playmaker,
Vertical Black Lines Represent Season Changes, Purple Lines Represent Playoff Games (1.25x Value)",
            y = "Career Bayesian Updated Per-Game Structure vs Playmaker",
            x = "Number of Games",
            caption = "Data includes: 2011+, @SethDataScience")+
          theme_reach()+
          facet_wrap(~player) +
          geom_vline(aes(xintercept = QBSeason_Change_variable,
                         color = "black"))+
          geom_vline(aes(xintercept = QBPlayoffs_variable,
                         color = "purple"))+
        geom_label(aes(x = GameDataLabel$MaxQBTime,
                       y = GameDataLabel$FloorLabel, color = "blue",
                       hjust = 0.5, vjust = 1,
                       label = paste0((100*round(GameDataLabel$FloorLabel,4)),"%")))+
      geom_label(aes(x = GameDataLabel$MaxQBTime,
                     y = GameDataLabel$CeilingLabel, color = "darkgreen",
                     hjust = 0.5, vjust = 1,
                     label = paste0((100*round(GameDataLabel$CeilingLabel,4)),"%")))
      
    })

    output$Plot2 <- renderPlot({
      
      
      req(input$seasons1)
      
      GameDataFull <- GameData %>% 
        filter((player == input$QB1 & season %in% input$seasons1) |
                 player != input$QB1) %>% 
        group_by(QBname) %>% 
        mutate(QBTime = tryCatch(
          #try to do this
          {
            seq(1,n(),1)
          },
          #if an error occurs, tell me the error
          error=function(e) {
            message('An Error Occurred')
            print(e)
            return(0)
          },
          #if a warning occurs, tell me the warning
          warning=function(w) {
            message('A Warning Occurred')
            print(w)
            return(0)
          },
          finally = 0
        ),
        Season_Change_variable = case_when(lag(season, 1) != season ~ Time-0.5),
        Playoffs_variable = case_when(season < 2021 & week > 17 & between(lag(week, 1), 15, 17) ~ Time-0.5,
                                      season >= 2021 & week > 18 & between(lag(week, 1), 15, 18) ~ Time - 0.5),
        QBSeason_Change_variable = case_when(is.na(Season_Change_variable) == 0 ~ (QBTime-0.5)),
        QBPlayoffs_variable = case_when(is.na(Playoffs_variable) == 0 ~ (QBTime-0.5))) %>%
        group_by(QBname) %>% 
        mutate(
          # Calculate the posterior parameters
          posterior_mean_HighFloor = (mean(GameData$HighFloor) / var(GameData$HighFloor)   + cumsum(HighFloor) / var(HighFloor)) / (1 / var(GameData$HighFloor)   + cumsum(Count) / var(HighFloor)),
          posterior_variance_HighFloor = 1 / (1 / var(GameData$HighFloor)   + cumsum(Count) / var(HighFloor)),
          
          posterior_mean_HighCeiling = (mean(GameData$HighCeiling) / var(GameData$HighCeiling)   + cumsum(HighCeiling) / var(HighCeiling)) / (1 / var(GameData$HighCeiling)   + cumsum(Count) / var(HighCeiling)),
          posterior_variance_HighCeiling = 1 / (1 / var(GameData$HighCeiling)   + cumsum(Count) / var(HighCeiling)),
          
          posterior_mean_WAR = (mean(GameData$WAR) / var(GameData$WAR)   + cumsum(WAR) / var(WAR)) / (1 / var(GameData$WAR)   + cumsum(Count) / var(WAR)),
          posterior_variance_WAR = 1 / (1 / var(GameData$WAR)   + cumsum(Count) / var(WAR)),
          
          posterior_mean_FinEPA = (mean(GameData$FinEPA) / var(GameData$FinEPA)   + cumsum(FinEPA) / var(FinEPA)) / (1 / var(GameData$FinEPA)   + cumsum(Count) / var(FinEPA)),
          posterior_variance_FinEPA = 1 / (1 / var(GameData$FinEPA)   + cumsum(Count) / var(FinEPA)),
          
          MeanFloor = case_when(Time <= (NFLEndSmooth) ~ (posterior_mean_HighFloor + (((NFLEndSmooth+1)-Time)*mean(GameData$HighFloor)))/((NFLEndSmooth+2)-Time),
                                T ~ posterior_mean_HighFloor),
          
          MeanCeiling = case_when(Time <= (NFLEndSmooth) ~ (posterior_mean_HighCeiling + (((NFLEndSmooth+1)-Time)*mean(GameData$HighCeiling)))/((NFLEndSmooth+2)-Time),
                                  T ~ posterior_mean_HighCeiling),
          
          MeanWAR = case_when(Time <= (NFLEndSmooth) ~ (posterior_mean_WAR + (((NFLEndSmooth+1)-Time)*mean(GameData$WAR)))/((NFLEndSmooth+2)-Time),
                              T ~ posterior_mean_WAR),
          
          MeanEPA = case_when(Time <= (NFLEndSmooth) ~ (posterior_mean_FinEPA + (((NFLEndSmooth+1)-Time)*mean(GameData$FinEPA)))/((NFLEndSmooth+2)-Time),
                              T ~ posterior_mean_FinEPA),
          
        ) %>% 
        group_by() %>%   
        filter(!is.na(MeanFloor), !is.na(MeanWAR)) %>%
        mutate(PRWar = percent_rank(MeanWAR),
               PREPA = percent_rank(MeanEPA),
               Diff = PREPA - PRWar,
               PRFloor = percent_rank(MeanFloor),
               PRCeiling = percent_rank(MeanCeiling)) 
      
      GameDataLabel <- GameDataFull %>% 
        filter(player == input$QB1,
               season %in% input$seasons1) %>% 
        group_by(QBname) %>% 
        reframe(FloorLabel = tail(PRFloor, 1),
                CeilingLabel = tail(PRCeiling, 1),
                WarLabel = tail(PRWar, 1),
                EPALabel = tail(PREPA, 1),
                MaxQBTime = tail(QBTime, 1))
      
      GameDataFull %>% 
        filter(player == input$QB1,
               season %in% input$seasons1) %>% 
        ggplot(aes(x = QBTime, 
                   y = PRWar, color = "black")) +
        geom_point()+
        geom_line()+
        geom_smooth()+
        geom_point(aes(x = QBTime,
                       y = PREPA, color = "red"))+
        geom_line(aes(x = QBTime,
                      y = PREPA, color = "red"))+
        geom_smooth(aes(x = QBTime,
                        y = PREPA, color = "red"))+
        scale_y_continuous(breaks = seq(0,1,0.25), limits = c(0,1),
                           labels = scales::percent_format(accuracy = 1))+
        scale_x_continuous(breaks = seq(0,200,10))+
        geom_hline(yintercept = 0.75, linetype = "dashed")+
        geom_hline(yintercept = 0.5, linetype = "dashed")+
        geom_hline(yintercept = 0.25, linetype = "dashed")+
        scale_color_identity(aesthetics = c("color", "fill"))+
        labs(title = paste0(
          "Percentile Normalized Career Bayesian Updated Per-Game WAR and EPA ",
          "from ",
          suppressWarnings(min(GameData$season[GameData$season %in% input$seasons1])),
          "-",
          suppressWarnings(max(GameData$season[GameData$season %in% input$seasons1])),
          " "),
          subtitle = "Weighted towards 50% for the first 8 Games (Not if Selecting Seasons), Red Line is Opp/WP Adjusted EPA/Play,
Vertical Black Lines Represent Season Changes, Purple Lines Represent Playoff Games (1.25x Value)",
          y = "Career Bayesian Updated Per-Game WAR and EPA",
          x = "Number of Games",
          caption = "Data includes: 2011+, @SethDataScience")+
        theme_reach()+
        facet_wrap(~player) +
        geom_vline(aes(xintercept = QBSeason_Change_variable,
                       color = "black"))+
        geom_vline(aes(xintercept = QBPlayoffs_variable,
                       color = "purple"))+
        geom_label(aes(x = GameDataLabel$MaxQBTime,
                       y = GameDataLabel$WarLabel, color = "black",
                       hjust = 0.5, vjust = 1,
                       label = paste0((100*round(GameDataLabel$WarLabel,4)),"%")))+
        geom_label(aes(x = GameDataLabel$MaxQBTime,
                       y = GameDataLabel$EPALabel, color = "red",
                       hjust = 0.5, vjust = 1,
                       label = paste0((100*round(GameDataLabel$EPALabel,4)),"%")))
            
    })
    
    output$Plot3 <- renderPlot({
      
      
      req(input$seasons1)
      
      GameDataFull <- GameData %>% 
        filter((player == input$QB1 & season %in% input$seasons1) |
                 player != input$QB1) %>% 
        group_by(QBname) %>% 
        mutate(QBTime = tryCatch(
          #try to do this
          {
            seq(1,n(),1)
          },
          #if an error occurs, tell me the error
          error=function(e) {
            message('An Error Occurred')
            print(e)
            return(0)
          },
          #if a warning occurs, tell me the warning
          warning=function(w) {
            message('A Warning Occurred')
            print(w)
            return(0)
          },
          finally = 0
        ),
        Season_Change_variable = case_when(lag(season, 1) != season ~ Time-0.5),
        Playoffs_variable = case_when(season < 2021 & week > 17 & between(lag(week, 1), 15, 17) ~ Time-0.5,
                                      season >= 2021 & week > 18 & between(lag(week, 1), 15, 18) ~ Time - 0.5),
        QBSeason_Change_variable = case_when(is.na(Season_Change_variable) == 0 ~ (QBTime-0.5)),
        QBPlayoffs_variable = case_when(is.na(Playoffs_variable) == 0 ~ (QBTime-0.5))) %>%
        group_by(QBname) %>% 
        mutate(
          # Calculate the posterior parameters
          posterior_mean_HighFloor = (mean(GameData$HighFloor) / var(GameData$HighFloor)   + cumsum(HighFloor) / var(HighFloor)) / (1 / var(GameData$HighFloor)   + cumsum(Count) / var(HighFloor)),
          posterior_variance_HighFloor = 1 / (1 / var(GameData$HighFloor)   + cumsum(Count) / var(HighFloor)),
          
          posterior_mean_HighCeiling = (mean(GameData$HighCeiling) / var(GameData$HighCeiling)   + cumsum(HighCeiling) / var(HighCeiling)) / (1 / var(GameData$HighCeiling)   + cumsum(Count) / var(HighCeiling)),
          posterior_variance_HighCeiling = 1 / (1 / var(GameData$HighCeiling)   + cumsum(Count) / var(HighCeiling)),
          
          posterior_mean_WAR = (mean(GameData$WAR) / var(GameData$WAR)   + cumsum(WAR) / var(WAR)) / (1 / var(GameData$WAR)   + cumsum(Count) / var(WAR)),
          posterior_variance_WAR = 1 / (1 / var(GameData$WAR)   + cumsum(Count) / var(WAR)),
          
          posterior_mean_FinEPA = (mean(GameData$FinEPA) / var(GameData$FinEPA)   + cumsum(FinEPA) / var(FinEPA)) / (1 / var(GameData$FinEPA)   + cumsum(Count) / var(FinEPA)),
          posterior_variance_FinEPA = 1 / (1 / var(GameData$FinEPA)   + cumsum(Count) / var(FinEPA)),
          
          MeanFloor = case_when(Time <= (NFLEndSmooth) ~ (posterior_mean_HighFloor + (((NFLEndSmooth+1)-Time)*mean(GameData$HighFloor)))/((NFLEndSmooth+2)-Time),
                                T ~ posterior_mean_HighFloor),
          
          MeanCeiling = case_when(Time <= (NFLEndSmooth) ~ (posterior_mean_HighCeiling + (((NFLEndSmooth+1)-Time)*mean(GameData$HighCeiling)))/((NFLEndSmooth+2)-Time),
                                  T ~ posterior_mean_HighCeiling),
          
          MeanWAR = case_when(Time <= (NFLEndSmooth) ~ (posterior_mean_WAR + (((NFLEndSmooth+1)-Time)*mean(GameData$WAR)))/((NFLEndSmooth+2)-Time),
                              T ~ posterior_mean_WAR),
          
          MeanEPA = case_when(Time <= (NFLEndSmooth) ~ (posterior_mean_FinEPA + (((NFLEndSmooth+1)-Time)*mean(GameData$FinEPA)))/((NFLEndSmooth+2)-Time),
                              T ~ posterior_mean_FinEPA),
          
        ) %>% 
        group_by() %>%   
        filter(!is.na(MeanFloor), !is.na(MeanWAR)) %>%
        mutate(PRWar = percent_rank(MeanWAR) * 100,
               PREPA = percent_rank(MeanEPA) * 100,
               Diff = PREPA - PRWar,
               PRFloor = percent_rank(MeanFloor) * 100,
               PRCeiling = percent_rank(MeanCeiling) * 100) 
      
      GameDataDistro <- GameDataFull %>% 
        filter(player == input$QB1) %>% 
        group_by(QBname) %>% 
        mutate(
          posterior_variance_HighFloor = 1 / (1 / var(GameDataFull$PRFloor, na.rm = T)  + cumsum(Count) / var(PRCeiling, na.rm = T)),
          posterior_variance_HighCeiling = 1 / (1 / var(GameDataFull$PRCeiling, na.rm = T)   + cumsum(Count) / var(PRCeiling, na.rm = T)),
          posterior_variance_WAR = 1 / (1 / var(GameDataFull$PRWar, na.rm = T)   + cumsum(Count) / var(PRWar, na.rm = T)),
          posterior_variance_EPA = 1 / (1 / var(GameDataFull$PREPA, na.rm = T)   + cumsum(Count) / var(PREPA, na.rm = T))
          )%>%
        slice_tail(n = 1) %>% 
        mutate(xmin = case_when(min(PREPA, PRFloor, PRCeiling, PRWar) <= 20 ~ 0,
                                min(PREPA, PRFloor, PRCeiling, PRWar) >= 50 ~ 50,
                                T ~ min(PREPA, PRFloor, PRCeiling, PRWar) - 5),
               xmax = case_when(max(PREPA, PRFloor, PRCeiling, PRWar) >=80 ~ 100,
                                max(PREPA, PRFloor, PRCeiling, PRWar) <= 50 ~ 50,
                                T ~ max(PREPA, PRFloor, PRCeiling, PRWar) + 5))
      
      ggplot(GameDataDistro) +
        stat_function(
          fun = dnorm,
          args = with(GameDataDistro, c(mean = tail(PRFloor,1), sd = sqrt(tail(posterior_variance_HighFloor, 1)))),
          geom = "area",
          fill = "blue",
          alpha = 0.2,
          aes(color = "blue")) +
        geom_vline(aes(xintercept = tail(PRFloor, 1), color = "blue"), linetype = "dashed")+
        stat_function(
          fun = dnorm,
          args = with(GameDataDistro, c(mean = tail(PRCeiling,1), sd = sqrt(tail(posterior_variance_HighCeiling, 1)))),
          geom = "area",
          fill = "darkgreen",
          alpha = 0.2,
          aes(color = "darkgreen")) +
        geom_vline(aes(xintercept = tail(PRCeiling, 1), color = "darkgreen"), linetype = "dashed")+
        stat_function(
          fun = dnorm,
          args = with(GameDataDistro, c(mean = tail(PREPA,1), sd = sqrt(tail(posterior_variance_EPA, 1)))),
          geom = "area",
          fill = "red",
          alpha = 0.2,
          aes(color = "red")) +
        geom_vline(aes(xintercept = tail(PREPA, 1), color = "red"), linetype = "dashed")+
        stat_function(
          fun = dnorm,
          args = with(GameDataDistro, c(mean = tail(PRWar,1), sd = sqrt(tail(posterior_variance_WAR, 1)))),
          geom = "area",
          fill = "black",
          alpha = 0.2,
          aes(color = "black")) +
        geom_vline(aes(xintercept = tail(PRWar, 1), color = "black"), linetype = "dashed")+
        geom_vline(aes(xintercept = 50), linewidth = 1)+
        geom_hline(aes(yintercept = 0))+
        scale_x_continuous(breaks = seq(0,100,5), limits = c(GameDataDistro$xmin, GameDataDistro$xmax))+
        scale_color_identity(aesthetics = c("color", "fill"))+
        labs(title = paste0(
          "Career Bayesian Updated Floor and Ceiling Distributions"),
          subtitle = "Distributions generated from the Bayesian Updated Mean (dashed line) and Variance (normal curve). Solid black line is the 50th Percentile.",
          y = "Probability Density Function",
          x = "Percentile",
          caption = "Data includes: 2011+, @SethDataScience")+
        theme_reach()+
        facet_wrap(~player)
      
    })

    output$table = DT::renderDataTable({
      
      
      req(input$seasons1)
      
      GameDataFull <- GameData %>% 
        filter((player == input$QB1 & season %in% input$seasons1) |
                 player != input$QB1) %>% 
        group_by(QBname) %>% 
        mutate(QBTime = tryCatch(
          #try to do this
          {
            seq(1,n(),1)
          },
          #if an error occurs, tell me the error
          error=function(e) {
            message('An Error Occurred')
            print(e)
            return(0)
          },
          #if a warning occurs, tell me the warning
          warning=function(w) {
            message('A Warning Occurred')
            print(w)
            return(0)
          },
          finally = 0
        ),
        Season_Change_variable = case_when(lag(season, 1) != season ~ Time-0.5),
        Playoffs_variable = case_when(season < 2021 & week > 17 & between(lag(week, 1), 15, 17) ~ Time-0.5,
                                      season >= 2021 & week > 18 & between(lag(week, 1), 15, 18) ~ Time - 0.5),
        QBSeason_Change_variable = case_when(is.na(Season_Change_variable) == 0 ~ (QBTime-0.5)),
        QBPlayoffs_variable = case_when(is.na(Playoffs_variable) == 0 ~ (QBTime-0.5))) %>%
        group_by(QBname) %>% 
        mutate(
          # Calculate the posterior parameters
          posterior_mean_HighFloor = (mean(GameData$HighFloor) / var(GameData$HighFloor)   + cumsum(HighFloor) / var(HighFloor)) / (1 / var(GameData$HighFloor)   + cumsum(Count) / var(HighFloor)),
          posterior_variance_HighFloor = 1 / (1 / var(GameData$HighFloor)   + cumsum(Count) / var(HighFloor)),
          
          posterior_mean_HighCeiling = (mean(GameData$HighCeiling) / var(GameData$HighCeiling)   + cumsum(HighCeiling) / var(HighCeiling)) / (1 / var(GameData$HighCeiling)   + cumsum(Count) / var(HighCeiling)),
          posterior_variance_HighCeiling = 1 / (1 / var(GameData$HighCeiling)   + cumsum(Count) / var(HighCeiling)),
          
          posterior_mean_WAR = (mean(GameData$WAR) / var(GameData$WAR)   + cumsum(WAR) / var(WAR)) / (1 / var(GameData$WAR)   + cumsum(Count) / var(WAR)),
          posterior_variance_WAR = 1 / (1 / var(GameData$WAR)   + cumsum(Count) / var(WAR)),
          
          posterior_mean_FinEPA = (mean(GameData$FinEPA) / var(GameData$FinEPA)   + cumsum(FinEPA) / var(FinEPA)) / (1 / var(GameData$FinEPA)   + cumsum(Count) / var(FinEPA)),
          posterior_variance_FinEPA = 1 / (1 / var(GameData$FinEPA)   + cumsum(Count) / var(FinEPA)),
          
          MeanFloor = case_when(Time <= (NFLEndSmooth) ~ (posterior_mean_HighFloor + (((NFLEndSmooth+1)-Time)*mean(GameData$HighFloor)))/((NFLEndSmooth+2)-Time),
                                T ~ posterior_mean_HighFloor),
          
          MeanCeiling = case_when(Time <= (NFLEndSmooth) ~ (posterior_mean_HighCeiling + (((NFLEndSmooth+1)-Time)*mean(GameData$HighCeiling)))/((NFLEndSmooth+2)-Time),
                                  T ~ posterior_mean_HighCeiling),
          
          MeanWAR = case_when(Time <= (NFLEndSmooth) ~ (posterior_mean_WAR + (((NFLEndSmooth+1)-Time)*mean(GameData$WAR)))/((NFLEndSmooth+2)-Time),
                              T ~ posterior_mean_WAR),
          
          MeanEPA = case_when(Time <= (NFLEndSmooth) ~ (posterior_mean_FinEPA + (((NFLEndSmooth+1)-Time)*mean(GameData$FinEPA)))/((NFLEndSmooth+2)-Time),
                              T ~ posterior_mean_FinEPA),
          
        ) %>% 
        group_by() %>%   
        filter(!is.na(MeanFloor), !is.na(MeanWAR)) %>%
        mutate(PRWar = percent_rank(MeanWAR),
               PREPA = percent_rank(MeanEPA),
               Diff = PREPA - PRWar,
               PRFloor = percent_rank(MeanFloor),
               PRCeiling = percent_rank(MeanCeiling)) %>% 
        group_by(QBname) %>% 
        mutate(Count = n()) %>% 
        filter(Count >= 4) 
      
      GameDataTable <- GameDataFull %>% 
            group_by(season, player) %>% 
            mutate(Selection = case_when(player == input$QB1 &
                                           season %in% input$seasons1 ~ "Selection",
                                         T ~ "")) %>% 
            slice_tail(n = 1) %>% 
            group_by(player) %>% 
            mutate(Year = seq(1,n(),1)) %>% 
            group_by() %>% 
        mutate(Value = (PRFloor * 3 + PRCeiling * 2 +
                            PRWar * 2 + PREPA)/8) %>% 
        arrange(desc(Value)) %>% 
            arrange(desc(Selection)) %>%
          mutate(season = as.factor(season),
                 Selection = as.factor(Selection),
                 team_name = as.factor(team_name),
                 Year = as.integer(Year)) %>% 
            select(season, Selection, Year, player, team_name,
                   PRFloor, PRCeiling, PREPA, PRWar, Diff, Support, Difficulty)%>% 
          mutate(PRWar = round(round(PRWar,4)*100,2),
                 PREPA = round(round(PREPA,4)*100,2),
                 Diff = round(round(Diff,4)*100,2),
                 PRFloor = round(round(PRFloor,4)*100,2),
                 PRCeiling = round(round(PRCeiling,4)*100,2),
                 Difficulty = round(round(Difficulty, 4)*100,2),
                 Support = round(Support,2)) 
        
        
        PRWarbrks <- quantile(GameDataTable$PRWar, probs = seq(.05, .95, .01), na.rm = TRUE)
        PRWarbrksy <- round(seq(255, 40, length.out = length(PRWarbrks) + 1), 0)
        PRWarbrksclrs <- paste0("rgb(", PRWarbrksy, "," , 255-PRWarbrksy , ",", 0, ")")
        
        PREPAbrks <- quantile(GameDataTable$PREPA, probs = seq(.05, .95, .01), na.rm = TRUE)
        PREPAbrksy <- round(seq(255, 40, length.out = length(PREPAbrks) + 1), 0)
        PREPAbrksclrs <- paste0("rgb(", PREPAbrksy, "," , 255-PREPAbrksy , ",", 0, ")")
        
        PRCeilingbrks <- quantile(GameDataTable$PRCeiling, probs = seq(.05, .95, .01), na.rm = TRUE)
        PRCeilingbrksy <- round(seq(255, 40, length.out = length(PRCeilingbrks) + 1), 0)
        PRCeilingbrksclrs <- paste0("rgb(", PRCeilingbrksy, "," , 255-PRCeilingbrksy , ",", 0, ")")
        
        PRFloorbrks <- quantile(GameDataTable$PRFloor, probs = seq(.05, .95, .01), na.rm = TRUE)
        PRFloorbrksy <- round(seq(255, 40, length.out = length(PRFloorbrks) + 1), 0)
        PRFloorbrksclrs <- paste0("rgb(", PRFloorbrksy, "," , 255-PRFloorbrksy , ",", 0, ")")
        
        
        Diffbrks <- quantile(GameDataTable$Diff, probs = seq(.05, .95, .01), na.rm = TRUE)
        Diffbrksy <- round(seq(255, 40, length.out = length(Diffbrks) + 1), 0)
        Diffbrksclrs <- paste0("rgb(", Diffbrksy, "," , 255-Diffbrksy , ",", 0, ")")
        
        
        Supportbrks <- quantile(GameDataTable$Support, probs = seq(.05, .95, .01), na.rm = TRUE)
        Supportbrksy <- round(seq(255, 40, length.out = length(Supportbrks) + 1), 0)
        Supportbrksclrs <- paste0("rgb(", Supportbrksy, "," , 255-Supportbrksy , ",", 0, ")")
        
        
        Difficultybrks <- quantile(GameDataTable$Difficulty, probs = seq(.05, .95, .01), na.rm = TRUE)
        Difficultybrksy <- round(seq(255, 40, length.out = length(Difficultybrks) + 1), 0)
        Difficultybrksclrs <- paste0("rgb(", Difficultybrksy, "," , 255-Difficultybrksy , ",", 0, ")")
        
        
        
        datatable(GameDataTable, rownames = F, filter = "top",
                  extensions = 'Buttons', options = list(pageLength = 15,
                                                         dom = 'Bfrtip',                      
                                                         buttons = c('csv', 'excel')))%>% 
            formatStyle("Selection",
                        backgroundColor = styleEqual(c("", "Selection"),
                                                     c("","gold"))
            )%>%
            formatStyle("PRWar", 
                        backgroundColor = styleInterval(PRWarbrks,PRWarbrksclrs)
            ) %>%
            formatStyle("PREPA", 
                        backgroundColor = styleInterval(PREPAbrks,PREPAbrksclrs)
            ) %>%
            formatStyle("PRCeiling", 
                        backgroundColor = styleInterval(PRCeilingbrks,PRCeilingbrksclrs)
            ) %>%
            formatStyle("PRFloor", 
                        backgroundColor = styleInterval(PRFloorbrks,PRFloorbrksclrs)
            ) %>%
            formatStyle("Diff", 
                        backgroundColor = styleInterval(Diffbrks,Diffbrksclrs)
            )   %>%
          formatStyle("Support", 
                      backgroundColor = styleInterval(Supportbrks,Supportbrksclrs)
          ) %>%
          formatStyle("Difficulty", 
                      backgroundColor = styleInterval(Difficultybrks,Difficultybrksclrs)
          )
    })
    
    output$table_current = DT::renderDataTable({
      
      
      
      GameDataFull <- GameData %>% 
        mutate(MaxSeason = max(season, na.rm = T)) %>%
        group_by(QBname) %>% 
        mutate(ActivePlayer = if_else(max(season) == MaxSeason, 1, 0)) %>% 
        filter((ActivePlayer == 1 & season == MaxSeason) |
                 ActivePlayer == 0) %>% 
        group_by(QBname) %>% 
        mutate(QBTime = tryCatch(
          #try to do this
          {
            seq(1,n(),1)
          },
          #if an error occurs, tell me the error
          error=function(e) {
            message('An Error Occurred')
            print(e)
            return(0)
          },
          #if a warning occurs, tell me the warning
          warning=function(w) {
            message('A Warning Occurred')
            print(w)
            return(0)
          },
          finally = 0
        ),
        Season_Change_variable = case_when(lag(season, 1) != season ~ Time-0.5),
        Playoffs_variable = case_when(season < 2021 & week > 17 & between(lag(week, 1), 15, 17) ~ Time-0.5,
                                      season >= 2021 & week > 18 & between(lag(week, 1), 15, 18) ~ Time - 0.5),
        QBSeason_Change_variable = case_when(is.na(Season_Change_variable) == 0 ~ (QBTime-0.5)),
        QBPlayoffs_variable = case_when(is.na(Playoffs_variable) == 0 ~ (QBTime-0.5))) %>%
        group_by(QBname) %>% 
        mutate(
          # Calculate the posterior parameters
          posterior_mean_HighFloor = (mean(GameData$HighFloor) / var(GameData$HighFloor)   + cumsum(HighFloor) / var(HighFloor)) / (1 / var(GameData$HighFloor)   + cumsum(Count) / var(HighFloor)),
          posterior_variance_HighFloor = 1 / (1 / var(GameData$HighFloor)   + cumsum(Count) / var(HighFloor)),
          
          posterior_mean_HighCeiling = (mean(GameData$HighCeiling) / var(GameData$HighCeiling)   + cumsum(HighCeiling) / var(HighCeiling)) / (1 / var(GameData$HighCeiling)   + cumsum(Count) / var(HighCeiling)),
          posterior_variance_HighCeiling = 1 / (1 / var(GameData$HighCeiling)   + cumsum(Count) / var(HighCeiling)),
          
          posterior_mean_WAR = (mean(GameData$WAR) / var(GameData$WAR)   + cumsum(WAR) / var(WAR)) / (1 / var(GameData$WAR)   + cumsum(Count) / var(WAR)),
          posterior_variance_WAR = 1 / (1 / var(GameData$WAR)   + cumsum(Count) / var(WAR)),
          
          posterior_mean_FinEPA = (mean(GameData$FinEPA) / var(GameData$FinEPA)   + cumsum(FinEPA) / var(FinEPA)) / (1 / var(GameData$FinEPA)   + cumsum(Count) / var(FinEPA)),
          posterior_variance_FinEPA = 1 / (1 / var(GameData$FinEPA)   + cumsum(Count) / var(FinEPA)),
          
          MeanFloor = case_when(Time <= (NFLEndSmooth) ~ (posterior_mean_HighFloor + (((NFLEndSmooth+1)-Time)*mean(GameData$HighFloor)))/((NFLEndSmooth+2)-Time),
                                T ~ posterior_mean_HighFloor),
          
          MeanCeiling = case_when(Time <= (NFLEndSmooth) ~ (posterior_mean_HighCeiling + (((NFLEndSmooth+1)-Time)*mean(GameData$HighCeiling)))/((NFLEndSmooth+2)-Time),
                                  T ~ posterior_mean_HighCeiling),
          
          MeanWAR = case_when(Time <= (NFLEndSmooth) ~ (posterior_mean_WAR + (((NFLEndSmooth+1)-Time)*mean(GameData$WAR)))/((NFLEndSmooth+2)-Time),
                              T ~ posterior_mean_WAR),
          
          MeanEPA = case_when(Time <= (NFLEndSmooth) ~ (posterior_mean_FinEPA + (((NFLEndSmooth+1)-Time)*mean(GameData$FinEPA)))/((NFLEndSmooth+2)-Time),
                              T ~ posterior_mean_FinEPA),
          
        ) %>% 
        group_by() %>%   
        filter(!is.na(MeanFloor), !is.na(MeanWAR)) %>%
        mutate(PRWar = percent_rank(MeanWAR),
               PREPA = percent_rank(MeanEPA),
               Diff = PREPA - PRWar,
               PRFloor = percent_rank(MeanFloor),
               PRCeiling = percent_rank(MeanCeiling)) %>% 
        group_by(QBname) %>% 
        mutate(Count = n()) %>% 
        filter(Count >= 4) 
      
      
      GameDataTable <- GameDataFull %>% 
        group_by(season, player) %>% 
        mutate(Selection = case_when(player == input$QB1 &
                                       season %in% input$seasons1 ~ "Selection",
                                     T ~ "")) %>% 
        slice_tail(n = 1) %>% 
        group_by(player) %>% 
        mutate(Year = seq(1,n(),1),
               Value = (PRFloor * 3 + PRCeiling * 2 +
                          PRWar * 2 + PREPA)/8) %>% 
        group_by() %>% 
        arrange(desc(PRWar)) %>% 
        arrange(desc(Value)) %>% 
        mutate(season = as.factor(season),
               Selection = as.factor(Selection),
               team_name = as.factor(team_name),
               Year = as.integer(Year)) %>% 
        filter(season  == MaxSeason) %>% 
        select(season, Selection, Year, player,
               team_name,
               PRFloor, PRCeiling, PREPA, PRWar,
               Diff, Support, Difficulty)%>% 
        mutate(PRWar = round(round(PRWar,4)*100,2),
               PREPA = round(round(PREPA,4)*100,2),
               Diff = round(round(Diff,4)*100,2),
               PRFloor = round(round(PRFloor,4)*100,2),
               PRCeiling = round(round(PRCeiling,4)*100,2),
               Difficulty = round(round(Difficulty, 4)*100,2),
               Support = round(Support,2)) 
      
      
      PRWarbrks <- quantile(GameDataTable$PRWar, probs = seq(.05, .95, .01), na.rm = TRUE)
      PRWarbrksy <- round(seq(255, 40, length.out = length(PRWarbrks) + 1), 0)
      PRWarbrksclrs <- paste0("rgb(", PRWarbrksy, "," , 255-PRWarbrksy , ",", 0, ")")
      
      PREPAbrks <- quantile(GameDataTable$PREPA, probs = seq(.05, .95, .01), na.rm = TRUE)
      PREPAbrksy <- round(seq(255, 40, length.out = length(PREPAbrks) + 1), 0)
      PREPAbrksclrs <- paste0("rgb(", PREPAbrksy, "," , 255-PREPAbrksy , ",", 0, ")")
      
      PRCeilingbrks <- quantile(GameDataTable$PRCeiling, probs = seq(.05, .95, .01), na.rm = TRUE)
      PRCeilingbrksy <- round(seq(255, 40, length.out = length(PRCeilingbrks) + 1), 0)
      PRCeilingbrksclrs <- paste0("rgb(", PRCeilingbrksy, "," , 255-PRCeilingbrksy , ",", 0, ")")
      
      PRFloorbrks <- quantile(GameDataTable$PRFloor, probs = seq(.05, .95, .01), na.rm = TRUE)
      PRFloorbrksy <- round(seq(255, 40, length.out = length(PRFloorbrks) + 1), 0)
      PRFloorbrksclrs <- paste0("rgb(", PRFloorbrksy, "," , 255-PRFloorbrksy , ",", 0, ")")
      
      
      Diffbrks <- quantile(GameDataTable$Diff, probs = seq(.05, .95, .01), na.rm = TRUE)
      Diffbrksy <- round(seq(255, 40, length.out = length(Diffbrks) + 1), 0)
      Diffbrksclrs <- paste0("rgb(", Diffbrksy, "," , 255-Diffbrksy , ",", 0, ")")
      
      
      Supportbrks <- quantile(GameDataTable$Support, probs = seq(.05, .95, .01), na.rm = TRUE)
      Supportbrksy <- round(seq(255, 40, length.out = length(Supportbrks) + 1), 0)
      Supportbrksclrs <- paste0("rgb(", Supportbrksy, "," , 255-Supportbrksy , ",", 0, ")")
      
      
      Difficultybrks <- quantile(GameDataTable$Difficulty, probs = seq(.05, .95, .01), na.rm = TRUE)
      Difficultybrksy <- round(seq(255, 40, length.out = length(Difficultybrks) + 1), 0)
      Difficultybrksclrs <- paste0("rgb(", Difficultybrksy, "," , 255-Difficultybrksy , ",", 0, ")")
      
      
      
      datatable(GameDataTable, rownames = F, filter = "top",
                extensions = 'Buttons', options = list(pageLength = 15,
                                                       dom = 'Bfrtip',                      
                                                       buttons = c('csv', 'excel')))%>% 
        formatStyle("Selection",
                    backgroundColor = styleEqual(c("", "Selection"),
                                                 c("","gold"))
        )%>%
        formatStyle("PRWar", 
                    backgroundColor = styleInterval(PRWarbrks,PRWarbrksclrs)
        ) %>%
        formatStyle("PREPA", 
                    backgroundColor = styleInterval(PREPAbrks,PREPAbrksclrs)
        ) %>%
        formatStyle("PRCeiling", 
                    backgroundColor = styleInterval(PRCeilingbrks,PRCeilingbrksclrs)
        ) %>%
        formatStyle("PRFloor", 
                    backgroundColor = styleInterval(PRFloorbrks,PRFloorbrksclrs)
        ) %>%
        formatStyle("Diff", 
                    backgroundColor = styleInterval(Diffbrks,Diffbrksclrs)
        )   %>%
        formatStyle("Support", 
                    backgroundColor = styleInterval(Supportbrks,Supportbrksclrs)
        ) %>%
        formatStyle("Difficulty", 
                    backgroundColor = styleInterval(Difficultybrks,Difficultybrksclrs)
        )
    })
    
    
    output$QBPlot <- renderPlot({
      
      
      QBEPA %>% 
        group_by() %>% 
        filter(between(season, 
                       min(QBEPA$season[QBEPA$QBname == input$QB]),
                       max(QBEPA$season[QBEPA$QBname == input$QB]))) %>% 
        mutate(Alpha = case_when(QBname == input$QB ~ 1,
                                 T ~ 0.6),
               Size = case_when(QBname == input$QB ~ 0.035,
                                T ~ 0.025),
               Count = percent_rank(Count),
               Count = case_when(QBname == input$QB ~ 1,
                                 T ~ Count),
               Label = case_when(QBname == input$QB ~ rank)) %>% 
        ggplot(aes(x = season, y = LastEPA, group = QBname))+
        geom_line(aes(color = Color, alpha = Alpha, size = Size))+
        geom_point(aes(color = Color, fill = Color, alpha = Alpha, size = Count))+
        geom_text(aes(label = Label, color = "white"))+
        geom_smooth(aes(x = season, y = LastEPA, group = ""), method = "lm", se = F)+
        scale_color_identity(aesthetics = c("color", "fill"))+
        scale_x_continuous(breaks = seq(1999,2030,1))+
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
        geom_hline(yintercept = 0, linetype = "dashed")+
        theme_reach()+
        labs(
          y= "Final Cumulative EPA",
          x= "Season",
          title= paste0(input$QB, 
                        " Cumulative Opponent and WP Adjusted EPA"),
          subtitle = "Regular and Postseason included, Number listed is Season Rank,
     (Total Offense Rushes, Passes, Penalties only for games with QB as Starter)",
          caption = "@SethDataScience"
        ) 
    })
    
    output$QBPlot2 <- renderPlot({
      
      
      QBEPA %>% 
        group_by() %>% 
        filter(between(season, 
                       min(QBEPA$season[QBEPA$QBname == input$QB]),
                       max(QBEPA$season[QBEPA$QBname == input$QB]))) %>% 
        mutate(Alpha = case_when(QBname == input$QB ~ 1,
                                 T ~ 0.6),
               Size = case_when(QBname == input$QB ~ 0.035,
                                T ~ 0.025),
               Count = percent_rank(Count),
               Count = case_when(QBname == input$QB ~ 1,
                                 T ~ Count),
               Label = case_when(QBname == input$QB ~ raterank)) %>% 
        ggplot(aes(x = season, y = EPArate, group = QBname))+
        geom_line(aes(color = Color, alpha = Alpha, size = Size))+
        geom_point(aes(color = Color, fill = Color, alpha = Alpha, size = Count))+
        geom_text(aes(label = Label, color = "white"))+
        geom_smooth(aes(x = season, y = EPArate, group = ""), method = "lm", se = F)+
        scale_color_identity(aesthetics = c("color", "fill"))+
        scale_x_continuous(breaks = seq(1999,2030,1))+
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
        geom_hline(yintercept = 0, linetype = "dashed")+
        theme_reach()+
        labs(
          y= "Final Cumulative EPA/Total Plays",
          x= "Season",
          title= paste0(input$QB, 
                        " Cumulative Opponent and WP Adjusted EPA/Total Plays"),
          subtitle = "Regular and Postseason included, Number listed is Season Rank,
     (Total Offense Rushes, Passes, Penalties only for games with QB as Starter)",
          caption = "@SethDataScience"
        ) 
    })
    
    output$QBtable = DT::renderDataTable({
      
      QBEPAfilter <- QBEPA %>% 
        mutate(Selection = case_when(QBname == input$QB ~ "Selection",
                                     T ~ ""),
               LastEPA = round(LastEPA, 2),
               EPArate = round(EPArate, 3)) %>% 
        select(Selection, season, QBname, posteam, LastEPA, EPArate, rank, raterank) %>% 
        filter(season >= input$QBSeason[1] & season <= input$QBSeason[2]) %>% 
        group_by(QBname) %>% 
        mutate(AvgLastEPA = round(mean(LastEPA, na.rm = T),2),
               AvgEPARate = round(mean(EPArate, na.rm = T),3),
               AvgRank = round(mean(rank, na.rm = T),2),
               AvgRateRank = round(mean(raterank, na.rm = T),2)) %>% 
        group_by() %>% 
        arrange(desc(EPArate)) %>% 
        mutate(Rank = seq(1, n(), 1)) %>% 
        select(Rank, everything())  %>%
        arrange(desc(Selection)) %>% 
        select(!rank) %>% 
        select(!raterank) 
      
      LastEPAbrks <- quantile(QBEPAfilter$LastEPA, probs = seq(.05, .95, .01), na.rm = TRUE)
      LastEPAbrksy <- round(seq(255, 40, length.out = length(LastEPAbrks) + 1), 0)
      LastEPAbrksclrs <- paste0("rgb(", LastEPAbrksy, "," , 255-LastEPAbrksy , ",", 0, ")")
      
      AvgLastEPAbrks <- quantile(QBEPAfilter$AvgLastEPA, probs = seq(.05, .95, .01), na.rm = TRUE)
      AvgLastEPAbrksy <- round(seq(255, 40, length.out = length(AvgLastEPAbrks) + 1), 0)
      AvgLastEPAbrksclrs <- paste0("rgb(", AvgLastEPAbrksy, "," , 255-AvgLastEPAbrksy , ",", 0, ")")
      
      EPAratebrks <- quantile(QBEPAfilter$EPArate, probs = seq(.05, .95, .01), na.rm = TRUE)
      EPAratebrksy <- round(seq(255, 40, length.out = length(EPAratebrks) + 1), 0)
      EPAratebrksclrs <- paste0("rgb(", EPAratebrksy, "," , 255-EPAratebrksy , ",", 0, ")")
      
      
      AvgEPARatebrks <- quantile(QBEPAfilter$AvgEPARate, probs = seq(.05, .95, .01), na.rm = TRUE)
      AvgEPARatebrksy <- round(seq(255, 40, length.out = length(AvgEPARatebrks) + 1), 0)
      AvgEPARatebrksclrs <- paste0("rgb(", AvgEPARatebrksy, "," , 255-AvgEPARatebrksy , ",", 0, ")")
      
      
      AvgRankbrks <- quantile(QBEPAfilter$AvgRank, probs = seq(.05, .95, .01), na.rm = TRUE)
      AvgRankbrksy <- round(seq(255, 40, length.out = length(AvgRankbrks) + 1), 0)
      AvgRankbrksclrs <- paste0("rgb(", 255-AvgRankbrksy, "," , AvgRankbrksy , ",", 0, ")")
      
      
      AvgRateRankbrks <- quantile(QBEPAfilter$AvgRateRank, probs = seq(.05, .95, .01), na.rm = TRUE)
      AvgRateRankbrksy <- round(seq(255, 40, length.out = length(AvgRateRankbrks) + 1), 0)
      AvgRateRankbrksclrs <- paste0("rgb(", 255-AvgRateRankbrksy, "," , AvgRateRankbrksy , ",", 0, ")")
      
      
      
      
      
      datatable(QBEPAfilter, rownames = F, filter = "top",
                extensions = 'Buttons', options = list(pageLength = 32,
                                                       dom = 'Bfrtip',                      
                                                       buttons = c('csv', 'excel')), callback = JS(js))%>% 
        formatStyle("Selection",
                    backgroundColor = styleEqual(c("", "Selection"),
                                                 c("","gold"))
        )%>%
        formatStyle("LastEPA", 
                    backgroundColor = styleInterval(LastEPAbrks,LastEPAbrksclrs)
        ) %>%
        formatStyle("EPArate", 
                    backgroundColor = styleInterval(EPAratebrks,EPAratebrksclrs)
        ) %>%
        formatStyle("AvgLastEPA", 
                    backgroundColor = styleInterval(AvgLastEPAbrks,AvgLastEPAbrksclrs)
        ) %>%
        
        formatStyle("AvgEPARate", 
                    backgroundColor = styleInterval(AvgEPARatebrks,AvgEPARatebrksclrs)
        ) %>%
        
        formatStyle("AvgRank", 
                    backgroundColor = styleInterval(AvgRankbrks,AvgRankbrksclrs)
        ) %>%
        
        formatStyle("AvgRateRank", 
                    backgroundColor = styleInterval(AvgRateRankbrks,AvgRateRankbrksclrs)
        ) 
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
