###############################################################

# ARAapp: Filling gaps in the ecological knowledge of spiders using 
# an automated and dynamic approach to analyze systematic collected community data

#Bach et al. 2023

###############################################################

# R code to reproduce the shiny application and analyses
# missing database querries and first preprocessing steps

# Author: Alexander Bach
# Last update: 30.06.2023

###############################################################


header <- dashboardHeader(title = "ARAapp v1.10")

sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebarmenu",
    menuItem(div(HTML("<b>Welcome</b>")), tabName = "welcome", icon = icon("home", lib = "glyphicon"),
              selected = TRUE, startExpanded = TRUE,
             menuSubItem("Start", tabName = "welcome", selected = TRUE, icon = NULL),
             menuSubItem("News", tabName = "changelog", selected = FALSE, icon = NULL),
             menuSubItem("FAQ", tabName = "faq", selected = FALSE, icon = NULL)),
    menuItem(div(HTML("<b>Tools</b>")), tabName = "tools", icon = icon("stats", lib = "glyphicon"),
             selected = FALSE),
    menuItem(div(HTML("<b>Trait data</b>")), tabName = "traits", icon = icon("wrench", lib = "glyphicon"),
             selected = FALSE)),
    conditionalPanel(
      condition = "input.sidebarmenu == 'tools'",
    selectInput("SpeciesInput", "Species", Species),
    selectInput("Analysis", "Tool", Tool, selected = "Begleitarten"),
    actionButton("help", label = "Help", icon = icon("question-sign", lib = "glyphicon"))),
    conditionalPanel(
      condition = "input.sidebarmenu == 'traits'",
      br(),
      downloadButton("downloadData", "Download", class = "butt1"),
      tags$style(type='text/css', "#downloadData { margin-left: 15px; color: #444;}"),
      br(),
      br(),
      fileInput("file1", "Upload species data",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values",
                  ".csv"),
      ),
      actionButton("help_trait", label = "Help", icon = icon("question-sign", lib = "glyphicon")),
      tags$style(type='text/css', "#help_trait { text-align: left; width: 95px;}"),
      actionButton("traitlegend", label = "Legend", icon = icon("info-sign", lib = "glyphicon")),
      tags$style(type='text/css', "#traitlegend { text-align: left; width: 95px;}")
    )
)

body <- dashboardBody(
    
  tabItems( 
    tabItem(tabName = "welcome", #WelcomePage
            h2("Welcome to the Exploratory Analysis Tools of the ARAMOB research data repository"),
            #h3("Analyzing pitfall trap data"),
            br(),
            h4("Community data available for analyses:"),
            br(),
          fluidRow(
            valueBox(NoOfSites, "Sites", color = "aqua", icon = icon("globe", lib = "glyphicon")),
            valueBox(NoOfSpec, "Species", color = "aqua", icon = icon("spider")),
            valueBox(NoOfUnits, "Individuals", color = "aqua", icon = icon("option-horizontal", lib = "glyphicon"))
            ),
          fluidRow(
            box(title = img(src='dfg_logo_blau.jpg', align = "left", style="width: 200px"), width = 4, headerBorder = FALSE,
                "The ARAMOB Exploratory Analysis Tools were developed within the DFG project", em("Semantic enrichment 
                 and mobilization of data in distributed repositories for taxonomy and ecology of spiders"),
                br(),
                br(),
                tags$a(href = "https://aramob.de/de/projekt/dfg-projekt/", "More...")),
            box(title = img(src='DWB.png', align = "left", style="width: 80px"), width = 4, headerBorder = FALSE,
              "The foundation of the data utilized in the project stems from the research data that was mobilized, 
               enriched, and subsequently stored in the Diversity Workbench. virtual research environment.",
              br(),
              br(),
              tags$a(href = "https://diversityworkbench.net/Portal/Diversity_Workbench", "More...")),
            box(title = img(src='AraGes.png', align = "left", style="width: 45px"), width = 4, headerBorder = FALSE,
                "The Arachnologische Gesellschaft is a German association that aims to foster scientific exchange regarding the taxonomy, 
                 biology, and ecology of arachnids located within Central Europe.",
                br(),
                br(),
                tags$a(href = "https://arages.de", "More...")),
          )
          ), #end WelcomePage
    
    tabItem(tabName = "changelog", #Changelog page
          fluidRow(
            column(width = 6,
            box(width = NULL,
                title = "23.02.2023",
                tags$ul(tags$li("changing language from german to english"))),
            box(width = NULL,
                title = "05.10.2022",
                tags$ul(tags$li("the companion species tool now has additional manual filtering options"))),
            box(width = NULL,
                title = "26.08.2020",
                tags$ul(tags$li("uploading and downloading of own species lists to filter the trait list is now possible"))),
            box(width = NULL,
              title = "19.08.2020",
              tags$ul(tags$li("FAQ implementation"),
                      tags$li("News implenmantion"),
                      tags$li("fixing minor display errors"))
            ),
            box(width = NULL,
              title = "12.08.2020",
              tags$ul(tags$li("implementation of the companion species tool"),
                      tags$li("fixing minor display errors"))
                ),            
            box(width = NULL,
              title = "24.07.2020",
              tags$ul(tags$li("implementing ecological niche tool"),
                      tags$li("minor bug fixes"))
               ),
            box(width = NULL,
              title = "11.06.2020",
              tags$ul(tags$li("Tools have gone online"))
               )
            ))), #End changelog page
    
    tabItem(tabName = "faq", #FAQ page
            h2("Frequently Asked Questions"),
            br(),
            box(title = "Where is the data sourced from?", collapsed = TRUE, collapsible = TRUE, width = 9,
                "All the data used for the analyses presented here are obtained from the", 
                tags$a(href = "https://aramob.de/de/datenbasis/datenbasis/", "ARAMOB data repository"),"."),
            box(title = "Which data is used in the tools?", collapsed = TRUE, collapsible = TRUE, width = 9,
                "Currently, we exclusively utilize data derived from ground trap investigations to ensure better comparability. However, 
                in the future, users will have the ability to actively customize  this selection within the modules."),
            box(title = "Why does the app display data form more sites than the ARAMOB site?", collapsed = TRUE, collapsible = TRUE, width = 9,
                "With ARAMOB, users have the option to archive their data either publicly or privately. The tools always access the complete dataset, 
                whereas only the datasets made available to the public can be accessed on the", tags$a(href = "https://aramob.de/de/auswertung/", 
                "ARAMOB evaluation page."), "The ARAMOB data records themselves cannot be accessed directly from the tools."),
            box(title = "Why do the number of sites vary between different tools?", collapsed = TRUE, collapsible = TRUE, width = 9,
                "Each module employs distinct filters and has specific requirements regarding data completeness. For instance, the phenology module 
                 necessitates high temporal resolution data. Older data, such as those from sources like gray literature (e.g., master's, diploma, or 
                 doctoral theses), are often only available in aggregated form over the study period, making them unsuitable for phenological studies."),
            box(title = "How are the relative count numbers calculated?", collapsed = TRUE, collapsible = TRUE, width = 9,
                "To calculate the relative counts of individuals in relation to each other, a normalization process is required to account for 
                variations in time and space. For this purpose, we utilize the Catch per Unit Effort (CPUE) method, which has been adapted to pitfall trap 
                data by Saska et al. (2021)."),
            box(title = "Where can I report errors?", collapsed = TRUE, collapsible = TRUE, width = 9,
                "For any errors or general inquiries about the tools, please feel free to contact alexander.bach [at] bio5.rwth-aachen.de at any time."),
            
            ), #end FAQ page
  
    tabItem(tabName = "tools", #Tools
        conditionalPanel( 
      condition = "input.Analysis == 'Phenology'",
    box(titel = "Plot", width = 12, plotlyOutput("Niche")
    )),
    conditionalPanel(
      condition = "input.Analysis == 'Habitat preferences'",
      box(width = 12, plotlyOutput("Niche1")
    )),
    conditionalPanel(
      condition = "input.Analysis == 'Vertical distribution'",
      box(width = 12, plotlyOutput("Altitude")
      )),
    conditionalPanel(
      condition = "input.Analysis == 'Ecological niches'",
      box(width = 12, plotlyOutput("NicheEnt")
      )),
    conditionalPanel(
      condition = "input.Analysis == 'Companion species'",
      box(width = 12, plotlyOutput("Coen")
      )),

    conditionalPanel( #ARAphenology
      condition = "input.Analysis == 'Phenology'",
    box(width = 6,
      strong(textOutput("Spec1", inline = TRUE)), "was found in", textOutput("SitesPhen2", inline = TRUE), "of", textOutput("SitesPhen1", inline = TRUE), " sites." 
    ),
    box(titel = "Parameter",
      selectInput("QuantInput", "Quantitative measure", list("Relative activity density", "Activity density (Raw data)"), 
                  selected = "Relative activity density"),
      sliderInput("TimeSpan", "Maximum collection days", min = 1, max = 60, step = 1, value = 31),
      sliderInput("Height", "Altitude", min = 0, max = 2000, step = 100, value = c(0, 2000)),
      checkboxInput("expert", "Expert options", value = FALSE),
      conditionalPanel(
        condition = "input.expert == 1",
        selectInput("MethodInput", "Collection Method", choices = levels(ARAMOB_Pheno$CollectingMethod),
                    selected = "Bodenfalle")
        ))
    ), #end ARAphenology
    
    conditionalPanel( #ARAniche
      condition = "input.Analysis == 'Ecological niches'",
      box(
        strong(textOutput("Spec4", inline = TRUE)), "was found in", textOutput("SitesNiche2", inline = TRUE), "of", SitesNiche1, " sites."
        ),
      box(titel = "Parameter",
          checkboxInput("SpecAbu", "Show relative activity density on site scores", value = TRUE),
          radioButtons("Axis", "Gradient", choices = list("Axis 1 - Shading", "Axis 2 - Moisture")),
          sliderInput("bins", "Bin width", min = 0, max = 1, step = .001, value = .05))
    ), #end ARAniche
    
    conditionalPanel( #ARAcoenosis
      condition = "input.Analysis == 'Companion species'",
      box(
        strong(textOutput("Spec5", inline = TRUE)), "was found in", textOutput("SitesCoenosis2", inline = TRUE), "of", textOutput("SitesCoenosis1", inline = TRUE), " sites."
      ),
      box(titel = "Parameter",
          sliderInput("BegleitFreq", "Frequency limit value to plot companion species", value = 0.5, min = 0, max = 1, step = 0.01, sep = ""),
          sliderInput("CollectionYear", "Collection Year", value = c(ColMinYear, ColMaxYear), min = ColMinYear, max = ColMaxYear, step = 1, sep = ""),
          sliderInput("CollectionTimeSpan", "Minimum Collection Time Span", value = 1, min = 0, max = 365, step = 1))
    ), #end ARAcoenosis
      
      conditionalPanel( #ARAhabitat 
        condition = "input.Analysis == 'Habitat preferences'",
        box(width = 6, tableOutput("EUNIS_table_count")),
        box(
        selectInput("Dataformat", "Quantitative measure", list("Relative activity density", "Frequency"), selected = "Relative activity density"),
        selectInput("Habitat", "EUNIS Class", list( "All",
                                                     "C: Inland surface waters",
                                                     "D: Mires, bogs and fens", 
                                                     "E: Grassland and lands dominated by forbs, mosses and lichens",
                                                     "F: Heathland, scrub and tundra",
                                                     "G: Woodland, forest and other wooded land",
                                                     "H: Inland unvegetated or sparsely vegetated habitats",
                                                     "I: Regularly or recently cultivated agricultural, horticultural and domestic habitats"
        ), selected = "All"),
        radioButtons("levels", "Habitat Level", list("1st Level", "2nd Level", "3rd Level")))
        ), #end ARAhabitat 
    
    
    
    conditionalPanel( #ARAaltitude 
      condition = "input.Analysis == 'Vertical distribution'",
      box(
        strong(textOutput("Spec3", inline = TRUE)), "has been found in", textOutput("SitesAlti2", inline = TRUE), "of", SitesAlti1, " sites." 
      ),
      box(
        selectInput("DataformatAlt", "Quantitative measure", list("Relative activity density", "Frequency"), selected = "Relative activity density"),
        checkboxInput("OutlierRemoval", "Remove Outliers", value = FALSE)
      ))#end #ARAaltitude 
    

    
    ), #end tools
    
    #traitdata
    tabItem(tabName = "traits",
            box(dataTableOutput("traitdaten"), width = 10)
            )
  ))

footer <- dashboardFooter(right = "RWTH Aachen University, Institute for Environmental Research: Alexander Bach, 
                                              Richard Ottermanns, Martina Roß-Nickoll",
                          br())

dashboardPage(
  header,
  sidebar,
  body,
  footer = footer
)