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


shinyServer(function(input, output, session) {
  
  #### build shiny UI ####
  
  #build legend for trait table
  observeEvent(input$traitlegend, {
    showModal(
      modalDialog(
        title = "Table legend",
        size = "l",
        strong("biomass:"),
        " Fresh weight in mg, calculated with: ", em("exp(a + b(ln body size)), a = -1.72389, b = 2.69638."), "(from Penell et al. 2018)",
        br(),
        br(),
        strong("light preference: "), "calculated from", em("niche position shading (1-value):"), "0,25 - low (prefers shading), 0,5 - indifferent, 0,75 - high (photophilic);
         values from", em("niche width shading"),"(Entling et al. 2007)",
        br(),
        br(),
        strong("moisture prefrence: "), "calculated from", em("niche position moisture (1-value):"), "0,25 - low (xerophilic), 0,5 - indifferent, 0,75 - high (hygrophilic);
        values from", em("niche width moisture"), "(Entling et al. 2007)",
        br(),
        br(),
        strong("forest affinity: "),
        br(),
        tags$ul(
          tags$li(
            "w - mainly found in forests, without preference for light or closed forests"
          ),
          tags$li(
            "wg - mainly found in forests, with strong affinity to closed forest habitats"
          ),
          tags$li(
            "wl - mainly found in forests, with strong affinity to light forests, forest edges, or glades"
          ),
          tags$li(
            "m - occurring in both open landscapes and forest habitats, but without a preference for
                      forest habitats or where a distinction between mm or mo was not possible"
          ),
          tags$li("mm - occurring equally in open landscapes and forest habitats"),
          tags$li(
            "mo - strong affinity to open landscapes, but also regularly occurring in forests,
                      at forest edges, or in glades"
          ),
          tags$li(
            "o - only occurring in open landscapes or other habitats without forest cover like caves or
                      buildings; see Schneider et al. (2021) and Blick et al. (2019)"
          )
        ),
        br(),
        br(),
        em(
          "Schneider A, Blick T, Pauls SU & Dorow WHO 2021 The list of forest affinities for animals in Central Europe –
          A valuable resource for ecological analysis and monitoring in forest animal communities?
         – Forest Ecology and Management 479: 118542 - doi: https://doi.org/10.1016/j.foreco.2020.118542"
        ),
        br(),
        em(
          "Entling W, Brandl R, Schmidt MH, Bacher S & Nentwig W 2007 Niche properties of Central European spiders:
         shading, moisture and the evolution of the habitat niche. –
         Global Ecology and Biogeography 16: 440–448 - doi: 10.1111/j.1466-8238.2006.00305.x "
        ),
        br(),
        em(
          "Blick T, Buchholz S, Kielhorn K-H & Muster C 2019 Die Waldbindung der Spinnen (Araneae) Deutschlands.
         In: Waldbindung ausgewählter Tiergruppen Deutschlands. BfN Skripten 544, Bonn - Bad Godesberg. pp. 26–56 -
         doi: 10.19217/skr544 "
        ),
        br(),
        em(
          "Penell A, Raub F & Höfer H 2018 Estimating biomass from body size of European spiders based on regression models.
         – Journal of Arachnology 46: 413–419 - doi: 10.1636/joa-s-17-044.1"
        )
      )
    )
  })
  
  #build help file for trait data
  observeEvent(input$help_trait, {
    showModal(
      modalDialog(
        title = "Instructions for using trait data with your own species list",
        size = "l",
        "Via",
        em("Upload species list"),
        "users have the ability to upload their own species list for the purpose of
         supplementing it with trait data. Only files with the extension .csv are accepted, and each species name
         must be listed in a separate row without any accompanying header line. It is mandatory that the nomenclature
         utilized adheres to the most up-to-date version of the",
        tags$a(href = "https://wsc.nmbe.ch/", "World Spider Catalog"),
        ". After uploading, the newly generated trait list can be downloaded via",
        em("Download"),
        ".",
        br(),
        br(),
        br(),
        tags$code(
          br(),
          "Acartauchenius scurrilis (O. P.-Cambridge, 1872)",
          br(),
          "Pardosa pullata (Clerck, 1757)",
          br(),
          "Pelecopsis radicicola (L. Koch, 1872)",
          br(),
          "Zelotes petrensis (C.L. Koch, 1839)",
          br(),
          "Zora spinimana (Sundevall, 1833)"
        )
      )
    )
  })
  
  #build help file for ARAphenology
  observeEvent(input$help, {
    if (input$Analysis == "Phenology") {
      showModal(
        modalDialog(
          title = "Explanations about the phenology tool",
          size = "l",
          strong("Quantitative measure:"),
          br(),
          br(),
          em("Relative activity density:"),
          "The metadata, specifically the trapping days and pitfall numbers, are utilized to normalize the count 
           data to a specific point in time. This normalization process enables the comparison of count data across studies.",
          br(),
          br(),
          em("Raw data:"),
          "Untransformed count data is used here.",
          br(),
          br(),
          strong("Collection method:"),
          br(),
          br(),
          "Here, different collection methods can be compared with each other,
           however, so far only data from pitfall traps are implemented.",
          br(),
          br(),
          strong("Maximum collection days"),
          br(),
          br(),
          "This feature permits the user to establish the maximum collection period, which typically refers to the time duration between 
          two emptying dates. In order to minimize the error, the tool calculates the median date for a collection period of 30 days, for 
          instance, between July 1st and July 31st.",
          br(),
          br(),
          strong("Altitude:"),
          "The sliders can be used to narrow down the phenology of the studied species to a certain altitude level."
        )
      )
      
      #build help file for ARAhabitat
    } else if (input$Analysis == "Habitat preferences") {
      showModal(
        modalDialog(
          title = "Explanations about the habitat preferences tool",
          size = "l",
          "The default representation displays the distribution of the chosen species across the first level of the EUNIS classification. 
           To obtain a more detailed resolution of the EUNIS classification, the user must first select the desired", strong("(Habitat Ebene)"),
          "and subsequently the relevant", strong("(Habitat Typ)"), "supercatgeory.",
          br(),
          br(),
          strong("Quantitative measure:"),
          br(),
          br(),
          em("Relative activity density:"),
          "The metadata, specifically the trapping days and pitfall numbers,are utilized to normalize the count data to a specific point in time. 
           This normalization process enables the comparison of count data across studies.",
          br(),
          br(),
          em("Frequency:"),
          "Here, the percentage frequency of a species across the available locations is used",
          br(),
          br(),
        )
      )
      
      #build help file for ARAniche
    } else if (input$Analysis == "Ecological niches") {
      showModal(
        modalDialog(
          title = "Explanations about the ecological niche tools",
          size = "l",
          "The objective of this tool is to provide the user with insights into the ecological niches of the selected species in regards to their 
           light and moisture requirements. The data filtering and calculations follow a methodology similar to that published by Entling et al. (2009):",
          em("Niche properties of Central European spiders: shading, moisture and the evolution of the habitat niche"),".",
          "The tool filters the site scores (which are normalized between 0-1) of the locations where the selected species was observed
          from the overall outcome of the correspondence analysis, and subsequently displays them in the form of a histogram.",
          br(),
          "It should be noted that this tool is currently in the development stage and is largely dependent on the underlying data. In particular, 
          the outcomes of the 2nd gradient, i.e., humidity, may not yet accurately reflect reality, as the moisture gradient in the dataset is not yet 
          broad enough.",
          br(),
          "In the lower graph, the temporally normalized count data in conjunction with the site scores obtained from the correspondence analysis 
          conducted using presence-absence data is displayed. This approach aids in the comparison of the abundance of the selected species in relation to 
          the corresponding niche positions."
        )
      )
      
      #build help file for ARAniche
    } else if (input$Analysis == "Companion species") {
      showModal(
        modalDialog(
          title = "Explanations about the companion species tool",
          size = "l",
          "This section showcases the percentage frequency with which a spider species was observed alongside the selected species. As the frequency threshold 
           on the slider is lowered, the diagram may become less clear. If the number of species surpasses a certain threshold, not all species names will be 
           visible on the y-axis. In such cases, users may hover their mouse over the corresponding bars to display the species name.",
        )
      )
      
      #build help file for ARAaltitude
    } else if (input$Analysis == "Vertical distribution") {
      showModal(
        modalDialog(
          title = "Explanations about the vertical distribution tool",
          size = "l",
          "The default display exhibits the distribution of the chosen species across altitude levels.
           Currently, the altitude levels are aggregated into classes of 100.",
          br(),
          br(),
          strong("Quantitative measure:"),
          br(),
          br(),
          em("Relative activity density:"),
          "The metadata, specifically the trapping days and pitfall numbers, are utilized to normalize the count data to a specific point in time. 
           This normalization process enables the comparison of count data across studies.",
          br(),
          br(),
          em("Frequency:"),
          "Here, the percentage frequency of a species across the available locations is used",
          br(),
          br(),
        )
      )
    }
    
    
  })
  
  #Transfer of user specified species name to the UI for different tools
  output$Spec1 <- renderText(input$SpeciesInput)
  output$Spec2 <- renderText(input$SpeciesInput)
  output$Spec3 <- renderText(input$SpeciesInput)
  output$Spec4 <- renderText(input$SpeciesInput)
  output$Spec5 <- renderText(input$SpeciesInput)
  
  #### ARAphenology Tool ####
  
  #identify maximum number of sites available for ARAphenology
  output$SitesPhen2 <-
    renderText(nrow(as.matrix(unique(Filter_Pheno()[c("LastIdentificationCache","LocalityDescription")]))))
  
  #identify number of sites available after applying user specified filters
  Filter_SitesPhen1 <- reactive({
    MinAltitude <- input$Height[1]
    MaxAltitude <- input$Height[2]
    
    Filter_SitesPhen1 <- subset(
      ARAMOB_Pheno,
        CollectionTimeSpan <= input$TimeSpan &
        Altitude <= MaxAltitude & Altitude > MinAltitude &
        CollectingMethod == input$MethodInput
    )
    
    return(Filter_SitesPhen1)
    
  })
  
  #generate total number of sites available for ARAphenology tool
  output$SitesPhen1 <- renderText(nrow(as.matrix(unique(Filter_SitesPhen1()[c("LocalityDescription")]))))
  
  #applying user specific filters on ARAphenology analysis
  Filter_Pheno <- reactive({
    MinAltitude <- input$Height[1]
    MaxAltitude <- input$Height[2]
    
    Filter_Pheno <-
      subset(
        ARAMOB_Pheno,
          CollectionTimeSpan <= input$TimeSpan &
          Altitude <= MaxAltitude & Altitude > MinAltitude &
          CollectingMethod == input$MethodInput &
          LastIdentificationCache == input$SpeciesInput
      )
    
    return(Filter_Pheno)
    
  })
  
  #start ARAphenology
  Phenology <- reactive({
    SpiderData_Pheno <- Filter_Pheno()[complete.cases(Filter_Pheno()[, c(3, 7)]), ]
    
    #calculate normalized activity density according to Saska et al. (2021)
    if (input$QuantInput == "Relative activity density") {
      SpiderData_Pheno$NumberOfUnits <- ((SpiderData_Pheno$NumberOfUnits / SpiderData_Pheno$CollectionTimeSpan) / SpiderData_Pheno$Value)
    }
    
    #create data frame for ggplot output
    Phenology <- SpiderData_Pheno[, c(9, 8, 5)]
    Phenology$date <- format(as.Date(Phenology$date, format = "%Y-%m-%d"), "%m/%d")
    Phenology$Gender <- as.factor(Phenology$Gender)
    
    return(Phenology)
  })
  
  #ARAphenology plotting
  output$Niche <- renderPlotly({
    #Function for two decimal values in plotting on y axis
    scaleFUN <- function(x) sprintf("%.2f", x)
    
    #Building Plot
    p_both <-
      ggplot(Phenology(), aes(x = as.Date(date, "%m/%d"), y = as.numeric(NumberOfUnits))) +
      geom_point(size = 2, aes(color = Gender, shape = Gender)) +
      labs(x = "",
           color = "Gender") +
      ggtitle(paste("Phenology of", input$SpeciesInput)) +
      scale_x_date(
        date_labels = "%B",
        date_breaks = "1 month",
        limits = as.Date(c('1/1', '12/01'), format = "%m/%d")
      )  +
      scale_y_continuous(labels = scaleFUN) +
      expand_limits(y = 0) +
      theme_gray(base_size = 10)
    
    if (input$QuantInput == "Relative activity density") {
      p_both <- ggplotly(p_both +
                           labs(y = "Relative activity density"))
      
      
    } else {
      if (input$QuantInput == "Activtiy density (Raw data)") {
        p_both <- ggplotly(p_both +
                             labs(y = "Activtiy density (Raw data)"))
      }
    }
    
  })
  
  #### ARAhabitat Tool ####
  
  #get number of sites with a presence value of the selected species to show in UI
  output$SitesEUNIS2 <- renderText({
    length(which(SpiderData_Etrans[, c(input$SpeciesInput)] != 0))
  })
  
  #create data frame showing the total number of sites per EUNIS class and the number of sites with a presence value of the chosen species
  EUNIS_table <- reactive({
    HabitatClass <- input$Habitat %>% substr(start = 1, stop = 1)
    Abundance <- SpiderData_Etrans[, c(input$SpeciesInput)]
    
    if (HabitatClass == "A") {
      EUNISSites <- tibble(Abundance, ARAMOB_Sites_E$first)#, rownames(ARAMOB_Sites_E))  #put it together
      
    } else if (HabitatClass != "A" && input$levels == "2nd Level") {
      EUNISSites  <- tibble(Abundance, ARAMOB_Sites_E$second)
      
    } else if (HabitatClass != "A" && input$levels == "3rd Level") {
      EUNISSites  <- tibble(Abundance, ARAMOB_Sites_E$third)
    }
    
    colnames(EUNISSites) <- c("Abundance", "Class")
    
    if (HabitatClass != "A") {
      EUNISSites <-
        filter(EUNISSites, substr(Class, 1, 1) == HabitatClass)
      
    }
    
    EUNIS_table <- EUNISSites %>%
      group_by(Class) %>%
      dplyr::summarize(Count = sum(Abundance != 0),
                       Total = n())
    
    colnames(EUNIS_table) <-
      c("EUNIS Class", "Sites with presence value", "Total Sites")
    
    return(EUNIS_table)
    
  })
  
  #render above created data frame for UI with error handler
  output$EUNIS_table_count <- renderTable({
    validate(
      need(
        try(EUNIS() != "")
        , "Warning: If an EUNIS class is to be viewed in detail,a sublevel (2 or 3) must be selected. "
      )
    )
    
    EUNIS_table()
  },
  
  hover = TRUE, bordered = TRUE)
  
  #create data frame used in ARAhabitat plot
  EUNIS <- reactive({
    Taxon <- input$SpeciesInput #Transferring Taxon to reactive
    HabitatClass <- input$Habitat %>% substr(start = 1, stop = 1)
    
    Abundance <-
      SpiderData_Etrans[, c(Taxon)] #Choose Taxon from CrossTable (Abundance)
    
    
    if (HabitatClass == "A") {
      Frequ <-
        SpiderData_FreqFirst[c(Taxon), ] #Choose Taxon from CrossTable (Frequency)
      #Abundance <- Abundance/ARAMOB_Sites_E$first_sums #Calculate relative Abundance based on Occurence in EUNIS Class
      EUNIS <-
        tibble(Abundance, ARAMOB_Sites_E$first)#, rownames(ARAMOB_Sites_E))  #put it together
      
    } else if (HabitatClass != "A" && input$levels == "2nd Level") {
      Frequ <- SpiderData_FreqSecond[c(Taxon), ]
      #Abundance <- Abundance/ARAMOB_Sites_E$second_sums
      EUNIS <- tibble(Abundance, ARAMOB_Sites_E$second)
      
    } else if (HabitatClass != "A" && input$levels == "3rd Level") {
      Frequ <- SpiderData_FreqThird[c(Taxon), ]
      #Abundance <- Abundance/ARAMOB_Sites_E$third_sums
      EUNIS <- tibble(Abundance, ARAMOB_Sites_E$third)
    }
    
    colnames(EUNIS) <- c("Abundance", "Class")
    EUNIS <-
      aggregate(
        EUNIS$Abundance,
        by = list(EUNIS$Class),
        FUN = sum,
        drop = FALSE
      )
    Frequ <- as.numeric(t(Frequ))
    EUNIS <- cbind(EUNIS, Frequ)
    colnames(EUNIS) <- c("Class", "Abundance", "Frequency")
    
    if (HabitatClass != "A") {
      EUNIS <- filter(EUNIS, substr(Class, 1, 1) == HabitatClass)
      
    }
    
    EUNISLabels <-
      EUNIS_Hierachy[(EUNIS_Hierachy$EUNIS_Class %in% EUNIS$Class), ]
    EUNISLabels <-
      paste(EUNISLabels[, 1], EUNISLabels[, 2], sep = " ")
    
    EUNIS <- cbind(EUNIS, EUNISLabels)
    colnames(EUNIS) <-
      c("Class", "Abundance", "Frequency", "HabitatName")
    EUNIS <- subset(EUNIS, Abundance != 0)
    
    return(EUNIS)
    
  })
  
  #create ARAhabitat plot
  output$Niche1 <- renderPlotly({
    validate(
      need(
        try(EUNIS() != "")
        , "Warning: If an EUNIS class is to be viewed in detail,
                  a sublevel (2 or 3) must be selected. "
      )
    )
    
    if (input$Dataformat == "Relative activity density") {
      p <- ggplot(EUNIS(), aes(Class, Abundance, fill = HabitatName)) +
        geom_bar(stat = "identity") +
        ggtitle(paste("Habitat types of", input$SpeciesInput)) +
        labs(x = "EUNIS Class", y = "Relative activity density") +
        theme_minimal(base_size = 10)
      
      ggplotly(p) %>%
        layout(legend = list(
          orientation = "h",
          y = -0.25,
          title = ""
        ))
      
    } else {
      p <- ggplot(EUNIS(), aes(Class, Frequency, fill = HabitatName)) +
        geom_bar(stat = "identity") +
        ylim(c(0, 1)) +
        ggtitle(paste("Habitat types of", input$SpeciesInput)) +
        labs(x = "EUNIS Class", y = "Frequency") +
        theme_minimal(base_size = 10)
      options(viewer = NULL)
      ggplotly(p)
    }
  })
  
  #### ARAaltitude Tool ####
  
  #get number of sites with a presence value of the selected species to show in UI
  output$SitesAlti2 <- renderText({
    length(which(CrossTable_Alt_t[, c(input$SpeciesInput)] != 0))
    
  })
  
  #create data frame used in ARAaltitude plot
  Altitude <- reactive({
    #Taxon <- "Alopecosa pulverulenta (Clerck, 1757)" #for testing purposes
    Taxon <- input$SpeciesInput
    Frequ_Alt <- as.numeric(t(SpiderData_FreqAlt[c(Taxon), ]))
    
    Abundance <- CrossTable_Alt_t[, c(Taxon)]
    
    Altitude <- tibble(Abundance, ARAMOB_Sites_Alt$Altitude, ARAMOB_Sites_Alt$CollectionEventID)
    colnames(Altitude) <- c("Abundance", "Altitude")
    
    # Identify outlier values in the Abundance column
    if (input$OutlierRemoval == TRUE) {
      outliers <- boxplot(Altitude$Abundance)$out
      # Replace the outlier values with zeros
      Altitude$Abundance <-
        ifelse(Altitude$Abundance %in% outliers, 0, Altitude$Abundance)
    }
    
    Altitude <- aggregate(Altitude$Abundance, by = list(Altitude$Altitude), FUN = sum, drop = FALSE) %>% 
                  cbind(Frequ_Alt, SumsAltGroups$CountNumber)
    colnames(Altitude) <-  c("Class", "Abundance", "Frequency", "Count")
    
    #Altitude$Abundance <- Altitude$Abundance/Altitude$Count #Normierung auf eine Höhenklasse
    return(Altitude)
    
  })
  
  #create ARAaltitude plot
  output$Altitude <- renderPlotly({
    if (input$DataformatAlt == "Relative activity density") {
      p <- ggplot(Altitude(), aes(Class, Abundance)) +
        geom_bar(
          stat = "identity",
          color = "forestgreen",
          fill = "forestgreen"
        ) +
        ggtitle(paste("Vertical distribution of", input$SpeciesInput)) +
        coord_flip() +
        labs(x = "Metres above sea level", y = "Relative activity density") +
        theme_minimal(base_size = 10)
      
      ggplotly(p)
      
    } else {
      p <- ggplot(Altitude(), aes(Class, Frequency)) +
        geom_bar(
          stat = "identity",
          color = "forestgreen",
          fill = "forestgreen"
        ) +
        ggtitle(paste("Vertical distribution of", input$SpeciesInput)) +
        coord_flip() +
        labs(x = "Metres above sea level", y = "Frequency") +
        theme_minimal(base_size = 10)
      ggplotly(p)
    }
  })
  
  #### ARAniche tool ####
  
  #transpose data
  Crosstable_Niche_t <- as.data.frame(as.matrix(t(CrossTable_Niche)))
  
  NicheResponse <- reactive({
    
    Chosen_Species <- input$SpeciesInput
    #Chosen_Species <- "Mermessus trilobatus (Emerton, 1882)"
    
    Species_Abundance <- Crosstable_Niche_t[, Chosen_Species]
    
    #Deleting rare species according to Entling et al. (2007)
    RareSpecies <- as.matrix(colSums(Crosstable_Niche_t  > 0))
    RareSpecies <- as.matrix(RareSpecies[RareSpecies[, 1] > 5,])
    Crosstable_Niche_t <- Crosstable_Niche_t[, colnames(Crosstable_Niche_t) %in% row.names(RareSpecies)]
    
    #Presence Absence transformation according to Entling et al. (2007)
    Crosstable_Niche_t  <- decostand(Crosstable_Niche_t , "pa")

    #applying corresponcence analysis on data
    ordmodel <- vegan::cca(Crosstable_Niche_t)

    #saving scores  
    Ord_Summary <- vegan::scores(ordmodel) 
    Ord_Sites <- as.data.frame(Ord_Summary$sites) 
    colnames(Ord_Sites) <- c("Ax1", "Ax2")
    
    #scaling Sitescores between 0 an 1
    Scaled_Ord_sites <- decostand(Ord_Sites, "range")
    
    #Choose selected taxon
    Species_Presence <- Crosstable_Niche_t[, Chosen_Species]
    
    #define calibration species for shading
    Calibration_Species_forest <- "Histopona torpida (C.L. Koch, 1837)"
    
    CalibForest_Species_Presence <- Crosstable_Niche_t[, Calibration_Species_forest]
    
    Calibration_Scores_Forest <- as.data.frame(cbind(Scaled_Ord_sites$Ax1, CalibForest_Species_Presence)) %>% 
                                    subset(CalibForest_Species_Presence != 0)
    
    #calibrate model based on species
    if (mean(Calibration_Scores_Forest$V1) < 0.5) {
      Axis1 <- 1 - Scaled_Ord_sites$Ax1
      Axis2 <- 1 - Scaled_Ord_sites$Ax2
    } else {
      Axis1 <- Scaled_Ord_sites$Ax1
      Axis2 <- Scaled_Ord_sites$Ax2
    }
    
    #create species subset
    if (input$Axis == "Axis 1 - Shading") {
      SiteScore_Species <-
        as.data.frame(cbind(Axis1, Species_Presence, Species_Abundance))
    } else if (input$Axis == "Axis 2 - Moisture") {
      SiteScore_Species <-
        as.data.frame(cbind(Axis2, Species_Presence, Species_Abundance))
    }
    
    NicheResponse <- subset(SiteScore_Species, Species_Presence != 0)
    
    return(NicheResponse)
  })
  
  #define number of sites with presence value of selected species to show in user UI
  output$SitesNiche2 <- renderText({
    
    length(which(Crosstable_Niche_t[, c(input$SpeciesInput)] != 0))
    
  })
  
  #generate ARAniche plots
  output$NicheEnt <- renderPlotly({
    #Errorhandling when selected a rare species
    validate(need(try(NicheResponse() != "")
                  , "Warnung: Hier liegen leider nicht genug Daten vor."))
    
    p1 <- ggplot(data = NicheResponse(), aes(x = Axis1)) +
      ggtitle(paste("Niche 1 (Shading) of", input$SpeciesInput)) +
      geom_histogram(
        col = "red",
        fill = "yellow",
        alpha = .4,
        breaks = seq(0, 1, by = input$bins)) +
      labs(x = "Shading value: open land -> forest",
           y = paste("Number of sites")) +
      xlim(c(0, 1)) +
      theme_minimal(base_size = 10)
    
    p2 <- ggplot(data = NicheResponse(), aes(x = Axis2)) +
            ggtitle(paste("Niche 2 (Moisture) of", input$SpeciesInput)) +
            geom_histogram(
              col = "red",
              fill = "blue",
              alpha = .4,
              breaks = seq(0, 1, by = input$bins)) +
              labs(x = "Moisture value: wet -> dry",
                y = paste("Number of sites")) +
              xlim(c(0, 1)) +
              theme_minimal(base_size = 10)
    
    p3 <-  ggplot(data = NicheResponse(), aes(x = Axis1, y = Species_Abundance)) +
            geom_line() +
            geom_area(fill = "yellow", alpha = 0.4) +
            ggtitle(paste("Niche 1 (Shading) of", input$SpeciesInput)) +
            geom_point() +
            labs(x = "Shading value: open land -> forest", y = "Relative activity density") +
            theme_minimal(base_size = 10) +
            xlim(c(0, 1))
    
    p4 <- ggplot(data = NicheResponse(), aes(x = Axis2, y = Species_Abundance)) +
            geom_area(fill = "blue", alpha = .4) +
            ggtitle(paste("Niche 2 (Moisture) of", input$SpeciesInput)) +
            geom_point() +
            geom_line() +
            labs(x = "Moisture value: wet -> dry", y = "Relative activity density") +
            theme_minimal(base_size = 10) +
            xlim(c(0, 1))
    
    #decide which plot to be shown
    if (input$Axis == "Axis 1 - Shading" && input$SpecAbu == TRUE) {
      subplot(p1,
              p3,
              nrows = 2,
              shareX = TRUE,
              titleY = TRUE) #%>% layout(title='text with <br> linebreak')
      
    } else if (input$Axis == "Axis 1 - Shading" &&
               input$SpecAbu == FALSE) {
      ggplotly(p1)
      
    } else if (input$Axis == "Axis 2 - Moisture" &&
               input$SpecAbu == TRUE) {
      subplot(p2,
              p4,
              nrows = 2,
              shareX = TRUE,
              titleY = TRUE)
      
    } else if (input$Axis == "Axis 2 - Moisture" &&
               input$SpecAbu == FALSE) {
      ggplotly(p2)
      
    }
  })
  
#### ARAcoenosis tool ####
  
  #create dataset for sites with selected species presence only
  Filter_SitesCoenosis2 <- reactive({
    
    MinYear <- input$CollectionYear[1]
    MaxYear <- input$CollectionYear[2]
    
    Filter_SitesCoenosis2 <- subset(ARAMOB_Sites,
                                      CollectionTimeSpan >= input$CollectionTimeSpan &
                                      CollectionEndYear <= MaxYear & CollectionEndYear > MinYear)
    
    Filter_SitesCoenosis2 <- filter(ARAMOB_Species,
                                      LocalityDescription %in% Filter_SitesCoenosis2$LocalityDescription)
    Filter_SitesCoenosis2 <-  subset(Filter_SitesCoenosis2, LastIdentificationCache == input$SpeciesInput)
    
    
    return(Filter_SitesCoenosis2)
    
  })
  
  #create data set for total number of sites available
  Filter_SitesCoenosis1 <- reactive({
    
    MinYear <- input$CollectionYear[1]
    MaxYear <- input$CollectionYear[2]
    
    Filter_SitesCoenosis1 <- subset(ARAMOB_Sites,
                                    CollectionTimeSpan >= input$CollectionTimeSpan &
                                    CollectionEndYear <= MaxYear &  CollectionEndYear > MinYear)
    
    Filter_SitesCoenosis1 <- subset(Filter_SitesCoenosis1, LocalityDescription %in% colnames(CrossTable_E))
    
    return(Filter_SitesCoenosis1)
    
  })
  
  #get number of total sites and sites with presence value of selected species to show in UI
  output$SitesCoenosis1 <- renderText(nrow(Filter_SitesCoenosis1()))
  output$SitesCoenosis2 <- renderText(nrow(as.matrix(unique(Filter_SitesCoenosis2()[c("LocalityDescription")]))))
  
  #create data frame for ARAcoenosis plot
  Coenosis <- reactive({
    
    MinYear <- input$CollectionYear[1]
    MaxYear <- input$CollectionYear[2]
    
    Coenosis1 <- subset(ARAMOB_Sites,
                          CollectionTimeSpan >= input$CollectionTimeSpan &
                          CollectionEndYear <= MaxYear & CollectionEndYear > MinYear)
    
    Coenosis1 <- (Coenosis1$LocalityDescription)
    
    Coenosis2 <-CrossTable_E[, (colnames(CrossTable_E) %in% Coenosis1)]

    Coenosis2 <- Coenosis2[, !(Coenosis2[input$SpeciesInput, ] == 0)]
    TotalSites <- ncol(Coenosis2)
    
    Coenosis1 <- as.data.frame(rowSums(Coenosis2 != 0))
    Coenosis1$freq <- Coenosis1[, 1] / TotalSites
    Coenosis1 <- Coenosis1[Coenosis1[, 1] != 0,]
    colnames(Coenosis1) <- c("Sites", "Frequency")
    
    Coenosis1 <- Coenosis1[!(row.names(Coenosis1) %in% input$SpeciesInput), ] %>% 
                    arrange(Sites)
    
    Coenosis <- Coenosis1[Coenosis1[, 2] > input$BegleitFreq,] %>% 
                    rownames_to_column(var = "Taxon")
    
    Coenosis$Taxon <- as.factor(Coenosis$Taxon)
    
    return(Coenosis)
    
  })
  
  #create ARAcoenosis plot
  output$Coen <- renderPlotly({
    if (nrow(Coenosis()) > 20) {
      fig <-
        plot_ly(
          x = Coenosis()$Frequency,
          y = Coenosis()$Taxon,
          type = "bar",
          orientation = "h"
        ) %>%
        layout(
          title = list(
            text = paste("Companion species of ", input$SpeciesInput),
            x = 0.05
          ),
          yaxis = list(
            categoryorder = "array",
            categoryarray = Coenosis()$Taxon,
            tickfont = list(size = 6)
          )
        )
    } else {
      fig <-
        plot_ly(
          x = Coenosis()$Frequency,
          y = Coenosis()$Taxon,
          type = "bar",
          orientation = "h"
        ) %>%
        layout(
          title = list(
            text = paste("Companion species of ", input$SpeciesInput),
            x = 0.05
          ),
          yaxis = list(
            categoryorder = "array",
            categoryarray = Coenosis()$Taxon,
            tickfont = list(size = 12)
          )
        )
    }
    
    fig
    
  })
  
#### Trait data list  ####
  
  #upload handler
  Filtered_TraitData <- reactive({
    if (is.null(input$file1))
      return(NULL)
    
    UserSpeciesList <- read.csv2(input$file1$datapath, header = FALSE)
    
    Filtered_TraitData <-
      filter(TRAIT_list, Species %in% UserSpeciesList$V1) %>% setDT()
    
    return(Filtered_TraitData)
    
  })
  
  #display user specific trait data or all trait data
  output$traitdaten <- renderDataTable({
    if (is.null(input$file1)) {
      TRAIT_list
    } else {
      Filtered_TraitData()
    }
  })
  
  ##trait data download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (is.null(input$file1)) {
        write.csv(TRAIT_list, file)
      } else {
        write.csv(Filtered_TraitData(), file)
      }
    }
  )
  
})