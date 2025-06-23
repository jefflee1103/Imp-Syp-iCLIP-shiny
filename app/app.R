# ==============================================================================
#
# Shiny App for Displaying iCLIP Data from "Imp Syp iCLIP" Sci Adv Publication
#
# Author: Jeffrey Y Lee
# Date: June 2025
#
# Description:
# This Shiny application provides an interactive platform to explore the datasets
# associated with the "Imp Syp iCLIP" Sci Adv publication. It includes an
# introduction to the study, a searchable table of the full dataset, and a
# tool to explore specific gene binding sites.
#
#
# --- FILE STRUCTURE INSTRUCTIONS ---
#
# To run this app, please ensure your directory structure is as follows:
#
# /your_app_directory/
# |-- app.R          (this file)
# |-- full_data.rds  (your main data frame, saved as an RDS file)
# |
# |-- www/           (a folder named 'www' for static files)
# |   |-- science_advances_logo.png (logo for the journal)
# |   |-- intro_image_1.png         (first image for introduction)
# |   |-- intro_image_2.png         (second image for introduction)
# |   |-- intro_image_3.png         (third image for introduction)
# |
# |-- plots/         (a folder named 'plots' for pre-generated ggplots)
# |   |-- GENESYMBOL_1.png  (e.g., FUS.png, TARDBP.png)
# |   |-- GENESYMBOL_2.png
# |   |-- ...
#
# ==============================================================================


# 1. LOAD LIBRARIES
# ------------------------------------------------------------------------------
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)


# 2. LOAD DATA
# ------------------------------------------------------------------------------
# Load your primary dataset. This should be a data frame.
# The `tryCatch` block prevents the app from crashing if the file is not found.
full_data <- tryCatch({
  readRDS("full_data.rds")
}, error = function(e) {
  # Create a placeholder dataframe if the file is missing
  data.frame(
    Gene_Symbol = c("GeneA", "GeneB", "GeneC"),
    chromosome = c("chr1", "chr2", "chrX"),
    log2FC = c(1.5, -0.5, 2.1),
    p_value = c(0.001, 0.2, 0.0005),
    peak_sequence = c("AUCG", "GCUA", "UUUU"),
    notes = c("Note 1", "Note 2", "Note 3"),
    stringsAsFactors = FALSE
  )
})


# 3. DEFINE UI (USER INTERFACE)
# ------------------------------------------------------------------------------
ui <- navbarPage(
  title = "Imp Syp iCLIP targetss",
  collapsible = TRUE,
  
  # --- Main Page Tab ---
  tabPanel("Data",
           sidebarLayout(
             # --- Sidebar Panel ---
             sidebarPanel(
               width = 3,
               
               # Publication Information Panel
               wellPanel(
                 h4("Publication"),
                 tags$p("This web resource accompanies the publication:"),
                 tags$p(
                   tags$strong("Imp/IGF2BP and Syp/SYNCRIP temporal RNA interactomes uncover combinatorial networks of regulators of Drosophila brain development")
                 ),
                 tags$p("Lee JY, et al. (2025)"),
                 # hr(),
                 tags$a(
                   href = "https://www.science.org/doi/10.1126/sciadv.adr6682", 
                   target = "_blank",
                   tags$img(src = "science_advances_logo.png", height = "30px", style = "margin-right: 10px;"),
                   tags$br(),
                   "Link to the Article"
                 ),
                 tags$br(),
                 tags$br(),
                 tags$p(
                   tags$a(href = "https://github.com/jefflee1103/Lee2024_Imp-Syp-iCLIP", icon("github"), "GitHub source data", target = "_blank")
                 )
               ),
               
               # Contact Details Panel
               wellPanel(
                 h4("Contact"),
                 tags$p("Jeffrey Y Lee", tags$a(href = "mailto:jeff.lee@glasgow.ac.uk", icon("envelope"), "Email", target = "_blank")),
                 tags$p("Ilan Davis", tags$a(href = "mailto:ilan.davis@glasgow.ac.uk", icon("envelope"), "Email", target = "_blank")),
                 # hr(),
                 tags$br(),
                 tags$p(
                   tags$a(href = "https://x.com/jefflee1103", icon("x-twitter"), "Twitter", target = "_blank")
                 ),
                 tags$p(
                   tags$a(href = "https://orcid.org/0000-0002-5146-0037", icon("orcid"), "ORCID", target = "_blank")
                 ),
                 tags$p(
                   tags$a(href = "https://bsky.app/profile/jeffylee.bsky.social", icon("bluesky"), "Bluesky", target = "_blank")
                 ),
                 tags$p(
                   tags$a(href = "https://scholar.google.com/citations?user=CxsNxLsAAAAJ&hl=en", icon("google"), "Google Scholar", target = "_blank")
                 )
               )
             ),
             
             # --- Main Panel ---
             mainPanel(
               width = 9,
               tabsetPanel(
                 id = "main_tabs",
                 
                 # Tab 1: Introduction
                 tabPanel("Introduction",
                          h3("Imp Syp iCLIP Data Explorer"),
                          p("This web application provides a user-friendly interface to explore the data from our recent study on the RNA-binding proteins (RBPs) IGFP2BP (Imp) and Syncrip (Syp) during larval brain development iCLIP (individual-nucleotide resolution crosslinking and immunoprecipitation) was used to identify the transcriptome-wide binding sites of these proteins across three developmental stages."),
                          p("Here, you can browse the complete dataset, search for specific genes of interest, and visualise the binding profiles. The goal of this resource is to enhance the reproducibility and accessibility of our findings."),
                          hr(),
                          fluidRow(
                            column(4,
                                   tags$figure(
                                     tags$img(src = "intro_image_1.png", width = "100%", style="border: 1px solid #ddd; border-radius: 4px; padding: 5px;"),
                                     tags$figcaption("Fig. 1: A brief, descriptive caption for the first image, e.g., 'Schematic of the iCLIP experimental workflow.'")
                                   )
                            ),
                            column(4,
                                   tags$figure(
                                     tags$img(src = "intro_image_2.png", width = "100%", style="border: 1px solid #ddd; border-radius: 4px; padding: 5px;"),
                                     tags$figcaption("Fig. 2: Caption for the second image, e.g., 'Volcano plot showing differentially bound transcripts.'")
                                   )
                            ),
                            column(4,
                                   tags$figure(
                                     tags$img(src = "intro_image_3.png", width = "100%", style="border: 1px solid #ddd; border-radius: 4px; padding: 5px;"),
                                     tags$figcaption("Fig. 3: Caption for the third image, e.g., 'Motif analysis of high-confidence binding sites.'")
                                   )
                            )
                          )
                 ),
                 
                 # Tab 2: Explore Full Data Table
                 tabPanel("Explore full data table",
                          h3("Complete iCLIP Dataset"),
                          p("The table below contains the full set of identified Imp and Syp RNA targets. Use the search boxes at the top of each column to filter the data. You can also sort columns by clicking on the headers."),
                          hr(),
                          # Column name descriptions
                          div(style="background-color: #f5f5f5; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                              h4("Column Definitions"),
                              p(HTML("<strong>Gene_Symbol:</strong> The official symbol for the gene associated with the binding site.")),
                              p(HTML("<strong>chromosome:</strong> The chromosome on which the binding site is located.")),
                              p(HTML("<strong>log2FC:</strong> The log2 fold change of binding enrichment.")),
                              p(HTML("<strong>p_value:</strong> The statistical significance of the enrichment.")),
                              p(HTML("<strong>peak_sequence:</strong> The RNA sequence of the identified peak region.")),
                              p(HTML("<strong>...:</strong> Add descriptions for your other columns here."))
                          ),
                          DT::dataTableOutput("full_data_table")
                 ),
                 
                 # Tab 3: Explore Binding Sites
                 tabPanel("Explore Imp/Syp binding sites",
                          h3("Visualize Binding Sites for a Specific Gene"),
                          p("Enter an official gene symbol in the text box below to visualize its pre-computed binding profile and view all associated binding peaks from the dataset."),
                          
                          # Text input for gene search
                          textInput("gene_search_input", "Enter Gene Symbol:", placeholder = "e.g., FUS"),
                          
                          hr(),
                          
                          # Display area for the plot
                          h4(textOutput("plot_title")),
                          imageOutput("gene_plot"),
                          
                          hr(),
                          
                          # Display area for the filtered table
                          h4(textOutput("table_title")),
                          DT::dataTableOutput("filtered_gene_table")
                 )
               )
             )
           )
  )
)


# 4. DEFINE SERVER LOGIC
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- Server logic for Tab 2: Full Data Table ---
  output$full_data_table <- DT::renderDataTable({
    DT::datatable(
      full_data,
      options = list(
        pageLength = 15,
        autoWidth = TRUE,
        scrollX = TRUE
      ),
      filter = 'top', # Adds search boxes to each column
      rownames = FALSE,
      class = 'cell-border stripe'
    )
  })
  
  # --- Server logic for Tab 3: Gene-specific Exploration ---
  
  # Reactive value for the searched gene, converting to uppercase for consistency
  searched_gene <- reactive({
    req(input$gene_search_input) # Ensure input is not empty
    toupper(trimws(input$gene_search_input))
  })
  
  # Dynamic title for the plot
  output$plot_title <- renderText({
    paste("Binding Profile for", searched_gene())
  })
  
  # Dynamic title for the filtered table
  output$table_title <- renderText({
    paste("Binding Sites in", searched_gene())
  })
  
  # Render the pre-generated plot image
  output$gene_plot <- renderImage({
    # Construct the file path for the plot
    # Assumes plots are saved as 'GENESYMBOL.png' in the 'plots/' directory
    image_path <- file.path("plots", paste0(searched_gene(), ".png"))
    
    if (!file.exists(image_path)) {
      # Handle case where plot for the entered gene does not exist
      # You can create a placeholder "Not Found" image and point to it here
      # For now, it returns an empty, invisible plot
      return(list(src = "",
                  contentType = "image/png",
                  alt = "Plot not found for the specified gene.",
                  width = 0, height = 0))
    }
    
    # Return a list containing information about the image
    list(src = image_path,
         contentType = "image/png",
         alt = paste("Plot for", searched_gene()),
         width = "100%", # Make the plot responsive to panel width
         height = "auto")
    
  }, deleteFile = FALSE) # IMPORTANT: Set to FALSE as we are using static files
  
  # Render the filtered data table for the searched gene
  output$filtered_gene_table <- DT::renderDataTable({
    # `req` ensures that this code only runs when searched_gene() is available
    req(searched_gene()) 
    
    # Filter the main dataframe. This assumes you have a 'Gene_Symbol' column.
    # The `toupper` ensures case-insensitive matching.
    filtered_df <- full_data %>%
      filter(toupper(Gene_Symbol) == searched_gene())
    
    DT::datatable(
      filtered_df,
      options = list(
        pageLength = 10,
        autoWidth = TRUE
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    )
  })
  
}


# 5. RUN THE APPLICATION
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
