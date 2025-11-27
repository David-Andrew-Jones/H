library(shiny)
library(tidyverse)

#' Data - change paths as needed to run locally
l_fits <- list.files(path = "/Users/dajones/Miscellaneous/HDI/Shiny_calc_CostofCancerbyStage/", pattern = "rds") %>% 
        map(~read_rds(paste0("/Users/dajones/Miscellaneous/HDI/Shiny_calc_CostofCancerbyStage/", .))) %>% 
        setNames(. ,list.files(path = "/Users/dajones/Miscellaneous/HDI/Shiny_calc_CostofCancerbyStage/", pattern = "rds"))






# Define UI for application that draws a histogram
ui <- fluidPage(
        
        # Application title
        titlePanel('Calculate predicted hospital costs'),
        
        # First row of drop downs
        fluidRow( 
                
                column(4, # allcause or net
                       selectInput(
                               inputId = "var_cost_type",
                               label = "Select cost type",
                               choices = c("allcause", "net"))),
                
                column(4, selectInput(
                        inputId = "var_cost_outcome",
                        label = "Select cost outcome",
                        choices = c("Total", "Annual: diag", "Annual: 0-1y", "Annual: 1-2y", "Annual: 2-3",
                                    "Annual: 3-4y", "Annual: 4-5y", "Annual: 5-6y",
                                    "Phase of care: diag", "Phase of care: End of Life", "Phase of care: 0-1y",
                                    "Phase of care: 1-2y", "Phase of care: 2-3y", "Phase of care: 3-4y",
                                    "Phase of care: 4-5y", "Phase of care: 5-6y"))),
                
                column(4, selectInput(
                        inputId = "var_cancer",
                        label = "Select cancer type",
                        choices = c("Colorectal", "Colorectal: Colon and rectosigmoid junction", "Colorectal: Rectal",
                                    "Head and neck", "Liver", "Lung" , 
                                    "Lymphoma", "Lymphoma: Hodgkin", "Lymphoma: High-grade NHL", "Lymphoma: Low-grade NHL",
                                    "Oesophagus", "Ovarian", "Pancreas")))
        ),
        
        # Second row of drop downs
        fluidRow( 
                
                column(2, selectInput(
                        inputId = "var_age",
                        label = "Select age",
                        choices = seq(from=50, to=79, by=1))),
                
                column(2, selectInput(
                        inputId = 'var_stage',
                        label = 'Select stage',
                        choices = c("Stage I", "Stage II", "Stage III", "Stage IV"))),
                
                column(2, selectInput(
                        inputId = 'var_gender',
                        label = 'Select gender',
                        choices = c("Female", "Male"))),
                
                column(2, selectInput(
                        inputId = 'var_ethn',
                        label = 'Select ethnicity group',
                        choices = c("White", "Asian", "Black", "Mixed", "Other"))),
                
                column(2, selectInput(
                        inputId = 'var_IMD',
                        label = 'Select IMD group',
                        choices = c("1 (most deprived)", "2", "3", "4", "5 (least deprived)"))),
                
                column(2, selectInput(
                        inputId = 'var_CCI',
                        label = 'Select CCI group',
                        choices = c("0", "1", "2", "3", "4 plus")))
        ),
        
        #Output

        fluidRow(
                
                column(4, tableOutput(outputId = "table"))
        )
        
)


# Define server logic required to make the map
server <- function(input, output, session) {
        
        # Will have to update this part depending on the names of model fits generated - easier doing it this way so there are nice names in the shiny drop down
        react_outcome <- reactive({
                case_when(input$var_cost_outcome == "Total" ~ "total",
                          input$var_cost_outcome == "Annual: diag" ~ "ac_diag",
                          input$var_cost_outcome == "Annual: 0-1y" ~ "ac_treat_0_1year",
                          input$var_cost_outcome == "Annual: 1-2y" ~ "ac_treat_1_2year",
                          input$var_cost_outcome == "Annual: 2-3y" ~ "ac_treat_2_3year",
                          input$var_cost_outcome == "Annual: 3-4y" ~ "ac_treat_3_4year",
                          input$var_cost_outcome == "Annual: 4-5y" ~ "ac_treat_4_5year",
                          input$var_cost_outcome == "Annual: 5-6y" ~ "ac_treat_5_6year",
                          input$var_cost_outcome == "Phase of care: diag" ~ "diag",
                          input$var_cost_outcome == "Phase of care: End of Life" ~ "eol",
                          input$var_cost_outcome == "Phase of care: 0-1y" ~ "treat_0_1year",
                          input$var_cost_outcome == "Phase of care: 1-2y" ~ "treat_1_2year",
                          input$var_cost_outcome == "Phase of care: 2-3y" ~ "treat_2_3year",
                          input$var_cost_outcome == "Phase of care: 3-4y" ~ "treat_3_4year",
                          input$var_cost_outcome == "Phase of care: 4-5y" ~ "treat_4_5year",
                          input$var_cost_outcome == "Phase of care: 5-6y" ~ "treat_5_6year",
                          .default = NA)
        })
        
        # Will have to update this part depending on the names cancers used - easier doing it this way so there are nice names in the shiny drop down
        react_cancer <- reactive({
                case_when(input$var_cancer == "Colorectal" ~ "Colorectal",
                          input$var_cancer == "Colorectal: Colon and rectosigmoid junction" ~ "Colorectal_colon",
                          input$var_cancer == "Colorectal: Rectal" ~ "Colorectal_rectum",
                          input$var_cancer == "Head and neck" ~ "Head_and_neck",
                          input$var_cancer == "Liver" ~ "Liver",
                          input$var_cancer == "Lung" ~ "Lung",
                          input$var_cancer == "Lymphoma" ~ "Lymphoma",
                          input$var_cancer == "Lymphoma: Hodgkin" ~ "Lymphoma_cHL",
                          input$var_cancer == "Lymphoma: high-grade NHL" ~ "Lymphoma_High_NHL",
                          input$var_cancer == "Lymphoma: low-grade NHL" ~ "Lymphoma_Low_NHL",
                          input$var_cancer == "Oesophagus" ~ "Oesophagus",
                          input$var_cancer == "Ovarian" ~ "Ovary",
                          input$var_cancer == "Pancreas" ~ "Pancreas",
                          .default = NA)
        })
        
        
        react_fit <- reactive({
                
                (keep(l_fits, str_detect(names(l_fits), paste0('allcause', "_", react_cancer() ,".",react_outcome()))))[[1]] 
        })
        
        
        react_patient <- reactive({
                
                tibble(age = as.numeric(input$var_age),
                       stage_best_2 = as.integer(ifelse(input$var_stage == "Stage II", 1, 0)),
                       stage_best_3 = as.integer(ifelse(input$var_stage == "Stage III", 1, 0)),
                       stage_best_4 = as.integer(ifelse(input$var_stage == "Stage IV", 1, 0)),
                       stage_best_missing = as.integer(ifelse(input$var_stage == "missing", 1, 0)),
                       sex_male = as.integer(ifelse(input$var_gender == "Male", 1, 0)),
                       eth_group_asian_or_asian_british = as.integer(ifelse(input$var_ethn == "Asian", 1, 0)),
                       eth_group_black_black_british_caribbean_or_african = as.integer(ifelse(input$var_ethn == "Black", 1, 0)),
                       eth_group_mixed_or_multiple_ethnic_groups = as.integer(ifelse(input$var_ethn == "Mixed", 1, 0)),
                       quintile_2015_2 = as.integer(ifelse(input$var_IMD == "2", 1, 0)),
                       quintile_2015_3 = as.integer(ifelse(input$var_IMD == "3", 1, 0)),
                       quintile_2015_4 = as.integer(ifelse(input$var_IMD == "4", 1, 0)),
                       quintile_2015_5 = as.integer(ifelse(input$var_IMD == "5 (least deprived)", 1, 0)),
                       chrl_tot_78_06_2 = as.integer(ifelse(input$var_CCI == "2", 1, 0)),
                       chrl_tot_78_06_3 = as.integer(ifelse(input$var_CCI == "3", 1, 0)),
                       chrl_tot_78_06_4 = as.integer(ifelse(input$var_CCI == "4 plus", 1, 0)))

        })
                
        react_prediction <- reactive({
                predict(react_fit(), newdata = react_patient(), se.fit = TRUE)
        })
        

        output$table <- renderTable({
                
                tibble(Cost = paste0("£", round(react_prediction()$fit)),
                       SE =  paste0("£", round(react_prediction()$se.fit)))
                
        })
        
}

# Run the application 
shinyApp(ui = ui, server = server)