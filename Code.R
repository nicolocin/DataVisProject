library(shiny)
library(dplyr)
library(knitr)
library(DT)
library(googleVis)
library(shinythemes)
library(rmarkdown)

dat <- read.csv("inpatientCharges2.csv", stringsAsFactors = FALSE) %>% tbl_df

# Ordering by # of discharges for each diagnosis 
datOrder <- select(dat, drg_code, discharges) %>% group_by(drg_code) %>%
  summarise(n=n()) %>% arrange(desc(n)) %>% mutate(rank = c(1:100)) 

################################################################################
# Map DRG codes to Major Diagnostic Criteria (MDC)
################################################################################
MDC.Code <- c(1,3,4,5,6,7,8,9,10,11,16,18,19,20,21,23)
MDC <- c("Nervous system","Ear, Nose, Mouth & Throat","Respiratory System",
         "Circulatory System", "Digestive System","Hepatobillary System & Pancreas",
         "Musculoskeletal System & Connective Tissue", 
         "Skin, Subcutaneous Tissue & Breast",
         "Endocrine, Nutritional & Metabolic System", "Kidney & Urinary Tract",
         "Blood, Blood Forming Organs & Immunological Disorders",
         "Infectious & Parasitic Disease & Disorders",
         "Mental Diseases & Disorders","Alcohol/Drug Use or Induced Mental Disorders",
         "Injuries, Poison & Toxic Effect of Drugs","Factors Influencing Health Status")
start <- c(20,129,163,215,326,405,453,573,614,652,799,853,876,894,901,939)
end <- c(103,159,208,316,395,446,566,607,645,700,816,872,887,897,923,951)


add_mdc <- function(data){
  n <- nrow(data)
  if(!is.numeric(n)) return(NULL)
  data$MDC.Code <- rep(0,n)
  data$MDC <- rep(0,n)
  
  for(i in 1:nrow(data)){
    for(j in 1:length(MDC)){
      if(start[j] <= data$drg_code[i] && data$drg_code[i] <= end[j]){
        data$MDC.Code[i] = MDC.Code[j]
        data$MDC[i] = MDC[j]
        break
      }}}
  return(data)
}


################################################################################
# Map State to Region
################################################################################
library(datasets)
data(state)
state.abb[51] <- "DC"
state.name[51] <- "Washington DC"
state.region[51] <-"South"
state.division[51] <- "South Atlantic"

add_state_info <- function(data){
  n <- nrow(data)
  if(!is.numeric(n)) return(NULL)
  data$State <- rep("",n)
  data$Region <- rep("",n)
  data$Division <- rep("",n)
  
  for(i in 1:nrow(data)){
    for(j in 1:51){
      if(data$state[i] == state.abb[j]){
        data$State[i] <- state.name[j]
        data$Region[i] <- as.character(state.region)[j]
        data$Division[i] <- as.character(state.division)[j]
        break
      }}}
  return(data)
}

mr <- function(x) {round(mean(x),0)}
sf <- function(x) {round(mean(x)*100,2)}

################################################################################
# 1 - Average charge and payments by region and state
################################################################################
byState <- group_by(dat, state) %>%
  summarise(Average.Covered.Charges= mr(charge) ,
            Average.Total.Payments=mr(total), 
            Average.Medicare.Payments=mr(mc),
            Average.Total.Medicare.Payments.Difference = mr(total-mc),
            Total.Discharges=n()) %>% 
  add_state_info %>% rename(State.Abb = state)

perChange <- function(var, base){ (var-base)/base}
avgACC <- mr(byState$Average.Covered.Charges)
avgATP <- mr(byState$Average.Total.Payments)
avgAMP <- mr(byState$Average.Medicare.Payments)
avgDiff <- mr(byState$Average.Total.Medicare.Payments.Difference)


################################################################################
# 2 - Average charge and payments by diagnosis
################################################################################

byMDC <- group_by(dat, drg_code, drg_name) %>% 
  summarise(
    Average.Covered.Charges= mr(charge) ,
    Average.Total.Payments=mr(total), 
    Average.Medicare.Payments=mr(mc), 
    Average.Total.Medicare.Payments.Difference = mr(total-mc),
    Total.Discharges=n()) %>%
  ungroup %>% add_mdc %>% 
  rename(DRG.Code= drg_code, DRG.Description=drg_name) %>% 
  arrange(desc(Total.Discharges))


################################################################################
# Shiny app
################################################################################
# Server
server <- function(input, output, session) {
  
  ##################################################
  # Map
  ##################################################
  
  output$map <- renderGvis({
    gvisGeoChart(byState, locationvar='State', colorvar= input$mapVar,
                 options=list(title= "Average Medicare Payment and Charges by State",
                              region='US', height=500, width=900,
                              displayMode='regions', resolution="provinces",
                              colorAxis="{colors:['red', 'blue']}"))
  }
  )

    
  output$mapTable = renderDataTable({
    select(byState, State.Abb, State, Region, Division,  
           input$mapVar, Total.Discharges)
  }, options = list(orderClasses = TRUE, 
                    lengthMenu = c(5, 10, 20), pageLength = 10))
  
  ##################################################
  # PLOT
  ##################################################
  
  
  # Filter range
#  data <- reactive({
#    limit <- input$plotRangeInput
#    range <- datOrder[1:limit,]$drg_code
#    byMDC %>filter% (DRG.Code %in% range) 
#  })


  # Graph plot
  output$plot <- renderGvis({
    gvisBubbleChart(byMDC, idvar="DRG.Description",
                    xvar= input$plotX, yvar= input$plotVar, colorvar="MDC", sizevar="Total.Discharges",
                    options=list(title= "Average Medicare Payment and Charges by Diagnosis",
                                 height=600, width=1150, explorer="{}",
                                 chartArea="{left:70, width:'70%',height:'80%'}", 
                                 bubble="{opacity:0.4, stroke:'none', textStyle:{color: 'none'}}"))
  })
  
  output$plotTable = renderDataTable({
    select(byMDC, MDC.Code, MDC, DRG.Code, DRG.Description, 
           input$plotVar, Total.Discharges) 
  }, options = list(orderClasses = TRUE, 
                    lengthMenu = c(5, 10, 20), pageLength = 10))
  
} 


################################################################################
# User interface
################################################################################
# MAP 
map_well <- wellPanel(
  h4("Compare Medicare Charges by State"),
  
  br(),
  
  selectInput("mapVar", "Choose variable to display",
              choices =  c("Average Covered Charges ($)" = "Average.Covered.Charges", 
                           "Average Medicare Payments ($)" = "Average.Medicare.Payments", 
                           "Average Total Payments ($)" = "Average.Total.Payments")),
  #                           "Average Total - Medicare Payments ($)" = 
  #                             "Average.Total.Medicare.Payments.Difference")),
  
  br(),
  
  helpText("Note: the difference between Total and Medicare payments",
           "includes co-payment, deductible amounts that the patient",
           "is responsible for, and if the amount paid for any other insurance",
           "that the patient may have.")
  
)

# PLOT
plot_sidebar <- sidebarPanel(
  
  h4("Compare Medicare Charges Data by Diagnosis"),
  
  br(),
  p("The color and size of
    each bubble represents the MDC (Major Diagnosis) and total 
    number of discharges for each diagnosis"),
  
  br(),
  
  selectInput("plotVar", "Choose variable to display",
              choices =  c("Average Covered Charges ($)" = "Average.Covered.Charges", 
                           "Average Medicare Payments ($)" = "Average.Medicare.Payments", 
                           "Average Total Payments ($)" = "Average.Total.Payments")),
  #                           "Average Total - Medicare Payments ($)" = 
  #                             "Average.Total.Medicare.Payments.Difference")),
  
  br(),
  
  radioButtons("plotX", "Arrange X-Axis by",
               c("MS-DRG" = "DRG.Code",
                 "Total Discharges" = "Total.Discharges")),
  
  br(),
  
#  sliderInput("plotRangeInput", 
#              label = "Number of Diagnoses to Display:",
#              min = 10, max = 100, value = 100),
#  
#  br(),
  
  helpText("* MCC - Major Comorbid Conditions"),
  helpText("  CC Comorbid Conditions")
  
  )


ui <- function(request) {
  navbarPage("Inpatient Medicare Charges and Payments Visualization",
             theme = shinytheme("spacelab"),
             
             tabPanel("Interactive Map by State",
                      column(3, map_well),
                      column(9, 
                             htmlOutput("map"),
                             dataTableOutput("mapTable"))
             ),
             
             tabPanel("Interactive Plot by Diagnosis",
                      sidebarLayout( 
                        plot_sidebar,
                        mainPanel(
                          htmlOutput("plot"),
                          dataTableOutput("plotTable")
                        )
                      )
             ),
             
             tabPanel("About",
                      includeMarkdown("About.Rmd")
             ),
             
             tabPanel("Findings",
                      includeMarkdown("Findings.Rmd")
             )
  )
}


################################################################################
# Run app
################################################################################
enableBookmarking(store = "url")
shinyApp(ui = ui, server = server)


