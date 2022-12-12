#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Employee Attrition"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      selectInput("variable", 
                  label="Please select a Variable to View",
                  choices=c("Attrition","Age", "MonthlyIncome", "Over Time" ))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
  library(lares)
  library(readr)
  plot_data <- reactive({
    inFile = input$file1
    data <- read.csv(inFile$datapath)
    }) 
    output$distPlot <- renderPlot({
      df = plot_data()
      if(input$variable == "Attrition"){
        
        library(ggplot2)
        #install.packages("cowplot")
        library(cowplot)
        attritions_number <- df_raw%>% group_by(Attrition) %>% dplyr::summarise(Count=n()) %>% ggplot(aes(x=Attrition, y=Count)) + geom_bar(stat="identity", fill="orange", color="grey40") + theme_bw() + coord_flip() + geom_text(aes(x=Attrition, y=0.01, label= Count), hjust=-0.8, vjust=-1, size=3,  colour="black", fontface="bold", angle=360) + labs(title="Employee Attrition (Amount)", x="Employee Attrition",y="Amount") + scale_fill_few(palette = 'Dark')+ theme_few()
        
        attrition_percentage <- df %>% group_by(Attrition) %>% dplyr::summarise(Count=n()) %>% 
          mutate(pct=round(prop.table(Count),2) * 100) %>% 
          ggplot(aes(x=Attrition, y=pct)) + geom_bar(stat="identity", fill = "dodgerblue", color="grey40") + 
          geom_text(aes(x=Attrition, y=0.01, label= sprintf("%.2f%%", pct)),
                    hjust=0.5, vjust=-3, size=4, 
                    colour="black", fontface="bold") + theme_bw() + labs(x="Employee Attrition", y="Percentage") + 
          labs(title="Employee Attrition (%)") + scale_fill_few(palette = 'Dark')+ theme_few()
        
        
        
        plot=plot_grid(attritions_number, attrition_percentage, align="h", ncol=2)
        
      } else if(input$variable == "Age"){
        plot = ggplot(df, aes(x = Age, fill = Attrition)) + geom_histogram(color="black")+
          labs(title="Age vs.Attirition",x="Attrition", y = "Monthly Income $")+ scale_fill_few(palette = 'Dark')+ theme_few() + ggtitle("Edwards Neighborhood Price of Home vs. Square Footage")
        
      } else if(input$variable == "MonthlyIncome"){
        # MonthlyIncome
        plot= ggplot(df, aes(x=Attrition, y=MonthlyIncome, color=Attrition, fill= Attrition)) +
          geom_boxplot( color = "black")+
          labs(title="Monthly Income vs. Attrition",x="Attrition", y = "Monthly Income $")+ scale_fill_few(palette = 'Dark')+ theme_few()        
      }
      plot
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
