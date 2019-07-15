library(readxl)
library(dragulaR)
library(shiny)
library(shinyjs)
library(rhandsontable)

knowledge<-read_excel("Knowledge.xlsx")

skills<-read_excel("Skills.xlsx")

abilities<-read_excel("Abilities.xlsx")

cmr<-read_excel("Content Model Reference.xlsx")

df<-read.csv("sampledata.csv",stringsAsFactors = FALSE)

makeElement <- function(data, name)
{
  div(style = "border-width:2px;border-style:solid;",
      drag = name,
      div(class = "active title", name),
      div(class = "active content"))
}



ui <- fluidPage(h1((strong("Defining the Minimally Competent Candidate"))),
                 tabsetPanel(
                   tabPanel("Select Data",
                     sidebarLayout(
                       sidebarPanel(
                         selectInput('dataset','Choose a dataset:',choices=c("Knowledge","Skills","Abilities","Definitions"))),
                            mainPanel(DT::dataTableOutput('selected'))
                   )
                   ),
                   tabPanel("Move KSAs",fluid=TRUE,
                            mainPanel(fluidRow(
                              style = "margin: 15px;",
                              column(6,
                                     h3(p("KSAs",span("not true",style="color:red"),"of MCC")),
                                     div(id = "Available", style = "min-height: 600px;",
                                         lapply(df$KSA, makeElement, data = df))
                              ),
                              column(6,
                                     h3(p("KSAs",span("true",style="color:green"),"of MCC")),
                                     div(id = "Model", style = "min-height: 600px;"))
                            ),
                            dragulaOutput("dragula")
                            )
                   ),
                   tabPanel("Make Custom KSAs",
                            sidebarLayout(
                              sidebarPanel(fileInput('file1', 'Choose CSV File',
                                                     accept=c('text/csv', 
                                                              'text/comma-separated-values,text/plain', 
                                                              '.csv'))),
                                           mainPanel(
                                             DT::DTOutput("custom")
                                             
                                           )
                            )
                   )
                 )
)
server <- function(input, output) {
 
  df<-reactive({
    inFile <- input$file1
    
    if (is.null(input$file1))
      return(NULL)
    
    read.csv(inFile$datapath,stringsAsFactors = FALSE)
  })

  output$custom <- DT::renderDT(df(), editable=list(target='cell'),extensions = 'Buttons', options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  ))
  
  values<-reactiveValues()
   x<-df
  
  proxy<-DT::dataTableProxy("custom")
  
  observeEvent(input$custom_cell_edit, {
    info = input$custom_cell_edit
    str(info)
    x <<- DT::editData(x(), info)
    DT::replaceData(proxy, x, resetPaging = FALSE)  # important
  })

  
  output$dragula <- renderDragula({
    dragula(c("Available", "Model"))
  })
  

  output$selected<-DT::renderDataTable(DT::datatable({
    if(input$dataset=="Knowledge"){
      data<-knowledge
    }
    if(input$dataset=="Skills"){
      data<-skills
    }
    if(input$dataset=="Abilities"){
      data<-abilities
    }
    if(input$dataset=="Definitions"){
      data<-cmr
    }
    data
  }))
  




  
}

shinyApp(ui = ui, server = server)
