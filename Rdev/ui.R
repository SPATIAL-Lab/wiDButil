library(openxlsx)
library(readxl)
ui = fluidPage(
  shinyalert::useShinyalert(),
  titlePanel(""),
  typelength <- uiOutput('numType'),
  tabsetPanel(
    tabPanel("File Input", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 checkboxInput('header', "Contains header in the first row"),
                 checkboxInput('one', "Data is stored on one sheet"),
                 fileInput('file', 'Choose a xlsx file', accept = c(".xlsx"), ),
               ),
               
               mainPanel(
                 tableOutput('table')
               )
             )
    ),
    tabPanel("Sites", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 p("Choose column that best fits, otherwise, choose NA"),
                 uiOutput('id',),
                 p("If Site_ID is NA, please enter an ID prefix. Otherwise, leave empty."),
                 uiOutput('projectname',),
                 uiOutput('sitename',),
                 uiOutput('latitude',),
                 uiOutput('longitude',),
                 uiOutput('elevation',),
                 uiOutput('address',),
                 uiOutput('city',),
                 uiOutput('state',),
                 uiOutput('country',),
                 uiOutput('scomment',),
                 actionButton('done', 'Next')
                 
               ),
               mainPanel(
                 tableOutput('siteTable')
               )
             )
    ),
    tabPanel("Samples", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 p("Choose column that best fits, otherwise, choose NA"),
                 uiOutput('sid',),
                 uiOutput('type',),
                 uiOutput('startD',),
                 uiOutput('startTZ',),
                 uiOutput('collectionD',),
                 uiOutput('collectionTZ',),
                 uiOutput('sampleVol',),
                 uiOutput('collectT',),
                 uiOutput('phase',),
                 uiOutput('depth',),
                 uiOutput('sampleSource',),
                 uiOutput('sampIgnore',),
                 uiOutput('sampCom',),
                 uiOutput('projectID',),
                 actionButton('done1', 'Next')
               ),
               mainPanel(
                 tableOutput('sampleTable')
               )
             )
    ),
    tabPanel("Type", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 p("Choose your type parameter that best fits, otherwise, choose NA"),
                 
                 uiOutput("watertype"),
                 
                 # uiOutput('bottle',),
                 # uiOutput('canal',),
                 # uiOutput('ground',),
                 # uiOutput('lake',),
                 # uiOutput('leaf',),
                 # uiOutput('mine',),
                 # uiOutput('ocean',),
                 # uiOutput('precip',),
                 # uiOutput('river',),
                 # uiOutput('snow',),
                 # uiOutput('soil',),
                 # uiOutput('spring',),
                 # uiOutput('stem',),
                 # uiOutput('sprinkler',),
                 # uiOutput('tap',),
                 # uiOutput('vapor',),
                 actionButton('done2', 'Next')
               ),
               mainPanel(
                 tableOutput('sampleTable2')
               )
             )
    ),
    tabPanel("Countries", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 uiOutput('startCC',),
                 actionButton('done3', 'Next')
               ),
               mainPanel(
                 p("If you agree with the recommendation, leave Choice column empty. Otherwise, type in the two-digit code that fits best."),
                 DT::DTOutput('mod_table'),
                 actionButton('done4', 'Next')
               )
             )
    ),
    tabPanel("Export", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 downloadButton('d1', 'Export')
               ),
               mainPanel(
                 
                 tabsetPanel(type = "tabs",
                             tabPanel("Sites", tableOutput("ss")),
                             tabPanel("Samples", tableOutput("sss"))
                             )
               )
             )
             
    )
  )
)