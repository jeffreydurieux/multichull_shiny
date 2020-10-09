# Chull ui
ui <- shinyUI(navbarPage(theme = shinythemes::shinytheme("united"),"multichull",
                         windowTitle = "multichull",

                         ################ load files ###########
                         tabPanel("", icon = icon("folder-open-o","fa-2x"),
                                  sidebarLayout(
                                    sidebarPanel(
                                      fileInput(inputId = 'loadfile',label = "Select file:",
                                                multiple = FALSE)

                                    ),# end sidebarPanel

                                    mainPanel(
                                      fluidRow(
                                        column(4,

                                               tableOutput('fileviewer')
                                        ),# end column
                                        column(8,
                                               tableOutput('ui')
                                        )# end column
                                      ) # end fluidRow

                                    )# end main panel
                                  )# end sidebar layout
                         ), # end loaddata panel

                         ############### chull options ##########
                         tabPanel(title = "", icon = icon("gears","fa-2x"),
                                  fluidPage(
                                    titlePanel("Analysis Options"),

                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("Chull",
                                                 radioButtons("bound", label = h4("What does the fit measure indicate?"),
                                                              choices = list("Badness-of-fit" = "lower", "Goodness-of-fit" = "upper"
                                                              ),selected = "lower"),
                                                 numericInput("PercentageFit",
                                                              label = h4("Required improvement in fit?"),
                                                              value = .01,min=0,max=1,step=.01),
                                                 br(),
                                                 actionButton('stChull',label = "Start CHull",
                                                              icon = icon("flash",lib = "glyphicon"))

                                        ), #end tabPanel CHull

                                        tabPanel("MultiChull",
                                                 radioButtons("boundM", label = h4("What does the fit measure indicate?"),
                                                              choices = list("Badness-of-fit" = "lower", "Goodness-of-fit" = "upper"
                                                              ),selected = "lower"),
                                                 numericInput("PercentageFitM",
                                                              label = h4("Required improvement in fit?"),
                                                              value = .01,min=0,max=1,step=.01),
                                                 br(),
                                                 actionButton('stMultiChull',label = "Start MultiCHull",
                                                              icon = icon("flash",lib = "glyphicon"))
                                        )# end tabPanel MultiChull
                                      )#end tabset
                                    )# end mainpanel



                                  )# end fluidpage

                         ), #end settings panel

                         ############# results panel #########
                         tabPanel(title = "", icon = icon("bar-chart-o","fa-2x"),
                                  fluidPage(
                                    titlePanel("Results"),

                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("Summary",
                                                 #verbatimTextOutput("summary"),

                                                 verbatimTextOutput("summary")
                                        ), #end tabPanel summary

                                        tabPanel("Plotly",
                                                 plotly::plotlyOutput("plotly")
                                        )# end tabPanel plotly
                                      )#end tabset
                                    )# end mainpanel



                                  )# end fluidpage

                         ), # end results panel


                         ########### information
                         tabPanel(title = "", icon = icon("info-sign","fa-2x", "glyphicon"),
                                  navlistPanel("CHull",id='info',br(), well = F,selected = "Details",
                                               tabPanel("Details",
                                                        h2("Details page"),
                                                        br(),
                                                        h3('youtube video tutorials here:')),

                                               tabPanel("References",
                                                        h2("References page"),
                                                        br(),
                                                        h3('CHull papers:'),
                                                        br(),
                                                        tags$a(href='https://bpspsychub.onlinelibrary.wiley.com/doi/full/10.1348/000711005X64817?casa_token=vzZewzwUsXoAAAAA%3ALsuycpnna2_nuQFtSWHr-En8Pjew6TLEIZRM1Snyckw1nvC4tBNkR1skUJcR6V4n0TtJ0dA7n0XQYQ', 'Ceulemans & Kiers (2006)', target = '_blank'),
                                                        br(),
                                                        tags$a(href='https://link.springer.com/article/10.3758/s13428-012-0238-5', 'Wilderjans, Ceulemans & Meers (2013)', target = '_blank')
                                                        ),
                                               tabPanel("Authors",
                                                        h2("Authors page"),
                                                        br(),
                                                        tags$a(href='https://www.universiteitleiden.nl/en/staffmembers/jeffrey-durieux#tab-1', 'Jeffrey Durieux', target = '_blank'))
                                  )#end navlistpanel

                         )# end tabPanel


))# end shinyUI navbarPage