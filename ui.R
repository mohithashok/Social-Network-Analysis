#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# 
#

library(shiny)
library(networkD3)
library(visNetwork)
library(shinythemes)
library(data.table)
# Define UI for applicatin
shinyUI(
  
  
  
  fluidPage(theme = shinytheme('sandstone'),
  
  # Application title
  titlePanel("Mohith Ashok - Social Network Analysis"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
     fileInput("file","Enter links file",accept = c("email-Eu-core.txt")),
     fileInput("file1","Enter nodes file",accept = c("email-Eu-core-department-labels.txt")),
     numericInput("n","Enter number of connections",10)
    ),
    mainPanel(tabsetPanel(tabPanel("First Page",uiOutput("First_Page")),
                          tabPanel("Network overview",simpleNetworkOutput("plot")),
                          tabPanel("Number of emails sent",dataTableOutput("sent")),
                          tabPanel("Number of emails recieved",dataTableOutput("recieved")),
                          tabPanel("2 hops of sent",fluidRow(column(12,visNetworkOutput("plot1")),
                                                             column(12,tableOutput("table1")),
                                                             sidebarPanel(numericInput("Rank1","Enter the Rank (1-10) of the user for which you want to visualize the 2hops",min=1,max=10,value=1)))),
                          tabPanel("2 hops of recieved",fluidRow(column(12,visNetworkOutput("plot2")),
                                                                 column(12,tableOutput("table2")),
                                                                 sidebarPanel(numericInput("Rank2","Enter the Rank (1-10) of the user for which you want to visualize the 2hops",min=1,max=10,value=1)))),
                          tabPanel("Degree centrality",fluidRow(column(12,visNetworkOutput("plot3")),
                                                                column(12,tableOutput("table3")),
                                                                sidebarPanel(numericInput("Rank3","Enter the Rank (1-10) of the user for which you want to visualize the 2hops",min=1,max=10,value=1)))),
                          tabPanel("Betweeness centrality",fluidRow(column(12,visNetworkOutput("plot4")),
                                                                    column(12,tableOutput("table4")),
                                                                    sidebarPanel(numericInput("Rank4","Enter the Rank (1-10) of the user for which you want to visualize the 2hops",min=1,max=10,value=1)))),
                          tabPanel("Indegree Centrality",fluidRow(column(12,visNetworkOutput("plot5")),
                                                            column(12,tableOutput("table5")),
                                                            sidebarPanel(numericInput("Rank5","Enter the Rank (1-10) of the user for which you want to visualize the 2hops",min=1,max=10,value=1)))),
                          tabPanel("Department Network",fluidRow(column(4,dataTableOutput("table")),
                                                                 column(8,visNetworkOutput("plot6")),
                                                                 sidebarPanel(numericInput("n1","Enter number of connections",10)))),
                          tabPanel("Observation",uiOutput("observation"))
                          )
       
    )
  )
))
