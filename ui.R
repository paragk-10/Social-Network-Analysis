library(shinythemes)
UI <- fluidPage( 
  theme =shinytheme("darkly"),
  titlePanel("ParagKedia_Social_Network_Analysis"),
            navlistPanel(
                   tabPanel("Upload Datasets",
                            fileInput("f1","Choose email sent/received data"), 
                            fileInput("f2","Choose department data")
                   ),
                   tabPanel("No of Emails Sent/Received",
                            tabsetPanel(id = "tab",
                                        tabPanel("Sent Info", tableOutput("sender")),
                                        tabPanel("Received Info", tableOutput("receiver"))
                            )
                   ),
                   tabPanel("Department Interactions",
                            tabsetPanel( id = "tab",
                                         tabPanel("Department-level Interaction", tableOutput("department"))
                            )
                   ),
                   tabPanel("Network Plots",
                            tabsetPanel(id="tab",
                                        tabPanel("Enter Number of Connections",
                                                textInput("hopCount", "Enter total connections to be displayed",value="5")
                                        ),
                                        tabPanel("Connectivity diagram", simpleNetworkOutput("plot"))
                            )
                   ),
                   tabPanel("2-hop Neighbors",
                            tabsetPanel(id = "tab",
                                        tabPanel("Sender list", simpleNetworkOutput("ShopConnection")),
                                        tabPanel("Receiver list", simpleNetworkOutput("RhopConnection"))
                            )
                   ),
                   tabPanel("Additional Operations",
                            tabsetPanel(id = "tab",
                                        tabPanel("Centrality", tableOutput("DegreeCentrality")),
                                        tabPanel("Betweenness", tableOutput("Betweeness")),
                                        tabPanel("In-Degree Centrality", tableOutput("InCentrality"))
                                        
                            )
                   )
                 )
)

shinyApp(ui=UI, server = Server)

#11- In all the three centralities  node '160' has the heigest number of Degree, Betweeness and In-Degree centrality.
