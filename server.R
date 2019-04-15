rm(list=ls(all=TRUE))
library(shiny)
library(igraph)
library(data.table)
library(networkD3)
library(dplyr)
library(DT)

Server<-function(input,output)
{
  context = reactive({
    if (is.null(input$f1))
      return(NULL)
    file1 = read.delim(input$f1$datapath,sep = "\t", header=F, col.names = c ("Sender","Receiver"))
    return(file1)
  })
  
  context2 = reactive({
    if (is.null(input$f2))
      return(NULL)
    file2 = read.delim(input$f2$datapath,sep = "\t", header=F, col.names =c ("Code","Department") )
    return(file2)
  })
  
  ##Calculate Degree Centrality
  calcentrality <- reactive({
    if(is.null(context())){return ()}
    graph <- graph_from_data_frame(context() ,directed = T)
    
    calDeg <- as.data.frame.character(V(graph))
    calDeg$Nodes <-row.names(calDeg)
    
    calDeg$deg_centrality <- igraph::degree(graph, mode="all")
    
    calDeg <- calDeg[with(calDeg, order(-deg_centrality)),]
    calDeg
  })
  
  ##Calculate Betweenness 
  calbetween <-reactive({
    if(is.null(context())){return ()}
    graph <- graph_from_data_frame(context() ,directed = T)
    
    calBet <- as.data.frame.character(V(graph))
    calBet$Nodes <-row.names(calBet)
    
    calBet$betweenness <- igraph::betweenness(graph)
    
    calBet <- calBet[with(calBet, order(-betweenness)),]
    calBet
  })
  
  ##Calculate Indegree Centrality
  calindegree <- reactive({
    if(is.null(context())){return ()}
    graph <- graph_from_data_frame(context() ,directed = T)
    
    calDeg <- as.data.frame.character(V(graph))
    calDeg$Nodes <-row.names(calDeg)
    
    calDeg$indeg_centrality <- igraph::degree(graph, mode="in")
    
    calInDeg <- calDeg[with(calDeg, order(-indeg_centrality)),]
    calInDeg
  })
  
  output$tb1 <- renderTable({head(context(),50)})
  output$tb2 <- renderTable({head(context2(),50)})
  
  #Network Plot
  output$plot <- renderSimpleNetwork({
    count <- as.numeric(reactive({input$hopCount})()[1])
    networkdata <- head(context(),count)
    simpleNetwork(networkdata,charge = -2, fontSize = 10, fontFamily = "serif", linkColour = "white", nodeColour = "yellow", zoom = T)
  })
  
  output$sender <- renderTable({
    freq= as.data.frame(table(context()[,1]))
    names(freq)=c("Sender","No of Emails Sent")
    freq
  })
  
  output$receiver <- renderTable({
    freq= as.data.frame(table(context()[,2]))
    names(freq)=c("Receiver","No of Emails Received")
    freq
  })
  
  #2-hop (Sender)
  output$ShopConnection <- renderSimpleNetwork({
    freq= as.data.frame(table(context()[,1]))
    names(freq)= c("Sender","Frequency")
    freq = freq[with(freq, order(-Frequency)), ] 
    SenderList <- head(freq,10)
    
    SenderMailList <- filter(context(),Sender %in% SenderList$Sender)
    
    #Finding 1st hop
    FirstReceiver <- unique(SenderMailList$Receiver)
    SenderVec <- SenderList$Sender
    FinalSenderList <- append(SenderVec,FirstReceiver)
    
    #Finding 2nd hop
    SenderMailList <- filter(context(),Sender %in% FinalSenderList)
    SenderMailList <- unique(SenderMailList)
    simpleNetwork(head(SenderMailList,50),charge = -2,fontSize = 10, fontFamily = "serif", linkColour = "white", nodeColour = "yellow", zoom = T)
    })
  
  #2-hop (Receiver) 
  output$RhopConnection <- renderSimpleNetwork({
    freq= as.data.frame(table(context()[,2]))
    names(freq)= c("Receiver","Frequency")
    freq = freq[with(freq, order(-Frequency)), ] 
    ReceiverList <- head(freq,10)
    
    ReceiverMailList <- filter(context(),Receiver %in% ReceiverList$Receiver)
    
    #Finding 1st hop
    FirstSender <- unique(ReceiverMailList$Sender)
    
    ReceiverVec <- ReceiverList$Receiver
    FinalReceiverList <- append(ReceiverVec,FirstSender)
    
    #Finding 2nd hop
    ReceiverMailList <- filter(context(),Receiver %in% FinalReceiverList)
    ReceiverMailList <- unique(ReceiverMailList)
    simpleNetwork(head(ReceiverMailList,50),charge = -2,fontSize = 10, fontFamily = "serif", linkColour = "white", nodeColour = "yellow", zoom = T )
  })
  
  output$DegreeCentrality <- renderTable({calcentrality()})
  output$Betweeness <- renderTable({calbetween() })
  output$InCentrality <- renderTable({calindegree()})
  
  #Department-level Interaction
  output$department <-renderTable({
    f1data <- context()
    f2data <- context2()
    
    names(f2data)[1] <- names(f1data)[1]
    SenderDeptData <- left_join(f1data, f2data , by = c(names(f1data)[1],names(f2data)[1]))
    
    names(f2data)[1] <- names(f1data)[2]
    EmpDeptData <- left_join(SenderDeptData, f2data , by = c(names(f1data)[2],names(f2data)[1]))
    names(EmpDeptData) <- c("Sender","Receiver","Sender_Dept","Receiver_Dept")
    DeptInteraction <- unique(EmpDeptData[,3:4] %>% group_by(Sender_Dept,Receiver_Dept))
  }) 
}

