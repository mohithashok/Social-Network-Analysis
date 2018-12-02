#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#


library(dplyr)
library(shiny)
library(igraph)
library(plyr)
library(visNetwork)
library(networkD3)
library(data.table)
#function for plotting 2hops
hop <- function(df,t1,rank,dep_df){
  
  g <- graph.data.frame(df)
  deg <- igraph::degree(g)
  deg1 <- igraph::betweenness(g,normalized = TRUE)
  r <- data.frame(V1=names(deg),Centrality=deg)
  r <- r[order(-r$Centrality),] #Centrality of vertices
  dep_df <- merge(dep_df,r,by='V1',all.x=T) # merging with vertice tabel
  t <- data.frame(V1=names(deg1),betweenness=deg1) #merging betweeneness
  dep_df <- merge(dep_df,t,by='V1',all.x=T)
  #obtaining 2 hops table
  t1 <- head(t1,10)
  t2 <- df[df[['V1']]==t1[rank,'V1'],]
  t3 <- subset(df,V1 %in% as.character(t2[['V2']]))
  hops <- rbind(t2,t3)
  a <- hops['V2']
  colnames(a) <- 'V1'
  ver <- rbind(hops['V1'],a['V1'])
  ver <- unique(ver)
  dep_df <- merge(dep_df,ver,by='V1',all.y=T)
  #flagging the main nodes and 1 hops and 2 hops
  dep_df$flag <- 4
  dep_df$flag[dep_df$V1 %in% hops$V2[hops[['V1']]==t1[rank,'V1']]] <- 3
  dep_df$flag[dep_df$V1==t1[rank,'V1']] <- 5
  
  net <- graph.data.frame(d=hops,dep_df,directed= F)
  if(colnames(t1)[2]!='freq')
  {V(net)$color <- V(net)$Dep}
  V(net)$shape[V(net)$flag!=5] <- 'circle'
  V(net)$shape[V(net)$flag==5] <- 'square'
  V(net)$size <- 30
  V(net)$title <- paste0('Node:',V(net)$name, ' Dep: ',V(net)$Dep, ' Betweeness: ',round(V(net)$betweenness * 100,2))
  #taking top 25 one hops and top 50 2 hops based on centrality
  cutoff <- sort(dep_df[dep_df$flag==3 ,'Centrality'],decreasing = TRUE)[10]
  netsp <- delete_vertices(net, V(net)[Centrality<cutoff & flag==3])
  netsp <- simplify(netsp)
  netsp <- delete_vertices(netsp,V(netsp)[degree(netsp)==0])
  cutoff1 <- sort(V(netsp)$Centrality[V(netsp)$flag==4],decreasing=TRUE)[40] 
  net_final <- delete_vertices(netsp, V(netsp)[Centrality<cutoff1 & flag==4])
  net_final <- simplify(net_final)
  net_final <- delete_vertices(net_final,V(net_final)[degree(net_final)==0])
  # giving a tree layout for easy visualization
  l <- layout_as_tree(net_final,root=match(5,V(net_final)$flag),flip.y = FALSE)
  E(net_final)$color <- "gray"
  #plotting
  visIgraph(net_final,layout = 'layout.norm',layoutMatrix = l,type = 'full',randomSeed = 0)
  
}
#function for table
totaltable <- function(df1,t11,rank1,dep_df1){
  
  g <- graph.data.frame(df1)
  deg <- igraph::degree(g)
  r <- data.frame(V1=names(deg),Centrality=deg)
  r <- r[order(-r$Centrality),] #Centrality of vertices
  dep_df <- merge(dep_df1,r,by='V1',all.x=T) # merging with vertice tabel
  #obtaining 2 hops table
  t1 <- head(t11,10)
  t2 <- df1[df1[['V1']]==t1[rank1,'V1'],]
  t3 <- subset(df1,V1 %in% as.character(t2[['V2']]))
  hops <- rbind(t2,t3)
  a <- hops['V2']
  colnames(a) <- 'V1'
  ver <- rbind(hops['V1'],a['V1'])
  ver <- unique(ver)
  dep_df <- merge(dep_df,ver,by='V1',all.y=T)
  #flagging the main node and 1 hops and 2 hops
  dep_df$flag <- 4
  dep_df$flag[dep_df$V1 %in% hops$V2[hops[['V1']]==t1[rank1,'V1']]] <- 3
  dep_df$flag[dep_df$V1==t1[rank1,'V1']] <- 5
  f <- c(nrow(dep_df[dep_df['flag']==3,]),nrow(dep_df[dep_df['flag']==4,]),dep_df$V1[dep_df['flag']==5])
  #f <- c(nrow(filter(dep_df['flag'], flag == 3)), nrow(filter(dep_df['flag'], flag == 4)), t1[rank,'V1'])
  output <- data.frame(cbind(c("Total one hops","Total two hops","User"),f))
  colnames(output) <- NULL
  return(output)
  }

shinyServer(function(input, output) {
     
  output$plot <- renderSimpleNetwork({ df <- if(is.null(input$file)) {return(NULL)} else {read.table(input$file$datapath, 
                                                                                                     header = FALSE)}
  
  simpleNetwork(head(df,input$n),zoom=TRUE) #plotting
 # plot(g)
  })
output$sent <- renderDataTable({df <- if(is.null(input$file)) {return(NULL)} else {read.table(input$file$datapath, 
                                                                                          header = FALSE)} 
t1<- count(df$V1) 
colnames(t1) <- c('V1','freq')
t1 <- t1[order(-t1$freq),]
}) #ordering by number of emails sent

#################################################
output$recieved <- renderDataTable({ df <- if(is.null(input$file)) {return(NULL)} else 
                                                        {read.table(input$file$datapath, header = FALSE)} 
t1 <-count(df$V2)
colnames(t1) <- c('V2','freq')
t1[order(-t1$freq),]}) #ordering by number of emails recieved

#########################################################################################
#2hop graphs for the questions asked
output$plot1 <- renderVisNetwork({ df <- read.table(input$file$datapath, header = FALSE)
dep_df <- read.table(input$file1$datapath,header = F)
colnames(dep_df) <- c('V1','Dep')
t1<- count(df$V1) 
colnames(t1) <- c('V1','freq')
t1 <- t1[order(-t1$freq),] #ordering by number of emails sent
hop(df=df,t1=t1,input$Rank1,dep_df)})
#######################################################################################

output$plot2 <- renderVisNetwork({df <- read.table(input$file$datapath, header = FALSE)
dep_df <- read.table(input$file1$datapath,header = F)
colnames(dep_df) <- c('V1','Dep')
t1<- count(df$V2) 
colnames(t1) <- c('V1','freq') 
t1 <- t1[order(-t1$freq),] #ordering by number of emails recieved
hop(df=df,t1=t1,input$Rank2,dep_df)})

#########################################################################################
output$plot3 <- renderVisNetwork({df <- read.table(input$file$datapath, header = FALSE)
dep_df <- read.table(input$file1$datapath,header = F)
colnames(dep_df) <- c('V1','Dep')
s <- graph_from_data_frame(df)
deg <- igraph::degree(s)
t1 <- data.frame(V1=names(deg),Centrality=deg)
t1 <- t1[order(-t1$Centrality),] #ordering by Centrality
hop(df=df,t1=t1,input$Rank3,dep_df)})

#########################################################################################
output$plot4 <- renderVisNetwork({df <- read.table(input$file$datapath, header = FALSE)
dep_df <- read.table(input$file1$datapath,header = F)
colnames(dep_df) <- c('V1','Dep')
s <- graph_from_data_frame(df)
deg <- igraph::betweenness(s,normalized=TRUE)
t1 <- data.frame(V1=names(deg),Betweeness=deg)
t1 <- t1[order(-t1$Betweeness),] #ordering by Betweeness
hop(df=df,t1=t1,input$Rank4,dep_df)})

####################################################################################33######
output$plot5 <- renderVisNetwork({df <- read.table(input$file$datapath, header = FALSE)
dep_df <- read.table(input$file1$datapath,header = F)
colnames(dep_df) <- c('V1','Dep')
s <- graph_from_data_frame(df)
deg <- igraph::degree(s,mode='in')
t1 <- data.frame(V1=names(deg),Indegree=deg)
t1 <- t1[order(-t1$Indegree),] #ordering by In centrality
hop(df=df,t1=t1,input$Rank5,dep_df)})

#########################################################################################
output$table <- renderDataTable({df <- read.table(input$file$datapath, header = FALSE)
dep_df <- read.table(input$file1$datapath,header = F)
colnames(dep_df) <- c('V1','Dep')
df <- merge(df,dep_df,by='V1',all.x = T)
df <- merge(df,dep_df,by.x='V2',by.y='V1',all.x=T) #getting department tabel
colnames(df) <- c('V1','V2','dep_V1','dep_V2')
dep <- count(df,vars =c( 'dep_V1','dep_V2')) #counting number of emails
dep})
###################################################################################################
output$plot6 <- renderVisNetwork({df <- if(is.null(input$file)) {return(NULL)} else {read.table(input$file$datapath, 
                                                                                                   header = FALSE)}
dep_df <- read.table(input$file1$datapath,header = F)
colnames(dep_df) <- c('V1','Dep')
df <- merge(df,dep_df,by='V1',all.x = T)
df <- merge(df,dep_df,by.x='V2',by.y='V1',all.x=T) #getting department tabel
colnames(df) <- c('V1','V2','dep_V1','dep_V2')
dep <- count(df,vars =c( 'dep_V1','dep_V2')) #counting the number of emails
colnames(dep) <- c('dep_V1','dep_V2','freq')
g <- graph.data.frame(head(dep,input$n1),directed = T)
visIgraph(g,randomSeed = 1)
})
#table outputs for each 2 hop graph
###################################################################################################
output$table1 <- renderTable({df <- read.table(input$file$datapath, header = FALSE)
dep_df <- read.table(input$file1$datapath,header = F)
colnames(dep_df) <- c('V1','Dep')
t1<- count(df$V1) 
colnames(t1) <- c('V1','freq')
t1 <- t1[order(-t1$freq),]
final <- totaltable(df=df,t1=t1,input$Rank1,dep_df) #finidng total number of 1 hops and 2 hops
final})
####################################################################################################
output$table2 <- renderTable({df <- read.table(input$file$datapath, header = FALSE)
dep_df <- read.table(input$file1$datapath,header = F)
colnames(dep_df) <- c('V1','Dep')
t1<- count(df$V2) 
colnames(t1) <- c('V1','freq') 
t1 <- t1[order(-t1$freq),]
final <- totaltable(df=df,t1=t1,input$Rank2,dep_df)
final})
##################################################################################################
output$table3 <- renderTable({df <- read.table(input$file$datapath, header = FALSE)
dep_df <- read.table(input$file1$datapath,header = F)
colnames(dep_df) <- c('V1','Dep')
s <- graph_from_data_frame(df)
deg <- igraph::degree(s)
t1 <- data.frame(V1=names(deg),Centrality=deg)
t1 <- t1[order(-t1$Centrality),] #ordering by Centrality
final <- totaltable(df=df,t1=t1,input$Rank3,dep_df)
final})
###################################################333333333######################################
output$table4 <- renderTable({df <- read.table(input$file$datapath, header = FALSE)
dep_df <- read.table(input$file1$datapath,header = F)
colnames(dep_df) <- c('V1','Dep')
s <- graph_from_data_frame(df)
deg <- igraph::betweenness(s,normalized = TRUE)
t1 <- data.frame(V1=names(deg),betweenness=deg)
t1 <- t1[order(-t1$betweenness),] #ordering by Centrality
final <- totaltable(df=df,t1=t1,input$Rank4,dep_df)
final})
################################################################################################33#
output$table5 <- renderTable({df <- read.table(input$file$datapath, header = FALSE)
dep_df <- read.table(input$file1$datapath,header = F)
colnames(dep_df) <- c('V1','Dep')
s <- graph_from_data_frame(df)
deg <- igraph::degree(s,mode='in')
t1 <- data.frame(V1=names(deg),Indegree=deg)
t1 <- t1[order(-t1$Indegree),] #ordering by Centrality
final <- totaltable(df=df,t1=t1,input$Rank5,dep_df)
final})
#################################################################################################
#text outputs
output$observation <- renderUI(HTML("<p><ul><li> 160 is the key player in this network with highest degree, betweenness and indegree Centrality. </li></p>
                                  <p><li> 86 is a 2 hop neighbor of 160 and it has the second highest betweenness centrality, Also 86 has a high number of 2 hop neighbors.</li></p>
                                   <p><li> 107 has the highest betweenness amongst the 1 hop neighbors of 160 and its present in the top 10 betweenness and top 10 indegree centrality.</li></p>
                                   <p><li> 160, 86 and 107 belong to department 36.</li></p>
                                   <p><li> Department 36 seems to be the key department as most of the key nodes are from this department</li></p>
                                   <p><li> All the top 10 from the 3 centralities have more than 650 2 hop neighbors except 160 and 377 which have around 570 </li></p>
                                   
                                   </ul>"))
#######################################################################################################
output$First_Page <- renderUI(HTML("<p><ul><li> All the 2 hop graphs have been sorted by top 10 one hops and top 40 2 hops based on centrality for visualization. </li></p>
                                  <p><li> The rank input that appears in the 2 hop graphs is to filter one user and display its 2 hop neighbors for better visualization.</li></p>
                                  <p><li> The node name , the department it belongs to and the betweenness of the node are availabe on the tool tip, please hover on the node to see them.</li></p>  
                                 <p><li> Please upload the file and move on to the next tab, thanks.</li></p>
                                    
                                    </ul>"))
})
