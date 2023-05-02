########################################################################
## Function get_igraph returns igraph from a dataframe
## Input argument: 
##     - df_ppt: needs to be a dataframe that only has one row, 
##                   and contain the columns named "nodes" and "edges" (FUN Qualtrics output)
## Output:
##      - an igraph object of participant
########################################################################
get_igraph = function(df_ppt, debug = FALSE){
  library(igraph)
  if (!("edges2" %in% colnames(df_ppt))){
    df_ppt$edges2 = df_ppt$edges
  }
  
  nodes = df_ppt%>%
    mutate(nodes=str_split(gsub("[^=A-Za-z,0-9{} ]+","",nodes),"(?<=\\}),\\s*"))%>% # remove \ in JSON 
    unnest() %>%
    mutate(id = str_extract(nodes,"(?<=id)[^,}]+"),  #  get ID
           label = str_extract(nodes,"(?<=label)[^,}]+"), #  get label
           closeness_org = str_extract(nodes,"(?<=closeness)[^,}]+")) %>%
    mutate_at('closeness_org',as.numeric) %>%
    mutate(closeness = 8 - ceiling(closeness_org/10)) %>%
    dplyr::select("id", "label", "closeness_org", "closeness")
  nodes = nodes %>% drop_na(id)
  
  
  if (length(which(nodes$closeness_org > 70)) != 0){
    print("node closeness bigger than 70")
    print(nodes$closeness_org)
  }
  
  edges = df_ppt%>%
    mutate(edges2=str_split(gsub("[^=A-Za-z,0-9{} ]+","",edges2),"(?<=\\}),\\s*"))%>%
    unnest()  %>%
    mutate(from = str_extract(edges2,"(?<=from)[^,}]+"),
           to = str_extract(edges2,"(?<=to)[^,}]+")) %>%
    dplyr::select("from", "to") %>%
    drop_na(from)
  
  if (debug == TRUE){
    print(df_ppt$ResponseId)
  }
  
  g = graph_from_data_frame(na.omit(edges), directed=FALSE, vertices=nodes) # construct igraph object
  g = simplify(g, remove.multiple = T) # remove duplicates
  if (length(V(g)$name) == 0){
    #print(df_ppt$nodes)
  }
  return(g)
}


########################################################################
## Function create_empty_df creates an empty dataframe
## Input argument: 
## Output:
##      - df with all the variables, no rows
########################################################################
create_empty_df = function(){
  df = data.frame(filename = character(),
                  ResponseID = character(),
                  pID = character(), 
                  density = numeric(), 
                  network_size = numeric(),
                  num_edges = numeric(),
                  components = numeric(),
                  walktrap_cluster = numeric(),
                  louvain_cluster = numeric(),
                  centrality_bw_graph = numeric(),
                  centrality_eigen_graph = numeric(),
                  centrality_bw_graph_norm = numeric(),
                  centrality_eigen_graph_norm = numeric(),
                  centrality_bw_vertex = numeric(),
                  centrality_eigen_vertex = numeric(),
                  centrality_bw_vertex_norm = numeric(),
                  centrality_eigen_vertex_norm = numeric(),
                  modularity = numeric(),
                  density = numeric(),
                  avg_closeness = numeric())
                  # avg_friendDrinking1 = numeric(),
                  # sd_friendDrinking1 = numeric(),
                  # avg_friendDrinking2 = numeric(),
                  # sd_friendDrinking2 = numeric())
  return(df)
}

########################################################################
## Function get_fun_df returns a df that contains processed FU data
## Input argument: 
##                - df_ppt: qualtrics that contains the following colums:
##                    "nodes", "edges", fu_drinking1_1" - 
##                   "fu_drinking1_63", "fu_drinking2_1" - 
##                   "fu_drinking2_63", 
##                - filename: name for the Qualtrics file
## Output:
##      - df that contains processed FU data
########################################################################
get_fun_df = function(df, filename, debug = FALSE,plot = TRUE){
  library(tidyr)
  library(dplyr)
  library(stringr)
  library(igraph)
  
  all_friends <- create_empty_df()
  
  for (rowNum in c(1:dim(df)[1])){
    #print(rowNum)
    df_ppt = df[rowNum,]
    print(df_ppt$ResponseId)
    
    if (df_ppt$nodes %in% c("[]", "")){
      print(paste0("No FUN data for ppt ", df_ppt$ResponseId))
      g = make_empty_graph(n = 0, directed = FALSE)

    } else {
      g = get_igraph(df_ppt, debug = debug)
      #df_ppt = recode_average_matrix(df_ppt, length(V(g)$name))
      wtc <- cluster_walktrap(g)
      lvc = cluster_louvain(g)
    
      l <- layout_with_fr(g)
      plot(wtc, g,vertex.label=NA, layout = l)
    
    
    
      ## Add ego node
      if (length(V(g)$name) > 0){
        g_ego <- add.vertices(g,1,id='EGO')
        df_g_ego <-as.data.frame(V(g_ego)$id)
        ego_degree <- which(df_g_ego$`V(g_ego)$id` == "EGO") - 1
        edgelist <- c(rbind(rep(ego_degree+1,ego_degree),1:ego_degree))
        g_ego <- add.edges(g_ego,edgelist)
        centrality_bw_vertex = betweenness(g_ego, normalized = FALSE)[which(df_g_ego$`V(g_ego)$id` == "EGO")]
        centrality_bw_vertex_norm = betweenness(g_ego, normalized = TRUE)[which(df_g_ego$`V(g_ego)$id` == "EGO")]
        centrality_eigen_vertex = eigen_centrality(g_ego, directed = FALSE, scale = FALSE)$vector[which(df_g_ego$`V(g_ego)$id` == "EGO")]
        centrality_eigen_vertex_norm = eigen_centrality(g_ego, directed = FALSE, scale = TRUE)$vector[which(df_g_ego$`V(g_ego)$id` == "EGO")]
        walktrap_cluster = length(wtc)
        louvain_cluster = length(lvc)
      
      } else {
        centrality_bw_vertex = NA
        centrality_eigen_vertex = NA
        centrality_bw_vertex_norm = NA
        centrality_eigen_vertex_norm = NA
        walktrap_cluster = NA
        louvain_cluster = NA
      }
    
    
      colors <- as.factor(as.character(V(g)$closeness))
      try(if (plot == TRUE){
        print(paste(filename, df_ppt$pID))
        # output_dir = paste0("~/Box/MURI_Data_830189/data/friendly_ocean/network_img/",
        #            df_ppt$pID, ".png")
        # if (!dir.exists(output_dir)){
        #   png(output_dir, 600, 600)
        #   print(plot.igraph(x = g, layout=layout_with_fr, vertex.size=1, main = df_ppt$pID,
        #                     color = wtc))
        #   dev.off()}
      
        output_dir = paste0("~/Box/MURI_Data_830189/data/friendly_ocean/network_img_withEGO/",
                          df_ppt$pID, ".png")  ### Need to update ###
        if (!dir.exists(output_dir)){
          png(output_dir, 600, 600)
          print(plot.igraph(x = g_ego, layout=layout_with_fr, vertex.size=1, main = df_ppt$pID))
          dev.off()}
        #print(plot.igraph(x = g_ego, layout=layout_with_fr, vertex.size=1, main = df_ppt$pID))
      })}
      all_friends = rbind(all_friends,data.frame(filename = filename,
                                               pID = df_ppt$pID,
                                               ResponseID = df_ppt$ResponseId,
                                               network_size = length(V(g)$name),
                                               num_edges = gsize(g),
                                               components = components(g)$no,
                                               walktrap_cluster = walktrap_cluster,
                                               louvain_cluster = louvain_cluster,
                                               centrality_bw_graph = igraph::centr_betw(g,normalized = FALSE, directed=FALSE)$centralization,
                                               centrality_bw_graph_norm = igraph::centr_betw(g,normalized = TRUE, directed=FALSE)$centralization,
                                               centrality_eigen_graph = igraph::centr_eigen(g,normalized = FALSE, directed=FALSE)$centralization,
                                               centrality_eigen_graph_norm = igraph::centr_eigen(g,normalized = TRUE, directed=FALSE)$centralization,
                                               centrality_bw_vertex = unname(centrality_bw_vertex),
                                               centrality_bw_vertex_norm = unname(centrality_bw_vertex_norm),
                                               centrality_eigen_vertex = unname(centrality_eigen_vertex),
                                               centrality_eigen_vertex_norm = unname(centrality_eigen_vertex_norm),
                                               modularity = modularity(wtc),
                                               density = edge_density(g),
                                               avg_closeness = mean(as.numeric(vertex_attr(g, "closeness")), na.rm = TRUE)))
                                               # avg_friendDrinking1 = df_ppt$avg_friendDrinking1,
                                               # sd_friendDrinking1 = df_ppt$sd_friendDrinking1,
                                               # avg_friendDrinking2 = df_ppt$avg_friendDrinking2,
                                               # sd_friendDrinking2 = df_ppt$sd_friendDrinking2))
  }
  return(all_friends)
}




