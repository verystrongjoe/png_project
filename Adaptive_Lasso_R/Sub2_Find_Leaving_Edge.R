Sub2_Find_Leaving_Edge <-
function(est_T=est_T,entering_edge=entering_edge,DELTA=DELTA,nV=nV)     # Function to find the leaving edge
{
inflow<-sum(est_T[entering_edge[2],-entering_edge[1]])    # Check whether there is any inflow to node j when the edge (i,j) is considered as an entering edge.
two_loop<-est_T[entering_edge[2],entering_edge[1]]        # Check whether the edge (j,i) exists when the edge (i,j) is considered as an entering edge.  
leaving_edge<-c(0,0,0)                                      # Initialize the leaving edge value
All_leaving_edges<- matrix(NA,nrow=1,ncol=3)
  
  temp_DELTA<-DELTA
  temp_est_T<-est_T
                 
if(inflow==0)                                             # If there is no inflow to node j,
{
  if(two_loop==0)                                         # If the edge (j,i) does not exist in the structure,
  {
    temp_est_T[entering_edge[1],entering_edge[2]]<-1           # Select the edge (i,j), and mark it as "Success"
    Success_Fail_index<-"S"
  }                                                       # If the edge (j,i) already exist in the structure,
  
  if(two_loop==1)      
  {
    if(temp_DELTA[entering_edge[1],entering_edge[2]]<temp_DELTA[entering_edge[2],entering_edge[1]])  # If the delta value of the edge (i,j) is smaller than the delta value of the edge (j,i),
    {
      temp_est_T[entering_edge[1],entering_edge[2]]<-1        # Select the edge (i,j),
      leaving_edge<-c(entering_edge[2],entering_edge[1],temp_DELTA[entering_edge[2],entering_edge[1]]) # Unselect the edge (j,i) as a leaving edge,
      temp_est_T[entering_edge[2],entering_edge[1]]<-0 
      Success_Fail_index<-"S"                            # Mark it as "Success"
      All_leaving_edges<-rbind(All_leaving_edges,leaving_edge)  
    } else { 
      Success_Fail_index<-"F"                            # Otherwise, mark it as "Fail"
    } 

  }

  
}


if(inflow>0)                                           # If there is any inflow to node j,
{  
############
  threshold<-temp_DELTA[entering_edge[1],entering_edge[2]]                  
  
  Keep_running_index <- "K"
      
while(Keep_running_index=="K")
{

  Keep_running_index <- "NO"
  
                                         
  Cycle_matrix<-temp_est_T                                  # Initialize the Cycle check matrix,
  Cycle_matrix[]<-0
                                                       # Apply the BFS (breath first search) algorithm to find the cycle,
  list_of_heads<-as.list(NULL)                         # Initialize the list_of_head, which contains the information of the head of edges
  count<-1                                             # count indicates the level of depth when the BFS is applied.
  list_of_heads[[count]]<-entering_edge[1]             # the head i of the entering edge is put into the first level in the list_of_head
  Cycle_matrix[,list_of_heads[[count]]]<-temp_est_T[,list_of_heads[[count]]]  # Put the column of i into the column of Cycle_matrix
    
  while(sum(list_of_heads[[count]]==entering_edge[2])==0 & sum(list_of_heads[[count]])>0)   # run the inside while-loop until the head of arrows do not meet the j node and there exists a further level to search
  {
    Cycle_matrix[,list_of_heads[[count]]]<-temp_est_T[,list_of_heads[[count]]]   # Put the column of i into the column of Cycle_matrix  
    
    if(length(list_of_heads[[count]])==1)                                   # If the number of node in the current search level in list_of_heads is 1, 
    {
      list_of_heads[[count+1]]<-c(1:nV)[as.logical(temp_est_T[,list_of_heads[[count]]])] # Save the heads of arrows into the next level of list_of_heads,
    }
    if(length(list_of_heads[[count]])>1)                                          # If the number of node in the current search level in list_of_heads is more than 1,
    {
      list_of_heads[[count+1]]<- c(1:nV)[as.logical(apply(temp_est_T[,list_of_heads[[count]]],1,sum))] # Save the heads of arrows from multiple nodes into the next level of list_of_heads,
    }
    count<-count+1                                                             # update the level
  }
  
  if(sum(list_of_heads[[count]]==entering_edge[2])==0)                    # If the set of the nodes in the last level of list_of_heads do not contain the node j,
  {                                                                       
    temp_est_T[entering_edge[1],entering_edge[2]]<-1                           # Select the edge (i,j) as an entering edge
    Success_Fail_index<-"S"                                               # Mark it as "Success"
  }


################################

  
  if(sum(list_of_heads[[count]]==entering_edge[2])>0)    # beginning of main IF   # If the set of the nodes in the last level of list_of_heads contain the node j,
  {
  
   
    list_of_tails<-as.list(NULL)                                            # Initialize the list_of_tail
    count<-1                                                                # Set the level of height
    list_of_tails[[count]]<-entering_edge[2]                                # Put the node j into the first level of list_of_tails

    while(sum(list_of_tails[[count]]==entering_edge[1])==0 )                # Run while-loop until list_of_tail does not have the node i
    {
      if(length(list_of_tails[[count]])==1)                                 # If the number of node in the current search level in list_of_tails is 1, 
      {
        list_of_tails[[count+1]]<-c(1:nV)[as.logical(Cycle_matrix[list_of_tails[[count]],])]   # Save the tails of arrows into the next level of list_of_tails,
      }
      if(length(list_of_tails[[count]])>1)                                  # If the number of node in the current search level in list_of_tails is more than 1, 
      {
        list_of_tails[[count+1]]<- c(1:nV)[as.logical(apply(Cycle_matrix[list_of_tails[[count]],],2,sum))] # Save the tails of arrows from multiple nodes into the next level of list_of_tails,
      }
      count<-count+1                                                         # update the level
    }    
    
    leaving_list<-c(0,0,-Inf)                                                   
    for(index in 1:(count-1))                                                   # Find the possible leaving edges
    {
      if(length(list_of_tails[[index]])==1 & length(list_of_tails[[index+1]])==1 )
      {
        selected_value<-temp_DELTA[list_of_tails[[index]],list_of_tails[[index+1]]]
        if( selected_value > max(leaving_list[3],threshold) )
        {
          leaving_list<-c(list_of_tails[[index]],list_of_tails[[index+1]],selected_value)
        }
      }    
    }
    
  ###################
  
    if(leaving_list[1]==0)                                    # If there is no leaving edge in the list, it fails to find it.
    {                                                         # Mark it as "Fail"
      Success_Fail_index<-"F"
    }

    if(leaving_list[1]>0)                                     # If there is any leaving edge in the list, it successes to find it.
    {  
      temp_est_T[leaving_list[1],leaving_list[2]]<-0            # Take out the leaving edge in the structure matrix
      

      threshold <- threshold - leaving_list[3]
      temp_DELTA[leaving_list[1], temp_DELTA[leaving_list[1],]<0 ]  <- threshold - 100

      All_leaving_edges<-rbind(All_leaving_edges,leaving_list)
      Keep_running_index <- "K"
    }
    
  }          # End of main IF

}
###############
}
  
  if(Success_Fail_index=="S")
  {
    est_T <- temp_est_T
  }
  
  
  result_Find_leaving_edge <- list(Success_Fail_index=Success_Fail_index,est_T=est_T,All_leaving_edges=All_leaving_edges)
  return(result_Find_leaving_edge)
}
