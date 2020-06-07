Sub1_adap_cLasso_Improving_Search <-
function(x.stand=x.stand,lambda=lambda,search.index=1,search.space=NULL,gamma_weight=gamma_weight,initial_weight =initial_weight,initial_lambda=initial_lambda,second_lambda=NULL)
{
######################################
## Subordinate function of adaplasso()
## This function runs the improving search algorithm.
## This function calls two other subordiante functions:
######################################

  ## Get the information of data x.
  x.stand<-as.matrix(x.stand)
  Data_size<-dim(x.stand)[1]         
  nV<-dim(x.stand)[2]               

  ## Calculate the estimated covariance matrix.
  #est_S<-1/Data_size*t(x.stand)%*%x.stand   

  ## Generate identity matrix.                 
  Identity<-Matrix(0,nrow=nV,ncol=nV)   
  for(i in 1:nV)
  {
  Identity[i,i]<-1
  }

######################################
# Step 0: Initialization
######################################

  Space<-Matrix(0,nrow=nV,ncol=nV)      

  if(search.index==0)
  {
  Space[]<-1
  diag(Space)<-0
  }
  
  
  if(search.index==1)
  {
  ## Find the weight
  Weight<-Matrix(0,nrow=nV,ncol=nV)      
  
  for(child_index in 1:nV)
  {           
   yy<-x.stand[,child_index]       # Select yy (child) and xx (parents) in the LASSO type penalized LR
   xx<-x.stand[,-child_index]
   #xx<-as.matrix(xx)
   
   if(initial_weight =="Lasso"  )
   {
   lm_fit<-glmnet(xx, yy,family=c("gaussian"),lambda=initial_lambda/2) # Run lm
   coef_list<-coef(lm_fit)[-1]
   coef_list[coef_list==0]<-1/10^4
   
   Weight_list<-1/abs(coef_list)
   Weight_list[Weight_list>10^4]<-10^4

   Weight_list<-Weight_list^gamma_weight
   lambda_2<-lambda/2*sum(Weight_list)/length(Weight_list)
    
   glmnet_fit<-glmnet(xx, yy,family=c("gaussian"),lambda=lambda_2,penalty.factor=Weight_list) # Run glmnet
   coef_list_2<-coef(glmnet_fit)[-1]

   coef_list_2[coef_list_2!=0]<-1
   Space[child_index,-child_index]<-coef_list_2   # Update est_A matrix
   Weight_list[coef_list_2==0]<-0
   Weight[child_index,-child_index]<-Weight_list   
   }                                            
     

   if(initial_weight =="OLS"  )
   {
   lm_fit<-lm(yy ~ xx) # Run lm
   coef_list<-coef(lm_fit)[-1]
   
   Weight_list<-1/abs(coef_list)
   Weight_list[Weight_list>10^4]<-10^4

   Weight_list<-Weight_list^gamma_weight
   lambda_2<-lambda/2*sum(Weight_list)/length(Weight_list)
    
   glmnet_fit<-glmnet(xx, yy,family=c("gaussian"),lambda=lambda_2,penalty.factor=Weight_list) # Run glmnet
   coef_list_2<-coef(glmnet_fit)[-1]

   coef_list_2[coef_list_2!=0]<-1
   Space[child_index,-child_index]<-coef_list_2   # Update est_A matrix
   Weight_list[coef_list_2==0]<-0
   Weight[child_index,-child_index]<-Weight_list   
   }    

  }
  }
                                        
  if(search.index==2)
  {
  Space<-search.space
  diag(Space)<-0
  }  
  

#####################

  if ( !is.null(second_lambda) )
  { 
    lambda <- second_lambda
  }
 
  est_A<-Matrix(0,nrow=nV,ncol=nV)   # Initialize est_A and est_T matrix
  est_T<-Matrix(0,nrow=nV,ncol=nV)
  
  DELTA<-est_T                       # Initialize Delta matrix
  DELTA[]<-0  

  # Update Delta matrix
  for(child_index in 1:nV)
  {
      Updated_Table_result<-Sub2_Update_DELTA_Weight_Matrix(x.stand,child_index,est_T,est_A,DELTA,Data_size,lambda,Space,nV,Weight)
      est_T<-Updated_Table_result$est_T
      est_A<-Updated_Table_result$est_A
      DELTA<-Updated_Table_result$DELTA
  }

#######################

# Initialize the old est_T

Child_Search_index <- Matrix(1,nrow=1,ncol=nV)  
S<-Space


    residual_matrix <- x.stand - x.stand%*%t(est_A)       # Calculate log likelihood
    log_L<-sum((residual_matrix)^2)/Data_size
    lambda_sum<-lambda*sum( Weight*abs(est_A))                           # Calculate the penalty term
    objective_value<-log_L+lambda_sum                                    # Calculate the objective function value

pre_objective_value<-objective_value+100

   
while( objective_value < pre_objective_value )     # Run the algorithm until there is no increase in the objective function value
{

pre_objective_value<-objective_value
  
while( min(apply(S*DELTA,1,min))< 0  )      # Run the algorithm until there is no positive value in Delta matrix amoung the unsearched edges
{

######################################
# Step 1: Find the entering edge
######################################

temp_matrix<-S*DELTA            # Find the edge which gives the most negative delta among unselcted and unsearched edges
each_row_min_value<-apply(temp_matrix,1,min)
entering_selected_row<-which.min(each_row_min_value)
entering_selected_col<-which.min(temp_matrix[entering_selected_row,])
entering_edge<-c(entering_selected_row,entering_selected_col)          # Select the entering edge

check_tie<-(sum(est_T[entering_edge[1],])==0) & (sum(est_T[entering_edge[2],])==0) 

  if(check_tie) # Check whether there is a tie
  {
  S[entering_edge[2],entering_edge[1]]<-0   # If the tie exists, the other edge is checked in the search matrix S.
  }
 
######################################
# Step 2: Find the leaving edge to avoid a cycle
######################################

  Find_leaving_edge_result<-Sub2_Find_Leaving_Edge(est_T,entering_edge,DELTA,nV)  # Apply the function to find the leaving edge
  Success_Fail_index<-Find_leaving_edge_result$Success_Fail_index
  est_T<-Find_leaving_edge_result$est_T
  All_leaving_edges<-Find_leaving_edge_result$All_leaving_edges
                                              
######################################
# Step 3: Update Table
######################################
if(Success_Fail_index=="F")                                 # If "Success" (it means that the candidate entering edge can be selected), update the table est_T.
{
 S[entering_edge[1],entering_edge[2]]<-0
}

if(Success_Fail_index=="S")                                 # If "Success" (it means that the candidate entering edge can be selected), update the table est_T.
{
  DELTA[entering_edge[1],]<-0
  child_index_list<-entering_edge[1]
  S[entering_edge[1],entering_edge[2]]<-0

  if(length(All_leaving_edges)>3)                            # If there is a leaving edge, add the edge into children list
  {
    for(i in 2:length(All_leaving_edges[,1]))
    {
    DELTA[All_leaving_edges[i,1],]<-0
    child_index_list<-c(child_index_list,All_leaving_edges[i,1])
    S[All_leaving_edges[i,1],All_leaving_edges[i,2]]<-0
    }
  }

  for(child_index in child_index_list)       # The changed row (child row) is updated in est_T, est_A, and Delta matrix
  {
      Updated_Table_result<-Sub2_Update_DELTA_Weight_Matrix(x.stand,child_index,est_T,est_A,DELTA,Data_size,lambda,Space,nV,Weight)
      est_T<-Updated_Table_result$est_T
      est_A<-Updated_Table_result$est_A
      DELTA<-Updated_Table_result$DELTA
  }
}        # End, If "Success", update the table est_T.



######################################
# Step 4:  Update Table from positive delta selection
######################################


if(Success_Fail_index=="S")                                  # If "Success" (it means that the candidate entering edge can be selected), update the table est_T for the positive delta value.
{
  new_child_index_list<-0
  
  for(child_index in child_index_list)
  {
  if(sum( (DELTA[child_index,]>0) & (as.logical(est_T[child_index,])) )>0)
  {
   TF_temp_logic<-( DELTA[child_index,]>0 & as.logical(est_T[child_index,]) )
   est_T[child_index,TF_temp_logic]<-0
   new_child_index_list<-c(new_child_index_list,child_index)
  }
  }
#####################

  if(length(new_child_index_list)>1)
  {
  for(child_index in new_child_index_list[-1])
  {
      Updated_Table_result<-Sub2_Update_DELTA_Weight_Matrix(x.stand,child_index,est_T,est_A,DELTA,Data_size,lambda,Space,nV,Weight)
      est_T<-Updated_Table_result$est_T
      est_A<-Updated_Table_result$est_A
      DELTA<-Updated_Table_result$DELTA
  }
  }
  
}                     
  
####################

}              # The while loop ending

S <- Space-est_T 
    
    residual_matrix <- x.stand - x.stand%*%t(est_A)       # Calculate log likelihood
    log_L<-sum((residual_matrix)^2)/Data_size
    lambda_sum<-lambda*sum( Weight*abs(est_A))                           # Calculate the penalty term
    objective_value<-log_L+lambda_sum                                    # Calculate the objective function value

}



  BIC_penalty <- sum( est_T )*log(Data_size)/Data_size
  #BIC_regression <- log_L + BIC_penalty

  profiled_log_L<-sum( log( apply( (residual_matrix)^2 , 2,sum )/Data_size  ) )
  #BIC_likelihood <- profiled_log_L + BIC_penalty


result_Delta_Algorithm <- list(est_T=est_T,est_A=est_A,DELTA=DELTA,Space=Space,Weight=Weight,objective_value=objective_value,log_L=log_L,profiled_log_L=profiled_log_L,BIC_penalty=BIC_penalty)  # Return the values of results
return(result_Delta_Algorithm)
}



