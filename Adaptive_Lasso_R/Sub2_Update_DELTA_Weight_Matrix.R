Sub2_Update_DELTA_Weight_Matrix <-
function(Data_stand=Data_stand,child_index=child_index,est_T=est_T,est_A=est_A,DELTA=DELTA,Data_size=Data_size,penalty_lambda=penalty_lambda,Space=Space,nV=nV,Weight=Weight)
{ 
##################            
    est_A[child_index,]<-0
    
    yy<-Data_stand[,child_index]                    # Decide yy (child) and xx (parents) in the LASSO type penalized LR
    xx<-Data_stand[,as.logical(est_T[child_index,])]
    xx<-as.matrix(xx)
    
    Weight_list<-Weight[child_index,as.logical(est_T[child_index,])]

    initial_para_temp <- est_A[child_index,as.logical(est_T[child_index,])]
    initial_para_pos<-initial_para_temp
    initial_para_pos[initial_para_pos<0]<-0
    initial_para_neg<- -initial_para_temp
    initial_para_neg[initial_para_neg<0]<-0
    initial_para<-c(initial_para_pos,initial_para_neg)
            
      if(length(xx)!=0)                               # If the number of parents is not zero
      {

#########c
       S_1<-1/Data_size*t(xx)%*%xx
       S_2<-rbind(cbind(S_1,-S_1),cbind(-S_1,S_1))
       two_S_2<-2*S_2

       d_1<-2/Data_size*t(yy)%*%xx
       d_2<-cbind(-d_1,d_1)
       Weights<-cbind(t(Weight_list),t(Weight_list))

       d_3<-d_2+penalty_lambda*Weights
       t_d_3<-t(d_3)

      grad_p_logL<-function(beta)                         # gradient function for the LASSO type penalized LR
      {
      two_S_2%*%beta  + t_d_3 
      }
        
      p_logL<-function(beta)                           # function for the LASSO type penalized LR
      {
      t(beta)%*%S_2%*%beta  + d_3%*%beta 
      }
 
       num_parent<-dim(xx)[2]                           # Count the number of parents
       fit_plm<-optim(initial_para,p_logL,grad_p_logL,method="L-BFGS-B",lower=rep(0,2*num_parent),upper=rep(Inf,2*num_parent))       # Run the optimization algoithm to solve the lasso problem
       two_beta<-fit_plm$par  
       coef_list<-two_beta[1:num_parent]-two_beta[(num_parent+1):(2*num_parent)]
       func_value<-1/Data_size*t(yy)%*%yy+fit_plm$value
###########    
      est_A[child_index,as.logical(est_T[child_index,])]<-coef_list   # Update est_A matrix
      M_original_selected_parent<-func_value          # M value with original selected parent ,
      }
 
      if(length(xx)==0)                                  # If the number of parents is zero,
      {
      M_original_selected_parent<-sum((yy)^2)/Data_size    
      }
 
##################

  
    for(entering_index in c(1:nV)[as.logical(Space[child_index,])] )        # Update by entering parent index
    {
    if((entering_index!=child_index))
    {
    
#######################
      if(est_T[child_index,entering_index]==1)             # Update the entry with a edge
      {
      yy<-Data_stand[,child_index]
      temp_TF<-est_T[child_index,]
      temp_TF[entering_index]<-0                          # Enter 0 into the entry of entering parent index
      xx<-Data_stand[,as.logical(temp_TF)]                # Decide xx (parents) in the LASSO type penalized LR
      xx<-as.matrix(xx)

      Weight_list<-Weight[child_index,as.logical(temp_TF)]
 
    initial_para_temp <- est_A[child_index,as.logical(temp_TF)]
    initial_para_pos<-initial_para_temp
    initial_para_pos[initial_para_pos<0]<-0
    initial_para_neg<- -initial_para_temp
    initial_para_neg[initial_para_neg<0]<-0
    initial_para<-c(initial_para_pos,initial_para_neg)        
 
        
      if(length(xx)!=0)                                 # If the number of parents is not zero,
      {

#########
       S_1<-1/Data_size*t(xx)%*%xx
       S_2<-rbind(cbind(S_1,-S_1),cbind(-S_1,S_1))
       two_S_2<-2*S_2

       d_1<-2/Data_size*t(yy)%*%xx
       d_2<-cbind(-d_1,d_1)
       Weights<-cbind(t(Weight_list),t(Weight_list))

       d_3<-d_2+penalty_lambda*Weights
       t_d_3<-t(d_3)

      grad_p_logL<-function(beta)                          # gradient function for the LASSO type penalized LR
      {
      two_S_2%*%beta  + t_d_3 
      }
        
      p_logL<-function(beta)                           # function for the LASSO type penalized LR
      {
      t(beta)%*%S_2%*%beta  + d_3%*%beta 
      }
 
       num_parent<-dim(xx)[2]                           # Count the number of parents
       fit_plm<-optim(initial_para,p_logL,grad_p_logL,method="L-BFGS-B",lower=rep(0,2*num_parent),upper=rep(Inf,2*num_parent))
       two_beta<-fit_plm$par  
       coef_list<-two_beta[1:num_parent]-two_beta[(num_parent+1):(2*num_parent)]
       func_value<-1/Data_size*t(yy)%*%yy+fit_plm$value
###########                              # Get coefficient values
      M_wo_selected_parent<-func_value                 # M value without selected parent
      }
 
      if(length(xx)==0)                                   # If the number of parents is zero...
      {
      M_wo_selected_parent<- sum((yy)^2)/Data_size     
      }
    
      DELTA[child_index,entering_index]<-M_original_selected_parent-M_wo_selected_parent    # Caculate the increment of the objective function value, and update the delta table
      
      }
#######################

      if(est_T[child_index,entering_index]==0)                # Update the entry with no edge
      {
      
      temp_est_T<-est_T
      temp_est_T[child_index,entering_index]<-1               # Enter 1 into the entry of entering parent index

      temp_TF<-temp_est_T[child_index,]
      xx<-Data_stand[,as.logical(temp_TF)]                    # Decide xx (parents) in the LASSO type penalized LR
      xx<-as.matrix(xx)

      Weight_list<-Weight[child_index,as.logical(temp_TF)]
 
    initial_para_temp <- est_A[child_index,as.logical(temp_TF)]
    initial_para_pos<-initial_para_temp
    initial_para_pos[initial_para_pos<0]<-0
    initial_para_neg<- -initial_para_temp
    initial_para_neg[initial_para_neg<0]<-0
    initial_para<-c(initial_para_pos,initial_para_neg)        
          
      if(length(xx)!=0)                                       # If the number of parents is not zero,
      {  

#########
       S_1<-1/Data_size*t(xx)%*%xx
       S_2<-rbind(cbind(S_1,-S_1),cbind(-S_1,S_1))
       two_S_2<-2*S_2

       d_1<-2/Data_size*t(yy)%*%xx
       d_2<-cbind(-d_1,d_1)
       Weights<-cbind(t(Weight_list),t(Weight_list))

       d_3<-d_2+penalty_lambda*Weights
       t_d_3<-t(d_3)

      grad_p_logL<-function(beta)                           # gradient function for the LASSO type penalized LR
      {
      two_S_2%*%beta  + t_d_3 
      }
        
      p_logL<-function(beta)                           # function for the LASSO type penalized LR
      {
      t(beta)%*%S_2%*%beta  + d_3%*%beta 
      }
 
       num_parent<-dim(xx)[2]                           # Count the number of parents
       fit_plm<-optim(initial_para,p_logL,grad_p_logL,method="L-BFGS-B",lower=rep(0,2*num_parent),upper=rep(Inf,2*num_parent))
       two_beta<-fit_plm$par  
       coef_list<-two_beta[1:num_parent]-two_beta[(num_parent+1):(2*num_parent)]
       func_value<-1/Data_size*t(yy)%*%yy+fit_plm$value
########### 
      M_w_selected_parent<-func_value                         # M value with selected parent
      }
      
      if(length(xx)==0)                                       # If the number of parents is zero,
      {
      M_w_selected_parent<- sum((yy)^2)/Data_size 
      }
      
      DELTA[child_index,entering_index]<-M_w_selected_parent-M_original_selected_parent    # Caculate the increment of the objective function value, and update the delta table
       
      }
  
    }
    }
    
result_Updated_Table <- list(est_T=est_T,est_A=est_A,DELTA=DELTA)
return(result_Updated_Table)
 
}
