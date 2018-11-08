#' Dynamic knappsack with rcpp
#'
#'@importFrom Rcpp cppFunction
#'
#'@param df The first parameter i.e. a data frame
#'@param W The second parameter i.e. weight constraint
#'
#'@export 
#'
knapsack_dynamic_cpp<-function(df, W){
  
  
  cppFunction("
  NumericVector knapcpp(NumericVector obj_value, NumericVector obj_weight, int W){
              
              int n = obj_weight.size();
              NumericMatrix value_matrix(n+1, W);
              NumericMatrix keep_matrix(n+1, W);
              double value_one;
              double value_two;
              int w_left;
              int index = 0;
              NumericVector final(n+1);
              int W1 = W-1;
              int obje = n-1;
              
              for(int obj = 1; obj<n+1; obj++){
              
              value_one = 0;
              value_two = 0;
              
              for(int w=0; w<W; w++){
              value_one = value_matrix(obj-1,w);
              w_left = w - obj_weight(obj-1);
              if(w_left>=0){value_two = obj_value(obj-1) + value_matrix(obj-1, w_left+1);}
              
              if(value_two>value_one){keep_matrix(obj,w)= 1;value_matrix(obj,w)=value_two;
              }else{keep_matrix(obj,w)= 0;value_matrix(obj,w)=value_one; }
              
              }}
              
              final(index) = value_matrix(n-1,W-1); 
              
              while(obje> 0 && W1>0){
              
              if(keep_matrix(obje,W1)==1){index=index+1;final(index)=obje;obje=obje-1; W1=W1 - obj_weight(obje)+1;
              }else{obje=obje-1;}}
              
              
              
              
              
              return final;
}")

  
  
  
  
  df<-df[order(df[,1]),]
  
  results1<-knapcpp(df[,2],df[,1],W)
  
  results<-list()
  

  results[["value"]]<-results1[1]
  results1<-results1[which(results1>0)]
  results1<-as.numeric(rownames(df)[results1])
  results[["elements"]]<-results1[-1]
  
  

  return(results)  
  
}
