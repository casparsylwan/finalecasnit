
#' Tests Knapsack using dynamic programming
#'
#' @param df The first parameter i.e. a data frame
#' @param W The second parameter i.e. weight constraint
#'
#' @return
#' @export
#'

knapsack_dynamic<-function(df, W){

  df<-df[order(df[,1]),]
  #rownames(df)<-c(1:length(df[,1]))
  value<-df[,2]
  weight<-df[,1]

  n_obj<-length(weight)

  knapp_mat<-matrix(NA, nrow=n_obj+1,ncol=W+1);keep_mat<-matrix(NA, nrow=n_obj,ncol=W)
  colnames(knapp_mat)<-as.character(c(0:W))   ;colnames(keep_mat)<-as.character(c(1:W))
  rownames(knapp_mat)<-as.character(c(0:n_obj));rownames(keep_mat)<-as.character(c(1:n_obj))
  knapp_mat[,"0"]<-0;knapp_mat["0",]<-0       ;#keep_mat[,"0"]<-0;keep_mat["0",]<-0

  for(obj in 1:n_obj){
    for(w in 1:W){
      value_one<- knapp_mat[as.character(obj-1),as.character(w)]
      if(w-weight[obj]<0){value_two<-0
      }else{value_two<-knapp_mat[as.character(obj-1),as.character(w-weight[obj])]+value[obj]}

      if(value_one>value_two){knapp_mat[as.character(obj),as.character(w)] <- value_one
      keep_mat[as.character(obj),as.character(w)]<-0
      }else{knapp_mat[as.character(obj),as.character(w)] <- value_two
      keep_mat[as.character(obj),as.character(w)]<-1} }}


  nc<-dim(keep_mat)[2]
  nr<-dim(keep_mat)[1]
  obj_to_keep_vec<-c()
  index1<-1

  while(nr>=2 & nc>1 & W>0){
    nc<-dim(keep_mat)[2]
    nr<-dim(keep_mat)[1]
    keep<-keep_mat[nr,nc]
    nc1<-nc
    nr1<-nr

    if(keep==1){obj_to_keep_vec[index1]<-rownames(df)[nr];nc<-(W-weight[nr]+1);W<-nc-1
    index1<-index1+1; keep_mat<-keep_mat[-nr,-(nc:nc1)]; nr<-nr-1
    }else{keep_mat<-keep_mat[-nr,];nr<-nr-1}
  }
  print(index1)
  if(W>=weight[1]){obj_to_keep_vec[index1]<-rownames(df)[1]}

  a<-dim(knapp_mat)[1]
  b<-dim(knapp_mat)[2]
  results<-list()
  results[["Value"]]<-knapp_mat[a,b]
  results[["elelments"]]<-as.numeric(obj_to_keep_vec)
  return(results)

}
