

mix_pops<-function(pops, # a character vector of populations to mix
                  probs = NULL, #a numeric vector indicating the probability of each population, default is equal probs
                  data,
                  seed=123
                  ){
  set.seed(seed)
  
  #number of studies in the data
  st<-dim(data)[[1]]
  iter<-dim(data)[[4]]
  n<-attributes(data)$n
  
  if(is.null(probs)){
    probs<-rep(1/length(pops), length(pops))
  }#end probs
  
  listn<-lapply(1:length(n), function(s){ 
    listi<-lapply(1:iter, function(i){
      l<-lapply(1:length(pops), function(p){
        x<-dat[sample(1:st, size = probs[p]*st),,pops[p],i, s]
        rownames(x)<-rep(pops[p], times=nrow(x))
        return(x)
      })
      df<-rlist::list.rbind(l)
    })
    arri<-abind(listi, along = 3)
    dim(arri)<-c(30,3,1,1000)
    return(arri)
  })
  
  arrn<-abind(listn, along = 5)
  
  dimnames(arrn)<-list(dimnames(data)[[1]],
                       dimnames(data)[[2]],
                       paste0("m_", paste0(pops, collapse = "*")),
                       dimnames(data)[[4]],
                       dimnames(data)[[5]]
                       )
  
  return(arrn)
  
  
}# end mix_pop()

mixed<-mix_pops(pops = pops,
         data=dat
         )

abind(dat, mixed, along=3)
dim(mixed)
pops<-c("delta-0.2_tau0","delta0.2_tau0")
dimnames(dat)

i<-1
s<-1
p<-1
for(p in 1:length(pops)){
  
}
data<-dat
dat[,,pops[p],i, s]
sample(1:30, size = 15)

dat[sample(1:st, size = probs[p]*st),,pops[p],i, s]


 
l<-lapply(1:length(pops), function(p){
  x<-dat[sample(1:st, size = probs[p]*st),,pops[p],i, s]
  rownames(x)<-rep(pops[p], times=nrow(x))
  return(x)
})
df<-rlist::list.rbind(l)
#df<-df[order(as.numeric(rownames(df))),]




  l<-lapply(1:length(pops), function(p){
    x<-dat[sample(1:st, size = probs[p]*st),,pops[p],i, s]
    rownames(x)<-rep(pops[p], times=nrow(x))
    return(x)
  })
  df<-rlist::list.rbind(l)
  


listn<-lapply(1:length(n), function(s){ 
  listi<-lapply(1:iter, function(i){
    l<-lapply(1:length(pops), function(p){
      x<-dat[sample(1:st, size = probs[p]*st),,pops[p],i, s]
      rownames(x)<-rep(pops[p], times=nrow(x))
      return(x)
    })
    df<-rlist::list.rbind(l)
  })
  arri<-abind(listi, along = 3)
  dim(arri)<-c(30,3,1,1000)
  return(arri)
})

arrn<-abind(listn, along = 5)

