exercicio2 <- function(){
  library(psych)
  df<-createDF()
  genero(df)
  idade(df)
  ano(df)
  frequencia(df)
  statsDF<-stats(df)
  allplots(df,statsDF)
  correlations(df)
}

createDF<-function(){
  base_folder="C:/Users/AF/MEOCloud/UFP/Mestrado/3�Semestre/SSAP/Estat�sticas_an�lise_de_dados/"
  filename="dados_stat_as2016.csv"
  df<-read.csv(paste(base_folder,filename,sep=""),header = TRUE)
  names<-c("Id","Importante","Conceitos","Ciclo de Vida", "An�lise Requisitos","Gest�o de projetos","Modelos e bases de dados","An�lise estruturada","DFD's","E-R","L�gica","AOO","Casos de Uso","Classes","Sequencia","Doc Conceitos","Doc An. Estruturada","Doc An OO","Doc An Soft","Sugest�es","Genero","Idade","Frequencia","Ano","Trabalha")
  names(df)<-names
  df
}

allplots<-function(df,statsDF){
  df<-df[,3:19]
  lapply(colnames(df),function(x){
    base_folder<-"C:/Users/AF/MEOCloud/UFP/Mestrado/3�Semestre/SSAP/Estat�sticas_an�lise_de_dados/plots/"
    filename<-(paste(x,".png",sep=""))
    png(filename=paste(base_folder,filename,sep=""))
    
    max<-max(df[,x],na.rm=TRUE)
    min<-min(df[,x],na.rm=TRUE)
    
    breaks<-max-min
    #breaks<-7
    h<-hist(df[,x],breaks=breaks,ylab="frequencia",xlab="valores",main=x,col="blue")
    means<-as.numeric(statsDF[5,x])
    sds<-as.numeric(statsDF[6,x])
    
    xfit<-seq(min,max,length=40) 
    yfit<-dnorm(xfit,mean=means,sd=sds) 
    yfit <- yfit*diff(h$mids[1:2])*length(df[,x]) 
    lines(xfit, yfit, col="black", lwd=2)
    
    abline(v=statsDF[9,x],col="green",lwd=2)
    abline(v=statsDF[10,x],col="green",lwd=2)
    
    dev.off()
  })
}

stats<-function(df){
  #exclui algumas colunas
  df<-df[-c(1,20,26)]
  #aplica��o em todas as colunas da df de diversas fun��es estat�sticas
  mins<-sapply(df,min, na.rm=TRUE)
  maxs<-sapply(df,max, na.rm=TRUE)
  means<-sapply(df, mean, na.rm=TRUE)
  sds<-sapply(df,sd,na.rm=TRUE)
  medians<-sapply(df,median,na.rm=TRUE)
  modes<-sapply(df,getmode)
  confidenceLower<-sapply(df,getConfidence, "-")
  confidenceUpper<-sapply(df,getConfidence, "+")
  lower<-means-sds
  upper<-means+sds
  #cria data frame com todas as fun��es estat�sticas
  statsDF<-data.frame(mins,maxs,lower,upper,means,sds,medians,modes,confidenceLower,confidenceUpper)
  #transposi��o da data frame
  #t(statsDF)
  #nova data frame referente apenas �s quest�es
  questionsDF<-statsDF[2:19,]
  #obtem o nome da coluna cujos valores s�o os maiores ou menores
  maxValues<-rownames(questionsDF)[apply(questionsDF,2,which.max)]
  minValues<-rownames(questionsDF)[apply(questionsDF,2,which.min)]
  
  #inclui os nomes das colunas com maiores e menores valores na data frame de estatisticas
  statsDF<-rbind(statsDF,minValues)
  statsDF<-rbind(statsDF,maxValues)
  names(statsDF)[names(statsDF)=="24"] <- "minValues"
  
  t(statsDF)
}

getConfidence<-function(v,op){
  s<-sd(v, na.rm=TRUE)
  n<-sqrt(length(v))
  avg<-mean(v, na.rm=TRUE)
  
  if(op=="+"){
    result<-avg+s/n
  }else{
    result<-avg-s/n
  }
  result
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

ano<-function(df){
  base_folder<-"C:/Users/AF/MEOCloud/UFP/Mestrado/3�Semestre/SSAP/Estat�sticas_an�lise_de_dados/plots/"
  filename<-(paste("ano",".png",sep=""))
  png(filename=paste(base_folder,filename,sep=""))
  cleanedAno<-df$Ano[!is.na(df$Ano)]
  cleanedAno<-cleanedAno[cleanedAno!=0]
  hist(cleanedAno,main="Anos dos alunos",breaks=3,ylab="Frequencias",xlab="Anos",col="blue")
  print (length(cleanedAno))
  dev.off()
}

genero<-function(df){
  base_folder<-"C:/Users/AF/MEOCloud/UFP/Mestrado/3�Semestre/SSAP/Estat�sticas_an�lise_de_dados/plots/"
  filename<-(paste("genero",".png",sep=""))
  png(filename=paste(base_folder,filename,sep=""))
  cleanedGenero<-df$Genero[!is.na(df$Genero)]
  hist(cleanedGenero,main="Generos",ylab="Frequencia",xlab="1=H 2=M",breaks=2,col="blue")
  dev.off()
  
}

idade<-function(df){
  base_folder<-"C:/Users/AF/MEOCloud/UFP/Mestrado/3�Semestre/SSAP/Estat�sticas_an�lise_de_dados/plots/"
  filename<-(paste("idade",".png",sep=""))
  png(filename=paste(base_folder,filename,sep=""))
  
  #cria intervalos de valores
  interval<-c(17,24,34,44,54,64)
  labels<-c("18-24","25-34","35-44","45-54","55-64")
  #adiciona uma coluna � DF com o valor do contentor relativo � idade dos inquiridos
  contentorIdade <- cut(df$Idade,breaks=interval,labels=labels)
  #cria uma tabela de frequencia com os intervalos dos contentores
  tableIdade<-table(contentorIdade)
  plot(tableIdade,main="Idades",ylab="Frequencia",xlab="Idades",col="blue")
  
  dev.off()
}
frequencia<-function(df){
  base_folder<-"C:/Users/AF/MEOCloud/UFP/Mestrado/3�Semestre/SSAP/Estat�sticas_an�lise_de_dados/plots/"
  filename<-(paste("frequencia",".png",sep=""))
  png(filename=paste(base_folder,filename,sep=""))
  
  cleanedFrequencia<-df$Frequencia[!is.na(df$Frequencia)]
  hist(cleanedFrequencia,main="Frequencia",ylab="Frequencias",xlab="1=Dia 2=P�s-Laboral",col="blue")
  
  dev.off()
}

correlations<-function(df){
  df<-df[-c(1,2,20:26)]
  correl<-corr.test(df)$r
  #correl[correl>0 & correl<0.2]<-"muito fraca"
  #correl[correl>=0.2 & correl<0.4]<-"fraca"
  #correl[correl>=0.4 & correl<0.7]<-"moderada"
  #correl[correl>=0.7 & correl<0.9]<-"forte"
  #correl[correl>=0.9 & correl<1]<-"muito forte"
  
  correl[correl==1]<-NA
  correl[correl>0 & correl<0.2]<-1
  correl[correl>=0.2 & correl<0.4]<-2
  correl[correl>=0.4 & correl<0.7]<-3
  correl[correl>=0.7 & correl<0.9]<-4
  correl[correl>=0.9 & correl<1]<-5
  
  correl
}