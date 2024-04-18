data<-read.csv("D:\\CARS.csv")
output<-file("D:\\output.txt","w")
makes<-unique(data$Make)
for(make in makes){
  make_data<-subset(data,Make==make)
  origins<-unique(make_data$Origin)
  for(origin in origins){
    title<-paste(make,"[",origin,"]")
    writeLines(title, con = output)
    make_origin_data<-subset(make_data,Origin==origin)
    types<-unique(data$Type)
    res1<-""
    res2<-""
    for (type in types) {
      type_data <- subset(make_origin_data,Type==type)
      type_count <- nrow(type_data)
      res1<-paste(res1,type,"=",type_count," ")
      if(type_count>0){
        max_invoice_string <- max(type_data$Invoice)
        max_invoice<-as.numeric(gsub("[,\\$]","",max_invoice_string))
        res2<-paste(res2,type,"=",max_invoice,"RUB"," ")
      }
      else{
        res2<-paste(res2,type,"=0RUB"," ")
      }
    }
    writeLines(res1, con = output)
    writeLines(res2, con = output)
  }
}
close(output)

data<-read.csv("D:\\CARS.csv")
data$Invoice<- as.numeric(gsub("[,\\$]", "", data$Invoice))*60
types<-unique(data$Type)
origins<-unique(data$Origin)

df<- data.frame(
  Type=types
)

for(origin in origins){
  origin_data<-subset(data,Origin==origin)
  v<-character(0)
  for(type in types){
    origin_type_data<-subset(origin_data,Type==type)
    if(nrow(origin_type_data)>0)
    {
      makes<-unique(origin_type_data$Make)
      sumprice<-0
      for(make in makes){
        origin_type_make_data<-subset(origin_type_data,Make==make)
        maxprice<-max(origin_type_make_data$Invoice)
        sumprice=sumprice+maxprice
      }
      v<-c(v,paste(as.character(sumprice),"RUB"))
    }
    else
    {
      v<-c(v,"0RUB")
    }
  }
  df[[origin]] <- v
}
print(df)

print<-function(x)UseMethod("print")
print.data.frame<- function(data) {
  if(is.data.frame(data))
  {
    makes<-unique(data$Make)
    for(make in makes){
      make_data<-subset(data,Make==make)
      origins<-unique(make_data$Origin)
      for(origin in origins){
        title<-paste(make,"[",origin,"]")
        cat(title,"\n")
        make_origin_data<-subset(make_data,Origin==origin)
        types<-unique(data$Type)
        res1<-""
        res2<-""
        for (type in types) {
          type_data <- subset(make_origin_data,Type==type)
          type_count <- nrow(type_data)
          res1<-paste(res1,type,"=",type_count," ")
          if(type_count>0){
            max_invoice_string <- max(type_data$Invoice)
            max_invoice<-as.numeric(gsub("[,\\$]","",max_invoice_string))
            res2<-paste(res2,type,"=",max_invoice,"RUB"," ")
          }
          else{
            res2<-paste(res2,type,"=0RUB"," ")
          }
        }
        cat(res1,"\n")
        cat(res2,"\n")
      }
    }
  }
  else NextMethod("print")
}

write.csv<-function(x)UseMethod("write.csv")
write.csv.data.frame<- function(data) {
  csv_file <- "D:\\result.csv"
  if(is.data.frame(data)){
    data$Invoice<- as.numeric(gsub("[,\\$]", "", data$Invoice))*60
    types<-unique(data$Type)
    origins<-unique(data$Origin)
    
    df<- data.frame(
      Type=types
    )
    for(origin in origins){
      origin_data<-subset(data,Origin==origin)
      v<-numeric(0)
      for(type in types){
        origin_type_data<-subset(origin_data,Type==type)
        if(nrow(origin_type_data)>0)
        {
          makes<-unique(origin_type_data$Make)
          sumprice<-0
          for(make in makes){
            origin_type_make_data<-subset(origin_type_data,Make==make)
            maxprice<-max(origin_type_make_data$Invoice)
            sumprice=sumprice+maxprice
          }
          v<-c(v,paste(as.character(sumprice),"RUB"))
        }
        else
        {
          v<-c(v,"0RUB")
        }
      }
      df[[origin]] <- v
    }
    return(write.table(df, file = csv_file, sep = ",", col.names = TRUE, row.names = FALSE, quote = FALSE))
  }
  else NextMethod("write.csv")
}

data<-read.csv("D:\\CARS.csv")
print(data)
write.csv(data)


