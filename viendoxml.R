#conteo de paginas y revisiones
library("XML")
#result <- xmlParse(file = "C:\\Users\\Jonx\\Downloads\\enwiki-20161101-pages-meta-history1 7z\\enwiki-20161101-pages-meta-history1-p000000010p000002289.xml")
xmlDoc <- "C:\\Users\\Jonx\\Downloads\\enwiki-20161101-pages-meta-history1 7z\\enwiki-20161101-pages-meta-history1-p000000010p000002289.xml"
xmlDoc <- "C:\\Users\\Jonx\\Downloads\\WikiAnalisis\\Wiki de prueba\\enwiki-20161101-pages-meta-current1.xml"
result <- NULL
value<-list()
#function to use with xmlEventParse
ROW = function(node,...){
  #    children <- xmlChildren(node)
  #    print("pages")
  #    print(length(children[which(names(children) == "page")]))
  #    print("revisions")
  #    print(length(children[which(names(children) == "revision")]))
  print(class(node))
  ns <- getNodeSet(x,path = "//revision")
  value <- xmlValue(ns[[1]])
  print(value)
  #result <<- rbind(result, sapply(children,xmlValue))
}

#call the xmlEventParse
algo<-xmlEventParse(xmlDoc, handlers = NULL, branches = list(ROW = ROW),
              saxVersion = 2, trim = FALSE)