

web_scrapping <- function(nmax){
  
  library(rvest)
  library(xml2)
  library(dplyr)
  library(rjson)
  
  web_discursos <- html("http://www.beersandpolitics.com/discursos/")
  urls <- web_discursos %>% 
    html_nodes("#listadiscurso a") %>%
    html_attr("href")
  urls <- data_frame(urls)
  
  if(missing(nmax)) {
    nmax = nrow(urls)
  }
  
  for(i in 1:nmax) {
    url <- paste(c("http://www.beersandpolitics.com",urls[i,]),collapse='')
    pagina <- html(url)
  
    texto <- pagina %>% 
      html_nodes("hr + p") %>%
      html_text()
    autoryfecha <- pagina %>% 
      html_nodes("h1 + p") %>%
      html_text()
    
    autoryfecha <- strsplit(autoryfecha, " - ")
    
    fecha <- autoryfecha[[1]][1]
    autor <- autoryfecha[[1]][2]

    list <- list('speecher' = autor, 'date' = fecha, 'speech' = texto)
    exportJson <- toJSON(list)
    
    today <- Sys.Date()
    title <- paste(today,autor,fecha,sep = "_")
    title <- paste("results/",title,".json")
    write(exportJson, title)
    
  }
  
}