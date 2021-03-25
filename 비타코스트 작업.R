

library(data.table)
library(dplyr)
library(RSelenium)
library(rvest)
library(XML)
library(stringr)


driver <- rsDriver(browser=c("chrome"), chromever="88.0.4324.96", extraCapabilities = eCaps)

setwd("C:/Rselenium")  # 경로지정 필수

system("cmd.exe /c java -Dwebdriver.gecko.driver='geckodriver.exe' -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445", 
       intern = T, timeout = 2)


remDr <- remoteDriver(remoteServerAddr = 'localhost',
                      port = 4445, # 포트번호 입력
                      browserName = "chrome")

remDr$open()


remDr$navigate('https://www.vitacost.com/new-arrivals?s=NewArrivals')

modalclose <-  remDr$findElement(using = 'xpath', value = '//*[@id="emailModalDesktopClose"]')
modalclose$clickElement()

remDr$deleteAllCookies()

show60 <-  remDr$findElement(using = 'xpath', value = '//*[@id="IamMasterFrameYesIam_ctl02_Pagination_Top_UserControl_showAllLink"]')
show60$clickElement()

modalclose <-  remDr$findElement(using = 'xpath', value = '//*[@id="emailModalDesktopClose"]')
modalclose$clickElement()

remDr$deleteAllCookies()


# url 갱신 ------------------------------------------------------------------



# new_url03161700<- data.frame()
# new_url <- new_url03161700
# new_url03181623 <- data.frame()


# check <- new_url03181623[-grep("#ProductReviews", new_url03181623$.),]  %>% unique() %>% data.frame()
# new_url03181623 <- new_url03181623 %>% unique()

# new_url03181623$. <- gsub("/pd_section-pr#ProductReviews", "",new_url03181623$. )

for( i in 1: 38){
  print(i)
  html <- remDr$getPageSource()[[1]]
  html <- read_html(html)
  
  url <-  html %>% html_nodes(xpath = '//*[@id="resultsForm"]/ul') %>% html_nodes('li div div a')  %>% html_attr('href')
  url <- url %>% unique()
  url <- url[-grep("CheckOut", url)]
  url <- paste0("https://www.vitacost.com",url) %>% data.frame()
  
  # all_url <- data.frame()
  new_url03181623 <- bind_rows(new_url03181623, url)
  
  remDr$deleteAllCookies()
  
  click_next <- remDr$findElement(using = 'xpath', value = ' //*[@id="IamMasterFrameYesIam_ctl02_Pagination_Bottom_UserControl_next"]')
  click_next$clickElement()
  
  remDr$deleteAllCookies()
  
  Sys.sleep(6)
  
  modalclose <-  remDr$findElement(using = 'xpath', value = '//*[@id="emailModalDesktopClose"]')
  modalclose$clickElement()
  Sys.sleep(3)
  
  remDr$deleteAllCookies()
  
}

# new_url <- new_url03181623

# detail 크롤링 --------------------------------------------------------------

# i = 1932
for( i in 1932: nrow(new_url)){
  
  print(i)
  
  remDr$deleteAllCookies()
  
  remDr$navigate(new_url[i,1])
  Sys.sleep(8)
  
  # html <- remDr$getPageSource()[[1]]
  # html <- read_html(html)
  # 
  # body <- html %>% html_nodes('body') %>% html_text()
  # 
  # if(identical(grepl("Oops!", body), integer(0))){
  #   
  # }else{
  #   
  #   remDr$deleteAllCookies()
  #   remDr$navigate(new_url[i,1])
  #   Sys.sleep(8)
  # 
  # }
  # 

  modalclose <-  remDr$findElement(using = 'xpath', value = '//*[@id="emailModalDesktopClose"]')
  modalclose$clickElement()
  Sys.sleep(3)
  
  
  ## category (collapse = ", ")
  category <- html %>% html_nodes(xpath = '//*[@id="ContentWrapper"]/div/div/div[1]/nav/h3') %>% html_nodes('a') %>% html_attr('href')
  category <- str_sub(category, 2, nchar(category))
  category <- paste0(category, collapse = ", ")
  
  ## product name
  product <- html %>% html_nodes(xpath = '//*[@id="pdTitleBlock"]/h1') %>% html_text()
  
  ## product details
  product_details <-  html %>% html_nodes('.section div div div')  %>% html_nodes('#productDetails') %>% html_text()
  
  srt <- str_locate(product_details,"Description")[2]+1
  
  lst <- nchar(product_details)
  
  if(identical(grep("Directions", product_details), integer(0))){
    
    product_details <- str_sub(product_details,srt,lst)
    
  } else {
    
    d_srt <- str_locate(product_details,"Directions")[1]-1
    product_details <- str_sub(product_details, srt, d_srt)

  }

  
  product_details <- gsub("  ","",product_details)
  product_details <- gsub("\n\n",'\n',product_details)
  product_details <- gsub("\n\n",'\n',product_details)
  product_details <- str_trim(product_details)
  # cat(product_details)


# 공백제거 --------------------------------------------------------------------

  
  # gsub("\n+( )+\n", "\n", a)
  # gsub("\\s+", "\n", a)
  # stringr::str_squish(a)

  # check <- gsub("\\s+", "\n", product_details)
  # cat(check)
  # 
  # check<-  gsub("\n+( )+\n", "\n", product_details)
  # cat(check)

# -----------------------------------------------------------------------------
  
  
  ## facts 부분 
  
  # null인 부분 확인 

    # facts <-  html %>% html_nodes('#nutritionFacts')

  
  type <- html %>%  html_nodes('.ifTtl') %>% html_text()
  t1 <- html %>% html_nodes('.ifSngSz') %>% html_text()
  t2 <- html %>% html_nodes('.ifSngPC') %>% html_text()
  type <- paste0(type,"\n", t1,"\n", t2)
  
  if(length(type)>2) {
    type <- type[1:2]
  }
  # cat(type)
  
  table <- html %>% html_nodes('.ifChrtInr table') %>% html_table() 

  if(length(table) >= 2){
    # j = 1

    table1 <- table[[1]] %>% data.frame()
    table1 <-table1[-which(table1$X2 == ""),]
    table1 <- paste0(table1$X1,"   ",table1$X2,"   ", table1$X3, collapse = "\n")
    table1 <- str_trim(table1) %>% data.frame()
    
    table2 <- table[[2]] %>% data.frame()
    table2 <-table2[-which(table2$X2 == ""),]
    table2 <- paste0(table2$X1,"   ",table2$X2,"   ", table2$X3, collapse = "\n")
    table2 <- str_trim(table2) %>% data.frame()
    table <- bind_rows(table1, table2)
    names(table) <- "Table"
    
  }else {
    t <- html %>% html_nodes('.ifChrtInr table') %>% html_table()  %>% data.frame()
    t <-t[-which(t$X2 == ""),]
    t <- paste0(t$X1,"   ",t$X2,"   ", t$X3, collapse = "\n")
  
    t <- str_trim(t)
    table <- t
  } 
  
# ingredients -------------------------------------------------------------

ingredients <- html %>% html_nodes('.ifOi') %>% html_text()
  
if(identical(ingredients, character(0))){
  ingredients <- ""
} else if(length(ingredients) != 1){
  ingredients <- ingredients[1:2]
}

# dataframe ---------------------------------------------------------------

  one <- data.frame("Category" = category,
                    "Product" = product,
                    "Description" = product_details,
                    "Facts" = type,
                    "Table" = table,
                    "Ingredients"= ingredients,
                    "url" = new_url[i,1]
                    )

  all <- bind_rows(all, one)
  remDr$deleteAllCookies()
  remDr$deleteAllCookies()
}


# setwd("C:/Users/home/Desktop/최혜원/R실습/K-RISS/Vitacost 크롤링")
# writexl::write_xlsx(all_0318,"Vitacost크롤링_0318.xlsx")

# all_data <- all %>% unique()
# all_0316 <- all[1:2214,]
# all_0318 <- all[2215:4458,] 

check <- full_join(new_url03161700, new_url03181623, keep=TRUE)
sum(is.na(check$..x))
sum(is.na(check$..y))
