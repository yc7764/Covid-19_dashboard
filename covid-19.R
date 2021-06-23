if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(XML)) install.packages("XML", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(formattable)) install.packages("formattable", repos = "http://cran.us.r-project.org")
if(!require(sp)) install.packages("sp", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(maptools)) install.packages("maptools", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(leaflet.extras)) install.packages("leaflet.extras", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
if(!require(sp)) install.packages("sp", repos = "http://cran.us.r-project.org")
if(!require(rgdal)) install.packages("rgdal", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")

many_def_plotly <- function(df_overseas){
  many_def = df_overseas[order(df_overseas$natDefCnt,decreasing = TRUE),]
  sub = many_def[1:20,]
  
  plot_ly(data = sub, x = ~natDefCnt, y = ~reorder(nationNm, natDefCnt), type = 'bar', orientation = 'h')%>%
    config(displayModeBar = F)%>%
    layout(xaxis = list(title = "확진자 수"),
           yaxis = list (title = ""))
}

selected_sido_plotly <- function(df, region){
  temp <- df[df$gubun == region,]
  
  labels = c('지역 발생 수','해외 유입 수')
  values = c(temp[1,'localOccCnt'], temp[1,'overFlowCnt'])
  
  p <- plot_ly(type='pie', labels=labels, values=values, 
          textinfo='label+percent',
          insidetextorientation='radial')%>%
    config(displayModeBar = F)%>%
    layout(title = "감염 경로별 신규 확진자")
  p
}

preproc_total <- function(date){
  tmp_date = format(date,"%Y%m%d")
  fileUrl <- paste0('http://openapi.data.go.kr/openapi/service/rest/Covid19/getCovid19InfStateJson?serviceKey=9NBSdjxDj%2FSdwI5DMFU7mS9FXkMmDQ9dLSIcfX0ejQLUv0vIzrN87WuFqZBx2ZnTdM5R81A0SSjFrv8CIpj5YQ%3D%3D&pageNo=1&numOfRows=10&startCreateDt=20200409&endCreateDt=',tmp_date,'&')
  doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
  rootNode <- xmlRoot(doc)
  items <- rootNode[[2]][['items']]
  size <- xmlSize(items)
  
  if(size == 0){
    tmp_date = format(date-1,"%Y%m%d")
    fileUrl <- paste0('http://openapi.data.go.kr/openapi/service/rest/Covid19/getCovid19InfStateJson?serviceKey=9NBSdjxDj%2FSdwI5DMFU7mS9FXkMmDQ9dLSIcfX0ejQLUv0vIzrN87WuFqZBx2ZnTdM5R81A0SSjFrv8CIpj5YQ%3D%3D&pageNo=1&numOfRows=10&startCreateDt=20200409&endCreateDt=',tmp_date,'&')
    doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
    rootNode <- xmlRoot(doc)
    items <- rootNode[[2]][['items']]
    
    size <- xmlSize(items)
  }
  
  item <- list()
  item_temp_dt<-data.table()
  
  for(j in 1:size){
    item_temp <- xmlSApply(items[[j]],xmlValue)
    item_temp_dt <- data.table( accDefRate = as.numeric(item_temp[1]),
                                accExamCnt = as.numeric(item_temp[2]),
                                accExamCompCnt = as.numeric(item_temp[3]),
                                careCnt = as.numeric(item_temp[4]),
                                clearCnt = as.numeric(item_temp[5]),
                                deathCnt = as.numeric(item_temp[7]),
                                decideCnt = as.numeric(item_temp[8]),
                                examCnt = as.numeric(item_temp[9]),
                                resutlNegCnt = as.numeric(item_temp[10]),
                                stateDt =as.Date(item_temp[12],"%Y%m%d"))
    item[[j]]<-item_temp_dt
  }
  total <- rbindlist(item)
  total <- data.frame(total)
  total <- total[-which(duplicated(total$stateDt)),]
}

preproc_byAge <- function(date){
  tmp_date = format(date,"%Y%m%d")
  fileUrl <- paste0('http://openapi.data.go.kr/openapi/service/rest/Covid19/getCovid19GenAgeCaseInfJson?serviceKey=9NBSdjxDj%2FSdwI5DMFU7mS9FXkMmDQ9dLSIcfX0ejQLUv0vIzrN87WuFqZBx2ZnTdM5R81A0SSjFrv8CIpj5YQ%3D%3D&pageNo=1&numOfRows=10&startCreateDt=20200409&endCreateDt=',tmp_date,'&')
  doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
  rootNode <- xmlRoot(doc)
  items <- rootNode[[2]][['items']]
  size <- xmlSize(items)
  
  if(size == 0){
    tmp_date = format(date-1,"%Y%m%d")
    fileUrl <- paste0('http://openapi.data.go.kr/openapi/service/rest/Covid19/getCovid19GenAgeCaseInfJson?serviceKey=9NBSdjxDj%2FSdwI5DMFU7mS9FXkMmDQ9dLSIcfX0ejQLUv0vIzrN87WuFqZBx2ZnTdM5R81A0SSjFrv8CIpj5YQ%3D%3D&pageNo=1&numOfRows=10&startCreateDt=20200409&endCreateDt=',tmp_date,'&')
    doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
    rootNode <- xmlRoot(doc)
    items <- rootNode[[2]][['items']]
    
    size <- xmlSize(items)
  }
  
  item <- list()
  item_temp_dt<-data.table()
  
  for(j in 1:size){
    item_temp <- xmlSApply(items[[j]],xmlValue)
    item_temp_dt <- data.table( confCase = as.numeric(item_temp[1]),
                                confCaseRate = as.numeric(item_temp[2]),
                                stateDt = as.Date(item_temp[3]),
                                criticalRate = as.numeric(item_temp[4]),
                                deathRate = as.numeric(item_temp[4]),
                                death = as.numeric(item_temp[5]),
                                gubun = item_temp[7])
    item[[j]]<-item_temp_dt
  }
  total_byAge_bygender <- rbindlist(item)
  total_byAge_bygender <- data.frame(total_byAge_bygender)
}

preproc_byRegion <- function(date){
  tmp_date = format(date,"%Y%m%d")
  fileUrl <- paste0('http://openapi.data.go.kr/openapi/service/rest/Covid19/getCovid19SidoInfStateJson?serviceKey=9NBSdjxDj%2FSdwI5DMFU7mS9FXkMmDQ9dLSIcfX0ejQLUv0vIzrN87WuFqZBx2ZnTdM5R81A0SSjFrv8CIpj5YQ%3D%3D&pageNo=1&numOfRows=10&startCreateDt=20200409&endCreateDt=',tmp_date,'&')
  doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
  rootNode <- xmlRoot(doc)
  items <- rootNode[[2]][['items']]
  size <- xmlSize(items)
  
  if(size == 0){
    tmp_date = format(date-1,"%Y%m%d")
    fileUrl <- paste0('http://openapi.data.go.kr/openapi/service/rest/Covid19/getCovid19SidoInfStateJson?serviceKey=9NBSdjxDj%2FSdwI5DMFU7mS9FXkMmDQ9dLSIcfX0ejQLUv0vIzrN87WuFqZBx2ZnTdM5R81A0SSjFrv8CIpj5YQ%3D%3D&pageNo=1&numOfRows=10&startCreateDt=20200409&endCreateDt=',tmp_date,'&')
    doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
    rootNode <- xmlRoot(doc)
    items <- rootNode[[2]][['items']]
    
    size <- xmlSize(items)
  }
  
  item <- list()
  item_temp_dt<-data.table()
  
  for(j in 1:size){
    item_temp <- xmlSApply(items[[j]],xmlValue)
    if(item_temp[12] == '-')
      item_temp[12] = '0'
    item_temp_dt <- data.table( stateDt = as.Date(item_temp[1]),
                                deathCnt = as.numeric(item_temp[2]),
                                defCnt = as.numeric(item_temp[3]),
                                gubun = item_temp[4],
                                gubunEn = item_temp[6],
                                incDec = as.numeric(item_temp[7]),
                                isolClearCnt = as.numeric(item_temp[8]),
                                isolIngCnt = as.numeric(item_temp[9]),
                                localOccCnt = as.numeric(item_temp[10]),
                                overFlowCnt = as.numeric(item_temp[11]),
                                qurRate = as.numeric(item_temp[12]))
    item[[j]]<-item_temp_dt
  }
  total_byregion <- rbindlist(item)
  total_byregion <- data.frame(total_byregion)
  #total_byregion <- total_byregion[-which(duplicated(total_byregion$gubun)),]
}

preproc_overseas <- function(date){
  tmp_date = format(date,"%Y%m%d")
  fileUrl <- paste0('http://openapi.data.go.kr/openapi/service/rest/Covid19/getCovid19NatInfStateJson?serviceKey=9NBSdjxDj%2FSdwI5DMFU7mS9FXkMmDQ9dLSIcfX0ejQLUv0vIzrN87WuFqZBx2ZnTdM5R81A0SSjFrv8CIpj5YQ%3D%3D&pageNo=1&numOfRows=10&startCreateDt=',tmp_date,'&endCreateDt=',tmp_date,'&')
  doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
  rootNode <- xmlRoot(doc)
  items <- rootNode[[2]][['items']]
  size <- xmlSize(items)
  
  if(size == 0){
    tmp_date = format(date-1,"%Y%m%d")
    fileUrl <- paste0('http://openapi.data.go.kr/openapi/service/rest/Covid19/getCovid19NatInfStateJson?serviceKey=9NBSdjxDj%2FSdwI5DMFU7mS9FXkMmDQ9dLSIcfX0ejQLUv0vIzrN87WuFqZBx2ZnTdM5R81A0SSjFrv8CIpj5YQ%3D%3D&pageNo=1&numOfRows=10&startCreateDt=',tmp_date,'&endCreateDt=',tmp_date,'&')
    doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
    rootNode <- xmlRoot(doc)
    items <- rootNode[[2]][['items']]
    
    size <- xmlSize(items)
  }
  
  item <- list()
  item_temp_dt<-data.table()
  
  for(j in 1:size){
    item_temp <- xmlSApply(items[[j]],xmlValue)
    item_temp_dt <- data.table( areaNm = item_temp[1],
                                areaNmEn = item_temp[3],
                                createDt = as.Date(item_temp[4]),
                                natDeathCnt = as.numeric(item_temp[5]),
                                natDeathRate = as.numeric(item_temp[6]),
                                natDefCnt = as.numeric(item_temp[7]),
                                nationNm = item_temp[8],
                                nationNmEn = item_temp[10])
    item[[j]]<-item_temp_dt
  }
  df <- rbindlist(item)
  df <- data.frame(df)
  
  df[df$nationNm=="미국","nationNmEn"] <- "USA"
  df[df$nationNm=="콩고","nationNmEn"] <- "Republic of Congo"
  df[df$nationNm=="DR콩고","nationNmEn"] <- "Democratic Republic of the Congo"
  df[df$nationNm=="영국","nationNmEn"] <- "UK"
  df[df$nationNm=="남아프리카공화국","nationNmEn"] <- "South Africa"
  df[df$nationNm=="코트디부아르","nationNmEn"] <- "Ivory Coast"
  df[df$nationNm=="북마케도이나","nationNmEn"] <- "Macedonia"
  df[df$nationNm=="몬테네그로","nationNmEn"] <- "Montenegro"
  df[df$nationNm=="콜롬비아","nationNmEn"] <- "Colombia"
  
  df
}

overseas_map <- function(df){
  world <- map("world", fill=TRUE, plot=FALSE)
  world_map <- map2SpatialPolygons(world, sub(":.*$", "", world$names))
  world_map <- SpatialPolygonsDataFrame(world_map,
                                        data.frame(country=names(world_map), 
                                                   stringsAsFactors=FALSE), 
                                        FALSE)
  sub <- df[,c('nationNm','nationNmEn','natDefCnt','natDeathCnt')]
  cnt <- df$nationNmEn
  
  target <- subset(world_map, country %in% cnt)
  target@data <- merge(x = target@data, y = sub, by.x = 'country' ,by.y = 'nationNmEn')
  
  bins <- c(0, 500, 1000, 5000, 10000, 50000, 1000000,5000000,10000000)
  pal <- colorBin("YlOrRd", domain = ~target$natDefCnt, bins = bins)
  
  labels <- paste("국가 : ", target@data$nationNm,
                  ", 확진자 수 : ", target@data$natDefCnt, 
                  ', 사망자 수 : ', target@data$natDeathCnt)
  
  map <- leaflet(target)%>%
    setView(lng=127.7669, lat=35.90776, zoom=4)%>%
    addProviderTiles('CartoDB.Positron',
                     options = tileOptions(minZoom=2, maxZoom=6))%>%
    addPolygons(data=target, 
                fillColor = ~pal(natDefCnt),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"),
                group = "확진자 수")%>%
    addPolygons(data=target, 
                fillColor = ~pal(natDeathCnt),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")
                ,group = "사망자 수")%>%
    addLegend(pal = pal, values = target$natDefCnt, opacity = 0.7,
              title = "확진자 수", position= "bottomright", labFormat = labelFormat(suffix ="명"))%>%
    addLayersControl(c("확진자 수", "사망자 수"),
                     options = layersControlOptions(collapsed = FALSE))
  map
}

korea_map <- function(shapefile, df){
  tmp <- shapefile
  
  tmp@data <- left_join(tmp@data, df, by = c('CTP_KOR_NM2' = 'gubun'))
  shp1_84 <- spTransform(tmp,CRS("+init=epsg:4326"))
  
  bins <- c(0, 10, 50, 100, 200, 500, 1000, 2000, 5000, 10000)
  pal <- colorBin("YlOrRd", domain = ~shp1_84$defCnt, bins = bins)
  
  labels <- paste("국가 : ", shp1_84@data$CTP_KOR_NM2,
                  ", 확진자 수 : ", shp1_84@data$defCnt)
  id <- c("서울", "부산", "대구", "인천", "광주", "대전", "울산", "세종", "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주")

  map <- leaflet(shp1_84)%>%
    setView(lng=127.7669, lat=36.3000, zoom=8)%>%
    addProviderTiles('CartoDB.Positron',
                     options = tileOptions(minZoom=2))%>%
    addPolygons(data = shp1_84, fillColor = ~pal(defCnt),
                weight = 2, layerId = id,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"))%>%
    addLegend(pal = pal, values = shp1_84$defCnt, opacity = 0.7,
              title = "확진자 수", position= "bottomright", labFormat = labelFormat(suffix ="명"))
  map
}

first_date = "2020-04-09"
current_date = Sys.Date()
total <- preproc_total(current_date)
total_overseas <- preproc_overseas(current_date)
total_region <- preproc_byRegion(current_date)
total_age <- preproc_byAge(current_date)

header <- dashboardHeader(title = "covid 19 현황판")

body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'total',
            tags$style(type = "text/css", "#korMap {height: calc(100vh - 80px) !important;}"),
            leafletOutput("korMap", width = '100%', height = '100%'),
            absolutePanel(id = "controls", class = "panel panel-default",
                          top = 75, left = 65, bottom = 'auto', right = 'auto',
                          draggable = TRUE, height = "auto",
                          h2('국내 코로나 현황',align="center"),
                          h5(textOutput("kor_date_reactive"),align="right"),
                          h4('지역별 누적 확진자 수',align="right"),
                          div (style = 'max-height : 200px; overflow-y : scroll;', plotlyOutput ( "region_plotly")),
                          h3('누적 확진자 수',align="right"),
                          h4(textOutput("kor_defCnt"),align="right"),
                          plotlyOutput("defCnt_plotly", width = 400, height = 200),
                          h3('누적 사망자 수',align="right"),
                          h4(textOutput("kor_deathCnt"),align="right"),
                          plotlyOutput('deathCnt_plotly', width = 400, height = 200)),
            absolutePanel(id = "controls2", class = 'panel panel-default', top = 200, left = 1350, bottom = 'auto', right = 'auto',
                            draggable = TRUE, height = "auto",
                          h2(textOutput("sido_name"),align="left"),
                          plotlyOutput('sido_plotly', width = 230, height = 300),
                          htmlOutput('sido_info'),
                          dateInput('region_date',
                                    label = h5("원하는 날짜를 입력하세요"),
                                    min = as.Date(first_date,"%Y-%m-%d"),
                                    max = as.Date(current_date-1,"%Y-%m-%d"),
                                    value = as.Date(current_date-1,"%Y-%m-%d"),
                                    language = 'ko'
                          ))
    ),
    tabItem(tabName = 'overseas',
      tags$style(type = "text/css", "#overseasMap {height: calc(100vh - 80px) !important;}"),
      leafletOutput("overseasMap", width = "100%", height = "100%"),
      absolutePanel(id = "controls", class = "panel panel-default",
                    top = 75, left = 65, width = 350, fixed=TRUE,
                    draggable = TRUE, height = "auto",
                    h2("세계 코로나 현황", align = "center"),
                    h3(textOutput("worldDefCnt"),align="right"),
                    h4(textOutput("worldDeathCnt"),align="right"),
                    h6(textOutput("clean_date_reactive"), align = "right"),
                    h4('확진자 수가 가장 많은 국가',align="right"),
                    plotlyOutput ( "overseas_plotly"),
                    dateInput('plot_date',
                              label = h5("원하는 날짜를 입력하세요"),
                              min = as.Date(first_date,"%Y-%m-%d"),
                              max = as.Date(current_date,"%Y-%m-%d"),
                              value = as.Date(current_date,"%Y-%m-%d"),
                              language = 'ko'
                    )
      )
    ),
    tabItem(tabName = 'data',
            sidebarLayout(
            sidebarPanel(pickerInput("first_select", "Level:",   
                                     choices = c("국내", "해외"),
                                     selected = c("국내"),
                                     multiple = FALSE),
                         pickerInput("second_select", "분류:",   
                                     choices = c("일일 데이터", "지역별", "연령별/성별"), 
                                     selected = c("일일 데이터"),
                                     multiple = FALSE),
                         conditionalPanel("input.second_select=='지역별'",
                                          pickerInput('region_select', "지역이름:",   
                                                      choices = total_region[1:19,'gubun'], multiple = TRUE)),
                         conditionalPanel("input.second_select=='연령별/성별'",
                                          pickerInput('age_select', "상세 분류:",   
                                                      choices = total_age[1:11,'gubun'], multiple = TRUE, selected = '0-9')),
                         pickerInput("detail_select", "상세 항목:",   
                                     choices = colnames(total),
                                     multiple = FALSE),
                         dateRangeInput(
                           inputId = "dates",
                           label = h3("데이터 날짜 범위"),
                           start = first_date,
                           end = current_date,
                           min = first_date,
                           max = current_date)
            ),
            mainPanel(
              tabsetPanel(
                tabPanel("Line", plotlyOutput('line_plotly')),
                tabPanel("Bar", h5("new")))
              )
            )
    )
  )
)
sidebar <- dashboardSidebar(collapsed = TRUE,
  sidebarMenu(
    menuItem("국내 코로나 현황", tabName = "total", icon = icon("map")),
    menuItem("해외 코로나 현황", tabName = "overseas", icon = icon("map")),
    menuItem("코로나 데이터 보드", tabName = "data", icon = icon("list-alt"))
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session){
  selected_region_kor <- '합계'
  
  temp_df <- total
  
  total_region_sub <- subset(total_region, stateDt == current_date)
  if(nrow(total_region_sub)==0){
    total_region_sub <- subset(total_region, stateDt == current_date-1)
  }
    
  shp1 <- readOGR(dsn = "C:/Users/jc/Documents",
                  layer = "CTPRVN",
                  stringsAsFactors = FALSE)
  shp1$CTP_KOR_NM2 <- c("서울", "부산", "대구", "인천", "광주", "대전", "울산", "세종", "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주")

  output$defCnt_plotly <- renderPlotly({
    plot_ly(data = total, x = ~stateDt, y = ~decideCnt, type = 'scatter', mode = 'lines')%>%
      config(displayModeBar = F)%>%
      layout(xaxis = list(title = "날짜"),
             yaxis = list (title = "누적 확진자 수"))
  })
  output$deathCnt_plotly <- renderPlotly({
    plot_ly(data = total, x = ~stateDt, y = ~deathCnt, type = 'scatter', mode = 'lines')%>%
      config(displayModeBar = F)%>%
      layout(xaxis = list(title = "날짜"),
             yaxis = list (title = "누적 사망자 수"))
  })
  output$region_plotly <- renderPlotly({
    plot_ly(data = total_region_sub, x = ~defCnt, y = ~reorder(gubun, defCnt), type = 'bar', orientation = 'h')%>%
      config(displayModeBar = F)%>%
      layout(xaxis = list(title = "확진자 수"),
             yaxis = list (title = ""))
  })
  output$kor_defCnt <- renderText({
    inc <- total[1,'decideCnt'] - total[2,'decideCnt']
    paste0('(+',comma(inc, format = "d") , ') ', comma(total[1,'decideCnt'], format = "d"), "명")
  })
  output$kor_deathCnt <- renderText({
    inc <- total[1,'deathCnt'] - total[2,'deathCnt']
    paste0('(+',comma(inc, format = "d") , ') ', comma(total[1,'deathCnt'], format = "d"), "명")
  })
  output$sido_plotly <- renderPlotly({
    selected_sido_plotly(total_region_sub, '합계')%>%
      layout(showlegend = F)
  })
  output$sido_name <- renderText({
    selected_region_kor
  })
  output$sido_info <- renderUI({
    temp <- total_region_sub[total_region_sub$gubun == selected_region_kor,]
    HTML(paste0(
      h4("누적 확진자 수 : ", comma(temp[1,'defCnt'], format = "d"), "명", align = 'right'),
      h4("누적 사망자 수 : ", comma(temp[1,'deathCnt'], format = "d"), "명", align = 'right'),
      h4("신규 확진자 수 : ", comma(temp[1,'incDec'], format = "d"), "명", align = 'right'),
      h4("누적 격리 해제 : ", comma(temp[1,'isolClearCnt'], format = "d"), "명", align = 'right'),
      h4("격리중 : ", comma(temp[1,'isolIngCnt'], format = "d"), "명", align = 'right'))
    )
  })
  output$clean_date_reactive <- renderText({
    format(as.POSIXct(input$plot_date),"%Y년 %B %d일")
  })
  output$worldDefCnt <- renderText({
    paste0(comma(sum(total_overseas$natDefCnt), format = "d"), "\n 확진자 수")
  })
  output$worldDeathCnt <- renderText({
    paste0(comma(sum(total_overseas$natDeathCnt), format = "d"), " 사망자 수")
  })
  output$overseas_plotly <- renderPlotly({
    many_def_plotly(total_overseas)
  })
  output$overseasMap <- renderLeaflet({ 
    overseas_map(total_overseas)
  })
  output$korMap <- renderLeaflet({ 
    korea_map(shp1, total_region_sub)
  })
  output$kor_date_reactive <- renderText({
    format(as.POSIXct(current_date),"%Y년 %B %d일")
  })
  output$line_plotly <- renderPlotly({
    plot_ly(data = temp_df, x = ~stateDt, y = ~temp_df[[input$detail_select]], type = 'scatter', mode = 'lines')%>%
      config(displayModeBar = F)%>%
      layout(xaxis = list(title = "날짜"),
             yaxis = list (title = input$detail_select))
  })
  observeEvent(input$plot_date,{
    total_overseas <- preproc_overseas(input$plot_date)
    output$overseasMap <- renderLeaflet({ 
      overseas_map(total_overseas)
    })
    output$worldDefCnt <- renderText({
      paste0(comma(sum(total_overseas$natDefCnt), format = "d"), " 확진자 수")
    })
    output$worldDeathCnt <- renderText({
      paste0(comma(sum(total_overseas$natDeathCnt), format = "d"), " 사망자 수")
    })
    output$overseas_plotly <- renderPlotly(
      many_def_plotly(total_overseas)
    )
  })
  observeEvent(input$region_date,{
    total_region_sub <- subset(total_region, stateDt == input$region_date)
    output$korMap <- renderLeaflet({ 
      korea_map(shp1, total_region_sub)
    })
    output$region_plotly <- renderPlotly({
      plot_ly(data = total_region_sub, x = ~defCnt, y = ~reorder(gubun, defCnt), type = 'bar', orientation = 'h')%>%
        config(displayModeBar = F)%>%
        layout(xaxis = list(title = "확진자 수"),
               yaxis = list (title = ""))
    })
    output$sido_plotly <- renderPlotly({
      selected_sido_plotly(total_region_sub, '합계')%>%
        layout(showlegend = F)
    })
    output$sido_name <- renderText({
      selected_region_kor
    })
    output$sido_info <- renderUI({
      temp <- total_region_sub[total_region_sub$gubun == selected_region_kor,]
      HTML(paste0(
        h4("누적 확진자 수 : ", comma(temp[1,'defCnt'], format = "d"), "명", align = 'right'),
        h4("누적 사망자 수 : ", comma(temp[1,'deathCnt'], format = "d"), "명", align = 'right'),
        h4("신규 확진자 수 : ", comma(temp[1,'incDec'], format = "d"), "명", align = 'right'),
        h4("누적 격리 해제 : ", comma(temp[1,'isolClearCnt'], format = "d"), "명", align = 'right'),
        h4("격리중 : ", comma(temp[1,'isolIngCnt'], format = "d"), "명", align = 'right'))
      )
    })
  })
  observeEvent(input$korMap_shape_click,{
    selected_region_kor <- input$korMap_shape_click$id
    
    output$sido_name <- renderText(
      selected_region_kor
    )
    output$sido_plotly <- renderPlotly({
      selected_sido_plotly(total_region_sub, selected_region_kor)%>%
        layout(showlegend = F)
    })
    output$sido_info <- renderUI({
      temp <- total_region_sub[total_region_sub$gubun == selected_region_kor,]
      HTML(paste0(
        h4("누적 확진자 수 : ", comma(temp[1,'defCnt'], format = "d"), "명", align = 'right'),
        h4("누적 사망자 수 : ", comma(temp[1,'deathCnt'], format = "d"), "명", align = 'right'),
        h4("신규 확진자 수 : ", comma(temp[1,'incDec'], format = "d"), "명", align = 'right'),
        h4("누적 격리 해제 : ", comma(temp[1,'isolClearCnt'], format = "d"), "명", align = 'right'),
        h4("격리중 : ", comma(temp[1,'isolIngCnt'], format = "d"), "명", align = 'right'))
      )
    })
  })
  observeEvent(input$first_select,{
    if(input$first_select == '국내'){
      updatePickerInput(session, "second_select", "분류:",   
                        choices = c("일일 데이터", "지역별", "연령별/성별"), 
                        selected = c("일일 데이터"))
      updatePickerInput(session, "third_select", "상세 항목:",   
                        choices = colnames(total))
    }
    if(input$first_select == '해외'){
      updatePickerInput(session, "second_select", "국가 이름:",   
                        choices = total_overseas[,'nationNm'])
      updatePickerInput(session, "third_select", "상세 항목:",   
                        choices = colnames(total_overseas))
    }
  })
  observeEvent(input$second_select,{
    if(input$second_select == '일일 데이터'){
      updatePickerInput(session, "detail_select", "상세 항목:", choices = colnames(total))
      temp_df <- total
      output$line_plotly <- renderPlotly({
        plot_ly(data = temp_df, x = ~stateDt, y = ~temp_df[[input$detail_select]], type = 'scatter', mode = 'lines')%>%
          config(displayModeBar = F)%>%
          layout(xaxis = list(title = "날짜"),
                 yaxis = list (title = input$detail_select))
      })
    }
    if(input$second_select == '지역별'){
      updatePickerInput(session, "detail_select", "상세항목:", choices = colnames(total_region_sub))
      temp_df <- total_region[total_region$gubun == '서울',]
      output$line_plotly <- renderPlotly({
        plot_ly(data = temp_df, x = ~stateDt, y = ~temp_df[[input$detail_select]], type = 'scatter', mode = 'lines')%>%
          config(displayModeBar = F)%>%
          layout(xaxis = list(title = "날짜"),
                 yaxis = list (title = input$detail_select))
      })
    }
    if(input$second_select == '연령별/성별'){
      updatePickerInput(session, "detail_select", "상세항목:", choices = colnames(total_age))
      temp_df <- total_age[total_age$gubun == '0-9',]
      output$line_plotly <- renderPlotly({
        plot_ly(data = temp_df, x = ~stateDt, y = ~temp_df[[input$detail_select]], type = 'scatter', mode = 'lines')%>%
          config(displayModeBar = F)%>%
          layout(xaxis = list(title = "날짜"),
                 yaxis = list (title = input$detail_select))
      })
    }
  })
  observeEvent(input$age_select,{
    temp_df <- subset(total_age, gubun %in% input$age_select)
    output$line_plotly <- renderPlotly({
      plot_ly(data = temp_df, x = ~stateDt, y = ~temp_df[[input$detail_select]], type = 'scatter', mode = 'lines', color = ~gubun)%>%
        config(displayModeBar = F)%>%
        layout(xaxis = list(title = "날짜"),
               yaxis = list (title = input$detail_select))
    })
  })
  observeEvent(input$region_select,{
    temp_df <- subset(total_region, gubun %in% input$region_select)
    output$line_plotly <- renderPlotly({
      plot_ly(data = temp_df, x = ~stateDt, y = ~temp_df[[input$detail_select]], type = 'scatter', mode = 'lines', color = ~gubun)%>%
        config(displayModeBar = F)%>%
        layout(xaxis = list(title = "날짜"),
               yaxis = list (title = input$detail_select))
    })
  })
  observeEvent(input$dates,{
    temp_df <- subset(total_age, stateDt>input$dates[1]&stateDt<input$dates[2])
    temp_df <- subset(temp_df, gubun %in% input$age_select)
    output$line_plotly <- renderPlotly({
      plot_ly(data = temp_df, x = ~stateDt, y = ~temp_df[[input$detail_select]], type = 'scatter', mode = 'lines', color = ~gubun)%>%
        config(displayModeBar = F)%>%
        layout(xaxis = list(title = "날짜"),
               yaxis = list (title = input$detail_select))
    })
  })
}
shinyApp(ui, server)
