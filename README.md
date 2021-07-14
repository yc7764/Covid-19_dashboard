# Covid-19_dashboard
코로나19 바이러스 관련 통계 데이터를 시각화하여 실시간 현황 파악, 다양한 관점에서 데이터를 관찰할 수 있는 웹 애플리케이션을 개발
통계적인 계산과 데이터 분석에 특화된 프로그래밍 언어인 R언어로 작성하였으며, 아래의 패키지를 사용
 - shiny, shinydashboard, shinywidgets
 - xml
 - data.table, formattable
 - maps, maptools, geojsonio, rgdal
 - leaflet
 - ggplot2, plotly
 - sf, sp
 - dplyr
# 시스템 구성도
애플리케이션은 XML to dataframe converter, server, UI 총 세가지로 구성되어있다

XML to dataframe converter
--------------------------
 - XML형식의 공공데이터를 받아와 dataframe형식으로 데이터를 변환
 - 공공데이터포털에 xml형태로 실시간 현황 데이터를 요청
    - 보건복지부_코로나19 감염_현황
    - 보건복지부_코로나19 연령별 성별감염_현황
    - 보건복지부_코로나19 시 도발생_현황
    - 보건복지부_코로나19 해외발생_현황
 - 데이터의 결측값, 이상값 검사
 - XML 트리의 노드를 순회하며 데이터를 읽어와 data frame 형태로 변환
![1](https://user-images.githubusercontent.com/59434021/125584220-9cc32038-3379-4e33-acf3-ad72ff22aa90.PNG)

Server
------
 - 변환된 dataframe형식의 데이터를 분석하여 그래프와 분석결과를 UI에 rendering
 - UI로부터 입력을 받아 데이터를 재분석하고 분석결과를 rendering

UI
--
UI는 해외 코로나 현황판, 국내 코로나 현황판, 코로나 데이터보드 세가지 카테고리로 선택할 수 있다.
 - 해외 코로나 현황판  
   ![image](https://user-images.githubusercontent.com/59434021/125585132-3042b52a-1093-48c5-955e-a89d1b098da5.png)  
    - 국가별로 데이터 내용에 따라 다른 색으로 표시했으며, 해당 색의 데이터 단위는 지도의 가장 오른쪽 하단에 표시
    - 마우스 커서를 특정 국가를 지정할 경우 해당 국가의 이름과 데이터를 텍스트로 출력
   ![image](https://user-images.githubusercontent.com/59434021/125585523-214e4195-385b-4ce8-a621-1191b6f3f013.png)  
    - 지도의 우측 상단에서 보고자하는 데이터를 선택  
   ![image](https://user-images.githubusercontent.com/59434021/125586164-28af3b0e-8a31-45e3-be83-88d469ea6101.png)  
    - 왼쪽의 패널을 이용하여 원하는 날짜를 선택하여 해당 날짜의 데이터로 변경할 수 있고, 날짜가 변경되면 지도와 그래프가 변경  
    ![image](https://user-images.githubusercontent.com/59434021/125586738-22e6ba21-7334-4ff8-a82f-0c0095a91969.png)  
 - 국내 코로나 현황판
   ![image](https://user-images.githubusercontent.com/59434021/125587195-efa797cd-a11b-463c-a2de-73bf2de710f8.png)  
   - 국내의 코로나 데이터만을 출력해주며 막대와 선, 원을 이용하여 그래프로 출력
   - 특정 지역을 클릭하면 해당 지역의 데이터를 오른쪽 패널에 출력  
   ![image](https://user-images.githubusercontent.com/59434021/125587578-f16a16cb-0c42-42f1-9ee9-41cc95f86d85.png)  
 - 코로나 데이터보드  
 ![image](https://user-images.githubusercontent.com/59434021/125588056-85cc878a-b6dd-4cc6-98c2-89cbfc2b25cb.png)  
    - 다양한 측면에서 데이터를 볼 수 있도록 사용자가 원하는 데이터를 이용해 그래프를 출력
    - 데이터의 날짜와 종류를 사용자가 선택할 수 있고 그래프의 일정 부분을 확대 또는 축소하는 등 그래프 조정도 가능
      ![image](https://user-images.githubusercontent.com/59434021/125588600-123458df-dfd2-4c26-bb07-15a2949e69c4.png)
