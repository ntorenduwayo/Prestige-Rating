# Prestige-Rating
# Perform Multiple Linear Regression
## Project Overview
Prestige is a widespread respect and admiration felt for someone or something based on a perception of their achievements or quality. Hence, this project focused on the impact of the income, education, and women on the prestige. To understand these relationships, I applied a multiple linear regression analysis via R, and Jupyter Notebook. </br>
I used the Prestige dataset that consists of 102 observations with 6 variables. This dataset is publicly available and can be accessed from the following [Link](http://socserv.socsci.mcmaster.ca/jfox/books/Companion/data/Prestige.txt). </br>

The description of the dataset variables are as follows:
- education: 
The average number of years of education for occupational incumbents.
- income: 
The average income of occupational incumbents, in dollars.
- women: 
The percentage of women in the occupation.
- prestige:
The average prestige rating for the occupation.
- census: 
The code of the occupation used in the survey.
- type: 
Professional and managerial(prof), white collar(wc), blue collar(bc), or missing (NA) (Fox and Weisberg 2011) </br>
## Results
### Data Exploration 
#### Table 1: Data Summary
![Data Exploration](https://user-images.githubusercontent.com/34750363/178194797-8625ebb6-7169-4ec0-846d-a8adb1cfebaf.png)

#### Graph 1: Data Distribution
![R graphs](https://user-images.githubusercontent.com/34750363/178195082-8a50ac77-4ec6-4405-bc4b-18a5f5d3a342.png)

#### Graph 3: Correlogran plots
![correlogram](https://user-images.githubusercontent.com/34750363/178195315-96c089e2-56fa-41ed-9199-b06e723c729d.jpg)

#### Table 2: Correlation Heatmap
![R heatmap](https://user-images.githubusercontent.com/34750363/178195669-bd468293-e3a9-47ca-828c-2f2a389671f2.png)

#### Table 3: Multiple Linear Regression Output (With regressors centered to their mean)
![Multiple Regression Output 3](https://user-images.githubusercontent.com/34750363/178196174-76666531-5444-4893-a4b9-ba67291361cc.png)

#### Graph 4: Model Diagnostic Plots 
![R diagnostic plots Model3](https://user-images.githubusercontent.com/34750363/178196553-61350634-18b6-4a31-8ead-2121c59fc90d.png)

#### Graph 5: 3D Model Visualization
![Multiple Regresson 3D Plot1](https://user-images.githubusercontent.com/34750363/178196736-ea9e5468-083a-45b5-820e-61023192f940.png)




