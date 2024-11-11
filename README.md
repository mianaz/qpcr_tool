# A Shiny App for quick analysis and plotting of qPCR fold changes

<img width="1478" alt="image" src="https://github.com/user-attachments/assets/fdc997d1-c415-42b5-9d59-7648cae2da92">

## Get Started
To run the App locally, download ui.R and server.R, and click on "Run App" on the upper right corner.
<img width="219" alt="image" src="https://github.com/user-attachments/assets/a19578ac-be2a-439a-8114-15ec362f34a7">

To deploy to shinyapps.io:
```r
library(rsconnect)
rsconnect::setAccountInfo(name='jdoe', token='XXXXXXXX', secret='XXXXXXXX') # change to your own token and secret
rsconnect::deployApp("path/to/app/")
```
An online version has been deployed [here](https://miana-zeng.shinyapps.io/qpcrtool/). Note that the app might be slow due to traffic.
