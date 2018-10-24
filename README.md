# foldercloud
An R function to find all references in a folder and create a wordcloud.
To install run the following code in R:<br/>
```
#install packages
install.packages("devtools")
devtools::install_github("JConigrave/foldercloud")

#load library
library(foldercloud)

#set working directory
setwd("C:/Users/username/Desktop")

#run function on the path of a folder containing pdfs (or one with pdfs in subfolders)
foldercloud("C:/Users/username/Desktop/folder with pdfs")
```

