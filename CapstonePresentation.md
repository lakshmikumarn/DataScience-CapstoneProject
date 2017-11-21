Capstone Project Presentation
========================================================
author: Laks
date: 11/20/2017
autosize: true

<h2>  Text Mining and Prediction </h2> 

<h4>A 'Shiny App' based project is all about Text Mining using the data set provided and predict next word for given set of words. It uses R package "*tm*" to perform various text mining operations. </h4>

<small> Application URL: 
https://laksnarayanan.shinyapps.io/Capstone/ 

GitHubURL: 
</small>

<style>
body {
    background-color: #FFB890;
}
</style>

About the App
========================================================
<small>
- Application (app) is built using Shinyapps and launched in shinyapps.io for public access
- The app is backed by ngrams dictionary (with about **69 million** records of quadgrams) built using the 3 data sets
- Uses in-memory lightweight RDBMS database **SQLLite** for querying the ngram dictionary
- At the heart of it is the *ngram* dictionary creation - creating series of words combined in various meaningful permutations

***

![Application Screenshot](AppScreen1.png)


</small>

Application Algorithm & Insights
========================================================
<small>
- Ngram dictionary was built one-time in SQLLite in **batch mode** to avoid choking in CPU and Memory
- For each iteration, 1000 lines (configurable) from 3 files are extracted sequentially, performed cleanup on text document, created ngrams and finally, flushed into SQLLite table. This is performed until we exhaust all lines in each of three files
- A **recovery pointer** is tracked & persisted to pickup the processing of files from where it is left in case of crash
- To have a balance, we created **quadgrams (4 words)** versus too higher or too lower numbered-grams
- When set of words are entered in the text box and clicked "predict next word' button, the server uses SQLLite to query the ngrams dictionary for matching words, sort **top 10** (configurable) matches by higher frequencies and  present list of next word(s), i.e. next to the last word of given sentence
</small>

Application in Action
========================================================
![Application Screenshot](AppScreen2.png)
![Application Screenshot](AppScreen3.png)
***
<small>
- Instead of presenting **only one** next word, as it typically shown in mobile phones, we present **list** of next words by the order of number of occurrences in the text corpus
- If there is no next word, it could be either the ngram pruning took it out or the system has to learn refining the dictionary. 
- Shinyapps.io is not guaranteeing persistent storage, so there is a risk of losing the 2GB dictionary needing to republish
</small>

Future Explorations & Enhancements
========================================================
- Ability for in-built **phonetic search** on ngrams dictionary 
- Ability to perform **sentiment analysis** feature
- Ability to have the system **learn & update the ngrams dictionary** when there is no matching next word
- Ability to build a higher order ngram and use it for the lower order grams search
- Ability to host the dictionary in **cloud (AWS etc.)** and integrate with Shiny server through APIs - to mitigate the risk of losing the file
- Give **Auto-complete** like feature while typing (of course, high performance servers needed)
- Support for other **languages/locale**


