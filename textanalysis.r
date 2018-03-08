library(shiny)
library(gutenbergr)
library(dplyr)
library(stringr)
library(syuzhet)
 
fluidPage(theme = "bootstrap.css",
 
  titlePanel(h1("Sentimental Tweets from Project Gutenberg Books", align="center"),
             windowTitle="Tweets from Project Gutenberg"),
  sidebarLayout(
      sidebarPanel(
 
        selectInput(
          'book', 'Choose a book:', 
          multiple=FALSE,
          selectize = TRUE,
          choices=c("Enter some words of title or author" = "", gutenberg_works$searchstr)
          ),
         
        radioButtons(inputId = "sent",
             label = "Choose sentiment:",
             choices = c("Dark"="1", "Bright"="20"),
             selected="1",
             inline=TRUE),
         
        radioButtons(inputId = "meth",
                    label = "Choose a method to measure sentiment:",
                    choices = c("syuzhet", "bing", "afinn", "nrc"),
                    selected="syuzhet",
                    inline=TRUE),
         
        radioButtons(inputId = "char",
                     label = "Number of characters (max):",
                     choices = list("140", "280"),
                     inline=TRUE),
 
        checkboxInput(inputId = "auth",
                      label = "Add author",
                      value=FALSE),
         
        checkboxInput(inputId = "titl",
                      label = "Add title",
                      value=FALSE),
         
        checkboxInput(inputId = "post",
                      label="Add link to post (thanks!)",
                      value=TRUE),
         
        textInput(inputId = "adds",
                  label="Something else?",
                  placeholder="Maybe a #hastag?"),
         
        actionButton('do','Go!', 
                     class="btn btn-success action-button", 
                     css.class="btn btn-success")
  ),
   
 
   
  mainPanel(
     tags$br(),
     p("First of all, choose a book entering some keywords of its 
        title or author and doing dropdown navigation. Books are 
        downloaded from Project Gutenberg. You can browse the complete 
        catalog", tags$a(href = "https://www.gutenberg.org/catalog/", "here.")),
 
     p("After that, choose the sentiment of tweets you want to generate. 
        There are four possible methods than can return slightly different results. 
        All of them assess the sentiment of each word of a sentence and sum up the 
        result to give a scoring for it. The more negative is this scoring, 
        the", em("darker") ,"is the sentiment. The more positive, the ", em("brighter."), 
        " You can find a nice explanation of these techniques",
        tags$a(href = "http://www.matthewjockers.net/2017/01/12/resurrecting/", "here.")),
         
        p("Next parameters are easy: you can add the title and author of the book where 
          sentence is extracted as well as a link to my blog and any other string you want. 
          Clicking on the lower button you will get after some seconds a tweet below. 
          Click as many times you want until you like the result."),
      
     p("Finally, copy, paste and tweet. ",strong("Enjoy it!")),
     tags$br(),
     tags$blockquote(textOutput("tweet1")),
     tags$br()
 
)))

function(input, output) {
   
  values <- reactiveValues(default = 0)
   
  observeEvent(input$do,{
    values$default <- 1
  })
 
  book <- eventReactive(input$do, {
    GetTweet(input$book, input$meth, input$sent, input$char,
             input$auth, input$titl, input$post, input$adds)
  })
   
  output$tweet1 <- renderText({
    if(values$default == 0){
      "Your tweet will appear here ..."
    }
    else{
      book()
    }
  })
}

 
x <- tempdir() # Read the Project Gutenberg catalog and filter english works. I also create a column with # title and author to make searchings gutenberg_metadata %>%
  filter(has_text, language=="en", gutenberg_id>0, !is.na(author)) %>%
  mutate(searchstr=ifelse(is.na(author), title, paste(title, author, sep= " - "))) %>%
  mutate(searchstr=str_replace_all(searchstr, "[\r\n]" , "")) %>%
  group_by(searchstr) %>%
  summarize(gutenberg_id=min(gutenberg_id)) %>%
  ungroup() %>%
  na.omit() %>%
  filter(str_length(searchstr)<100)-> gutenberg_works
 
# This function generates a tweet according the UI settings (book, method, sentiment and
# number of characters). It also appends some optional strings at the end
GetTweet = function (string, method, sentim, characters,
                     author, title, link, hastag)
 {
  # Obtain gutenberg_id from book 
  gutenberg_works %>%
     filter(searchstr == string) %>%
     select(gutenberg_id) %>% .$gutenberg_id -> result
   
  # Download text, divide into sentences and score sentiment. Save results to do it once and
  # optimize performance
  if(!file.exists(paste0(x,"/","book",result,"_",method,".RDS")))
  {
    book=gutenberg_download(result)
    book[,2] %>% 
      as.data.frame() %>% 
      .$text %>% 
      paste(collapse=" ") -> text
     
    sentences_v <- get_sentences(text)
    sentiment_v <- get_sentiment(sentences_v, method=method) data.frame(sentence=sentences_v, sentiment=sentiment_v) %>% 
      mutate(length=str_length(sentence)) -> results
    saveRDS(results, paste0(x,"/","book",result,"_",method,".RDS"))
  }
    
  results=readRDS(paste0(x,"/","book",result,"_",method,".RDS"))
  book_info=gutenberg_metadata %>% filter(gutenberg_id==result)
   
  # Paste optional strings to append at the end
  post=""
  if (title)  post=paste("-", book_info[,"title"], post, sep=" ")
  if (author) post=paste0(post, " (", str_trim(book_info[,"author"]), ")")
  if (link)   post=paste(post, "https://wp.me/p7VZWY-16S", sep=" ")
  post=paste(post, hastag, sep=" ")
  length_post=nchar(post)
 
  # Calculate 5% quantiles
  results %>% 
    filter(length<=(as.numeric(characters)-length_post)) %>%
     mutate(sentiment=jitter(sentiment)) %>% 
     mutate(group = cut(sentiment, 
                        include.lowest = FALSE,
                        labels = FALSE,
                        breaks = quantile(sentiment, probs = seq(0, 1, 0.05)))) -> results
    
  # Obtain a sample sentence according sentiment and append optional string to create tweet
  results %>% 
     filter(group==as.numeric(sentim)) %>% 
     sample_n(1) %>% 
     select(sentence) %>% 
     .$sentence %>% 
     as.character() %>% 
     str_replace_all("[.]", "") %>% 
    paste(post, sep=" ") -> tweet
   
  return(tweet)
 
 }
