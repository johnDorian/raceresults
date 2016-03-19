
# Load the ggplot2 package which provides
# the 'mpg' dataset.
#library(ggplot2)
#data <- read_csv("data/race_results.csv")
# Define a server for the Shiny app
shinyServer(function(input, output, session) {
 
 
  

    #output$table <- DT::renderDataTable(
    #  DT::datatable(data, escape = TRUE, selection = 'single'), server = TRUE
    #)
  
  output$text <- renderText({
    data$highlight = 0
    data <- filter_data(input, output,session, data)
    plt_data <- data
    plt_data <- plt_data %>% group_by(Race, Gen) %>% arrange(`Race Time`) %>% mutate(gender_place = 1:n())
    plt_data <- ungroup(plt_data)
    plt_data$time <- sapply(plt_data["Race Time"], function(x) period_to_seconds(hms(x))/3600)
    plt_data <- plt_data %>% 
      arrange(time) %>% 
      mutate(place = 1:n(), hms = as.POSIXct(`Race Time`, format = "%H:%M:%S"))
    overal_place <- int_to_ranking(plt_data$Position[plt_data$highlight == 1])
    gen_place <- int_to_ranking(plt_data$gender_place[plt_data$highlight == 1])
    
    
    
    gender_place <- int_to_ranking(plt_data$Position[plt_data$highlight == 1])
    
    HTML(paste0('<center><h2>', plt_data$Name[plt_data$highlight==1], ' ', plt_data$Surname[plt_data$highlight==1], '</h2><br>
    <span class="timer">', format(plt_data$hms[plt_data$highlight==1], "%H:%M:%S"), '</span><br><br><br>
                <p>Overall position = ',  overal_place, '<br>
                Gender position = ',gen_place  , '</p></center>'))
  })
  
    output$plot = renderPlot({
      
        ## to do: add a title for the race and year based on s. 
       # keep_index <- sapply(data["Race Time"], function(x) !str_detect(x, "TIME UNKNOWN|DNF|No chip read"))
         #plt_data <- data[keep_index,]
      data$highlight = 0
      data <- filter_data(input, output,session, data)
        plt_data <- data
        plt_data <- plt_data %>% group_by(Race, Gen) %>% arrange(`Race Time`) %>% mutate(gender_place = 1:n())
        plt_data <- ungroup(plt_data)
        plt_data$time <- sapply(plt_data["Race Time"], function(x) period_to_seconds(hms(x))/3600)
        

        
        
        plt_data <- plt_data %>% 
          arrange(time) %>% 
          mutate(place = 1:n(), hms = as.POSIXct(`Race Time`, format = "%H:%M:%S"))
        
        

        ggplot() + geom_point(aes(hms, place), plt_data, size = 0.5, colour = "red")  + 
          xlab("Finishing time") + ylab("Place") +
           theme_bw(20) + geom_point(aes(hms, place),  filter(plt_data, highlight == 1), size = 5, colour = "red") #+
#           ggtitle(plt_title$plt_title)
    }  
    )
    
    
    
    
    
  })



filter_data <- function(input, output,session, data){
  # extract the url search terms
  query <- parseQueryString(session$clientData$url_search)
  # define the required search terms
  required_pars <-  c("year", "race", "bibno")
  # Create a backup of the orginal data
  org_data <- data
  # check that all the search terms are present. If not return the unmodified data frame. 
  if(all(names(query) %in% required_pars) & length(names(query))==3){
    # In here - therefore the search terms were correct. 
    
    # First up filter by year
    valuetoupdate = query['year']
    index <- data$Year == as.numeric(valuetoupdate)
    if(sum(index) > 0){
      data <- data[index,]
    } else {
      return(org_data)
    }
    # now filter for the race name. 
    valuetoupdate = query['race']
    index <- data$Race == valuetoupdate
    if(sum(index) > 0){
      data <- data[index,]
    } else {
      return(org_data)
    }
    # Now add the desired bib number to the top of the data frame. 
    valuetoupdate = query['bibno']
    index <- as.vector(data[,"BIB Number"] == as.numeric(valuetoupdate))
    if(sum(index) == 1){
      data$highlight = as.numeric(index)
    } else {
      return(org_data)
    }
      
    return(data)
    
    
  } else {
    return(data)
  }
  
  # filter for the race name
  
  # filter for the year
  
  # put the desired bib no. at the top of the dataframe.
  

}




int_to_ranking <- function(i){
  
  whereinf <- is.infinite(i)
  wherena <- is.na(i)
  whereneg <- sign(i) == -1L
  i <- suppressWarnings(as.integer(i))
  if(any(is.na(i) & (!whereinf) & (!wherena))) stop('could not convert some inputs to integer')
  
  last_digit <- as.numeric(substring(i, nchar(i)))
  ending <- sapply(last_digit + 1, switch, 'th', 'st', 'nd', 'rd', 'th', 'th', 'th', 'th', 'th', 'th')
  second_last_digit <- as.numeric(substring(i, nchar(i) - 1, nchar(i) - 1))
  ending[second_last_digit == 1L] <- 'th'
  out <- paste(i, ending, sep = '')
  
  out[whereinf] <- 'infinitieth'
  out[whereinf & whereneg] <- '-infinitieth'
  out[wherena] <- 'missingith'
  
  return(out)
}
  