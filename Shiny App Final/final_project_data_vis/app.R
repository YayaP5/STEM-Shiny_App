library(shiny)
library(maps)
library(tidyverse)
library(tidymodels)
library(tools) 
library(statebins)
library(scales)
library(ggplot2)


# Load data
data_jobs <- read_csv("data/data_jobs_cleaned.csv") %>%
  mutate(company = str_to_title(company)) %>%
  mutate(title = str_to_title(title)) %>%
   group_by(company) %>%
   filter(n() >= 100) %>%
   ungroup()

#TODO: make this capital somehow without the graph disappearing
company_list <- sort(unique(data_jobs$company)) 


# User interface ----
ui <- fluidPage(
  titlePanel("STEM Jobs Data Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create graph with Information Within the Selected Company"),
      selectInput("company", label = "Choose a Company to Display:", 
                  choices = company_list,
                  selected = "amazon"),
      
      checkboxGroupInput("titles", "Select Title From Company to Compare:",
                         #could also do a set selected equal to my choices that would be a unique list (like comapny_list) that has the specific job titles from the company 
                         choices = list("Data Scientist",
                                        "Software Engineer",
                                        "Software Engineering Manager",
                                        "Hardware Engineer",
                                        "Mechanical Engineer",
                                        "Product Designer",
                                        "Product Manager",
                                        "Human Resources",
                                        "Management Consultant",
                                        "Recruiter",
                                        "Sales",
                                        "Marketing",
                                        "Solution Architect",
                                        "Technical Program Manager",
                                        "Business Analyst")),
      
      
      
      
      radioButtons("var",
                   label = "Select Legend Variable:", 
                   choices = c("Total Yearly Compensation",
                               "Gender" ,
                               "Race",
                               "Education"),
                   selected = "Total Yearly Compensation")
    
    ),
    
     
    
      
    mainPanel(
      
      plotOutput("distPlot"),
      
      helpText(p("This data was taken from", 
                 a(href = "https://www.kaggle.com/datasets/jackogozaly/data-science-and-stem-salaries.", "Kaggle"),
                 "and modified/cleaned by myself to use in this shiny app.")),
      
      p('This shiny app was created to allow you to get a count of the gender, race, education, and total compensation of certain job titles within a few popular companies in the world.
        This dataset contains 62,642 salary records from top companies. There are 29 variables in this data set including company, level, title, location, race, gender, and etc. 
        It was created to increase salary/job transparancy within the tech field and help workers fight for better pay/benefits in their jobs. This shiny app provides a simple depiction of a few of those variables, and allows you to seperate and add on jobs available at each company to demonstrate the different distributions 
        in race, education, gender, and totalyearlycompensation. 
        Those variables are thought to sometimes impact different individuals experiences and opinions regarding a company. It could help you barter with your manager for a higher salary if most of the software engineers in your company are getting paid more than you.
        The racial and gender distribution in a company can impact how safe, represented, and secure you feel as well. You could see if your highest form of education might impact the salary margins in that company. These variables are a basis for analysis and comparison. 
        Try selecting a company of your choosing and youll be prompted with a list of selectable job titles held by the employees in that company that completed the survey.
        Dont worry if you add job titles that have no responses in that company, as it wont impact the results. Proceed to deselect and engage with the different titles listed. 
        See how the graph changes as you change the different variable to present.
        The widgets choosen were picked to optimize its function. Using a check box options for the titles allows you to pick and choose what jobs you want to see, without only limiting you to one option.
        The select widget for the company (which was also alphabetized) allows users to quickly search for a company if they have one in mind.'),
      
      helpText("Shiny App Creator: Yaelle DC Pierre")
    )
  )
)
# Server logic ----

server <- function(input, output, session){
  # makes a dataset filtering to selective company
  chosen_company <- reactive({ 
    data_jobs[which(data_jobs$company == input$company), ]
  })
  
  #creating new plotting data that has the company and titles they pick
  #plot_data <- reactive({ 
    #chosen_company() %>%
      #filter(title %in% input$titles)
  #})

  
  #output$title_selection <- renderUI({
  #})
  
  # ToDo- if I want I can try to change / fill by the company name
  ##### if I wanted unique colors, I would need a LIST of unique colors and then filter that list to what the users have checked. Then put that in a scale fill manual in the PLOT. 
  # color_titles <- reactive({
  #   switch(chosen_company()$title,
  #          "Data Scientist" = "deepskyblue4",
  #           "Software Engineer" = "aquamarine4",
  #           "Software Engineering Manager" = "deeppink4",
  #           "Hardware Engineer" = "darkslateblue",
  #           "Mechanical Engineer" = "cyan2",
  #           "Product Designer" = "brown3",
  #           "Product Manager" = "aquamarine2",
  #           "Human Resources" = "darksalmon",
  #           "Management Consultant" = "cadetblue",
  #           "Recruiter" = "blueviolet",
  #           "Sales" = "deeppink3",
  #           "Marketing" = "antiquewhite3",
  #           "Solution Architect" = "aliceblue",
  #           "Technical Program Manager" = "chocolate3",
  #           "Business Analyst" = "darkslategray3")
  # })
  # need this to be input$company because chosen company isnt being created until after so it cant find chosen_company
  # look up something like: reactive valve is being treated as a function
  observeEvent(input$company, {
    # taking data and getting unique options of title. Then setting choices to those unique options 
    # dtaa is reactive
    
    #filtering the data into two different sets because now we dont need to have a reactive valve
    x <- data_jobs[which(data_jobs$company == input$company), ] %>%
      pull(title)

    #x <- chosen_company() %>%
      # makes a vector of the titles in the chosen company 
      #pull(title)
    
    updateCheckboxGroupInput(session, "titles",
                             #unique because you dont want to see the choices over and over and you are pulling the unique x variables from the chosen company
                             choices = unique(x)
    )
    })
  
  #TODO: maybe remove the choices that arent a possibility in the company(ReactiveUI) 
  output$distPlot <- renderPlot({
    # render and reactive are similar in terms of them changing so a render is already reactive
    plot_data <- chosen_company() %>%
      filter(title %in% input$titles)
    
    var_hist <- switch(
      input$var,
      "Total Yearly Compensation" = "totalyearlycompensation",
      "Gender" = "gender",
      "Race" = "Race",
      "Education" = "Education"
    )
    
    
    # x label
    x_lab = "Total Yearly Compensation"
    x_lab <- switch(input$var,
                    "Total Yearly Compensation" = "Total Compensation in USD",
                    "Gender" = "Gender",
                    "Race" = "Races",
                    "Education" = "Education",
    )
    

  # because we are giving it a string for the var_hist 
    # now if you add a y, you expect everything to have strings 
    #TODO figure out how to put bar count on it with a UI statement maybe?
    if(var_hist == "totalyearlycompensation"){
      final_plot <- ggplot(plot_data, aes_string(x = var_hist)) +
        geom_histogram(
        color = "black",
        aes(fill = title)) +
        xlab(x_lab) +
        ylab('Count') +
        theme_minimal() +
        labs(title = paste0("Company Distrubution of ", input$var, " for the Company ",input$company)) +
        theme(legend.position = "top",
              legend.direction = "horizontal",
              axis.text.x = element_text(size = 15, angle = 20))
      
    } else{
      final_plot <- ggplot(plot_data, aes_string(x = var_hist)) +
      geom_bar(
        color = "black",
        aes(fill = title)) +
        xlab(x_lab) +
        ylab('Count') +
        theme_minimal() +
        labs(title = paste0("Company Distrubution of ", input$var, " for the Company ",input$company)) +
        theme(legend.position = "top",
              legend.direction = "horizontal",
              axis.text.x = element_text(size = 15, angle = 20))
    }
  final_plot  
  })
}
# Run app ----
shinyApp(ui = ui, server = server)
