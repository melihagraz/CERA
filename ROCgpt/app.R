library(shiny)
library(cutpointr)
library(pROC)
library(readxl)
library(dplyr)
library(ggplot2)
library(devtools)
library(gptchatteR)
library(xml2)
library(jsonlite)
library(httr)
library(shinycssloaders)
library(shinyjs)

# Define UI
chatter.auth("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")

# Define UI
ui <- fluidPage(
  
  # Load shinyjs
  
  # Add custom CSS for the loader
  tags$head(tags$style(HTML(".container-fluid { background-color: #f2f2f2; }"))),
  
  # Add custom CSS for the active tab panel
  tags$head(tags$style(HTML(".nav-tabs > .active > a, .nav-tabs > .active > a:hover, .nav-tabs > .active > a:focus {background-color: #007bff; color: #fff;}"))),
  
  # App 
  # App title
  titlePanel("CERA:ChatGPT-Enhanced ROC Analysis (ver.1.0)"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Select data source
      radioButtons("datasource", "Data Source:",
                   choices = c("Example Data", "Upload Data"),
                   selected = "Example Data"),
      
      # Example data selection
      conditionalPanel(condition = "input.datasource == 'Example Data'",
                       selectInput("example_data", "Select Example Data:",
                                   choices = c("aSAH", "NAFLD"))
      ),
      
      # Upload data selection
      conditionalPanel(condition = "input.datasource == 'Upload Data'",
                       fileInput("datafile", "Data Upload",
                                 accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                       tags$p(tags$b("Note: "), "Data should be in a comma-separated format with the first row serving as the header, containing variable names.", style = "color: gray;")
      ),
      
      # Select predictor variable
      selectInput("xvar", "Select marker", choices = NULL),
      
      # Select outcome variable
      selectInput("yvar", "Select outcome variable", choices = NULL),
      
      # Select method
      selectInput("method", "Select method", 
                  choices = c("youden_topleft", "cutoff", "maximized", "min_pvalue_approach")),
      
      # Select index (only visible if method is "best")
      conditionalPanel(condition = "input.method == 'youden_topleft'",
                       selectInput("index", "Select index", 
                                   choices = c("youden", "closest.topleft"))
      ),
      
      # Select cutoff value (only visible if method is "cutoff")
      conditionalPanel(condition = "input.method == 'cutoff'",
                       numericInput("cutoff", "Enter cutoff value", value = 0.5)
      ),
      
      # Select constrain metric (only visible if method is "maximized")
      # Select min_pvalue_approach method
      conditionalPanel(condition = "input.method == 'min_pvalue_approach'",
                       selectInput("min_pvalue_method", "Select min_pvalue_approach method", 
                                   choices = c("min_pvalue", "bonferroni"))
      ),
      
      actionButton("run_analysis", "Run analysis"),
      
      br(),
      
      shinycssloaders::withSpinner(downloadButton("download_report", "Download Report"),
                                   type = 1, color = "#0dc5c1"),
      tags$p("Note: Report generation may take some time depending on ChatGPT API connection.", style = "color: red;"),
      
      br(),
      
      tags$h3("CERA"),
      tags$p(
        style = "font-weight: bold; color: #0dc5c1;",
        "Send your feedbacks: ",
        tags$a("melih.agraz@giresun.edu.tr", href = "mailto:melih.agraz@giresun.edu.tr")
      )
      
    ),
    
    # Main panel
    mainPanel(
      tabsetPanel(
        tabPanel("Introduction",
                 tags$img(src = "flw23.png", height = "300px", width = "950px", deleteFile = FALSE),
                 tags$p("Workflow of the CERA Web Tool for ROC Analysis.")
        ),
        tabPanel("ROC Plot", plotOutput("roc_plot")),
        tabPanel("Table", tableOutput("roc_table")),
        tabPanel("Data Preview", 
                 tableOutput("data_preview"),
                 textOutput("between_plots3"), 
                 tags$p("Number of Observations: ", textOutput("num_observations")),
                 textOutput("between_plots4"), 
                 tags$p("Number of Labels: ", textOutput("num_labels")),
                 textOutput("between_plots5"), 
                 tags$p("Percentage of Missing Data (%): ", textOutput("num_missing")),
                 textOutput("between_plots6")
        ),
        
        tabPanel("Distribution", 
                 plotOutput("dist_plot"),
                 tableOutput("var_summary_table")),
        tabPanel("Class Distributions", 
                 textOutput("between_plots"), 
                 plotOutput("cl_dist_plot"),
                 textOutput("between_plots2"), 
                 plotOutput("extra_plot")),
        tabPanel("Description", verbatimTextOutput("sales_text"),
                 textOutput("between_plots33"))
      )
    )
  )
)
      

# Define server
server <- function(input, output, session) {
  source("rocCov4.R")
  
  
  output$download_in_progress <- reactiveVal(FALSE)
  outputOptions(output, "download_in_progress", suspendWhenHidden = FALSE)
  
  example_data <- reactive({
    if (input$datasource == "Example Data") {
      if (input$example_data == "aSAH") {
        # Load example data 1
        read.csv("dataExample.csv")
        # e.g., read.csv("example_data1.csv")
      } else if (input$example_data == "NAFLD") {
        # Load example data 2
        read.csv("NAFLD.csv")
      }
    } else {
      NULL
    }
  })
  uploaded_data <- reactive({
    req(input$datafile)
    if (tools::file_ext(input$datafile$name) == "csv") {
      read.csv(input$datafile$datapath)
    } else if (tools::file_ext(input$datafile$name) %in% c("xls", "xlsx")) {
      read_excel(input$datafile$datapath, sheet = 1)
    }
  })
  
  data <- reactive({
    if (input$datasource == "Example Data") {
      example_data()
    } else {
      uploaded_data()
    }
  })
  
  data <- reactive({
    req(input$datafile)
    if(tools::file_ext(input$datafile$name) == "csv") {
      read.csv(input$datafile$datapath)
    } else if(tools::file_ext(input$datafile$name) %in% c("xls", "xlsx")) {
      read_excel(input$datafile$datapath, sheet = 1)
    }
  })
  download_in_progress <- reactiveVal(FALSE)
  
  data <- reactive({
    if (input$datasource == "Example Data") {
      example_data()
    } else {
      uploaded_data()
    }
  })
  
  chatgpt_interpretation <- function(res) {
    roc_table <- generate_roc_table(res)
    
    dt <- data.frame("threshold" = roc_table[1],
                     "sensitivity" = roc_table[2],
                     "specificity" = roc_table[3],
                     "ppv" = roc_table[4],
                     "npv" = roc_table[5],
                     "accuracy" = roc_table[6])
    
    # Concatenate the strings without spaces or newline characters
    kv <- paste0("threshold: ", roc_table[1], ", sensitivity: ", roc_table[2], ", specificity: ", roc_table[3], ", accuracy: ", roc_table[6])
    
    question <- paste0("Can you interpret the results in the ROC biomarker analysis for the following data: ", kv)
    answ_start <- "Table 1 shows the performance measures of the ROC analysis."
    chatter.create(max_tokens = 1000)
    response <- chatter.chat(question, return_response = TRUE)
    placeholder <- response$choices$text
    
    # Remove newline characters from the generated response
    placeholder_clean <- gsub("\n", " ", placeholder)
    
    res_fin <- paste(answ_start, placeholder_clean, sep = " ")
    return(res_fin)
  }
  

  chatgpt_interpretationPL <- function(res) {
    roc_table <- generate_roc_tablePL(res)
    
    # Remove newline characters from roc_table
    roc_table_clean <- gsub("\n", " ", roc_table)
    
    question <- paste0("I want you to comment in English. Based on the AUC I will give, I want you to comment in English as if you were seeing an ROC plot. You're going to start like this. AUC is: ", roc_table_clean)
    
    answ_start <- "Figure 3 represents ROC plot."
    chatter.create(max_tokens = 1000)
    response <- chatter.chat(question, return_response = TRUE)
    placeholder <- response$choices$text
    
    res_fin <- paste(answ_start, gsub("\n", "", placeholder), sep = "")
    return(res_fin)
  }
  
  
  

  observe({
    req(data())
    updateSelectInput(session, "xvar", "Select marker", choices = names(data()))
    updateSelectInput(session, "yvar", "Select outcome variable", choices = names(data()))
  })
  
  pred_dis_inter <- function(data, x_var, y_var) {
    # Create a local copy of the data and convert x_var to a factor
    local_data <- data
    local_data$x <- local_data[[x_var]]
    local_data$y <- local_data[[y_var]]
    local_data$y <- as.factor(local_data$y)
    
    ggplot(local_data, aes_string(x = "y", y = "x")) + 
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(width = 0.2, size = 1, color = "black") +
      labs(title =  "",
           x = y_var,
           y = x_var) +
      theme_minimal()
  }
  
  chatgpt_interpret_dist_pred <- function(data, x_var) {
    local_data <- data
    local_data$x <- local_data[[x_var]]
    
    mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    
    mean_med_sd <- local_data %>%
      summarise(
        mean_x = mean(as.numeric(local_data$x), na.rm = TRUE),
        sd_x = sd(as.numeric(local_data$x), na.rm = TRUE),
        median_x = median(as.numeric(local_data$x), na.rm = TRUE),
        min_x = min(as.numeric(local_data$x), na.rm = TRUE),
        max_x = max(as.numeric(local_data$x), na.rm = TRUE),
        mode_x = mode(as.numeric(local_data$x))
      ) %>%
      ungroup() %>%
      as.data.frame()
    
    question <- paste("I provide you mean, standard deviation and median of the data. Could you please interpret the distribution of the data. mean:", paste("'", round(mean_med_sd[1],4),"'"),
                      "standard deviation: ", paste("'",round( mean_med_sd[2],4),"'"), "median: ",
                      paste("'", round(mean_med_sd[3],4),"'"),"minimum value: ", paste("'", round(mean_med_sd[4],4),"'"),
                      "maximum value: ", paste("'", round(mean_med_sd[5],4),"'"), 
                      "mode: ", paste("'", round(mean_med_sd[6],4),"'")) 
    answ_start<- paste("Figure 1 shows the distribution of", paste("'",x_var ,"'"),".")
    chatter.create(max_tokens = 1000)
    response <- chatter.chat(question, return_response = TRUE)
    placeholder <- response$choices$text
    res_fin<-paste(answ_start, gsub("\n", "", placeholder))
    return(res_fin)
  }
  
  
  generate_pred_dist_plot <- function(data, x_var) {
    # Create a local copy of the data and convert x_var to a factor
    local_data <- data
    local_data$x <- local_data[[x_var]]
    covariate <- sym(x_var)
    ggplot(local_data, aes(x = !!covariate)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.8) +
      geom_density(color = "red", size = 1.2) +
      labs(title = "",
           x = x_var,
           y = "Density") +
      theme_minimal()
  }
  
  
  
  generate_cl_dist_plot <- function(data, x_var, y_var) {
    # Create a local copy of the data and convert x_var to a factor
    local_data <- data
    local_data$x <- local_data[[x_var]]
    local_data$y <- local_data[[y_var]]
    local_data$y <- as.factor(local_data$y)
    
    ggplot(local_data, aes_string(x = "y", y = "x")) + 
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(width = 0.2, size = 1, color = "black") +
      labs(title = "",
           x = y_var,
           y = x_var) +
      theme_minimal()
  }
 
  
  output$between_plots6 <- renderText({
    "-----------------------------"
  })
  output$between_plots3 <- renderText({
    "-----------------------------"
  })
  output$between_plots5 <- renderText({
    "-----------------------------"
  })
  
  output$between_plots4 <- renderText({
    "-----------------------------"
  })
  
  output$between_plots <- renderText({
    "Class distirbutions of biomarkers represented via boxplot"
  })
  
  output$between_plots2 <- renderText({
    "Illustration of the probabilities of biomarker for a binary outputs"
  })
  
 
  
  # output$between_plots33 <- renderText({
  #   tags$div(
  #     style = "font-weight: bold; color: #0dc5c1;",
  #     "We need your feedback: ",
  #     tags$a("melih.agraz@giresun.edu.tr", href = "mailto:melih.agraz@giresun.edu.tr")
  #   )
  # })
  
  output$extra_plot <- renderPlot({
    
    # X1<-rnorm(500,mean = 0, sd = 2)
    # X2<-rnorm(500,mean = 5, sd = 2)
    # dtX<-c(X1,X2)
    # gr1<-rep(c("Group 1"), each = 500)
    # gr2<-rep(c("Group 2"), each = 500)
    # dtGr<-c(gr1,gr2)
    # 
    # df <- data.frame(value = aSAH$s100b, group = aSAH$outcome)
    

    output$extra_plot <- renderPlot({
      req(data(), input$xvar, input$yvar)
      
      
      df <- data.frame(value = data()[[input$xvar]], group = factor(data()[[input$yvar]]))
      
      ggplot(df, aes(x = value, fill = group)) +
        geom_density(alpha = 0.5) +
        geom_vline(xintercept = 0.1, linetype = "dashed", color = "black") +
        labs(title = "", x = "Value", y = "Density", fill = "Group") +
        xlim(min(df$value) - abs(min(df$value) * 0.2), max(df$value) + abs(max(df$value) * 0.2))
      
      })

  })  
 
  # output$extra_plot <- renderPlot({
  #   req(data(), input$xvar, input$yvar)
  #   
  #   # Print the values of input$xvar and input$yvar
  #   print(input$xvar)
  #   print(input$yvar)
  #   
  #   # Filter out missing and non-finite values
  #   filtered_data <- na.omit(data())
  #   filtered_data <- filtered_data[is.finite(filtered_data[[input$xvar]]), ]
  #   
  #   ggplot(filtered_data, aes_string(x = input$xvar, fill = input$yvar)) +
  #     geom_density(alpha = 0.5, position = "identity") +
  #     geom_vline(xintercept = 0.1, linetype = "dashed", color = "black") +
  #     geom_text(aes(x = 0.5, y = 0.02, label = "x=c"), color = "black", size = 4) +
  #     labs(title = "", x = "Value", y = "Density", fill = "Group") +
  #     guides(fill = guide_legend(title = "Group"))
  # })
  
  
  
  
  
  
  chatgpt_interpret_dist <- function(data, y_var, x_var) {
    local_data <- data
    local_data$x <- local_data[[x_var]]
    local_data$y <- local_data[[y_var]]
    
    summary_stats <- local_data %>%
      group_by(y) %>%
      summarise(
        mean_x = mean(as.numeric(x), na.rm = TRUE),
        min_x =  min(as.numeric(x), na.rm = TRUE),
        max_x =  max(as.numeric(x), na.rm = TRUE)
      ) %>%
      ungroup() %>%
      as.data.frame()
    
    question <- paste("I have a predictor and binary output. Y has two classes:",paste("'",summary_stats[1,1],"'"), "and", paste("'",summary_stats[2,1], "'"),
                      "I am providing statistics for",paste("'",summary_stats[1,1],"'"),":","mean value is:", paste("'",round(summary_stats[1,2],4),"'"),"min value is:", 
                      paste("'",round(summary_stats[1,3],4),"'"),"max value is:", paste("'",round(summary_stats[1,4],4),"'"), 
                      "I am providing statistics for",paste("'",summary_stats[2,1],"'"),":","mean value is:", paste("'",round(summary_stats[2,2],4),"'"),"min value is:", 
                      paste("'",round(summary_stats[2,3],4),"'"),"max value is:", paste("'",round(summary_stats[2,4],4),"'"), "Based on the statistics that i provided interpret the distributions of",paste("'",summary_stats[1,1],"'"), "and", paste("'",summary_stats[2,1], "'"), "and compare them." )
    
    answ_start <- paste("Figure 2 shows the class distributions of", paste("'",y_var ,"'"),"~",paste("'",x_var ,"'"),"." )
    chatter.create(max_tokens = 1000)
    response <- chatter.chat(question, return_response = TRUE)
    placeholder <- response$choices$text
    
    # Remove newline characters from the generated response
    placeholder_clean <- gsub("\n", "", placeholder)
    
    res_fin <- paste(answ_start, placeholder_clean)
    return(res_fin)
  }
  
  
  
  
  generate_roc_plot <- function(res) {
    if (is.list(res$best_res)) {
      plot(res$pl)
    } else if (is.list(res$specific_cutoff)){
      plot(res$pl)
    } else {
      plot(res$pl)
    }
  }
  
  # Generate ROC table
  generate_roc_table <- function(res) {
    if (is.list(res$best_res)) {
      return(res$best_res)
    } else if (is.list(res$specific_cutoff)){
      return(res$specific_cutoff)
    } else {
      return(list(a=res$maximum_sensitivity, b=res$maximum_specificity))
    }
  }
  
  generate_roc_tablePL <- function(res) {
    if(input$method == "maximized"){
      if (is.list(res$best_res)) {
        return(res$best_res[8])
      } else if (is.list(res$specific_cutoff)){
        return(res$specific_cutoff[8])
      } else {
        return(list(a=res$maximum_sensitivity[8], b=res$maximum_specificity[8]))
      }
    }else{
      if (is.list(res$best_res)) {
        return(res$best_res[7])
      } else if (is.list(res$specific_cutoff)){
        return(res$specific_cutoff[7])
      } else {
        return(list(a=res$maximum_sensitivity[7], b=res$maximum_specificity[7]))
      }
    }
  }
  
  # Display data preview
  # Display data preview
  output$data_preview <- renderTable({
    req(data())  
    first_four <- head(data(), n = 4)
    last_four <- tail(data(), n = 4)
    separator <- data.frame(matrix(rep("...", ncol(data())), nrow = 1, ncol = ncol(data())))
    names(separator) <- names(data())
    rbind(first_four, separator, last_four)
  })
  
  output$num_observations <- renderText({
    data() %>%
      nrow()
  })
  
  output$num_labels <- renderText({
    num_observations <- table(data()[[input$yvar]])
    paste(names(num_observations), num_observations, sep = " ")
  })
 
  output$num_missing <- renderText({
    missing_percentages <- round(colMeans(is.na(data()))*100,2)  
    missing_info <- paste(names(data()), missing_percentages, sep = ": ")
    missing_info
  })
  
  
  
  output$dist_plot <- renderPlot({
    req(input$xvar, data())
    generate_pred_dist_plot(data(), input$xvar)
  })
  
  output$cl_dist_plot <- renderPlot({
    req(input$xvar, input$yvar, data())
    generate_cl_dist_plot(data(), input$xvar, input$yvar)
  })
  
  
  output$sales_text <- renderText({
    if (input$method == "youden_topleft") {
      if (input$index == "youden") {
        return("Youden's J statistic, introduced in 1950, is used to determine the optimal cut-off poincby maximizing the distance from the diagonal line. Uses the following formula: max(sensitivities+specificities)
        
        
W. J. Youden (1950) ``Index for rating diagnostic tests''. Cancer, 3, 32--35. DOI: 10.1002/1097-0142(1950)               ")
      } else if (input$index == "closest.topleft") {
        return("The optimal threshold is the point nearest to the top-left corner of the plot,
which represents perfect sensitivity and specificity. 
               
      min((1-sensitivities)^2+(1-specificities)^2)")
      }
    } else if (input$method == "cutoff") {
      return("You can obtain performance metrics based on the cut-off point you have determined.")
    } else if (input$method == "maximized") {
      if (!is.null(input$constrain_metric)) {
        return("Uses the cutpointr function and it calculates the performance measures 
based on some constrains on specificity and sensitivity.")
      } else {
        return("ğğ")
      }
    } else if (input$method == "min_pvalue") {
      return("It is calculated the optimal cut-off value for the biomarker using the minimum p-value method, computes various performance measures.")
    }
  })
  
  
  sal_text <- function(input) {
    if (input$method == "youden_topleft") {
      if (input$index == "youden") {
        placeholder <- paste("In this study Youden's index is used to calculate optimal cut-off value.  Youden's index is a performance metric used in ROC analysis, which is commonly used in medical and biological research for measuring the performance of a classifier in a binary classification problem. The ROC curve shows the tradeoff between sensitivity and specificity at different threshold values. Youden's index measures the balance between sensitivity and specificity, and is used to select the optimal threshold value. The index value ranges from 0 to 1, with higher values indicating better classifier performance. Youden's index is often used in conjunction with other performance metrics in ROC analysis to determine the optimal threshold value for a classifier.")
        return(placeholder)
      } else if (input$index == "closest.topleft") {
        
        placeholder <- paste("The closest top left method is a graphical method used in ROC analysis for selecting the optimal threshold value. The method involves identifying the point on the ROC curve that is closest to the top left corner, which corresponds to a perfect classifier with 100% sensitivity and 100% specificity. The optimal threshold value is then chosen based on the coordinates of this point. The closest top left method is a simple and intuitive method that does not require the use of complex statistical techniques. However, it may not always provide the most accurate estimate of the optimal threshold value, particularly in cases where the ROC curve is not well-behaved or where there is a trade-off between sensitivity and specificity.")
        return(placeholder)
      }
    } else if (input$method == "cutoff") {
      placeholder <- paste("You can calculate performance measures by defining specific cutoffs in ROC analysis. Defining specific cutoff values allows you to determine the true positive rate, false positive rate, positive predictive value, negative predictive value, and accuracy of a classifier at that cutoff point. To do this, you must first select a threshold value on the ROC curve and calculate the corresponding true positive rate and false positive rate. From there, you can use these values to calculate the positive predictive value, negative predictive value, and accuracy at the selected threshold. By defining multiple cutoff values, you can also generate a cutoff-specific ROC curve and compare the performance of different classifiers at each cutoff value. Defining specific cutoff values is a flexible and useful method for assessing the performance of a classifier and determining the optimal threshold value for a given application.")
      return(placeholder)
    } else if (input$method == "maximized") {
      if (!is.null(input$constrain_metric)) {
        placeholder<-paste("The cutpointr function is a powerful tool that allows users to calculate performance measures based on specific constraints on sensitivity and specificity in ROC analysis. This method involves optimizing sensitivity and specificity simultaneously to find the optimal threshold value that maximizes both measures. The cutpointr function allows users to set constraints on sensitivity and specificity, which can be used to adjust the tradeoff between the two measures and obtain a threshold value that meets specific application requirements. By providing additional arguments such as the cost of false positives and false negatives, the cutpointr function can also incorporate the economic consequences of different classification decisions. This method is particularly useful in situations where sensitivity and specificity have different importance, and where the optimal threshold value is not immediately clear from the ROC curve alone. The cutpointr function provides a flexible and user-friendly way to optimize classifier performance based on specific needs and constraints.")
        return(placeholder)
      } else {
        placeholder<-paste("The cutpointr function is a powerful tool that allows users to calculate performance measures based on specific constraints on sensitivity and specificity in ROC analysis. This method involves optimizing sensitivity and specificity simultaneously to find the optimal threshold value that maximizes both measures. The cutpointr function allows users to set constraints on sensitivity and specificity, which can be used to adjust the tradeoff between the two measures and obtain a threshold value that meets specific application requirements. By providing additional arguments such as the cost of false positives and false negatives, the cutpointr function can also incorporate the economic consequences of different classification decisions. This method is particularly useful in situations where sensitivity and specificity have different importance, and where the optimal threshold value is not immediately clear from the ROC curve alone. The cutpointr function provides a flexible and user-friendly way to optimize classifier performance based on specific needs and constraints.")
        return(placeholder)
      }
    } else if (input$method == "min_pvalue") {
      placeholder <-paste("The minimum p-value approach is a statistical method used to identify the optimal cutoff value for a biomarker in ROC analysis. The method involves computing the p-value for each possible threshold value and selecting the value that corresponds to the smallest p-value as the optimal cutoff. The p-value is calculated based on the null hypothesis that the area under the ROC curve is equal to 0.5, which corresponds to a classifier with no discriminative power. By rejecting the null hypothesis and selecting a threshold value with a significant p-value, the minimum p-value approach ensures that the selected cutoff value has a high probability of separating the positive and negative cases accurately. Once the optimal cutoff value has been identified, various performance measures can be computed, including sensitivity, specificity, positive predictive value, negative predictive value, and accuracy. The minimum p-value approach is a widely used method in ROC analysis and provides a statistically rigorous way to select the optimal cutoff value for a given application.")
      return(placeholder)
    }
    
    return(placeholder)
  }
  
  # Compute summary statistics for selected variable
  var_summary <- reactive({
    req(input$xvar)
    var <- sym(input$xvar)
    data() %>%
      summarise(across({{var}}, ~ list(Mean = mean(., na.rm = TRUE), SD = sd(., na.rm = TRUE), Median = median(., na.rm = TRUE))))
  })
  output$loading <- reactive({ loading() })
  
  # Update predictor variable options based on selected data file
  observeEvent(input$datafile, {
    choices <- colnames(data())
    updateSelectInput(session, "xvar", choices = choices)
  })
  
  # Update outcome variable options based on selected data file
  observeEvent(input$datafile, {
    choices <- colnames(data())
    updateSelectInput(session, "yvar", choices = choices)
  })
  
  observeEvent(input$download_report, {
    output$download_in_progress(FALSE)# Show the loader when the button is clicked
    
    # Place your existing download action code here
    output$download_in_progress(FALSE) # Hide the loader after the download is complete
  })
  
  
  # Run analysis when "Run analysis" button is clicked
  analysis <- eventReactive(input$run_analysis, {
    xvar <- input$xvar
    yvar <- input$yvar
    method <- input$method
    index <- NULL
    if (method == "youden_topleft") {
      index <- input$index
    }
    cutoff <- NULL
    if (method == "cutoff") {
      cutoff <- input$cutoff
    }
    
    constrain_metric <- NULL
    min_cons <- NULL
    if (method == "maximized") {
      constrain_metric <- input$constrain_metric
      min_cons <- as.numeric(input$min_cons)
    }
    
    min_pvalue_method <- NULL
    if (method == "min_pvalue_approach") {
      method <- input$min_pvalue_method
    }
    
    res <- ROC_fin(data(), xvar, yvar, method = method, index = index, cutoff = cutoff,
                   constrain_metric = constrain_metric, min_cons = min_cons 
    )
    return(res)
  })
  
  
  output$roc_plot <- renderPlot({
    res <- analysis()
    generate_roc_plot(res)
  })
  
  # Display ROC table
  output$roc_table <- renderTable({
    if (is.null(analysis())) {
      return(NULL)
    }
    res <- analysis()
    generate_roc_table(res)
  })
  
  int_text<-paste("Note that the after running scispace API, we will enrich this Introduction part.")
  
  
  output$var_summary_table <- renderTable({
    summary_df <- var_summary()
    formatted_summary <- data.frame(
      Statistics = c("Mean of the Biomarker", "SD of the Biomarker", "Median of the Biomarker"),
      Results = c(summary_df[[1]]$Mean, summary_df[[1]]$SD, summary_df[[1]]$Median),
      stringsAsFactors = FALSE
    )
    formatted_summary
  })
  
  output$chatgpt_interpretation <- renderText({
    if (is.null(analysis())) {
      return(NULL)
    }
    res <- analysis()
    chatgpt_interpretation(res)
  })
  
  output$chatgpt_interpretationPL <- renderText({
    if (is.null(analysis())) {
      return(NULL)
    }
    res <- analysis()
    chatgpt_interpretationPL(res)
  })
  output$roc_results <- renderTable({
    if (is.null(analysis())) {
      return(NULL)
    }
    res <- analysis()
    generate_roc_table(res)
  })
  
  
  chatgpt_int <- function( data, x_var, y_var) {
    
    #---- clas dist
    local_data <- data
    #print(x_var)
    
    local_data$x <- local_data[[x_var]]
    local_data$y <- local_data[[y_var]]
    

    question3 <- paste("In this study, we explored the usefulness of", paste("'", x_var, "'"),
                       "as a new diagnostic biomarker for", paste("'", y_var, "'"),
                       ". ", paste("'", x_var, "'"),
                       "levels were higher in patients with", paste("'", y_var, "'"),
                       "relative to controls, consistent with our hypothesis that", paste("'", x_var, "'"),
                       "levels are elevated in patients with", paste("'", y_var, "'"),
                       ". Our analysis also demonstrates that", paste("'", x_var, "'"),
                       "is a potential biomarker for the diagnosis of", paste("'", y_var, "'"))
    
    question2<-paste("write a introduction at least 2000 character paragraph based on this:", question3)
    
    # print(question)
    chatter.create(max_tokens = 2000)
    response <- chatter.chat(question2, return_response = TRUE)
    placeholder1 <- response$choices$text
    
    # Remove newline characters
    placeholder1 <- gsub("\n", " ", placeholder1)
    
    return( placeholder1)
  }
  
  
  chatgpt_conc <- function(res, data, x_var, y_var) {
    
    #---- clas dist
    local_data <- data
    #print(x_var)
    
    local_data$x <- local_data[[x_var]]
    local_data$y <- local_data[[y_var]]
    
    #str(local_data)
    summary_stats <- local_data %>%
      group_by(y) %>%
      summarise(
        mean_x = mean(as.numeric(x), na.rm = TRUE),
        min_x =  min(as.numeric(x), na.rm = TRUE),
        max_x =  max(as.numeric(x), na.rm = TRUE)
      ) %>%
      ungroup() %>%
      as.data.frame()
    
    #----- ROC analysis
    roc_table <- generate_roc_table(res)
    
    dt<-data.frame("threshold"=roc_table[1],
                   "sensitivity"=roc_table[2],
                   "specificity"=roc_table[3],
                   "ppv"=roc_table[4],
                   "npv"==roc_table[5]  ,
                   "accuracy"==roc_table[6]  )
    
    
    question3<-paste("In this study, we perform a Receiver Operating Characteristic (ROC) curve analysis to identify optimal cut-off points based on, ", paste("'",x_var, "'"), "biomarkers for", paste("'",y_var, "'"),". The ROC curve is a graphical representation of a classifier's performance, illustrating the trade-off between sensitivity and specificity across various threshold levels. By analyzing the area under the ROC curve (AUC), we can evaluate the diagnostic accuracy of", paste("'",x_var, "'"), "biomarkers in predicting", paste("'",y_var, "'"),  "The primary objective of this study is to determine the most appropriate cut-off values for", paste("'",x_var, "'"),"biomarkers, which can be used for early detection and diagnosis of", paste("'",y_var, "'"),". This will enable clinicians to make better-informed decisions when assessing the risk of", paste("'",x_var, "'"), "in patients, ultimately leading to improved patient outcomes and more targeted treatment strategies. Use this information as well: AUC is", paste("'", roc_table[7], "'"), "and threshold is: ", paste("'", roc_table[1], "'"), "for biomarker ", paste("'",x_var, "'")  )  
    question2<-paste("write a conclusion at least 1000 character paragraph based on this:", question3)
    
    # print(question)
    chatter.create(max_tokens = 2000)
    response <- chatter.chat(question2, return_response = TRUE)
    placeholder1 <- response$choices$text
    
    # Remove newline characters
    placeholder1 <- gsub("\n", " ", placeholder1)
    
    return( placeholder1)
  }
  
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste("ROC_analysis_", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      # Trigger the spinner
      Sys.sleep(0.1)
      withProgress(message = 'Generating report', {
        # Place your existing download action code here
      })
    }
  )
  
  # Add download handler
  output$download_report <- downloadHandler(
    filename = function() {
      paste("ROC_analysis_", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      # Set loading to TRUE while generating the report
      download_in_progress(TRUE)
      
      # R Markdown template dosyasını geçici bir klasöre kopyalayın
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Rapor için parametreleri ayarlayın
      res <- analysis()
      roc_plot <- generate_roc_plot(res)
      roc_table <- generate_roc_table(res)
      cht_int_cls <- chatgpt_interpret_dist(data(),  input$yvar, input$xvar)
      chatgpt_interpret_dist_pred2 = chatgpt_interpret_dist_pred(data(),  input$xvar)
      cl_dist_plot <- generate_cl_dist_plot(data(), input$xvar, input$yvar)
      pred_dist_plot <- generate_pred_dist_plot(data(), input$xvar)
      chatgpt_conc2<- chatgpt_conc(res, data(), input$xvar,input$yvar)
      chatgpt_int2<- chatgpt_int(data(), input$xvar,input$yvar)
      params <- list(roc_plot = roc_plot, 
                     roc_table = roc_table, 
                     chatgpt_interpretation = chatgpt_interpretation,
                     chatgpt_interpretation = chatgpt_interpretation(res),
                     chatgpt_interpretationPL = chatgpt_interpretationPL(res),
                     pred_dist_plot=pred_dist_plot,
                     sales_text =  sal_text(input),
                     cl_dist_plot = cl_dist_plot,
                     cht_int_cls = cht_int_cls,
                     cht_pred_cls=chatgpt_interpret_dist_pred2,
                     combined_placeholder=chatgpt_conc2,
                     chatgpt_int=chatgpt_int2,
                     int_text=int_text
      )
      
      # R Markdown dosyasını işleyin ve parametreleri ile birlikte geçirin
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      download_in_progress(FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)
