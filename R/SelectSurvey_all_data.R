
#' SelectSurvey_all_data
#'
#' Running the command will ask you to select your tab delimited exported results from SelectSurvey and the Overview copy pasted into an excel file. Include your user start and end response dates to get response data. This function will import the data and  separate the Overview results for each question so that you can chose if you want to include all of the optional answears or not.
#'
#'
#' @return Imports the data and makes separate Overview results for each question so that you can chose if you want to include all of the optional answears or not.
#' @export
#'
#' @examples
#' SelectSurvey_all_data()
SelectSurvey_all_data <- function(){
  invisible(require(lubridate,
                    quietly = TRUE,
                    warn.conflicts = FALSE))
  invisible(require(openxlsx,
                    quietly = TRUE,
                    warn.conflicts = FALSE))
  invisible(require(stringr,
                    quietly = TRUE,
                    warn.conflicts = FALSE))
  invisible(require(ggplot2,
                    quietly = TRUE,
                    warn.conflicts = FALSE))
  print("Select tab delimited exported results")
  full_data <- read.csv(file.choose(), header = TRUE, sep = ",", row.names = NULL, check.names = FALSE)
  full_data$startPOSIXct <- dmy_hm(paste(full_data$`Date Started`, full_data$`Time Started`))
  full_data$endPOSIXct <- dmy_hm(paste(full_data$`Date Completed`, full_data$`Time Completed`))
  full_data$response_minutes <-  (full_data$endPOSIXct - full_data$startPOSIXct)/60
  assign("full_data", full_data, envir = .GlobalEnv)
  print(paste(" Your exported results object full_data has", dim(full_data)[2], "sets of data from ", dim(full_data)[1], "answers."))
  print("Select Results Overview copy pasted excel file ")
  Overview <- read.xlsx(file.choose(),  sheet = 1, startRow = 1, colNames = TRUE)
  assign("Overview", Overview, envir = .GlobalEnv)
  Qns <- which(sapply(str_locate_all(Overview$Variables, "[0-9]+"), function(x){as.numeric(x[1])}) == 2)
  ends <- c(which(Overview[,1] == "Total Respondents "))
  Qn_N <- length(Qns)
  for (i in 1:Qn_N) {
    Qn <- paste("Q", i, sep = "_")
    assign(Qn, Overview[(Qns)[i]:(ends)[i], ], envir = .GlobalEnv)
  }
  print(paste("Your Overview is for survey named", Overview[1,1], "with", Qn_N, "questions"))
  answr <- data.frame(total = as.numeric(Overview[(ends),2]), qn = Overview[(Qns),1])
  response_p <- ggplot(answr,  aes(x = qn, y = total)) +
    geom_col() +
    scale_y_continuous() +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
    theme(axis.text.x = element_text(angle = 0, hjust = 1),
          text = element_text(size = 10),
          plot.margin = unit(c(0.2,0.5,0.5,0.5),"cm"),
          axis.title.x = element_blank())  +
    geom_text(stat = "identity",
              aes(label = total, y = total) , vjust = 0.2, hjust = -0.2) +
    ylab("Number of responses") +
    ggtitle(paste("Responses to survey ", Overview[1,1])) +
    coord_flip()
  print(response_p)
}

#' SelectSurvey_Overview
#'
#'Running the command will ask you to select your SelectSurvey Overview copy pasted into an excel file. Include your user start and end response dates to get response data. This function will import the data and  separate the Overview results for each question so that you can chose if you want to include all of the optional answears or not.
#'
#' @return Imports the data and makes separate Overview results for each question so that you can chose if you want to include all of the optional answears or not.
#' @export
#'
#' @examples
#' SelectSurvey_Overview()
SelectSurvey_Overview <- function(){
  invisible(require(lubridate,
                    quietly = TRUE,
                    warn.conflicts = FALSE))
  invisible(require(openxlsx,
                    quietly = TRUE,
                    warn.conflicts = FALSE))
  invisible(require(stringr,
                    quietly = TRUE,
                    warn.conflicts = FALSE))
  invisible(require(ggplot2,
                    quietly = TRUE,
                    warn.conflicts = FALSE))
  print("Select Results Overview copy pasted excel file ")
  Overview <- read.xlsx(file.choose(),  sheet = 1, startRow = 1, colNames = TRUE)
  assign("Overview", Overview, envir = .GlobalEnv)
  print(paste("Your Overview is for survey named", Overview[1,1]))
  Qns <- which(sapply(str_locate_all(Overview$Variables, "[0-9]+"), function(x){as.numeric(x[1])}) == 2)
  ends <- c(which(Overview[,1] == "Total Respondents "))
  #ranges <- paste(Qns,ends,sep = ":")
  # total_answr <- Overview[(ends),2]
  Qn_N <- length(Qns)
  for (i in 1:Qn_N) {
    Qn <- paste("Q", i, sep = "_")
    assign(Qn, Overview[(Qns)[i]:(ends)[i], ], envir = .GlobalEnv)
  }}

#' SelectSurvey_exported_data
#'
#' @return Running the command will ask you to select your tab delimited exported results from SelectSurvey. Include your user start and end response dates to get response data.
#' @export
#'
#' @examples
#' SelectSurvey_exported_data()
SelectSurvey_exported_data <- function(){
  invisible(require(lubridate,
                    quietly = TRUE,
                    warn.conflicts = FALSE))
  invisible(require(openxlsx,
                    quietly = TRUE,
                    warn.conflicts = FALSE))
  invisible(require(stringr,
                    quietly = TRUE,
                    warn.conflicts = FALSE))
  invisible(require(ggplot2,
                    quietly = TRUE,
                    warn.conflicts = FALSE))
  print("Select tab delimited exported results")
  full_data <- read.csv(file.choose(), header = TRUE, sep = ",", row.names = NULL, check.names = FALSE)
  full_data$startPOSIXct <- dmy_hm(paste(full_data$`Date Started`, full_data$`Time Started`))
  full_data$endPOSIXct <- dmy_hm(paste(full_data$`Date Completed`, full_data$`Time Completed`))
  full_data$response_minutes <-  (full_data$endPOSIXct - full_data$startPOSIXct)/60
  assign("full_data", full_data, envir = .GlobalEnv)
  print(paste(" Your exported results object full_data has", dim(full_data)[2], "questions and ", dim(full_data)[1], "answers."))
}
