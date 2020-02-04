select_files_to_read <-
function(month = "Dec", year = 2019) {
    load("filespath.RData")

    if (month == "Dec") {
        month_num = 1
    } else if (month == "Nov") {
        month_num = 2
    } else if (month == "Oct") {
        month_num = 3
    } else if (month == "Sep") {
        month_num = 4
    } else if (month == "Aug") {
        month_num = 5
    } else if (month == "Jul") {
        month_num = 6
    } else if (month == "Jun") {
        month_num = 7
    } else if (month == "May") {
        month_num = 8
    } else if (month == "Apr") {
        month_num = 9
    } else if (month == "Mar") {
        month_num = 10
    } else if (month == "Feb") {
        month_num = 11
    } else {
        month_num = 12
    }

    files <- list("2019" = filespath[1:12],
                  "2018" = filespath[13:24],
                  "2017" = filespath[25:36],
                  "2016" = filespath[37:39])

    if (year == 2016) {
        yearOrder <- 4
        if (month == "Dec" | month == "Nov" | month == "Oct") {
            xlsname <<- paste0(month, year)
            year_char <- as.character(year)
            tempfilename <<- as.data.frame(read_xlsx(files[[yearOrder]][month_num]))
        } else {
            stop("Year 2016 have Dec, Nov and Oct files only.")
        }
    } else {
        xlsname <<- paste0(month, year)
        if (year == 2019) {
            yearOrder <- 1
        } else if (year == 2018) {
            yearOrder <- 2
        } else {
            yearOrder <- 3
        }
        tempfilename <<- as.data.frame(read_xlsx(files[[yearOrder]][month_num]))
    }
}
