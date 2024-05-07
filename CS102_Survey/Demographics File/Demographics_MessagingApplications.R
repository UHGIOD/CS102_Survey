library(readr)
library(dplyr)
library(tidyr)

messRespon<-read.csv('C:/Users/User/Documents/Rstudio Files/midtermsurvey/FINAL EXCEL/RESPONDENTS.csv')

                  #GRAPHING DEMOGRAPHICS UTILIZED IN THE SURVEY

#DEMOGRAPHIC: Age
age_freq<-table(messRespon$Age)

#Viewing of the frequency of the ages
age_freq

percentages <- round((age_freq / sum(age_freq)) * 100, 1)
pie(age_freq, 
    main = "Respondents by Age",
    col = rainbow(length(age_freq)),
    labels = paste("", format(percentages, nsmall = 1, digits = 2), "%"),
    cex = 0.6,
)
legend_labels <- paste(names(age_freq), "years old")
legend("bottomleft", legend = legend_labels, bty = "n", cex = 0.8, fill = rainbow(length(age_freq)))

#DEMOGRAPHIC:Gender 
gender_freq <- table(messRespon$Gender)

#Viewing of the frequency of the sex
gender_freq

percentages <- round((gender_freq / sum(gender_freq)) * 100, 1)
pie(gender_freq, 
    main = "Respondents by Sex",
    col = rainbow(length(gender_freq)),
    labels = paste("", format(percentages, nsmall = 1, digits = 2), "%"),
    cex = 0.6,
)
legend_labels <- paste(names(gender_freq))
legend("bottomleft", legend = legend_labels, bty = "n", cex = 0.8, fill = rainbow(length(gender_freq)))


#DEMOGRAPHIC:Type of Community
community_freq <- table(messRespon$Type.of.community)

#Viewing of the frequency of the community types
community_freq

percentages <- round((community_freq / sum(community_freq)) * 100, 1)
pie(community_freq, 
    main = "Respondents by Type of Community",
    col = rainbow(length(community_freq)),
    labels = paste("", format(percentages, nsmall = 1, digits = 2), "%"),
    cex = 0.6,
)
legend_labels <- paste(names(community_freq))
legend("bottomleft", legend = legend_labels, bty = "n", cex = 0.8, fill = rainbow(length(community_freq)))


#DEMOGRAPHIC:College
college_freq <- table(messRespon$College)

#Viewing of the frequency of the colleges (before transforming)
college_freq

single_respondent_colleges <- names(college_freq[college_freq == 1])
single_respondent_colleges

messRespon$College[messRespon$College %in% single_respondent_colleges] <- "Others"

college_freq <- table(messRespon$College)

#Viewing of the frequency of the colleges (after transforming)
college_freq

percentages <- round((college_freq / sum(college_freq)) * 100, 1)

pie_data <- college_freq
others_index <- "Others"
if (others_index %in% names(pie_data)) {
  pie_data[others_index] <- sum(pie_data[pie_data < 2])
  pie_data <- pie_data[pie_data >= 2]
}

pie(pie_data,
    main = "Respondents by College",
    col = rainbow(length(pie_data)),
    labels = paste("", format(percentages[pie_data >= 2], nsmall = 1, digits = 2), "%"),
    cex = 0.7)

legend_labels <- names(pie_data)
legend("topleft", legend = c(legend_labels, "Others"), bty = "n", cex = 0.6, fill = rainbow(length(pie_data) + 1))


#DEMOGRAPHIC: Courses

#As our data for courses are mixed, we first transform it by lowercasing the texts. 

corrected_courses <- tolower(messRespon$Course)

valid_courses <- c(
  "bachelor of science in information technology",
  "bachelor of science in architecture",
  "bachelor of science in civil engineering",
  "bachelor of elementary education major in physical science",
  "bachelor of science in information systems",
  "bachelor of science in industrial technology (bit) – level iii",
  "bachelor in human services",
  "bachelor of secondary education major in science",
  "bachelor of science in computer science",
  "bachelor in fashion design and merchandising (bsfdm) – level ii",
  "bachelor of science in nursing",
  "bachelor of science in business administration major in financial management",
  "bachelor of science in medical laboratory science",
  "bachelor of science in hospitality management",
  "bachelor of science in criminology",
  "bachelor of science in economics",
  "bachelor of science in biology with specialization in microbiology",
  "bachelor of science in forestry",
  "bachelor of science in marine engineering",
  "bachelor of science in business administration",
  "bachelor of art in english language studies",
  "bachelor of science in accountancy",
  "bachelor of library and information science",
  "bachelor of science in development communication",
  "bachelor of secondary major in english",
  "bachelor of science in office administration",
  "bachelor of science in pharmacy"
)


find_closest_match <- function(course) {
  distances <- sapply(valid_courses, function(valid_course) {
    sum(tolower(course) != tolower(valid_course))
  })
  closest_index <- which.min(distances)
  if (distances[closest_index] > 2) {
    return(course)
  } else {
    return(valid_courses[closest_index])
  }
}

corrected_courses <- sapply(corrected_courses, find_closest_match)


course_freq <- table(corrected_courses)
single_respondent_courses <- names(course_freq[course_freq == 1])
corrected_courses[corrected_courses %in% single_respondent_courses] <- "Others"
course_freq <- table(corrected_courses)

course_freq <- course_freq[order(-course_freq)]

#Viewing of the frequency of the courses
course_freq

percentages <- round((course_freq / sum(course_freq)) * 100, 1)

pie_data <- course_freq
others_index <- "Others"
if (others_index %in% names(pie_data)) {
  pie_data[others_index] <- sum(pie_data[pie_data < 2])
  pie_data <- pie_data[pie_data >= 2]
}

pie(pie_data, 
    main = "Respondents by Courses",
    col = rainbow(length(pie_data)),
    labels = paste("", format(percentages[pie_data >= 2], nsmall = 1, digits = 2), "%"),
    cex = 0.8)

legend_labels <- names(pie_data)
legend("topleft", legend = c(legend_labels, "Others"), bty = "n", cex = 0.7, fill = rainbow(length(pie_data) + 1))


#DEMOGRAPHIC:By Year Level
yearlvl_freq <- table(messRespon$Year.Level)
yearlvl_freq

yearlvl_freq["1st Year"] <- yearlvl_freq["1st Year"] + yearlvl_freq["1st  Year"]
yearlvl_freq <- yearlvl_freq[!names(yearlvl_freq) %in% "1st  Year"]

percentages <- round((yearlvl_freq / sum(yearlvl_freq)) * 100, 1)

#Viewing of the frequency of the number of the year level.
yearlvl_freq

barplot(as.vector(yearlvl_freq), 
        main = "Respondents by Year Level",
        col = rainbow(length(yearlvl_freq)),
        xlab = "Year Level",
        ylab = "Number of Respondents",
        ylim = c(0, max(yearlvl_freq) * 1.1),
        names.arg = names(yearlvl_freq))


text(x = barplot(as.vector(yearlvl_freq), plot = FALSE),
     y = yearlvl_freq,
     label = paste0(format(percentages, nsmall = 1, digits = 2), "%"),
     pos = 3, cex = 0.8)


#DEMOGRAPHIC:Application used in messaging
messagingapp_freq <- table(messRespon$Which.messaging.app.do.you.use.most.frequently.)

#Viewing of the frequency of the applications used in messaging
messagingapp_freq

percentages <- round((messagingapp_freq / sum(messagingapp_freq)) * 100, 1)


barplot(messagingapp_freq, 
        main = "Which messaging app do you use more frequently?",
        col = c("yellow", "blue", "green"), 
        names.arg = names(messagingapp_freq),
        ylim = c(0, max(messagingapp_freq) * 1.2), 
        xlab = "Messaging App",
        ylab = "Frequency",
        cex.names = 0.8 
)


text(x = 1:length(messagingapp_freq), y = messagingapp_freq, labels = paste0(percentages, "%"), pos = 3, cex = 0.8, col = "black")



#DEMOGRAPHIC:Frequency of usage using messaging apps
usage_length <- table(messRespon$How.often.do.you.use.messaging.apps.)

#Viewing of the frequency of the usage length.
usage_length

percentages <- round(prop.table(usage_length) * 100, 2)


barplot(usage_length,
        main = "Frequency of Usage of Messaging Applications",
        col = rainbow(length(usage_length)),
        names.arg = names(usage_length),
        xlab = "Usage Length",
        ylab = "Number of Respondents",
        cex.names = 0.8,
        las = 1,
        ylim = c(0, max(usage_length) * 1.1)
)


text(x = 1:length(usage_length), y = usage_length, labels = paste0(percentages, "%"), pos = 3, cex = 0.8, col = "black")


#DEMOGRAPHIC:How long have you been using the messaging app?
usage_length <- table(messRespon$How.long.have.you.been.using.messaging.apps.)

#Viewing of the frequency of the usage of the messaging app
usage_length

percentages <- round(prop.table(usage_length) * 100, 2)
barplot(usage_length,
        main = "How long have you been using the messaging app?",
        col = rainbow(length(usage_length)),
        names.arg = names(usage_length),
        xlab = "Usage Length",
        ylab = "Number of Respondents",
        cex.names = 0.8,
        las = 1,
        ylim = c(0, max(usage_length) * 1.1)
)


#DEMOGRAPHIC: Device used for messaging
device_type <- table(messRespon$What.type.of.device.do.you.primarily.use.for.messaging.)

#Viewing of the frequency of the devices used. 
device_type

percentages <- round(prop.table(device_type) * 100, 2)


barplot(device_type,
        main = "Type of device primarily use for messaging",
        col = rainbow(length(device_type)),
        names.arg = names(device_type),
        xlab = "Device Type",
        ylab = "Number of Respondents",
        cex.names = 0.8,
        las = 1,
        ylim = c(0, max(device_type) * 1.1)
)


text(x = 1:length(device_type), y = device_type, labels = paste0(percentages, "%"), pos = 3, cex = 0.8, col = "black")

