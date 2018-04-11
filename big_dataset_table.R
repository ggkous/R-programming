source("~/R_server/library.R")
source("~/R_server/same_feature.R")

setwd("~/R_server/")

combine_csv <- function() {
  path  <-  "~/R_server/source_data/"
  filenames <- list.files(path)
  filenames <-
    mixedsort(sort(filenames)) #start with 1
  
  combine_csv <- c()
  for (index in 1:length(filenames))
  {
    #one csv
    table_data <-
      read.table(paste0(path, filenames[index], sep = ""),
                 sep = ",",
                 header = TRUE)
    #add csv_number
    table_data[, "csv_number"]  <- c(index)
    
    #combine all csv
    combine_csv <- rbind(combine_csv, table_data)
  }
  
  return(combine_csv)
}
combine_csv()
student_id <- function(data) {
  test <- c()
  for (nrow in 1:nrow(data)) {
    data[, "student_id"] <- c(paste0("16000000", data$PeopleCount))
  }
  
  return(data)
}
student_id(data = combine_csv())

rename_dataset <- function(data) {
  emotion <<- c("ANGRY",
                "CONFUSED",
                "HAPPY",
                "CALM",
                "SAD",
                "DISGUSTED",
                "SURPRISED")
  
  #col rename
  new_dataset <-
    data.frame(
      frame_number = data$FrameNumber,
      people_count = data$PeopleCount,
      time = data$FrameTimePosition,
      gender = data$Gender,
      emotion_value_1 = round(data$EmotionsType1, 0),
      emotion_type_1 = data$EmotionsConfidence1,
      emotion_value_2 =  round(data$EmotionsType2, 0),
      emotion_type_2 = data$EmotionsConfidence2,
      emotion_value_3 = round(data$EmotionsType3, 0),
      emotion_type_3 = data$EmotionsConfidence3,
      age_range_high = data$AgeRangeHigh,
      age_range_low = data$AgeRangelow,
      csv_number = data$csv_number,
      student_id = data$student_id
    )
  
  return(new_dataset)
}
#rename_dataset(student_id(data = combine_csv()))

emotion_table <- function(data) {
  max_emotion_table <-
    sqldf(
      c(
        "select data.*,
        (case when emotion_value_1 >= emotion_value_2 and emotion_value_1 >= emotion_value_3 then emotion_value_1
        when emotion_value_2 >= emotion_value_3 then emotion_value_2
        else emotion_value_3
        end) as max_emotion_value,
        (case when emotion_value_1 >= emotion_value_2 and emotion_value_1 >= emotion_value_3 then emotion_type_1
        when emotion_value_2 >= emotion_value_3 then emotion_type_2
        else emotion_type_3
        end) as max_emotion
        from data;"
      )
    )
  
  max_emotion_table <<-
    max_emotion_table[,-c(5:10)]
  
  min_csv <<- min(data$csv_number)
  max_csv <<- max(data$csv_number)
  
  return(max_emotion_table)
}
#save variable

master_table <-
  emotion_table(rename_dataset(student_id(data = combine_csv())))

overall_line_chart_sql <- function(data) {
  statement <-
    sqldf(
      "select max_emotion,csv_number,count(max_emotion_value)as count from data
      group by csv_number,max_emotion"
    )
  return(statement)
}
#overall_line_chart_sql(master_table)

#zero
bigdataset_emotion_check_zero <- function(data) {
  data <- data %>%
    complete(
      max_emotion = emotion,
      csv_number = c(1:10),
      fill = list(count = 0)
    ) %>%
    select(csv_number, max_emotion, count) %>%
    arrange(csv_number, max_emotion) %>%
    as.data.frame()
  return(data)
}
#bigdataset_emotion_check_zero(overall_line_chart_sql(master_table))

overall_line_chart <- function(data) {
  ylim_max <- max(data$count)
  min <- min(data$csv_number)
  max <- max(data$csv_number)
  chart <- ggplot(data = data,
                  aes(x = csv_number,
                      y = count,
                      group = max_emotion)) +
    geom_line(aes(color = max_emotion)) +
    geom_point(aes(color = max_emotion)) +
    scale_x_continuous(limits = c(min,
                                  max),
                       breaks = seq(min,
                                    max,
                                    1)) +
    scale_manual_color() +
    theme_text_size(axis.text.x = element_text(size = 7, face = "bold")) +
    labs(
      title = "THE TREND OF EMOTIONAL CHANGES AMONG STUDENTS \n 所有學生情緒改變趨勢",
      x = "LESSON",
      y = "COUNT",
      color = "EMOTION"
    )
  
  
  plotly_chart <- ggplotly(chart)
  return (plotly_chart)
}

overall_line_chart_save <- function (chart) {
  directory <-
    "~/R_server/image/sample/html/line_chart/overall_line_chart/overall_line_chart.html"
  
  htmlwidgets::saveWidget(as_widget(chart),
                          selfcontained = TRUE,
                          file = directory)
  
  
  all <-  "~/R_server/image/sample/html/all/overall_line_chart.html"
  
  htmlwidgets::saveWidget(as_widget(chart),
                          selfcontained = TRUE,
                          file = all)
  
}

#make chart 總體所有學生情緒趨勢
#overall_line_chart_save(overall_line_chart(bigdataset_emotion_check_zero(overall_line_chart_sql(master_table))))

#angry
each_emotion_line_chart_angry_sql <- function(data) {
  statement <-
    sqldf(
      "select max_emotion,csv_number,count(max_emotion_value)as count from data where max_emotion in('ANGRY')
      group by csv_number,max_emotion"
    )
  return(statement)
}
#angry:add zero
angry_check_zero <- function(data) {
  data <- data %>%
    complete(
      max_emotion = "ANGRY",
      csv_number = c(1:10),
      fill = list(count = 0)
    ) %>%
    select(csv_number, max_emotion, count) %>%
    arrange(csv_number, max_emotion) %>%
    as.data.frame()
  return(data)
}
#angry_check_zero(each_emotion_line_chart_angry_sql(master_table))

#calm
each_emotion_line_chart_calm_sql <- function(data) {
  statement <-
    sqldf(
      "select max_emotion,csv_number,count(max_emotion_value)as count from data where max_emotion in ('CALM')
      group by csv_number,max_emotion"
    )
  return(statement)
}

#calm :add zero
calm_check_zero <- function(data) {
  data <- data %>%
    complete(
      max_emotion = "CALM",
      csv_number = c(1:10),
      fill = list(count = 0)
    ) %>%
    select(csv_number, max_emotion, count) %>%
    arrange(csv_number, max_emotion) %>%
    as.data.frame()
  return(data)
}
#calm_check_zero(each_emotion_line_chart_calm_sql(master_table))

#confused
each_emotion_line_chart_confused_sql <- function(data) {
  statement <-
    sqldf(
      "select max_emotion,csv_number,count(max_emotion_value)as count from data where max_emotion in ('CONFUSED')
      group by csv_number,max_emotion"
    )
  return(statement)
}

#confused :add zero
confused_check_zero <- function(data) {
  data <- data %>%
    complete(
      max_emotion = "CONFUSED",
      csv_number = c(1:10),
      fill = list(count = 0)
    ) %>%
    select(csv_number, max_emotion, count) %>%
    arrange(csv_number, max_emotion) %>%
    as.data.frame()
  return(data)
}
#confused_check_zero(each_emotion_line_chart_confused_sql(master_table))


#disgusted
each_emotion_line_chart_disgusted_sql <- function(data) {
  statement <-
    sqldf(
      "select max_emotion,csv_number,count(max_emotion_value)as count from data where max_emotion in ('DISGUSTED')
      group by csv_number,max_emotion"
    )
  return(statement)
}

#disgusted :add zero
disgusted_check_zero <- function(data) {
  data <- data %>%
    complete(
      max_emotion = "DISGUSTED",
      csv_number = c(1:10),
      fill = list(count = 0)
    ) %>%
    select(csv_number, max_emotion, count) %>%
    arrange(csv_number, max_emotion) %>%
    as.data.frame()
  return(data)
}
#disgusted_check_zero(each_emotion_line_chart_disgusted_sql(master_table))

#happy
each_emotion_line_chart_happy_sql <- function(data) {
  statement <-
    sqldf(
      "select max_emotion,csv_number,count(max_emotion_value)as count from data where max_emotion in ('HAPPY')
      group by csv_number,max_emotion"
    )
  return(statement)
}

#happy :add zero
happy_check_zero <- function(data) {
  data <- data %>%
    complete(
      max_emotion = "HAPPY",
      csv_number = c(1:10),
      fill = list(count = 0)
    ) %>%
    select(csv_number, max_emotion, count) %>%
    arrange(csv_number, max_emotion) %>%
    as.data.frame()
  return(data)
}
#happy_check_zero(each_emotion_line_chart_happy_sql(master_table))

#sad
each_emotion_line_chart_sad_sql <- function(data) {
  statement <-
    sqldf(
      "select max_emotion,csv_number,count(max_emotion_value)as count from data where max_emotion in ('SAD')
      group by csv_number,max_emotion"
    )
  return(statement)
}

#sad :add zero
sad_check_zero <- function(data) {
  data <- data %>%
    complete(
      max_emotion = "SAD",
      csv_number = c(1:10),
      fill = list(count = 0)
    ) %>%
    select(csv_number, max_emotion, count) %>%
    arrange(csv_number, max_emotion) %>%
    as.data.frame()
  return(data)
}
#sad_check_zero(each_emotion_line_chart_sad_sql(master_table))

#surprised
each_emotion_line_chart_surprised_sql <- function(data) {
  statement <-
    sqldf(
      "select max_emotion,csv_number,count(max_emotion_value)as count from data where max_emotion in ('SURPRISED')
      group by csv_number,max_emotion"
    )
  return(statement)
}

#surprised :add zero
surprised_check_zero <- function(data) {
  data <- data %>%
    complete(
      max_emotion = "SURPRISED",
      csv_number = c(1:10),
      fill = list(count = 0)
    ) %>%
    select(csv_number, max_emotion, count) %>%
    arrange(csv_number, max_emotion) %>%
    as.data.frame()
  return(data)
}
#surprised_check_zero(each_emotion_line_chart_surprised_sql(master_table))


each_emotion_line_chart <- function(data, emotion_eng, emotion_chin) {
  ylim_max <- as.integer(max(data$count))
  chart <- ggplot(data = data,
                  aes(x = csv_number,
                      y = count,
                      group = max_emotion)) +
    geom_line(aes(color = max_emotion)) +
    geom_point(aes(color = max_emotion)) +
    ylim(0, ylim_max + 25) +
    scale_x_continuous(limits = c(min_csv,
                                  max_csv),
                       breaks = seq(min_csv,
                                    max_csv,
                                    1)) +
    scale_manual_color() +
    theme_text_size(axis.text.x = element_text(size = 7, face = "bold")) +
    labs(
      title = paste0(
        "THE TREND OF ",
        emotion_eng,
        " EMOTIONAL CHANGES AMONG STUDNETS\n 學生們",
        emotion_chin,
        "的情緒改變趨勢"
      ),
      x = "LESSON",
      y = "COUNT",
      color = "EMOTION"
    )
  
  plotly_chart <- ggplotly(chart)
  
  return(plotly_chart)
}


#each_emotion_line_chart(angry_check_zero(each_emotion_line_chart_angry_sql(master_table)),emotion_eng="ANGER",emotion_chin="生氣")



each_emotion_line_chart_save <- function (chart, name) {
  directory <-
    paste0(
      "~/R_server/image/sample/html/line_chart/each_emotion_line_chart/",
      name,
      "_line_chart.html"
    )
  
  #htmltools::save_html(chart, directory)
  
  htmlwidgets::saveWidget(as_widget(chart),
                          selfcontained = TRUE,
                          file = directory)
  
  
  all <-  paste0("~/R_server/image/sample/html/all/",
                 name,
                 "_line_chart.html")
  
  htmlwidgets::saveWidget(as_widget(chart),
                          selfcontained = TRUE,
                          file = all)
  
}

#make chart 學生們的情勢改變趨勢
#each_emotion_line_chart_save(each_emotion_line_chart(angry_check_zero(each_emotion_line_chart_angry_sql(master_table)),emotion_eng="ANGER",emotion_chin="生氣"),name="angry")




overall_each_person_sql <- function(data) {
  statement <<-
    sqldf(
      "select max_emotion,people_count,count(max_emotion_value)as count,csv_number from data
      group by people_count,max_emotion,csv_number order by csv_number,people_count"
      
    )
}
#overall_each_person_sql(master_table)



overall_personal_emotion_sql <- function(data) {
  count <- function() {
    statement <- list()
    for (people in 1:max(data$people_count)) {
      statement[[people]] <-
        sqldf(
          paste0(
            "select max_emotion,count(max_emotion)as count,csv_number,people_count from data where people_count = ",
            people,
            " group by csv_number,max_emotion"
          )
        )
    }
    return(statement)
  }
  count()
  
  sum <- function(count_data) {
    sql <- list()
    table <- lapply(count_data, function(data) {
      sql <-
        sqldf(
          "select max_emotion,sum(count) as sum ,people_count from data
          group by max_emotion"
        )
    })
    return(table)
    }
  sum(count_data = count())
}

#overall_personal_emotion_sql(master_table)

histomgram_bar_total <- function(title, x, y, fill, max) {
  same = list(
    geom_bar(stat = "identity", width = 0.6),
    coord_cartesian(ylim = c(0, max + 25)),
    labs(
      title = title,
      x = x,
      y = y,
      fill = fill
    )
  )
}


overall_personal_ggplot_graph <- function(data) {
  chart <- list()
  plotly_chart <- list()
  
  for (index in 1:length(data)) {
    max_sum_value <- max(data[[index]]$sum)
    
    chart[[index]] <- ggplot(data = data[[index]],
                             aes(x = max_emotion,
                                 y = sum,
                                 fill = max_emotion)) +
      histomgram_bar_total(
        title = paste0(
          "THE PERFORMANCE OF EMOTIONS OF STUDENT ID: 16000000",
          index,
          " \n 16000000",
          index,
          "學生編號的情緒表現"
        ),
        x = "EMOTION",
        y = "COUNT",
        fill = "EMOTION",
        max = max_sum_value
      ) +
      scale_fill_color() +
      theme_text_size(axis.text.x = element_text(size = 7, face = "bold"))
    
    plotly_chart[[index]] <- ggplotly(chart[[index]])
  }
  return(plotly_chart)
}

#overall_personal_ggplot_graph(overall_personal_emotion_sql(master_table))

overall_personal_save <- function (chart) {
  print(chart)
  size <- length(chart)
  print_graph <- vector(size, mode = "list")
  
  directory <-
    "~/R_server/image/sample/html/histogram/personal_emotion_distribution/overall_each_person/emotion_distribution_person_"
  
  
  for (index in 1:length(chart)) {
    path <- paste0(directory, index, ".html")
    
    htmlwidgets::saveWidget(as_widget(chart[[index]]),
                            selfcontained = TRUE,
                            file = path)
    
  }
  
  #all graph directory
  all <-
    "~/R_server/image/sample/html/all/emotion_distribution_person_"
  for (index in 1:length(chart)) {
    path <- paste0(all, index, ".html")
    
    #htmltools::save_html(chart[[index]], path)
    
    htmlwidgets::saveWidget(as_widget(chart[[index]]),
                            selfcontained = TRUE,
                            file = path)
    
  }
}
#make chart 一名學生所有堂數的情緒表現: 16000000"

#overall_personal_save(overall_personal_ggplot_graph(overall_personal_emotion_sql(master_table)))



overall_person_line_chart_sql <- function(data) {
  count <- function() {
    statement <- list()
    for (people in 1:max(data$people_count)) {
      statement[[people]] <-
        sqldf(
          paste0(
            "select max_emotion,count(max_emotion)as count,csv_number,people_count from data where people_count = ",
            people,
            " group by csv_number,max_emotion"
          )
        )
    }
    return(statement)
  }
  
  
  emotion <- c("ANGRY",
               "CONFUSED",
               "HAPPY",
               "CALM",
               "SAD",
               "DISGUSTED",
               "SURPRISED")
  
  added_zero_list <- lapply(count(), function(count_data) {
    emotion_list <- count_data %>%
      complete(
        max_emotion = emotion,
        people_count,
        csv_number = c(1:10),
        fill = list(count = 0)
      ) %>%
      select(csv_number, people_count, max_emotion, count) %>%
      arrange(max_emotion, csv_number) %>%
      as.data.frame()
    return(emotion_list)
  })
  added_zero_list
  
}
#overall_person_line_chart_sql(master_table)


overall_person_line_chart_graph <- function(data) {
  size <- length(data)
  plotly_chart <- vector(size, mode = "list")
  chart <- vector(size, mode = "list")
  for (index in 1:length(data)) {
    chart[[index]] <- ggplot(data = data[[index]],
                             aes(x = csv_number,
                                 y = count,
                                 group = max_emotion)) +
      geom_line(aes(color = max_emotion)) +
      geom_point(aes(color = max_emotion)) +
      scale_x_continuous(limits = c(min_csv,
                                    max_csv),
                         breaks = seq(min_csv,
                                      max_csv,
                                      1)) +
      scale_manual_color() +
      theme_text_size(axis.text.x = element_text(size = 7, face = "bold")) +
      labs(
        title = paste0(
          "THE TREND OF EMOTIONAL CHANGES OF THE STUDENT ID: 16000000",
          index,
          "\n 16000000",
          index,
          "學生編號每種情緒改變趨勢"
        ),
        x = "LESSON",
        y = "COUNT",
        color = "EMOTION"
      )
    #ylim(1,max(data[[index]]$count) + 25)
    #print(max(data[[index]]$count))
    plotly_chart[[index]] <- ggplotly(chart[[index]])
    #print(plotly_chart[[1]])
  }
  return(plotly_chart)
  
}

#overall_person_line_chart_graph(overall_person_line_chart_sql(master_table))

overall_person_line_chart_graph_save <- function(chart) {
  size <- length(chart)
  directory <-
    "~/R_server/image/sample/html/line_chart/overall_each_person_line_chart/overall_line_chart_person_"
  
  for (index in 1:length(chart)) {
    path <- paste0(directory, index, ".html")
    
    #htmltools::save_html(chart[[index]], path)
    
    htmlwidgets::saveWidget(as_widget(chart[[index]]),
                            selfcontained = TRUE,
                            file = path)
  }
  
  all <-
    "~/R_server/image/sample/html/all/overall_line_chart_person_"
  
  for (index in 1:length(chart)) {
    path <- paste0(all, index, ".html")
    
    #htmltools::save_html(chart[[index]], path)
    
    htmlwidgets::saveWidget(as_widget(chart[[index]]),
                            selfcontained = TRUE,
                            file = path)
  }
  
}

#make chart 160000000學生編號每種情緒改變趨勢
#overall_person_line_chart_graph_save(overall_person_line_chart_graph(overall_person_line_chart_sql(master_table)))


overall_gender_sentiment_comparision_sql <- function(data) {
  count_emotion_each_person_lesson <- function() {
    statement <-
      sqldf(
        "select max_emotion,count(max_emotion)as count,gender,people_count,csv_number from data
        group by max_emotion,gender,people_count,csv_number order by people_count"
      )
    return(statement)
  }
  
  sum_emotion_count <- function(data) {
    statement <-
      sqldf(
        "select max_emotion, sum(count) as sum_emotion_count,gender from data
        group by max_emotion,gender"
      )
    return(statement)
  }
  
  gender_data <-
    sum_emotion_count(count_emotion_each_person_lesson())
  return(gender_data)
}
#overall_gender_sentiment_comparision_sql(master_table)


overall_gender_sentiment_comparision_graph <- function(data) {
  cols <- c("MALE" = "#3276BF", "FEMALE" = "#EA84C3")
  
  chart <- ggplot(data, aes(x = max_emotion, y = sum_emotion_count,
                            fill = gender)) +
    geom_bar(stat = "identity", width = 0.7) +
    theme(
      axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
      axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"), angle = 10)
    ) +
    theme_text_size(axis.text.x = element_text(size = 7, face = "bold")) +
    labs(
      title = "EMOTIONS AMONG DIFFERENT GENDERS\n 不同性別的心情",
      x = "\nEMOTION",
      y = "COUNT",
      fill = "GENDER"
    ) +
    coord_flip() +
    scale_fill_manual(values = c("#3276BF", "#EA84C3"),
                      limits = c("Male", "Female"))
  
  plotly_chart <- ggplotly(chart)
  return (plotly_chart)
}
#overall_gender_sentiment_comparision_graph(overall_gender_sentiment_comparision_sql(master_table))

overall_gender_sentiment_comparision_graph_save <- function(chart) {
  directory <-
    "~/R_server/image/sample/html/histogram/overall_gender_sentiment_comparision/overall_gender_sentiment_comparision.html"
  
  # all graph file
  htmlwidgets::saveWidget(as_widget(chart),
                          selfcontained = TRUE,
                          file = directory)
  
  all <-
    "~/R_server/image/sample/html/all/overall_gender_sentiment_comparision.html"
  
  htmlwidgets::saveWidget(as_widget(chart),
                          selfcontained = TRUE,
                          file = all)
  
}

#make chart 不同性別的心情
#overall_gender_sentiment_comparision_graph_save(overall_gender_sentiment_comparision_graph(overall_gender_sentiment_comparision_sql(master_table)))

#---------comparison---------

#angry_compared (student_id)
compared_person_emotion_line_chart_angry_sql <- function(data) {
  statement <-
    sqldf(
      "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('ANGRY')
      group by student_id,csv_number order by csv_number"
    )
  return(statement)
}

#compared_person_emotion_line_chart_angry_sql(master_table)

#calm_compared (student_id)
compared_person_emotion_line_chart_calm_sql <- function(data) {
  statement <-
    sqldf(
      "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('CALM')
      group by student_id,csv_number order by csv_number"
    )
  return(statement)
}
#compared_person_emotion_line_chart_calm_sql(master_table)

#confused_compared (student_id)
compared_person_emotion_line_chart_confused_sql <- function(data) {
  statement <-
    sqldf(
      "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('CONFUSED')
      group by student_id,csv_number order by csv_number"
    )
  return(statement)
}
#compared_person_emotion_line_chart_confused_sql(master_table)

#disgust_compared (student_id)
compared_person_emotion_line_chart_disgusted_sql <- function(data) {
  statement <-
    sqldf(
      "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('DISGUSTED')
      group by student_id,csv_number order by csv_number"
    )
  return(statement)
}
#compared_person_emotion_line_chart_disgusted_sql(master_table)

#happy_compared (student_id)
compared_person_emotion_line_chart_happy_sql <- function(data) {
  statement <-
    sqldf(
      "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('HAPPY')
      group by student_id,csv_number order by csv_number"
    )
  return(statement)
}
# compared_person_emotion_line_chart_happy_sql(master_table)

#sadness_compared (student_id)
compared_person_emotion_line_chart_sad_sql <- function(data) {
  statement <-
    sqldf(
      "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('SAD')
      group by student_id,csv_number order by csv_number"
    )
  return(statement)
}
# compared_person_emotion_line_chart_sad_sql(master_table)

#surprised_compared (student_id)
compared_person_emotion_line_chart_surprised_sql <- function(data) {
  statement <-
    sqldf(
      "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('SURPRISED')
      group by student_id,csv_number order by csv_number"
    )
  return(statement)
}
#compared_person_emotion_line_chart_surprised_sql(master_table)


#emotion :add zero
compared_person_emotion_check_zero <- function(data) {
  data <- data %>%
    complete(max_emotion,
             csv_number = c(1:10),
             fill = list(count = 0),
             student_id) %>%
    select(csv_number, student_id, count, max_emotion) %>%
    arrange(csv_number, student_id) %>% #order the data frame by this column
    as.data.frame()
  return(data)
}
#compared_person_emotion_check_zero(compared_person_emotion_line_chart_surprised_sql(master_table))


compared_overall_emotion_line_chart <-
  function(data, emotion_eng, emotion_chin) {
    data$student_id <- as.factor(data$student_id)
    min <- min(data$csv_number)
    max <- max(data$csv_number)
    chart <- ggplot(data = data,
                    aes(x = csv_number,
                        y = count,
                        group = student_id)) +
      geom_line(aes(color = student_id)) +
      geom_point(aes(color = student_id)) +
      scale_x_continuous(limits = c(min,
                                    max),
                         breaks = seq(min,
                                      max,
                                      1)) +
      theme_text_size(axis.text.x = element_text(size = 7, face = "bold")) +
      labs(
        title = paste0(
          "THE TREND OF ",
          emotion_eng,
          " AMONG STUDENTS \n 學生們的",
          emotion_chin,
          "情緒趨勢"
        ),
        x = "LESSON",
        y = "COUNT",
        color = "STUDENT"
      )
    
    plotly_chart <- ggplotly(chart)
    return (plotly_chart)
  }

#chart
#compared_overall_emotion_line_chart(compared_person_emotion_check_zero(compared_person_emotion_line_chart_surprised_sql(master_table)),emotion_eng="SURPRISE",emotion_chin="驚訝")


compared_overall_emotion_line_chart_save <- function(chart, name) {
  directory <-
    paste0(
      "~/R_server/image/sample/html/line_chart/compared_overall_emotion_line_chart/compared_overall_",
      name,
      "_emotion.html"
    )
  
  #htmltools::save_html(chart, directory)
  
  htmlwidgets::saveWidget(as_widget(chart),
                          selfcontained = TRUE,
                          file = directory)
  
  
  all <-
    paste0("~/R_server/image/sample/html/all/compared_overall_",
           name,
           "_emotion.html")
  
  htmlwidgets::saveWidget(as_widget(chart),
                          selfcontained = TRUE,
                          file = all)
  
}

#make chart學生們的不同情緒趨勢
#compared_overall_emotion_line_chart_save(compared_overall_emotion_line_chart(compared_person_emotion_check_zero(compared_person_emotion_line_chart_surprised_sql(master_table)),emotion_eng="SURPRISE",emotion_chin="驚訝"),name="surprised")


#------------pie chart---------------
overall_angry_emotion_pie_chart_sql <- function(data) {
  #count the emotion of each person
  count_emotion <-
    sqldf(
      "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('ANGRY')
      group by student_id,csv_number order by csv_number"
    )
  
  #find the angriest person in each lesson
  max_angry_person <-
    sqldf(
      "select max_emotion,csv_number,max(count) as max,student_id from count_emotion
      group by csv_number order by csv_number"
    )
  
  #count the frequency of each person in all lessons
  count_the_frequency <-
    sqldf(
      "select max_emotion,csv_number,max,student_id,count(student_id) as frequency from max_angry_person
      group by student_id"
    )
  
  #find the angriest person in all lesson by using percentage
  
  #the frequency for each person  / total(frequency) *100 = precent
  count_the_frequency$percentage <-
    round(count_the_frequency$frequency / sum(count_the_frequency$frequency) *
            100,
          0)
  
  return(count_the_frequency)
}

#overall_angry_emotion_pie_chart_sql(master_table)

overall_angry_emotion_pie_chart <- function(data) {
  chart_margin <- list(
    l = 150,
    #Sets the left margin
    r = 150,
    #Sets the right margin
    b = 10,
    #Sets the bottom margin
    t = 120,
    #Sets the top margin
    pad = 20,
    #Sets the amount of padding (in px) between the plotting area and the axis lines
    autoexpand = TRUE
  )
  
  chart <-
    plot_ly(
      data,
      labels = ~ student_id,
      values = ~ percentage,
      type = 'pie'
    ) %>%    #legend
    
    add_annotations(
      text = "STUDENT",
      xref = "paper",
      yref = "paper",
      x = 0.9,
      xanchor = "left",
      y = 0.8,
      yanchor = "bottom",
      legendtitle = TRUE,
      showarrow = FALSE
    ) %>%
    layout(
      title = 'THE PERCENTAGE OF ANGER AMONG STUDENTS \n 學生們的生氣百分比\n',
      showlegend = T,
      xaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      yaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      legend = list(
        y = 0.8,
        yanchor = "top",
        x = 0.9,
        xanchor = "left"
      ),
      font = list(size = 9),
      margin = chart_margin
    )
  
  return (chart)
}
#overall_angry_emotion_pie_chart(overall_angry_emotion_pie_chart_sql(master_table))

overall_angry_emotion_pie_chart_save <- function(chart) {
  directory <-
    "~/R_server/image/sample/html/pie_chart/overall_different_emotions/overall_angry_emotion_pie_chart.html"
  
  #htmltools::save_html(chart, directory)
  
  htmlwidgets::saveWidget(as_widget(chart),
                          selfcontained = TRUE,
                          file = directory)
  
  
  all <-
    "~/R_server/image/sample/html/all/overall_angry_emotion_pie_chart.html"
  
  htmlwidgets::saveWidget(as_widget(chart),
                          selfcontained = TRUE,
                          file = all)
  
}

#save graph
#overall_angry_emotion_pie_chart_save(overall_angry_emotion_pie_chart(overall_angry_emotion_pie_chart_sql(master_table)))

overall_calm_emotion_pie_chart_sql <- function(data) {
  #count the emotion of each person
  count_emotion <-
    sqldf(
      "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('CALM')
      group by student_id,csv_number order by csv_number"
    )
  
  #find the calmest person in each lesson
  max_calm_person <-
    sqldf(
      "select max_emotion,csv_number,max(count) as max,student_id from count_emotion
      group by csv_number order by csv_number"
    )
  
  #count the frequency of each person in all lessons
  count_the_frequency <-
    sqldf(
      "select max_emotion,csv_number,max,student_id,count(student_id) as frequency from max_calm_person
      group by student_id"
    )
  
  #find the calmest person in all lesson by using percentage
  
  #frequency for each person  / total(frequency) *100 = precent
  count_the_frequency$percentage <-
    round(count_the_frequency$frequency / sum(count_the_frequency$frequency) *
            100,
          0)
  
  
  return(count_the_frequency)
}

#overall_calm_emotion_pie_chart_sql(master_table)

overall_calm_emotion_pie_chart <- function(data) {
  chart_margin <- list(
    l = 150,
    #Sets the left margin
    r = 150,
    #Sets the right margin
    b = 10,
    #Sets the bottom margin
    t = 120,
    #Sets the top margin
    pad = 20,
    #Sets the amount of padding (in px) between the plotting area and the axis lines
    autoexpand = TRUE
  )
  
  chart <-
    plot_ly(
      data,
      labels = ~ student_id,
      values = ~ percentage,
      type = 'pie'
    ) %>%    #legend
    
    add_annotations(
      text = "STUDENT",
      xref = "paper",
      yref = "paper",
      x = 0.9,
      xanchor = "left",
      y = 0.8,
      yanchor = "bottom",
      legendtitle = TRUE,
      showarrow = FALSE
    ) %>%
    layout(
      title = 'THE PERCENTAGE OF CALM AMONG STUDENTS \n 學生們的冷靜百分比\n',
      showlegend = T,
      xaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      yaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      legend = list(
        y = 0.8,
        yanchor = "top",
        x = 0.9,
        xanchor = "left"
      ),
      font = list(size = 9),
      margin = chart_margin
    )
  
  return (chart)
}
overall_calm_emotion_pie_chart(overall_calm_emotion_pie_chart_sql(master_table))

overall_calm_emotion_pie_chart_save <- function(chart) {
  directory <-
    "~/R_server/image/sample/html/pie_chart/overall_different_emotions/overall_calm_emotion_pie_chart.html"
  
  #htmltools::save_html(chart, directory)
  
  htmlwidgets::saveWidget(as_widget(chart),
                          selfcontained = TRUE,
                          file = directory)
  
  
  all <-
    "~/R_server/image/sample/html/all/overall_calm_emotion_pie_chart.html"
  
  htmlwidgets::saveWidget(as_widget(chart),
                          selfcontained = TRUE,
                          file = all)
  
}

#save graph
#overall_calm_emotion_pie_chart_save(overall_calm_emotion_pie_chart(overall_calm_emotion_pie_chart_sql(master_table)))

overall_confused_emotion_pie_chart_sql <- function(data) {
  #count the emotion of each person
  count_emotion <-
    sqldf(
      "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('CONFUSED')
      group by student_id,csv_number order by csv_number"
    )
  
  #find the most confused person in each lesson
  max_confused_person <-
    sqldf(
      "select max_emotion,csv_number,max(count) as max,student_id from count_emotion
      group by csv_number order by csv_number"
    )
  
  #count the frequency of each person in all lessons
  count_the_frequency <-
    sqldf(
      "select max_emotion,csv_number,max,student_id,count(student_id) as frequency from max_confused_person
      group by student_id"
    )
  
  # find the most confused person in all lesson by using percentage
  
  #frequency for each person  / total(frequency) *100 = precent
  count_the_frequency$percentage <-
    round(count_the_frequency$frequency / sum(count_the_frequency$frequency) *
            100,
          0)
  
  return(count_the_frequency)
}

#overall_confused_emotion_pie_chart_sql(master_table)

overall_confused_emotion_pie_chart <- function(data) {
  chart_margin <- list(
    l = 150,
    #Sets the left margin
    r = 150,
    #Sets the right margin
    b = 10,
    #Sets the bottom margin
    t = 120,
    #Sets the top margin
    pad = 20,
    #Sets the amount of padding (in px) between the plotting area and the axis lines
    autoexpand = TRUE
  )
  
  chart <-
    plot_ly(
      data,
      labels = ~ student_id,
      values = ~ percentage,
      type = 'pie'
    ) %>%    #legend
    
    add_annotations(
      text = "STUDENT",
      xref = "paper",
      yref = "paper",
      x = 0.9,
      xanchor = "left",
      y = 0.8,
      yanchor = "bottom",
      legendtitle = TRUE,
      showarrow = FALSE
    ) %>%
    layout(
      title = 'THE PERCENTAGE OF CONFUSION AMONG STUDENTS \n 學生們的迷惘百分比\n',
      showlegend = T,
      xaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      yaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      legend = list(
        y = 0.8,
        yanchor = "top",
        x = 0.9,
        xanchor = "left"
      ),
      font = list(size = 9),
      margin = chart_margin
    )
  
  return (chart)
}
#overall_confused_emotion_pie_chart(overall_confused_emotion_pie_chart_sql(master_table))

overall_confused_emotion_pie_chart_save <- function(chart) {
  directory <-
    "~/R_server/image/sample/html/pie_chart/overall_different_emotions/overall_confused_emotion_pie_chart.html"
  
  #htmltools::save_html(chart, directory)
  
  htmlwidgets::saveWidget(as_widget(chart),
                          selfcontained = TRUE,
                          file = directory)
  
  
  all <-
    "~/R_server/image/sample/html/all/overall_confused_emotion_pie_chart.html"
  
  htmlwidgets::saveWidget(as_widget(chart),
                          selfcontained = TRUE,
                          file = all)
  
}

#save graph
#overall_confused_emotion_pie_chart_save(overall_confused_emotion_pie_chart(overall_confused_emotion_pie_chart_sql(master_table)))

overall_disgusted_emotion_pie_chart_sql <- function(data) {
  #count the emotion of each person
  count_emotion <-
    sqldf(
      "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('DISGUSTED')
      group by student_id,csv_number order by csv_number"
    )
  
  #find the most disgusted person in each lesson
  max_disgusted_person <-
    sqldf(
      "select max_emotion,csv_number,max(count) as max,student_id from count_emotion
      group by csv_number order by csv_number"
    )
  
  #count the frequency of each person in all lessons
  count_the_frequency <-
    sqldf(
      "select max_emotion,csv_number,max,student_id,count(student_id) as frequency from max_disgusted_person
      group by student_id"
    )
  
  #the most disgusted person in all lessons byusing percentage
  
  #frequency for each person  / total(frequency) *100 = precent
  count_the_frequency$percentage <-
    round(count_the_frequency$frequency / sum(count_the_frequency$frequency) *
            100,
          0)
  
  return(count_the_frequency)
}

#overall_disgusted_emotion_pie_chart_sql(master_table)

overall_disgusted_emotion_pie_chart <- function(data) {
  chart_margin <- list(
    l = 150,
    #Sets the left margin
    r = 150,
    #Sets the right margin
    b = 10,
    #Sets the bottom margin
    t = 120,
    #Sets the top margin
    pad = 20,
    #Sets the amount of padding (in px) between the plotting area and the axis lines
    autoexpand = TRUE
  )
  
  chart <-
    plot_ly(
      data,
      labels = ~ student_id,
      values = ~ percentage,
      type = 'pie'
    ) %>%    #legend
    
    add_annotations(
      text = "STUDENT",
      xref = "paper",
      yref = "paper",
      x = 0.9,
      xanchor = "left",
      y = 0.8,
      yanchor = "bottom",
      legendtitle = TRUE,
      showarrow = FALSE
    ) %>%
    layout(
      title = 'THE PERCENTAGE OF DISGUST AMONG STUDENTS \n 學生們的厭惡百分比\n',
      showlegend = T,
      xaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      yaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      legend = list(
        y = 0.8,
        yanchor = "top",
        x = 0.9,
        xanchor = "left"
      ),
      font = list(size = 9),
      margin = chart_margin
    )
  
  return (chart)
}
#overall_disgusted_emotion_pie_chart(overall_disgusted_emotion_pie_chart_sql(master_table))

overall_disgusted_emotion_pie_chart_save <- function(chart) {
  directory <-
    "~/R_server/image/sample/html/pie_chart/overall_different_emotions/overall_disgusted_emotion_pie_chart.html"
  
  #htmltools::save_html(chart, directory)
  
  htmlwidgets::saveWidget(as_widget(chart),
                          selfcontained = TRUE,
                          file = directory)
  
  
  all <-
    "~/R_server/image/sample/html/all/overall_disgusted_emotion_pie_chart.html"
  
  htmlwidgets::saveWidget(as_widget(chart),
                          selfcontained = TRUE,
                          file = all)
  
}

#save graph
#overall_disgusted_emotion_pie_chart_save(overall_disgusted_emotion_pie_chart(overall_disgusted_emotion_pie_chart_sql(master_table)))


overall_happy_emotion_pie_chart_sql <- function(data) {
  #count the emotion of each person
  count_emotion <-
    sqldf(
      "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('HAPPY')
      group by student_id,csv_number order by csv_number"
    )
  
  #find the happiest person in each lesson
  max_happy_person <-
    sqldf(
      "select max_emotion,csv_number,max(count) as max,student_id from count_emotion
      group by csv_number order by csv_number"
    )
  
  #count the frequency of each person in all lessons
  count_the_frequency <-
    sqldf(
      "select max_emotion,csv_number,max,student_id,count(student_id) as frequency from max_happy_person
      group by student_id"
    )
  
  #the happiest person in all lessons by using percentage
  
  #frequency for each person  / total(frequency) *100 = precent
  count_the_frequency$percentage <-
    round(count_the_frequency$frequency / sum(count_the_frequency$frequency) *
            100,
          0)
  
  return(count_the_frequency)
}

#overall_happy_emotion_pie_chart_sql(master_table)

overall_happy_emotion_pie_chart <- function(data) {
  chart_margin <- list(
    l = 150,
    #Sets the left margin
    r = 150,
    #Sets the right margin
    b = 10,
    #Sets the bottom margin
    t = 120,
    #Sets the top margin
    pad = 20,
    #Sets the amount of padding (in px) between the plotting area and the axis lines
    autoexpand = TRUE
  )
  
  chart <-
    plot_ly(
      data,
      labels = ~ student_id,
      values = ~ percentage,
      type = 'pie'
    ) %>%    #legend
    
    add_annotations(
      text = "STUDENT",
      xref = "paper",
      yref = "paper",
      x = 0.9,
      xanchor = "left",
      y = 0.8,
      yanchor = "bottom",
      legendtitle = TRUE,
      showarrow = FALSE
    ) %>%
    layout(
      title = 'THE PERCENTAGE OF HAPPINESS AMONG STUDENTS \n 學生們的開心百分比\n',
      showlegend = T,
      xaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      yaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      legend = list(
        y = 0.8,
        yanchor = "top",
        x = 0.9,
        xanchor = "left"
      ),
      font = list(size = 9),
      margin = chart_margin
    )
  
  return (chart)
}
#overall_happy_emotion_pie_chart(overall_happy_emotion_pie_chart_sql(master_table))

overall_happy_emotion_pie_chart_save <- function(chart) {
  directory <-
    "~/R_server/image/sample/html/pie_chart/overall_different_emotions/overall_happy_emotion_pie_chart.html"
  
  #htmltools::save_html(chart, directory)
  
  htmlwidgets::saveWidget(as_widget(chart),
                          selfcontained = TRUE,
                          file = directory)
  
  
  all <-
    "~/R_server/image/sample/html/all/overall_happy_emotion_pie_chart.html"
  
  htmlwidgets::saveWidget(as_widget(chart),
                          selfcontained = TRUE,
                          file = all)
  
}

#save graph
#overall_happy_emotion_pie_chart_save(overall_happy_emotion_pie_chart(overall_happy_emotion_pie_chart_sql(master_table)))

overall_sad_emotion_pie_chart_sql <- function(data) {
  #count the emotion of each person
  count_emotion <-
    sqldf(
      "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('SAD')
      group by student_id,csv_number order by csv_number"
    )
  
  #find the saddest person in each lesson
  max_sad_person <-
    sqldf(
      "select max_emotion,csv_number,max(count) as max,student_id from count_emotion
      group by csv_number order by csv_number"
    )
  
  #count the frequency of each person in all lessons
  count_the_frequency <-
    sqldf(
      "select max_emotion,csv_number,max,student_id,count(student_id) as frequency from max_sad_person
      group by student_id"
    )
  
  # find the saddest person in all lessons by using percentage
  
  #frequency for each person  / total(frequency) *100 = precent
  count_the_frequency$percentage <-
    round(count_the_frequency$frequency / sum(count_the_frequency$frequency) *
            100,
          0)
  
  return(count_the_frequency)
}

#overall_sad_emotion_pie_chart_sql(master_table)

overall_sad_emotion_pie_chart <- function(data) {
  chart_margin <- list(
    l = 150,
    #Sets the left margin
    r = 150,
    #Sets the right margin
    b = 10,
    #Sets the bottom margin
    t = 120,
    #Sets the top margin
    pad = 20,
    #Sets the amount of padding (in px) between the plotting area and the axis lines
    autoexpand = TRUE
  )
  
  chart <-
    plot_ly(
      data,
      labels = ~ student_id,
      values = ~ percentage,
      type = 'pie'
    ) %>%    #legend
    
    add_annotations(
      text = "STUDENT",
      xref = "paper",
      yref = "paper",
      x = 0.9,
      xanchor = "left",
      y = 0.8,
      yanchor = "bottom",
      legendtitle = TRUE,
      showarrow = FALSE
    ) %>%
    layout(
      title = 'THE PERCENTAGE OF SADNESS AMONG STUDENTS \n 學生們的傷心百分比\n',
      showlegend = T,
      xaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      yaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      legend = list(
        y = 0.8,
        yanchor = "top",
        x = 0.9,
        xanchor = "left"
      ),
      font = list(size = 9),
      margin = chart_margin
    )
  
  return (chart)
}
#overall_sad_emotion_pie_chart(overall_sad_emotion_pie_chart_sql(master_table))

overall_sad_emotion_pie_chart_save <- function(chart) {
  directory <-
    "~/R_server/image/sample/html/pie_chart/overall_different_emotions/overall_sad_emotion_pie_chart.html"
  
  #htmltools::save_html(chart, directory)
  
  htmlwidgets::saveWidget(as_widget(chart),
                          selfcontained = TRUE,
                          file = directory)
  
  
  all <-
    "~/R_server/image/sample/html/all/overall_sad_emotion_pie_chart.html"
  
  htmlwidgets::saveWidget(as_widget(chart),
                          selfcontained = TRUE,
                          file = all)
  
}

#save graph
#overall_sad_emotion_pie_chart_save(overall_sad_emotion_pie_chart(overall_sad_emotion_pie_chart_sql(master_table)))

overall_surprise_emotion_pie_chart_sql <- function(data) {
  #count the emotion of each person
  count_emotion <-
    sqldf(
      "select max_emotion,csv_number,count(max_emotion_value)as count,student_id from data where max_emotion in('SURPRISED')
      group by student_id,csv_number order by csv_number"
    )
  
  #find the most surprised person in each lesson
  max_surprise_person <-
    sqldf(
      "select max_emotion,csv_number,max(count) as max,student_id from count_emotion
      group by csv_number order by csv_number"
    )
  
  #count the frequency of each person in all lessons
  count_the_frequency <-
    sqldf(
      "select max_emotion,csv_number,max,student_id,count(student_id) as frequency from max_surprise_person
      group by student_id"
    )
  
  # find the most surprised person in all lessons by using percentage
  
  #frequency for each person  / total(frequency) *100 = precent
  count_the_frequency$percentage <-
    round(count_the_frequency$frequency / sum(count_the_frequency$frequency) *
            100,
          0)
  
  return(count_the_frequency)
}

#overall_surprise_emotion_pie_chart_sql(master_table)

overall_surprise_emotion_pie_chart <- function(data) {
  chart_margin <- list(
    l = 150,
    #Sets the left margin
    r = 150,
    #Sets the right margin
    b = 10,
    #Sets the bottom margin
    t = 120,
    #Sets the top margin
    pad = 20,
    #Sets the amount of padding (in px) between the plotting area and the axis lines
    autoexpand = TRUE
  )
  
  chart <-
    plot_ly(
      data,
      labels = ~ student_id,
      values = ~ percentage,
      type = 'pie'
    ) %>%    #legend
    
    add_annotations(
      text = "STUDENT",
      xref = "paper",
      yref = "paper",
      x = 0.9,
      xanchor = "left",
      y = 0.8,
      yanchor = "bottom",
      legendtitle = TRUE,
      showarrow = FALSE
    ) %>%
    layout(
      title = 'THE PERCENTAGE OF SURPRISE AMONG STUDENTS \n 學生們的驚訝百分比\n',
      showlegend = T,
      xaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      yaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      legend = list(
        y = 0.8,
        yanchor = "top",
        x = 0.9,
        xanchor = "left"
      ),
      font = list(size = 9),
      margin = chart_margin
    )
  
  return (chart)
}
#overall_surprise_emotion_pie_chart(overall_surprise_emotion_pie_chart_sql(master_table))

overall_surprise_emotion_pie_chart_save <- function(chart) {
  directory <-
    "~/R_server/image/sample/html/pie_chart/overall_different_emotions/overall_surprise_emotion_pie_chart.html"
  
  #htmltools::save_html(chart, directory)
  
  htmlwidgets::saveWidget(as_widget(chart),
                          selfcontained = TRUE,
                          file = directory)
  
  
  all <-
    "~/R_server/image/sample/html/all/overall_surprise_emotion_pie_chart.html"
  
  htmlwidgets::saveWidget(as_widget(chart),
                          selfcontained = TRUE,
                          file = all)
  
}

#save graph
#overall_surprise_emotion_pie_chart_save(overall_surprise_emotion_pie_chart(overall_surprise_emotion_pie_chart_sql(master_table)))