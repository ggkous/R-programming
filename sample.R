source("~/R_server/library.R")
#ctrl+a        ;Reformatting Source Code
#ctrl+shift+a  ;Reformatting Source Code



#set directory
setwd("~/R_server")

#check directory
getwd()

read_data <- function() {
  #set directory
  path = "~/R_server/sample_data/"
  
  #find the directory 
  filenames <- list.files(path)
  
  #change position,start with 1
  filenames <- mixedsort(sort(filenames))
  
  #read csv
  csv <-
    lapply(filenames, function(filenames)
      read.csv(paste0(path, filenames), sep = ",", header = TRUE))
  
  return(csv)
}

rename_dataset <- function(data) {
  #col rename
  new_dataset <- lapply(data, function(data)
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
      student_id = data$student_id,
      date = data$date
    ))
  
  return(new_dataset)
}

#read each table
rename_dataset(read_data())

emotion_table <- function(data) {
  max_emotion_table <- lapply(data, function(data)
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
    ))
  
  max_emotion_table <<-
    lapply(max_emotion_table, function(max_emotion_table)
      max_emotion_table[, -c(5:10)])
  
  return(max_emotion_table)
}




#emotion_table
emotion_table(rename_dataset(read_data()))



personal_emotion_sql <- function(data) {
  table <- mapply(function(data, people_count) {
    statement <-
      sqldf(
        paste0(
          "select max_emotion,max_emotion_value,count(max_emotion_value)as count from data where people_count = ",
          people_count,
          " group by people_count,max_emotion"
        )
      )
  },
  data, 1,
  SIMPLIFY = FALSE)
  
  return(table)
  
}

personal_emotion_sql(max_emotion_table)


histomgram_bar <- function(title, x, y, fill) {
  same = list(
    geom_bar(stat = "identity", width = 0.6),
    coord_cartesian(ylim = c(0, 100)),
    labs(
      title = title,
      x = x,
      y = y,
      fill = fill
    )
  )
}

scale_fill_color <- function() {
 
  color = list(scale_fill_manual(
    
    values = c(
      "#F8766D",
      "#C49A00",
      "#53B400",
      "#00C094",
      "#00B6EB",
      "#A58AFF",
      "#FB61D7"
    ),
    limits = c(
      "ANGRY",
      "CONFUSED",
      "HAPPY",
      "CALM",
      "SAD",
      "DISGUSTED",
      "SURPRISED"
    )
  ))
}


theme_text_size <- function(axis.text.x) {
  theme(
    rect = element_rect(fill = "transparent"),
    panel.background = element_rect(fill = "transparent"),
    plot.title = element_text(hjust = 0.5, size = rel(1.8)),
    axis.text = element_text(size = rel(1)),
    axis.text.x = axis.text.x,
    axis.text.y = element_text(size = rel(1.5), face = "bold"),
    axis.title.y = element_text(size = rel(1.5)),
    axis.title.x = element_text(size = rel(1.5)),
    legend.title = element_text(size = rel(1.5)),
    legend.text = element_text(size = rel(1.5)),
    plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")
  )
}

personal_ggplot_graph <- function(data) {
  size <- length(data)
  chart <- vector(size, mode = "list")
  plotly_chart <- vector(size, mode = "list")
  for (index in 1:length(data)) {
    chart[[index]] <- ggplot(data = data[[index]],
                             aes(x = max_emotion,
                                 y = max_emotion_value,
                                 fill = max_emotion)) +
      histomgram_bar(
        title = "THE PERFORMANCE OF SENTIMENT OF A STUDENT IN EACH LESSON \n 一名學生每堂的情緒表現",
        x = "EMOTION",
        y = "COUNT",
        fill = "EMOTION"
      ) +
      scale_fill_color() +
      theme_text_size(axis.text.x = element_text(size = rel(1.5), face = "bold"))
    plotly_chart[[index]] <- ggplotly(chart[[index]])
    print(plotly_chart[[2]])
  }
  return(plotly_chart)
}
personal_ggplot_graph(personal_emotion_sql(max_emotion_table))

personal_save <- function (chart) {
  size <- length(chart)
  print_graph <- vector(size, mode = "list")
  
  
  directory <-
    "~/R_server/image/sample/html/personal_emotion_distribution/each_person_each_lesson/personal_"
  
  for (index in 1:length(chart)) {
    path <- paste0(directory, index, ".html")
    
    htmltools::save_html(chart[[index]], path)
    
  }
}

personal_save(personal_ggplot_graph(personal_emotion_sql(max_emotion_table)))
