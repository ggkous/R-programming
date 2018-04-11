source("~/R_server/big_dataset_table.R")

#big dataset
# master_table <-
#   emotion_table(rename_dataset(student_id(data = combine_csv())))

#histogram

#一名學生所有堂數的情緒表現: 16000000"
overall_personal_save(overall_personal_ggplot_graph(overall_personal_emotion_sql(master_table)))


#不同性別的心情
overall_gender_sentiment_comparision_graph_save(overall_gender_sentiment_comparision_graph(overall_gender_sentiment_comparision_sql(master_table)))



#line chart

#總體所有學生情緒趨勢

overall_line_chart_save(overall_line_chart(bigdataset_emotion_check_zero(overall_line_chart_sql(master_table))))


#總體所有學生生氣的情緒趨勢
each_emotion_line_chart_save(
  each_emotion_line_chart(
    angry_check_zero(each_emotion_line_chart_angry_sql(master_table)),
    emotion_eng = "ANGER",
    emotion_chin = "生氣"
  ),
  name = "angry"
)


#總體所有學生冷靜的情緒趨勢
each_emotion_line_chart_save(
  each_emotion_line_chart(
    calm_check_zero(each_emotion_line_chart_calm_sql(master_table)),
    emotion_eng = "CALM",
    emotion_chin = "冷靜"
  )
  ,
  name = "calm"
)

#總體所有學生迷惘的情緒趨勢
each_emotion_line_chart_save(
  each_emotion_line_chart(
    confused_check_zero(each_emotion_line_chart_confused_sql(master_table)),
    emotion_eng = "CONFUSION",
    emotion_chin = "迷惘"
  )
  ,
  name = "confused"
)

#總體所有學生厭惡的情緒趨勢
each_emotion_line_chart_save(
  each_emotion_line_chart(
    disgusted_check_zero(each_emotion_line_chart_disgusted_sql(master_table)),
    emotion_eng = "DISGUST",
    emotion_chin = "厭惡"
  )
  ,
  name = "disgusted"
)

#總體所有學生開心的情緒趨勢
each_emotion_line_chart_save(
  each_emotion_line_chart(
    happy_check_zero(each_emotion_line_chart_happy_sql(master_table)),
    emotion_eng = "HAPPINESS",
    emotion_chin = "開心"
  )
  ,
  name = "happy"
)
#總體所有學生傷心的情緒趨勢
each_emotion_line_chart_save(
  each_emotion_line_chart(
    sad_check_zero(each_emotion_line_chart_sad_sql(master_table)),
    emotion_eng = "SADNESS",
    emotion_chin = "傷心"
  )
  ,
  name = "sad"
)

#總體所有學生驚訝的情緒趨勢
each_emotion_line_chart_save(
  each_emotion_line_chart(
    surprised_check_zero(each_emotion_line_chart_surprised_sql(master_table)),
    emotion_eng = "SURPRISE",
    emotion_chin = "驚訝"
  )
  ,
  name = "surprised"
)

#160000000學生編號每種情緒改變趨勢
overall_person_line_chart_graph_save(overall_person_line_chart_graph(overall_person_line_chart_sql(master_table)))


#comparison

#學生們的不同情緒趨勢
#angry
compared_overall_emotion_line_chart_save(compared_overall_emotion_line_chart(compared_person_emotion_check_zero(compared_person_emotion_line_chart_angry_sql(master_table)),emotion_eng="ANGER",emotion_chin="生氣"),name="angry")
#calm
compared_overall_emotion_line_chart_save(compared_overall_emotion_line_chart(compared_person_emotion_check_zero(compared_person_emotion_line_chart_calm_sql(master_table)),emotion_eng="CALM",emotion_chin="冷靜"),name="calm")
#confusion
compared_overall_emotion_line_chart_save(compared_overall_emotion_line_chart(compared_person_emotion_check_zero(compared_person_emotion_line_chart_confused_sql(master_table)),emotion_eng="CONFUSION",emotion_chin="迷惘"),name="confused")
#disgusted
compared_overall_emotion_line_chart_save(compared_overall_emotion_line_chart(compared_person_emotion_check_zero(compared_person_emotion_line_chart_disgusted_sql(master_table)),emotion_eng="DISGUST",emotion_chin="厭惡"),name="disgusted")
#happiness
compared_overall_emotion_line_chart_save(compared_overall_emotion_line_chart(compared_person_emotion_check_zero(compared_person_emotion_line_chart_happy_sql(master_table)),emotion_eng="HAPPINESS",emotion_chin="開心"),name="happy")
#sadness
compared_overall_emotion_line_chart_save(compared_overall_emotion_line_chart(compared_person_emotion_check_zero(compared_person_emotion_line_chart_sad_sql(master_table)),emotion_eng="SADNESS",emotion_chin="傷心"),name="sad")
#surprised
compared_overall_emotion_line_chart_save(compared_overall_emotion_line_chart(compared_person_emotion_check_zero(compared_person_emotion_line_chart_surprised_sql(master_table)),emotion_eng="SURPRISE",emotion_chin="驚訝"),name="surprised")


#pie chart

#the angriest student
overall_angry_emotion_pie_chart_save(overall_angry_emotion_pie_chart(overall_angry_emotion_pie_chart_sql(master_table)))

#the calmest student
overall_calm_emotion_pie_chart_save(overall_calm_emotion_pie_chart(overall_calm_emotion_pie_chart_sql(master_table)))

#the most confused student
overall_confused_emotion_pie_chart_save(overall_confused_emotion_pie_chart(overall_confused_emotion_pie_chart_sql(master_table)))

#the most disgusted student
overall_disgusted_emotion_pie_chart_save(overall_disgusted_emotion_pie_chart(overall_disgusted_emotion_pie_chart_sql(master_table)))

#the happiest student
overall_happy_emotion_pie_chart_save(overall_happy_emotion_pie_chart(overall_happy_emotion_pie_chart_sql(master_table)))

#the saddest student
overall_sad_emotion_pie_chart_save(overall_sad_emotion_pie_chart(overall_sad_emotion_pie_chart_sql(master_table)))

#the most surprised student
overall_surprise_emotion_pie_chart_save(overall_surprise_emotion_pie_chart(overall_surprise_emotion_pie_chart_sql(master_table)))