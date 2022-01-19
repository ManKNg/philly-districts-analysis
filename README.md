# philly-districts-analysis

An interactive tool to help you explore the many school districts in Philadelphia and how socio-economic factors might play an impact on the number of suspensions in Philadelphia. Included in this application are an interactive map where you can find information on average attendance rate and various socio-economic factors associated with each Philadelphia zip code and a regression model that will allow you to predict the number of suspensions based on the socio-economics factors in a school district.

This analysis focus on what a potential parent(s) or end-user might want to consider before enrolling their child into a school district in the city of Philadelphia. Using the provided Philadelphia school district dataset, I created an interactive map and regression model for users to explore each school districts in the Philadelphia area and how socio-economic factors could/could not impact the number of suspensions in Philadelphia. The map data is based on aggregated average attendance, low-income family % and number of suspensions for all school levels in each zip code, whereas the regression models is created from the raw Philly_schools.csv dataset.

The primary purpose of the map is to provide end-users a quick and dirty aggregated assessment for which districts in Philadelphia have the highest number of suspensions. If an area has a high number of suspensions, then it is very likely that the current students in that area might have a high chance of exposure to crime or other negative influences which might cause this increase in number of suspensions. If I was a parent, I might think twice about enrolling my child in that zip code.

The second portion of this app focuses on the relationship on how various socio-economic variables might/might not be a factor for influencing the number of suspensions in Philadelphia. Users can select the following variables: student’s attendance rate, low-income family rate, number of drugs infractions per 100 students, number of withdrawals, teacher attendance rate, number of students receiving gifted education and/or average teacher salary to measure how these independent factors could impact the number of suspensions. Based on these variables, it seems that attendance %, drugs infractions and teachers’ attendance are statistically significant (by conventional measures of statistical significance alpha = 0.05) on influencing the number of suspensions in Philadelphia. Outside of this it is interesting of note that a high rate of teachers’ attendance could lead to a decrease in the number of suspension (perhaps, implying a greater degree of student-teacher interactions).

Link to Webpage - https://mankng.shinyapps.io/final_project/
