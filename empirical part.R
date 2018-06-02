cl16 = read.csv("D:\\3 курс\\course_paper3\\final\\final_database_02.07.17\\cl16.csv")
cl16$per_point_diff = gsub("%", "", paste(cl16$per_point_diff))
cl16$per_point_diff = as.numeric(cl16$per_point_diff)
hist(cl16$per_point_diff, breaks = 20)
min(cl16$per_point_diff)
#0.04
max(cl16$per_point_diff)
#91.64
hist(cl16$per_point_diff, breaks = 35,
     main = "Distribution of differences in voting \n for rep and dem in 2016 in every county (%)",
     ylab = "Frequency",
     xlab = "Differences in per cent \n for all counties",
     col = "grey"
     )
h = hist(cl16$per_point_diff, breaks = 35,
         main = "Distribution of differences in voting \n for rep and dem in 2016 in every county (%)",
         ylab = "Frequency",
         xlab = "Differences in per cent \n for all counties",
         col = "grey"
)
sd(cl16$per_point_diff)
#20.8244
mean(cl16$per_point_diff)
#39.01085
mode(cl16$per_point_diff)
median(cl16$per_point_diff)
#40.05
boxplot(cl16$per_point_diff)

quantile(cl16$per_point_diff) #quartile
#0%   25%   50%   75%  100% 
#0.04 21.82 40.05 55.36 91.64

quantile(cl16$per_point_diff, seq(0, 1, 0.2)) # find quintiles
#0%   20%   40%   60%   80%  100% 
#0.04 17.63 33.42 46.66 58.31 91.64 

findInterval(cl16$per_point_diff, decile, all.inside = TRUE) # how to find out to which quantile the value belongs

decile = quantile(cl16$per_point_diff, prob = seq(0, 1, length = 11), type = 5) #decile
#0%       10%     20%     30%     40%     50%     60%     70%     80%     90%     100% 
#0.040    9.646   17.627  25.678  33.420  40.050  46.660  52.340  58.310  66.338  91.640 

#create 40 bins for our data
xfit = seq(min(cl16$per_point_diff), max(cl16$per_point_diff), length = 40)

#find the normal distribution
yfit = dnorm(xfit, mean = mean(cl16$per_point_diff), sd=sd(cl16$per_point_diff))
yfit = yfit*diff(h$mids[1:2])*length(cl16$per_point_diff)
lines(xfit, yfit)
