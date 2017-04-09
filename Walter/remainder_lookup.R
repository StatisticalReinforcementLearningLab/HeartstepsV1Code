## Remaining time = 1:136
## Current.hour is a function of the remaining time
## Current.state = 1
## Current.run.length = 1:136

time.step = 1:136
hour = (floor(time.steps/12)+14)%%24
current.run.length = 1:91

remainder.lookup = matrix(nrow = length(time.step), ncol = length(current.run.length))

for (i in 1:length(time.step)) {
  for (j in 1:length(current.run.length)) {
    remainder.lookup[i,j] = full.remainder.fn(remaining.time = time.step[i], current.state = 1, current.run.length[j], current.hour = hour[i], eta = 0.0)    
  }
}

write.table(remainder.lookup, "/Volumes/dav/HeartSteps/Walter/remainder_lookup.csv", sep = ",")
  