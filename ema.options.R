## options for EMA and research questions

## How hectic was your day today? 
ema1 <- c("Not all hectic" = 1, 2, 3, 4, "Very hectic" = 5)

## How stressful was your day today? 
ema2 <- c("Not at all stressful" = 1, 2, 3, 4, "Very stressful" = 5)

## How typical was today for a [Monday or Tuesday or Wednesday ... ]? 
ema3 <- c("Not at all typical" = 1, 2, 3, 4, "Completely typical" = 5)

## Did you do any of the following today? (choose all that apply)
ema4 <- c("cardio" = "Cardio exercise (running, swimming...)",
          "strength" = "Strength training (weights...)",
          "flex" = "Flexibility training (yoga, pilates...)",
          "housework" = "Heavy housework (scrubbing bathtub...)",
          "none" = "None of the above")

## At [time], you received the suggestion [suggestion text] and rated it
## thumbs-down. Why did you rate it this way? (check all that apply)
## nb: there is an 'other' option
ema6 <-
  c("motivate" = "The message did not motivate me to be active",
    "action" = "The message was not sufficiently actionable",
    "difficult" = "The suggested activity was too difficult to carry out",
    "doable" = "The suggested activity was not doable when the message arrived",
    "time" =
      "The suggestion came at a bad time (e.g. I was too busy, too stressed)",
    "active" = "The suggestion came too soon after I was last active")

## At [time], you received the suggestion [suggestion text] and rated it
## thumbs-up. Why did you rate it this way? (check all that apply) 
## nb: there is an 'other' option
ema7 <-
  c("motivate" = "The message motivated me to be active",
    "easy" = "The suggested activity was easy to carry out",
    "doable" = "The suggested activity was doable when it arrived",
    "interest" = "The message piqued my interest",
    "feel" = "The message made me feel good about working on my health")

## Did any of the following make it difficult for you to be active today?
## (choose all that apply) 
## nb: there is an 'other' option
research1 <- c("weather" = "Poor weather",
               "busy" = "No time/too busy",
               "place" = "No place to be active",
               "ill" = "Illness or injury",
               "sore" = "Sore muscles",
               "social" = "Social or family",
               "traffic" = "Traffic safety",
               "personal" = "Personal safety",
               "none" = "None of the above")

## Did any of the following make it easier for you to be active today?
## (choose all that apply) 
## nb: there is an 'other' option
research2 <- c("joined" = "Others joined me",
               "encourage" = "Others encouraged me",
               "weather" = "Good weather",
               "scheduled" = "I scheduled it in",
               "facilities" = "Facilities/exercise equipment",
               "location" = "Location/scenery",
               "none" = "None of the above")

## How energetic did you feel today? 
research3 <- c("Not at all" = 1, 2, 3, 4, "Very energetic" = 5)

## At least once today I felt an urge to get up and take a walk
research4 <- c("Strongly disagree" = 1, 2, 3, 4, "Strongly agree" = 5)
