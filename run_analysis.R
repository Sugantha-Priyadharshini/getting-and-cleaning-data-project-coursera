message("Trying to load data files in R...")

## Creates the list with the instructions needed by 'read.table()'
read.table_instructions <- list(
  # The first object is a list with name 'file'
  # that contains values for 'file' argument,
  # which indicates the path of each file.
  file = list(
    activity_labels = "UCI HAR Dataset/activity_labels.txt",
    features = "UCI HAR Dataset/features.txt",
    subject_train = "UCI HAR Dataset/train/subject_train.txt",
    y_train = "UCI HAR Dataset/train/y_train.txt",
    X_train = "UCI HAR Dataset/train/X_train.txt",
    subject_test = "UCI HAR Dataset/test/subject_test.txt",
    y_test = "UCI HAR Dataset/test/y_test.txt",
    X_test = "UCI HAR Dataset/test/X_test.txt"
  ),
  # The second object is a list with name 'colClasses'
  # that contains the values for 'colClasses' argument
  # that indicates the classes of all variables in each file.
  # It is supplied to correctly identify column classes,
  # and as side effect also improves reading speed.
  colClasses = list(
    activity_labels = c("integer", "character"),
    features = c("integer", "character"),
    subject_train = "integer",
    y_train = "integer",
    X_train = rep("numeric", 561),
    subject_test = "integer",
    y_test = "integer",
    X_test = rep("numeric", 561)
  ),
  # The third object is a list with name 'nrows'
  # that contains the values for 'nrows' argument
  # that indicates the number of rows to read in each file.
  # It is supplied for speed.
  nrows = list(
    activity_labels = 6,
    features = 561,
    subject_train = 7352,
    y_train = 7352,
    X_train = 7352,
    subject_test = 2947,
    y_test = 2947,
    X_test = 2947
  )
)

## Uses the instructions created above to load all needed data with 'Map()'.
## For each file the correct arguments are supplied to function 'read.table()',
## as well as some extra, common arguments for all files.
## Function 'with()' is used for clearer code.
data_files <- with(read.table_instructions,
                   Map(read.table,
                       file = file, colClasses = colClasses, nrows = nrows,
                       quote = "", comment.char = "",
                       stringsAsFactors = FALSE))

message("    ...data files were successfully loaded into R, \n",
        "       in the list with name 'data_files'.")




################################################################################
# STEP 1: Merges the training and the test sets to create one data set.
################################################################################

## Plan: 1. Bind the files of the train set together by columns.
##       2. Bind the files of the test set together by columns.
##       3. Bind the data frames created for test and train set
##          into one large dataset by rows.

## Merges the train and test sets
merged_data <- with(data_files,
                    rbind(cbind(subject_train, y_train, X_train),
                          cbind(subject_test,  y_test,  X_test)))




################################################################################
# STEP 2: Extracts only the measurements on the mean and standard deviation
#         for each measurement.
################################################################################

## Plan: 1. Find the target feature indexes which are the features
##          with names that contain either the string 'mean()' or 'std()',
##          from the data frame 'features'.
##       2. Add 2 to each index to adjust for the two extra column
##          in the beginning of the merged data frame, 'subject' and 'activity',
##          to create a vector with all target variables indexes.
##       3. Extract only the target variables from the merged data frame.

## Finds the target features indexes from the 'features' data frame,
## by searching for matches with pattens 'mean()' or 'std()'
target_features_indexes <- grep("mean\\(\\)|std\\(\\)",
                                data_files$features[[2]])

## Add 2 to each index to adjust for the first 2 column we have bind
## that should also be included
target_variables_indexes <- c(1, 2, # the first two columns that refer to
                              # 'subject' and 'activity'
                              # should be included
                              # adds 2 to correct the indexes
                              # of target features indexes because of
                              # the 2 extra columns we have included
                              target_features_indexes + 2)

## Extracts the target variables to create the target data frame
target_data <- merged_data[ , target_variables_indexes]




################################################################################
# STEP 3: Uses descriptive activity names to name the activities in the data set
################################################################################

## Replace activity values with a factor based on levels and labels
## contained in the activity_labels data file.
target_data[[2]] <- factor(target_data[[2]],
                           levels = data_files$activity_labels[[1]],
                           labels = data_files$activity_labels[[2]])




################################################################################
# STEP 4: Appropriately labels the data set with descriptive variable names.
################################################################################

## Plan: 1. Extracts the target variable names from 'features',
##          with the use of the target features indexes created in STEP 2
##       2. Corrects a typo that exists in some feature names
##       3. Create a new tidy dataset with the appropriate labels
##          for the variable names

## Extract the target variables names
descriptive_variable_names <- data_files$features[[2]][target_features_indexes]

## Correct a typo
descriptive_variable_names <- gsub(pattern = "BodyBody", replacement = "Body",
                                   descriptive_variable_names)

## Create a tidy data set with appropriate labels for the variable names
tidy_data <- target_data
names(tidy_data) <- c("subject", "activity", descriptive_variable_names)




################################################################################
# STEP 5: From the data set in step 4, creates a second, independent
#         tidy data set with the average of each variable
#         for each activity and each subject.
################################################################################

## Plan: 1. Group the tidy data table created in step 4,
##          by 'subject' and 'activity'
##       2. Summarize each variable to find the mean for the grouped values
##       3. Ungroup the data table
##       4. Add descriptive names to the variables of the new tidy data table
##       5. Write the data in a text file in the present working directory

## Create a dataset with the mean of each column for 'subject' and 'activity'
tidy_data_summary <- tidy_data %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean)) %>%
  ungroup()

## Replace the variable names of 'tidy_data_summary' with new descriptive ones.
## Just the prefix "Avrg-" will be added in all variable names,
## except the first two, 'subject' and 'activity'.
new_names_for_summary <- c(names(tidy_data_summary[c(1,2)]),
                           paste0("Avrg-", names(tidy_data_summary[-c(1, 2)])))
names(tidy_data_summary) <- new_names_for_summary

## Save the data frame created as a text file in working directory
write.table(tidy_data_summary, "tidy_data_summary.txt", row.names = FALSE)

message("The script 'run_analysis.R was executed successfully. \n",
        "As a result, a new tidy data set was created with name \n", 
        "'tidy_data_summary.txt' in the working directory.")

