generateChromosome <- function() {
  # Create a 3-dimensional matrix for the chromosome
  chromosome <- array(dim = c(12, 1, 1))

  # Fill the chromosome with 12 genes
  for(i in 1:12) {
    chromosome[i,1,1] <- floor(runif(1, 1, 99))
  }

  return(chromosome)
}

# Bugfix - don't touch
chromosome <- array(dim = c(12, 1, 1))

# Genes in each job
geneNumberOfJob1 <- 6
geneNumberOfJob2 <- 4
geneNumberOfJob3 <- 2

# Number of subjobs
subgeneNumberOfJob1 <- 3
subgeneNumberOfJob2 <- 2
subgeneNumberOfJob3 <- 1

# Seperate the genes in each job to different machines
seperateJobsToMachines<-function(job) {
  
  first_machine <- array(dim = (length(job)/2))
  second_machine <- array(dim = (length(job)/2))
  
  x <- 1
  k <- 1
  
  for(i in 1:length(job))
  {
    if(i <= length(job)/2){
      first_machine[x] <- job[i]
      x = x + 1
    }
    
    if(i > length(job)/2){
      second_machine[k]<-job[i]
      k=k+1
    }
  }
  
  return(list(first_machine, second_machine))
}

# Range of each job
rangeOfJob1 <- geneNumberOfJob1 #6
rangeOfJob2 <- rangeOfJob1 + geneNumberOfJob2 # 6 + 4
rangeOfJob3 <- rangeOfJob2 + geneNumberOfJob3 # 10 + 2

# Assign seperated genes of machines to each job
"first_job <- seperateJobsToMachines(chromosome[1:rangeOfJob1, 1, 1])
second_job <- seperateJobsToMachines(chromosome[rangeOfJob1:rangeOfJob2, 1, 1])
third_job <- seperateJobsToMachines(chromosome[(rangeOfJob2+1):rangeOfJob3, 1, 1])

# Convert list of jobs to array of jobs
first_job <- array(as.numeric(unlist(first_job)), dim=c(geneNumberOfJob1, 1))
second_job <- array(as.numeric(unlist(second_job)), dim=c(geneNumberOfJob2, 1))
third_job <- array(as.numeric(unlist(third_job)), dim=c(geneNumberOfJob3, 1))"

# Select the minimum of genes in a job by its subjob
selectMinimumGenesInJob <-function(subjob,job) {
  sortedJobAsc <- sort(job)

  selected_minimum_numbers <- array(dim = subjob)

  for(i in 1:subjob) {
    selected_minimum_numbers[i] <- sortedJobAsc[i]
  }
  
  return(selected_minimum_numbers)
}

# Assign selected minimum numbers to their jobs
"min_first_job <- selectMinimumGenesInJob(subgeneNumberOfJob1, first_job)
min_second_job <- selectMinimumGenesInJob(subgeneNumberOfJob2, second_job)
min_third_job <- selectMinimumGenesInJob(subgeneNumberOfJob3, third_job)

# Compare the minimum genes with their jobs and get its index
indexOfMinimumGenesInJob1 <- match(min_first_job, first_job)
indexOfMinimumGenesInJob2 <- match(min_second_job, second_job)
indexOfMinimumGenesInJob3 <- match(min_third_job, third_job)"

# Read the csv file for processing and setup times
processing_time <- read.csv("C:\\Users\\eren\\Desktop\\processing time.csv",TRUE,";")
m1_setup_time <- read.csv("C:\\Users\\eren\\Desktop\\Machine1 Setup time.csv",TRUE,";")
m2_setup_time <- read.csv("C:\\Users\\eren\\Desktop\\Machine2 Setup time.csv",TRUE,";")

# Returns how many genes in machine1 and machine2 and corresponding genes inside those machines
findMachine <- function(indexOfMinimumGenesInAJob, selectedMinimumGenesInAJob) {
  m1 <- 0
  m2 <- 0
  
  for (i in 1:length(indexOfMinimumGenesInAJob)) {
    if(indexOfMinimumGenesInAJob[i] <= length(indexOfMinimumGenesInAJob)) {
      m1 <- m1 + 1
    } else {
      m2 <- m2 + 1
    }
  }
  
  "m1_least <- array(dim = m1)
  m2_least <- array(dim = m2)
 
  j <- 1
  k <- 1
  
  
  for (i in 1:length(indexOfMinimumGenesInAJob)) {
    if(indexOfMinimumGenesInAJob[i] <= length(indexOfMinimumGenesInAJob)) {
      m1_least[j]<-selectedMinimumGenesInAJob[i]
      j<-j+1
    } else {
      m2_least<-selectedMinimumGenesInAJob[i]
      k<-k+1
    }
  }"
  
  minimumGenesInM1 <- array(dim = m1)
  minimumGenesInM2 <- array(dim = m2)
  counter <- 1

    if(m1 > 0 && m2 > 0) {
      for (j in 1:m1) {
        minimumGenesInM1[j] <- selectedMinimumGenesInAJob[j]
        counter <- counter + 1
      }
      
      for(k in 1:m2) {
        minimumGenesInM2[k]<- selectedMinimumGenesInAJob[counter]
        counter <- counter + 1
      }
    } else if (m1 == 0){
      minimumGenesInM1 <- 0
      
      for(j in 1:m2) {
        minimumGenesInM2[j] <- selectedMinimumGenesInAJob[j]
      }
    } else if (m2 == 0) {
      minimumGenesInM2 <- 0
      
      for (j in 1:m1) {
        minimumGenesInM1[j] <- selectedMinimumGenesInAJob[j]
      }
    }

  return(list(m1, m2, minimumGenesInM1, minimumGenesInM2))
}

"calculatedJob1 <- findMachine(indexOfMinimumGenesInJob1, min_first_job)
calculatedJob2 <- findMachine(indexOfMinimumGenesInJob2, min_second_job)
calculatedJob3 <- findMachine(indexOfMinimumGenesInJob3, min_third_job)

calculatedJob1 <- array(as.numeric(unlist(calculatedJob1)), dim=5)
calculatedJob2 <- array(as.numeric(unlist(calculatedJob2)), dim=4)
calculatedJob3 <- array(as.numeric(unlist(calculatedJob3), 0), dim=4)

optimized_first_time1 <- as.double(1)
optimized_second_time1 <- as.double(1)

optimized_first_time2 <- as.double(1)
optimized_second_time2 <- as.double(1)

optimized_first_time3 <- as.double(1)
optimized_second_time3 <- as.double(1)

if(calculatedJob1[1] != 0) {
  optimized_first_time1 <- as.double(processing_time[1,1] / calculatedJob1[1])
}
if(calculatedJob1[2] != 0) {
  optimized_second_time1 <- as.double(processing_time[2,1] / calculatedJob1[2])
}

if(calculatedJob2[1] != 0) {
  optimized_first_time2 <- as.double(processing_time[1,2] / calculatedJob2[1])
}
if(calculatedJob2[2] != 0) {
  optimized_second_time2 <- as.double(processing_time[2,2] / calculatedJob2[2])
}

if(calculatedJob3[1] != 0) {
  optimized_first_time3 <- as.double(processing_time[1,3] / calculatedJob3[1])
}
if(calculatedJob3[2] != 0) {
  optimized_second_time3 <- as.double(processing_time[2,3] / calculatedJob3[2])
}

optimized_times <- array(c(optimized_first_time1, optimized_first_time2, optimized_first_time3, optimized_second_time1, optimized_second_time2, optimized_second_time3), dim = 6)

calculatedJob1 <- findMachine(indexOfMinimumGenesInJob1, min_first_job)
calculatedJob2 <- findMachine(indexOfMinimumGenesInJob2, min_second_job)
calculatedJob3 <- findMachine(indexOfMinimumGenesInJob3, min_third_job)

genesInMachine1 <- c(calculatedJob1[3], calculatedJob2[3], calculatedJob3[3])
genesInMachine2 <- c(calculatedJob1[4], calculatedJob2[4], calculatedJob3[4])

genesInMachine1 <- array(as.numeric(unlist(genesInMachine1)))
genesInMachine2 <- array(as.numeric(unlist(genesInMachine2)))

sortedGenesInMachine1W0 <- sort(genesInMachine1)
sortedGenesInMachine2W0 <- sort(genesInMachine2)

sortedGenesInMachine1 <- 0 # Empty array
sortedGenesInMachine2 <- 0 # Empty array

j <- 1
x <- 1

for(i in sortedGenesInMachine1W0){
  if(i != 0) {
    sortedGenesInMachine1[j] <- i
    j <- j+1
  }
}

for(i in sortedGenesInMachine2W0){
  if(i != 0) {
    sortedGenesInMachine2[x] <- i
    x <- x+1
  }
}"

# sorted_machine: sorted genes in machines (ascending order)
# jobs: minimum genes in each job as list
findJob <- function(sorted_machine, jobs){
  placement <- 0
  counter <- 1
  j <- 1
  i <- 1
  k <- 1

    while(j <= length(sorted_machine)) {
      # Loop until k is 6 because we may have a machine with 6 selected genes
      while(k <= 6) {
        i <- sorted_machine[j]

        if(!is.na(i)){
          if(i==0){
            j <- j+1
            i <- sorted_machine[j]
            next
          }
        }
        jobs <- array(as.numeric(unlist(jobs)))
        bool <- i %in% jobs[k]
        if(bool == TRUE){
          # If counter is even, divide counter by 2 to get the correct job number
          # If counter is odd, divide counter by 3 and get the remainder " "
          if(counter %% 2 == 0) {
            counter <- counter/2
          } else if(counter %% 2 == 1) {
             counter <- counter %% 3
          }
          if(counter == 0) {
            counter <- 1
          }
          placement[j] <- counter
          j<-j+1
          break
        } else {
          counter <- counter + 1
          k <- k + 1
        }
      }
      k <- 1
      counter <- 1
    }
  
  return(placement)
}
"
jobs <- list(min_first_job, min_second_job, min_third_job)

jobsInMachine1 <- findJob(sortedGenesInMachine1, jobs)
jobsInMachine2 <- findJob(sortedGenesInMachine2, jobs)"

findBestSolution <- function(optimized_time, m1_setup_time, m2_setup_time, jobsInMachine1, jobsInMachine2){
  total1 <- 0
  total2 <- 0

  current_job <- 0
  previous_job <- 0
  
  for(i in 1:length(jobsInMachine1)) {
    current_job <- jobsInMachine1[i]
    if(i == 1) {
      previous_job <- jobsInMachine1[i]
      total1 <- total1 + optimized_time[current_job]
    } else if(current_job == previous_job) {
      total1 <- total1 + optimized_time[current_job]
    } else {
      previous_job <- jobsInMachine1[i-1]
      total1 <- total1 + optimized_time[current_job] + m1_setup_time[current_job,previous_job]
    }
  }

  for(i in 1:length(jobsInMachine2)) {
    current_job <- jobsInMachine2[i]
    if(i == 1) {
      previous_job <- jobsInMachine2[i]
      total2 <- total2 + optimized_time[current_job]
    } else if(current_job == previous_job){
      total2 <- total2 + optimized_time[current_job]
    } else {
      previous_job <- jobsInMachine2[i-1]
      total2 <- total2 + optimized_time[current_job] + m2_setup_time[current_job,previous_job]
    }
  }

  return(max(total1,total2))
}



"result <- findBestSolution(optimized_times,m1_setup_time,m2_setup_time,jobsInMachine1,jobsInMachine2)
print(result)"

all_chromosomes <-list()
results <- array(dim = 10)
best_result <- 0
ch_counter <- 1

while(ch_counter <= 10){
  chromosome <- generateChromosome()
  all_chromosomes[ch_counter] <- list(chromosome)
  
  first_job <- seperateJobsToMachines(chromosome[1:rangeOfJob1,1,1])
  second_job <- seperateJobsToMachines(chromosome[rangeOfJob1:rangeOfJob2,1,1])
  third_job <- seperateJobsToMachines(chromosome[(rangeOfJob2+1):rangeOfJob3,1,1])
  
  first_job <- array(as.numeric(unlist(first_job)), dim=c(geneNumberOfJob1, 1))
  second_job <- array(as.numeric(unlist(second_job)), dim=c(geneNumberOfJob2, 1))
  third_job <- array(as.numeric(unlist(third_job)), dim=c(geneNumberOfJob3, 1))
  
  min_first_job <- selectMinimumGenesInJob(subgeneNumberOfJob1, first_job)
  min_second_job <- selectMinimumGenesInJob(subgeneNumberOfJob2, second_job)
  min_third_job <- selectMinimumGenesInJob(subgeneNumberOfJob3, third_job)
  
  indexOfMinimumGenesInJob1 <- match(min_first_job, first_job)
  indexOfMinimumGenesInJob2 <- match(min_second_job, second_job)
  indexOfMinimumGenesInJob3 <- match(min_third_job, third_job)
  
  calculatedJob1 <- findMachine(indexOfMinimumGenesInJob1, min_first_job)
  calculatedJob2 <- findMachine(indexOfMinimumGenesInJob2, min_second_job)
  calculatedJob3 <- findMachine(indexOfMinimumGenesInJob3, min_third_job)
  
  calculatedJob1 <- array(as.numeric(unlist(calculatedJob1)), dim=5)
  calculatedJob2 <- array(as.numeric(unlist(calculatedJob2)), dim=4)
  calculatedJob3 <- array(as.numeric(unlist(calculatedJob3), 0), dim=4)
  
  optimized_first_time1 <- as.double(1)
  optimized_second_time1 <- as.double(1)
  
  optimized_first_time2 <- as.double(1)
  optimized_second_time2 <- as.double(1)
  
  optimized_first_time3 <- as.double(1)
  optimized_second_time3 <- as.double(1)
  
  if(calculatedJob1[1] != 0) {
    optimized_first_time1 <- as.double(processing_time[1,1] / calculatedJob1[1])
  }
  if(calculatedJob1[2] != 0) {
    optimized_second_time1 <- as.double(processing_time[2,1] / calculatedJob1[2])
  }
  
  if(calculatedJob2[1] != 0) {
    optimized_first_time2 <- as.double(processing_time[1,2] / calculatedJob2[1])
  }
  if(calculatedJob2[2] != 0) {
    optimized_second_time2 <- as.double(processing_time[2,2] / calculatedJob2[2])
  }
  
  if(calculatedJob3[1] != 0) {
    optimized_first_time3 <- as.double(processing_time[1,3] / calculatedJob3[1])
  }
  if(calculatedJob3[2] != 0) {
    optimized_second_time3 <- as.double(processing_time[2,3] / calculatedJob3[2])
  }
  
  optimized_times <- array(c(optimized_first_time1, optimized_first_time2, optimized_first_time3, optimized_second_time1, optimized_second_time2, optimized_second_time3), dim = 6)
  
  calculatedJob1 <- findMachine(indexOfMinimumGenesInJob1, min_first_job)
  calculatedJob2 <- findMachine(indexOfMinimumGenesInJob2, min_second_job)
  calculatedJob3 <- findMachine(indexOfMinimumGenesInJob3, min_third_job)
  
  genesInMachine1 <- c(calculatedJob1[3], calculatedJob2[3], calculatedJob3[3])
  genesInMachine2 <- c(calculatedJob1[4], calculatedJob2[4], calculatedJob3[4])
  
  genesInMachine1 <- array(as.numeric(unlist(genesInMachine1)))
  genesInMachine2 <- array(as.numeric(unlist(genesInMachine2)))
  
  sortedGenesInMachine1W0 <- sort(genesInMachine1)
  sortedGenesInMachine2W0 <- sort(genesInMachine2)
  
  sortedGenesInMachine1 <- 0 # Empty array
  sortedGenesInMachine2 <- 0 # Empty array
  
  j <- 1
  x <- 1
  
  for(i in sortedGenesInMachine1W0){
    if(i != 0) {
      sortedGenesInMachine1[j] <- i
      j <- j+1
    }
  }
  
  for(i in sortedGenesInMachine2W0){
    if(i != 0) {
      sortedGenesInMachine2[x] <- i
      x <- x+1
    }
  }
  
  jobs <- list(min_first_job, min_second_job, min_third_job)
  
  jobsInMachine1 <- findJob(sortedGenesInMachine1, jobs)
  jobsInMachine2 <- findJob(sortedGenesInMachine2, jobs)
  
  best_result <- findBestSolution(optimized_times,m1_setup_time,m2_setup_time,jobsInMachine1,jobsInMachine2)
  
  results[ch_counter] <- best_result
  
  ch_counter <- ch_counter + 1
  
  print(best_result)
  print(jobsInMachine1)
  print(jobsInMachine2)
}

minimum_best_solutions <- selectMinimumGenesInJob(2,results)
print(minimum_best_solutions)

best_solutions_locations <- match(minimum_best_solutions,results)

selected_chromosomes <- list()
previous_chromosomes <- list()

derivated_data <- array(c(4,2,7,5,11,1,2.4,0.7,8,2,5,1.1))
y_value <- 44.1

# Assign 2 best chromosome inside all_chromosomes based on their index to selected_chromosomes array
for(i in 1:length(best_solutions_locations)) {
  selected_chromosomes[i] <- list(all_chromosomes[best_solutions_locations[i]])
}

crossover <- function(chromosome1, chromosome2) {
  chromosome1 <- array(as.numeric(unlist(chromosome1)))
  chromosome2 <- array(as.numeric(unlist(chromosome2)))
  
  offspring_chromosome <- array(dim = length(chromosome1))

  for(k in 1:length(chromosome1)/2){
     offspring_chromosome[k] <- chromosome1[k]
  }

  for (j in 7:12) {
     offspring_chromosome[j] <- chromosome2[j]
  }
  return(offspring_chromosome)
}

#selected_chromosomes <- array(as.numeric(unlist(selected_chromosomes)))

"offspring_crossovers <- list(crossover(selected_chromosomes[1],selected_chromosomes[2]))
print(offspring_crossovers)"

mutation <- function(offspring_crossovers, x) {
  offspring_crossovers <- array(as.numeric(unlist(offspring_crossovers)))
  m_chromosome <- offspring_crossovers
  m_chromosome[x] <- m_chromosome[x]/2
  
  return(m_chromosome)
}

"mutated_chromosome <- mutation()
print(mutated_chromosome)"

"fitness <- function(mutated_chromosome, derivated_data, y_value) {
  mutated_chromosome <- array(as.numeric(unlist(mutated_chromosome)))
  total <- 0
  j <- 1
  fitness_value <- 0
  
  for(i in mutated_chromosome) {
    total <- total + (derivated_data[j] * mutated_chromosome[j])
    j <- j + 1 
  }
  
  fitness_value <- abs(1 / (y_value - total))

  return(fitness_value)
}"

"previous_chromosomes <- list()
previous_chromosomes <- list(selected_chromosomes)"



"mutated_fitness_value <- fitness(mutated_chromosome,derivated_data,y_value)
previous_fitness_value1 <- fitness(selected_chromosomes[1],derivated_data,y_value)
previous_fitness_value2 <- fitness(selected_chromosomes[2],derivated_data,y_value)

mins <- c(mutated_fitness_value,previous_fitness_value1,previous_fitness_value2)

min_fitness_values <- selectMinimumGenesInJob(2,mins)

min_fitness_locations <- match(min_fitness_values,mins)

print(selected_chromosomes)

previous_selected <- array(as.numeric(unlist(previous_chromosomes)))


if(min_fitness_locations[1] == 1){
  selected_chromosomes[1] <- list(mutated_chromosome)
}else if(min_fitness_locations[1] == 2){
  selected_chromosomes[1] <- list(previous_selected[1:12])
}else if(min_fitness_locations[1] == 3){
  selected_chromosomes[1] <- list(previous_selected[13:24])
}

if(min_fitness_locations[2] == 1){
  selected_chromosomes[2] <- list(mutated_chromosome)
}else if(min_fitness_locations[2] == 2){
  selected_chromosomes[2] <- list(previous_selected[1:12])
}else if(min_fitness_locations[2] == 3){
  selected_chromosomes[2] <- list(previous_selected[13:24])
}"


"print(mins)
print(min_fitness_locations)
print(selected_chromosomes)"


operationForBestSolution <- function(chromosome){
  chromosome <- array(as.numeric(unlist(chromosome)))
  first_job <- seperateJobsToMachines(chromosome[1:rangeOfJob1])
  second_job <- seperateJobsToMachines(chromosome[rangeOfJob1:rangeOfJob2])
  third_job <- seperateJobsToMachines(chromosome[(rangeOfJob2+1):rangeOfJob3])
  
  first_job <- array(as.numeric(unlist(first_job)), dim=c(geneNumberOfJob1, 1))
  second_job <- array(as.numeric(unlist(second_job)), dim=c(geneNumberOfJob2, 1))
  third_job <- array(as.numeric(unlist(third_job)), dim=c(geneNumberOfJob3, 1))
  
  min_first_job <- selectMinimumGenesInJob(subgeneNumberOfJob1, first_job)
  min_second_job <- selectMinimumGenesInJob(subgeneNumberOfJob2, second_job)
  min_third_job <- selectMinimumGenesInJob(subgeneNumberOfJob3, third_job)
  
  indexOfMinimumGenesInJob1 <- match(min_first_job, first_job)
  indexOfMinimumGenesInJob2 <- match(min_second_job, second_job)
  indexOfMinimumGenesInJob3 <- match(min_third_job, third_job)
  
  calculatedJob1 <- findMachine(indexOfMinimumGenesInJob1, min_first_job)
  calculatedJob2 <- findMachine(indexOfMinimumGenesInJob2, min_second_job)
  calculatedJob3 <- findMachine(indexOfMinimumGenesInJob3, min_third_job)
  
  calculatedJob1 <- array(as.numeric(unlist(calculatedJob1)), dim=5)
  calculatedJob2 <- array(as.numeric(unlist(calculatedJob2)), dim=4)
  calculatedJob3 <- array(as.numeric(unlist(calculatedJob3), 0), dim=4)
  
  optimized_first_time1 <- as.double(1)
  optimized_second_time1 <- as.double(1)
  
  optimized_first_time2 <- as.double(1)
  optimized_second_time2 <- as.double(1)
  
  optimized_first_time3 <- as.double(1)
  optimized_second_time3 <- as.double(1)
  
  if(calculatedJob1[1] != 0) {
    optimized_first_time1 <- as.double(processing_time[1,1] / calculatedJob1[1])
  }
  if(calculatedJob1[2] != 0) {
    optimized_second_time1 <- as.double(processing_time[2,1] / calculatedJob1[2])
  }
  
  if(calculatedJob2[1] != 0) {
    optimized_first_time2 <- as.double(processing_time[1,2] / calculatedJob2[1])
  }
  if(calculatedJob2[2] != 0) {
    optimized_second_time2 <- as.double(processing_time[2,2] / calculatedJob2[2])
  }
  
  if(calculatedJob3[1] != 0) {
    optimized_first_time3 <- as.double(processing_time[1,3] / calculatedJob3[1])
  }
  if(calculatedJob3[2] != 0) {
    optimized_second_time3 <- as.double(processing_time[2,3] / calculatedJob3[2])
  }
  
  optimized_times <- array(c(optimized_first_time1, optimized_first_time2, optimized_first_time3, optimized_second_time1, optimized_second_time2, optimized_second_time3), dim = 6)
  
  calculatedJob1 <- findMachine(indexOfMinimumGenesInJob1, min_first_job)
  calculatedJob2 <- findMachine(indexOfMinimumGenesInJob2, min_second_job)
  calculatedJob3 <- findMachine(indexOfMinimumGenesInJob3, min_third_job)
  
  genesInMachine1 <- c(calculatedJob1[3], calculatedJob2[3], calculatedJob3[3])
  genesInMachine2 <- c(calculatedJob1[4], calculatedJob2[4], calculatedJob3[4])
  
  genesInMachine1 <- array(as.numeric(unlist(genesInMachine1)))
  genesInMachine2 <- array(as.numeric(unlist(genesInMachine2)))
  
  sortedGenesInMachine1W0 <- sort(genesInMachine1)
  sortedGenesInMachine2W0 <- sort(genesInMachine2)
  
  sortedGenesInMachine1 <- 0 # Empty array
  sortedGenesInMachine2 <- 0 # Empty array
  
  j <- 1
  x <- 1
  
  for(i in sortedGenesInMachine1W0){
    if(i != 0) {
      sortedGenesInMachine1[j] <- i
      j <- j+1
    }
  }
  
  for(i in sortedGenesInMachine2W0){
    if(i != 0) {
      sortedGenesInMachine2[x] <- i
      x <- x+1
    }
  }
  
  jobs <- list(min_first_job, min_second_job, min_third_job)
  
  jobsInMachine1 <- findJob(sortedGenesInMachine1, jobs)
  jobsInMachine2 <- findJob(sortedGenesInMachine2, jobs)
  
  best_result <- findBestSolution(optimized_times,m1_setup_time,m2_setup_time,jobsInMachine1,jobsInMachine2)
  
  return(best_result)
}



minimum <- function(count,chromosome){
  sortedJobAsc <- sort(chromosome)
  
  selected_minimum_numbers <- array(dim = count)
  
  for(i in 1:count) {
    if((sortedJobAsc[i] %in% selected_minimum_numbers[i]) == FALSE){
      selected_minimum_numbers[i] <- sortedJobAsc[i]
    }
  }
  
  return(selected_minimum_numbers)
}


for(i in 1:50) {
  x <- 0
  offspring_crossover1 <- list(crossover(selected_chromosomes[1], selected_chromosomes[2]))
  offspring_crossover2 <- list(crossover(selected_chromosomes[2], selected_chromosomes[1]))
  
  x <- floor(runif(1, 7, 12))

  mutated_chromosome1 <- mutation(offspring_crossover1, x)
  mutated_chromosome2 <- mutation(offspring_crossover2, x)
  
  s_chromosome <- array(as.numeric(unlist(selected_chromosomes)))
  
  
  previous_chromosomes <- list()
  previous_chromosomes <- selected_chromosomes

  best_solution1 <- operationForBestSolution(mutated_chromosome1)
  best_solution2 <-   operationForBestSolution(mutated_chromosome2)
  previous_best_solution1 <- operationForBestSolution(s_chromosome[1:12])
  previous_best_solution2 <- operationForBestSolution(s_chromosome[13:24])
  
  s <- array(as.numeric(unlist(selected_chromosomes)))
  
   print("------------------------------------------")
   print("random mutation index")
   print(x)
   print("mutated chromosome1")
   print(mutated_chromosome1)
   print("mutated chromosome2")
   print(mutated_chromosome2)
   print("previous chromosome1")
   print(s[1:12])
   print("previous chromosome2")
   print(s[13:24])
  
  bestValues <- c(best_solution1[1],best_solution2[1],previous_best_solution1[1],previous_best_solution2[1])
  min_best_solution <- minimum(2, bestValues)
  min_best_locations <- match(min_best_solution, bestValues)
  
  previous_selected <- array(as.numeric(unlist(previous_chromosomes)))
  pre1 <- previous_selected[1:12]
  pre2 <- previous_selected[13:24]
  
  print("min best locations")
  print(min_best_locations)
  if(min_best_locations[1] == 1) {
    selected_chromosomes[1] <- list(mutated_chromosome1)
  } else if(min_best_locations[1] == 2){
    selected_chromosomes[1] <- list(mutated_chromosome2)
  } else if(min_best_locations[1] == 3) {
    selected_chromosomes[1] <- list(pre1)#list(previous_selected[1:12])
  } else if(min_best_locations[1] == 4) {
    selected_chromosomes[1] <- list(pre2)#list(previous_selected[13:24])
  }

  if(min_best_locations[2] == 1) {
    selected_chromosomes[2] <- list(mutated_chromosome1)
  } else if(min_best_locations[2] == 2){
    selected_chromosomes[2] <- list(mutated_chromosome2)
  } else if(min_best_locations[2] == 3) {
    selected_chromosomes[2] <- list(pre1)#list(previous_selected[1:12])
  } else if(min_best_locations[2] == 4) {
    selected_chromosomes[2] <- list(pre2)#list(previous_selected[13:24])
  }
  
  for(i in 1:length(best_solutions_locations)) {
    all_chromosomes[best_solutions_locations[i]]  <- array(selected_chromosomes[i])
  }
  
  for (i in 1:10) {
    results[i] <- operationForBestSolution(all_chromosomes[i])
  }
  

  minimum_best_solutions <- minimum(2,results)

  best_solutions_locations <- match(minimum_best_solutions,results)
  
  for(i in 1:length(best_solutions_locations)) {
    selected_chromosomes[i] <- list(all_chromosomes[best_solutions_locations[i]])
  }
  
  
  print("min best values")
  print(bestValues)
}

print("BEST SOLUTION")
best_result <- array(as.numeric(unlist(selected_chromosomes)))
print("job 1")
print(best_result[1:6])
print("job 2")
print(best_result[7:10])
print("job 3")
print(best_result[11:12])

print(max(results))

