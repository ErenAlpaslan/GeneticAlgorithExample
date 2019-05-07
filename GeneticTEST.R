#GA example


#12,1,1 matrix olusturuldu
matrix <- array(dim = c(12, 1, 1))

#for ile 12 tane deger matrixe atildi
for(i in 1:12){
  matrix[i,1,1] <- floor(runif(1, 1, 99))
}

#jobta kac tane gen oldugu
job1<-6
job2<- 4
job3<- 2

#subjob sayisi
subjob1 <- 3
subjob2 <- 2
subjob3 <- 1



#olusturulan 12'li matrixi machinelerine ayiriyor // joblarin icindeki genleri makinalara ayiriyor
seperateToMachine<-function(lenght,job){
  
  first_machine <- array(dim = (lenght/2))
  second_machine <- array(dim = (lenght/2))
  
  first_cell <- 1
  second_cell <- 1
  
  for(i in 1:lenght)
  {
    if(i <= lenght/2){
      first_machine[first_cell]<-job[i]
      first_cell=first_cell+1
    }
    if(i > lenght/2){
      second_machine[second_cell]<-job[i]
      second_cell=second_cell+1
    }
  }
  
  return(list(first_machine,second_machine))
  
}
#joblarin araliklari 6 -> 10 -> 12
limit1<- job1
limit2<- limit1 + job2
limit3<- limit2 + job3

#seperateMachineden donen listin araliklar arasinda ki degerleri joblarina esitliyor
first_job<-seperateToMachine(job1,matrix[1:limit1,1,1])
#print(first_job)

second_job <- seperateToMachine(job2,matrix[limit1:limit2,1,1])
#print(second_job)

third_job <- seperateToMachine(job3,matrix[(limit2+1):limit3,1,1])
#print(third_job)

#unlisted form of jobs (arraye ceviriyor)

first_job <- array(as.numeric(unlist(first_job)), dim=c(job1, 1))
#print(first_job)

second_job <- array(as.numeric(unlist(second_job)), dim=c(job2, 1))
#print(second_job)

third_job <- array(as.numeric(unlist(third_job)), dim=c(job3, 1))
#print(third_job)

#subjob sayisina gore minimum degerleri seciyor

selectMinimum <-function(subjob,job){
  min <- sort(job)
  selected_minimum_numbers<- array(dim = subjob)
  for(i in 1:subjob){
    selected_minimum_numbers[i] <- min[i]
  }
  
  return(selected_minimum_numbers)
}

#minimum degerleri joblarina gore arraylere atiyor

min_first_job <- selectMinimum(subjob1,first_job)
#print(min_first_job)

min_second_job <- selectMinimum(subjob2,second_job)
#print(min_second_job)

min_third_job <- selectMinimum(subjob3,third_job)
#print(min_third_job)

#minimum degerleri job ile karsilastirip konumunu tutuyor

first_locations <- match(min_first_job,first_job)
#print(first_locations)

second_locations<- match(min_second_job,second_job)
#print(second_locations)

third_locations <- match(min_third_job,third_job)
#print(third_locations)

#processing time array 
processing_time <- array(c(90,95,60,70,30,40),dim=c(2,3))
#print(processing_time)

#findmachine machine1 ve machine2 de kac tane gen oldugunu ve hangi machinede olduguna gore m1 ve m2 arraylerine atiyor

findMachine <- function(location,subjob,selected_numbers){
  
  m1 <- 0
  m2 <- 0
  
  for (i in 1:length(location)) {
    if(location[i]<= length(location)){
      m1<-m1+1
    }else{
      m2 <- m2+1
    }
  }
  
  m1_least <- array(dim = m1)
  m2_least <- array(dim = m2)
 
  j<-1
  k<-1
  
  
  for (i in 1:length(location)) {
    if(location[i] <= length(location)){
      m1_least[j]<-selected_numbers[i]
      j<-j+1
    }else{
      m2_least<-selected_numbers[i]
      k<-k+1
    }
  }
  
  assigned_m1 <- array(dim = m1)
  assigned_m2 <- array(dim = m2)
  counter <- 1
  

    if(m1 > 0 && m2>0){
      for (j in 1:m1) {
        assigned_m1[j] <- selected_numbers[j]
        counter <- counter+1
      }
      for(k in 1:m2){
        assigned_m2[k]<- selected_numbers[counter]
        counter <- counter +1
      }
    }else if(m1 == 0){
      assigned_m1 <- 0
      for(j in 1:m2){
        assigned_m2[j] <- selected_numbers[j]
      }
    }else if (m2 == 0) {
      assigned_m2 <- 0
      for (j in 1:m1) {
        assigned_m1[j] <- selected_numbers[j]
      }
    }
    
  
  
  
  return(list(m1,m2,assigned_m1,assigned_m2))
  
}



testofjob1 <- findMachine(first_locations,subjob1,min_first_job)
print(testofjob1)
testofjob1 <- array(as.numeric(unlist(testofjob1)), dim=5)
#print(testofjob1)

testofjob2 <- findMachine(second_locations,subjob2,min_second_job)
print(testofjob2)
testofjob2 <- array(as.numeric(unlist(testofjob2)), dim=4)

testofjob3 <- findMachine(third_locations,subjob3,min_third_job)
print(testofjob3)
testofjob3 <- array(as.numeric(unlist(testofjob3),0), dim=4)

sumof_machine1 <- testofjob1[1]+testofjob2[1]+testofjob3[1]
sumof_machine2 <- testofjob1[2]+testofjob2[2]+testofjob3[2]

#print(sumof_machine1)




optimized_first_time1 <- as.double(1)
optimized_second_time1 <- as.double(1)

optimized_first_time2 <- as.double(1)
optimized_second_time2 <- as.double(1)

optimized_first_time3 <- as.double(1)
optimized_second_time3 <- as.double(1)


if(testofjob1[1] != 0){
  optimized_first_time1 <- as.double(processing_time[1,1] / testofjob1[1])
}
if(testofjob1[2]!=0){
  optimized_second_time1 <- as.double(processing_time[2,1] / testofjob1[2])
}

if(testofjob2[1] != 0){
  optimized_first_time2 <- as.double(processing_time[1,2] / testofjob2[1])
}
if(testofjob2[2]!=0){
  optimized_second_time2 <- as.double(processing_time[2,2] / testofjob2[2])
}

if(testofjob3[1] != 0){
  optimized_first_time3 <- as.double(processing_time[1,3] / testofjob3[1])
}
if(testofjob3[2]!=0){
  optimized_second_time3 <- as.double(processing_time[2,3] / testofjob3[2])
}



m1_setup_time <- array(c(0,8,6,2,0,4,3,7,0), dim = c(3,3))
m2_setup_time <- array(c(0,2,6,4,0,3,2,1,0), dim = c(3,3))

#sortladiktan sonra hangi jobta oldugu hangi machinede oldugunu bul sonra optimezed processing time ekle eger job degisirse setup time ekle sonra hepsini topla (10)kere yap
#en buyuk degeri ve butun olusturulan rastgele arrayi tutucak en yuksek 2 degeri veren sonuc tutulup crossover yapilacak 

optimized_times <- array(c(optimized_first_time1,optimized_first_time2,optimized_first_time3,optimized_second_time1,optimized_second_time2,optimized_second_time3),dim = 6)

print(optimized_times)



testofjob1 <- findMachine(first_locations,subjob1,min_first_job)
testofjob2 <- findMachine(second_locations,subjob2,min_second_job)
testofjob3 <- findMachine(third_locations,subjob3,min_third_job)


sorted_machine1 <- c(testofjob1[3],testofjob2[3],testofjob3[3])
sorted_machine2 <- c(testofjob1[4],testofjob2[4],testofjob3[4])

sorted_machine1 <- array(as.numeric(unlist(sorted_machine1)))
sorted_machine2 <- array(as.numeric(unlist(sorted_machine2)))


sorted_machine1 <- sort(sorted_machine1)
sorted_machine2 <- sort(sorted_machine2)

print(testofjob1)
print(testofjob2)
print(testofjob3)

print(matrix)


print(sorted_machine1)
print(sorted_machine2)
