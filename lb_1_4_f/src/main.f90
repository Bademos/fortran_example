! Copyright 2015 Fyodorov S. A.
  
program lab_1_3
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable :: input_file, output_file, data_file
   
   type(student)              :: Group(STUD_AMOUNT)

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   data_file   = "class.dat"
   
   call Create_data_file(input_file, data_file)
   
   Group = Read_class_list(data_file)

   call Sort_rec(Group,2,size(Group)+1)
   call Output_class_list(output_file, Group, "Исходный список:", "rewind")





end program lab_1_3
