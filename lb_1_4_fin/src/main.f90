! Copyright 2015 Fyodorov S. A.
  
program lab_1_3
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable :: input_file, output_file, data_file
   integer                   :: allo
   type(student),allocatable              :: Group(:)

   input_file  = "../data/bigdata_1000000.txt"
   output_file = "output.txt"
   data_file   = "class.dat"
   
   call Create_data_file(input_file, data_file)
   Allocate(Group(stud_amount),stat = allo)
 
  
   Group = Read_class_list(data_file)
   print *,allocated(group), allo

!call Sort_ins(Group)
  call Sort_rec(Group,2,size(Group))
   call Output_class_list(output_file, Group, "Исходный список:", "rewind")





end program lab_1_3
