  
program lab_1_5
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable :: input_file, output_file
   
   type(student),pointer              :: Group_List => Null()
  ! integer                            :: N
   input_file  = "../data/listin.txt"
   output_file = "output.txt"
   
   Group_List => Read_class_list(input_file)   


   if (Associated(Group_List)) then 
      call Output_class_list(output_file, Group_List, "Исходный список:", "rewind")
      call Sort_class_list(Group_List,4) 
      !call InsertionSort(Group_List,Group_List)
      call SelectionSort(Group_List)
      call Output_class_list(output_file, Group_List, "Исходный список:", "rewind")
   end if





end program lab_1_5
