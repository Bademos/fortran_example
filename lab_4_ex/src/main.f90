! Copyright 2019 Fyodorov S. A.

program reference_lab_list
   use Environment
   use List_Process
   use List_IO

   implicit none
   character(:), allocatable :: input_file, output_file
   integer                   :: num_bracket = 0,pos = 1
   logical                   :: is_closed = .true.
   character(:), allocatable :: res

   class(node), pointer   :: List => Null()

   input_file  = "../data/list.txt"
   
   input_file  = "../data/test1.txt"

   input_file  = "../data/test2.txt"
   input_file  = "../data/test1.txt"
   input_file  = "../data/test1.txt"
   input_file  = "../data/test1.txt"
   input_file  = "../data/test1.txt"
   input_file  = "../data/test1.txt"
   input_file  = "../data/test1.txt"
   input_file  = "../data/test1.txt"
   input_file  = "../data/test1.txt"
   input_file  = "../data/test3.txt"



   output_file = "output.txt"
   
   List => Read_list(input_file)
   
   if (Associated(List)) then
      res =  Checking(List,is_closed,pos,num_bracket,"norm")
      call Output_list(output_file, List, "Исходный список:", "rewind",res)

   end if
   
end program reference_lab_list
