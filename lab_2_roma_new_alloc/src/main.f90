
program reference_lab_list
   use Environment
   use List_IO

   implicit none
   character(:), allocatable :: input_file, output_file
   integer                   :: num_bracket = 0,pos = 1
   logical                   :: is_closed = .true.
   character(:), allocatable :: res

   input_file  = "../data/test1.txt"
   input_file  = "../data/test1.txt"
   input_file  = "../data/input.txt"



   output_file = "output.txt"
   
   
end program reference_lab_list
