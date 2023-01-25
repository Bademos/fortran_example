
program reference_lab_list
   use Environment
  ! use  List_Process
   use List_IO

   implicit none
   character(:), allocatable :: input_file, output_file
   integer                   :: num_bracket = 0,pos = 1
   logical                   :: is_closed = .true.
   character(:), allocatable :: res

  ! class(symbol), pointer   :: List => Null()
  ! class(command), pointer  :: List_comm => Null()
   input_file  = "../data/test1.txt"
   input_file  = "../data/test1.txt"
   input_file  = "../data/input.txt"



   output_file = "output.txt"
   
  ! List => Read_sym_list(input_file,list_comm)
    
     ! call Output_list(output_file, List, "Исходный список:", "rewind")
   ! if (Associated(List_comm)) then
     ! call Process(List, 1, 3, 3)
    !  call Output_ex(output_file,list, list_comm, "poehali!", "rewind")
     ! call Output_command(output_file, List_comm, "Исходный список:", "rewind")
      
     ! call Conjuct(List, Read_sym_value(" pidory "))
     ! call Insert(List, 1, 1, Read_sym_value(" gandony "))

     ! call Output_list(output_file, List, "Исходный список:", "rewind")
  ! end if
   
end program reference_lab_list
