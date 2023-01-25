
program reference_lab_list
   use Environment
   use  List_Process
   use List_IO

   implicit none
   character(:), allocatable :: input_file, output_file, F1, F2, F4
   integer                   :: num_bracket = 0,pos = 1
   character(kind=CH_)       :: cr = 'u'
   logical                   :: isChr
   type(node), allocatable   :: List1, List2,List1_ref,List2_ref,Set1,Set2,Set3
   F1  = "../data/file1.txt"
   F2  = "../data/file2.txt"
   input_file  = "../data/input.txt"



   output_file = "output.txt"
   
   List1 = Read_list(F1)
   List1_ref = Read_list(F1)

   List2 = Read_list(F2)
   List2_ref = Read_list(F2)
  ! call conjuct(List1,List2)
   call setMaking(List1,Set1)
  call setMaking(List2,Set2)
 
  call alterSetDivide(set1,set2)
     

!call Output_list(output_file, List1, "Исходный список:", "rewind")
    if (Allocated(List1)) then
      !call Output_ex(output_file,list, list_comm, "poehali!", "rewind")

      call Output_list(output_file,list1_ref,list2_ref, set2, "rewind")
   end if
   
end program reference_lab_list
