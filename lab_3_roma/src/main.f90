
program reference_lab_list
   use Environment
   use  List_Process
   use List_IO

   implicit none
   character(:), allocatable :: input_file, output_file, F1, F2, F4
   integer                   :: num_bracket = 0,pos = 1
   character(kind=CH_)       :: cr = 'u'
   logical                   :: isChr
   class(symbol), pointer   :: List1 => Null(), List2 => Null(),&
         Set1=>Null(),Set2=>Null(),Set3=>Null()
   F1  = "../data/file1.txt"
   F2  = "../data/file2.txt"
   input_file  = "../data/input.txt"



   output_file = "output.txt"
   
   List1 => Read_list(F1)
   List2 => Read_list(F2)
   call setMaking(List1,Set1)
   call setMaking(List2,Set2)
   call setDivide(set1,set2,set3)
 !  if (.not.isContainChar(set2%char, set1,.false.)) then
 !     call Conjuct(set3,set2)
 !  end if
   
 !  if (isContainChar(set3%char, set1,.false.)) then
 !        set3 => set3%next%next
 !  end if
call control(set1,set2,set3)
      call Output_list(output_file, List1, "Исходный список:", "rewind")
    if (Associated(List1)) then
     ! call Output_ex(output_file,list, list_comm, "poehali!", "rewind")

      call Output_list(output_file, set3, "Исходный список:", "rewind")
   end if
   
end program reference_lab_list
