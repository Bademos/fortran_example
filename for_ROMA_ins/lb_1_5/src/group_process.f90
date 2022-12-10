
module Group_Process
   use Environment
   use Group_IO

   implicit none
   
contains
 
 
 pure  logical function Condition( Current,tmp_stud)
     


        type(student), pointer, intent(in)  ::Current, tmp_stud


       Condition = .false.
      if (Current%Num > tmp_stud%Num) then
            Condition = .true.
       end if
    end function Condition

  





pure recursive subroutine InsertionSort(List,lastSorted)
   type(student), pointer :: List, tmp
   type(student), pointer, intent(inout) :: lastSorted
   

   if (Associated(lastSorted%next)) then
      
      if (Condition(lastSorted%next, lastSorted))then
      tmp => lastSorted%next
      lastSorted%next => lastSorted%next%next
         call Paste(List,tmp)
         call InsertionSort(List,lastSorted)
      else
         call InsertionSort(List,lastSorted%next)
      end if
   end if
  end subroutine InsertionSort








pure  recursive subroutine Paste(current, itemToInsert)
   type(student) , pointer :: current, itemToInsert, tmp
   
   if (Condition(itemToInsert,current)) then
      
   tmp => current
   current => itemToInsert
   current%next => tmp

   
   else
      call Paste(current%next,itemToInsert)
   end if

end subroutine Paste

 























 !recursive subroutine  Iter(List)
 !   type(student), pointer :: List
 !   print*, List%Initials
 !   if(Associated(List%next)) then
 !      call Iter(List%next)
 !   end if
 !end subroutine Iter
 !



end module group_process
