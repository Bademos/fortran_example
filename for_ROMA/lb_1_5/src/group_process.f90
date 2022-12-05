
module Group_Process
   use Environment
   use Group_IO

   implicit none
   
contains
   pure recursive subroutine selectionsort(unsorted)
       type(student), pointer :: unsorted
 
       if (associated(unsorted)) then
          call chooseandpaste(unsorted,unsorted,unsorted%next)
          call selectionsort(unsorted%next)
       end if
 
    end subroutine selectionsort
 
 
    pure recursive subroutine chooseandpaste(unsorted, maximum,current)
       type(student), pointer :: unsorted
       type(student), pointer :: maximum
       type(student), pointer :: current,tmp
 
       if(associated(current)) then
          if(current%num < maximum%num) then
             call chooseandpaste(unsorted,current,current%next)
          else
             call chooseandpaste(unsorted,maximum,current%next)
          end if
 
       else
          if (.not.associated(unsorted,maximum)) then
             tmp => maximum
             maximum => maximum%next
             tmp%next => unsorted%next
             unsorted =>tmp
          end if
       end if
 
 
    end subroutine chooseandpaste
 

 
 

  
























 !recursive subroutine  Iter(List)
 !   type(student), pointer :: List
 !   print*, List%Initials
 !   if(Associated(List%next)) then
 !      call Iter(List%next)
 !   end if
 !end subroutine Iter
 !



end module group_process
