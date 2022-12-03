
module Group_Process
   use Environment
   use Group_IO

   implicit none
   
contains

 
 
 pure  logical function Condition( Current,tmp_stud)
     


        type(student), pointer, intent(in)  ::Current, tmp_stud


       Condition = .false.
       if ((Current%Surname <tmp_stud%Surname)) then
             Condition = .true.
       else if ((Current%Surname == tmp_stud%Surname) .and. (Current%Initials < tmp_stud%Initials)) then
            Condition = .true.
       end if
    end function Condition

  




    pure recursive subroutine Sort_class_list(ClassList, N)
     type(student), pointer, intent(inout)  :: ClassList
     integer, intent(in)                    :: N

     call Drop_down(ClassList, 1, N-1)

     if (N >= 3) &
        call Sort_class_list(ClassList, N-1)
  end subroutine Sort_class_list

  pure recursive subroutine Drop_down(ClassList, j, N)
     type(student), pointer  :: ClassList
     integer, intent(in)                    :: j, N

     if (Swap(ClassList)) &
        call Swap_from_current(ClassList)
     if (j < N) &
        call Drop_down(ClassList%next, j+1, N)
  end subroutine Drop_down

  pure logical function Swap(Current)
     type(student), intent(in)  :: Current

     Swap = .false.
        if (Current%Surname > Current%next%Surname) then
           Swap = .true.
        else if (Current%Surname==Current%next%Surname .and. Current%Initials>Current%next%Initials) then
           Swap = .true.
        end if
  end function Swap
   pure subroutine Swap_from_current(Current)
       type(student), pointer  :: Current
 
       type(student), pointer  :: tmp_stud
 
       tmp_stud       => Current%next
       Current%next   => Current%next%next
       tmp_stud%next  => Current
       Current        => tmp_stud
    end subroutine Swap_from_current





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





 pure  recursive subroutine Cut(List,tmp)
   type(student), pointer :: List, tmp
   
   tmp => List%next
  ! print *, tmp%Initials 
   end subroutine cut



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
