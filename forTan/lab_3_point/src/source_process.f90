
module Source_Process
   use Environment
   use Source_IO

   implicit none

contains

pure recursive subroutine InsertionSort(List,lastSorted)
    type(SourceLine), pointer :: List, tmp
    type(SourceLine), pointer, intent(inout) :: lastSorted
 
 
    if (Associated(lastSorted%next)) then
 
       if (lastSorted%next%date< lastSorted%date)then
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
    type(SourceLine) , pointer :: current, itemToInsert, tmp
 
    if (itemToInsert%Date<current%Date) then
 
    tmp => current
    current => itemToInsert
    current%next => tmp
 
 
    else
       call Paste(current%next,itemToInsert)
    end if
 
 end subroutine Paste




 
 
 











end module Source_process
