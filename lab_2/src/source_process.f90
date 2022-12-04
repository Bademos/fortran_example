
module Source_Process
   use Environment
   use Source_IO

   implicit none

contains

pure recursive subroutine Process(current, pos, from, to)
   type(SourceLine), pointer       :: current
   integer,intent(in)              :: pos, from, to
   type(SourceLine) ,pointer       :: tmp
   
   if(pos >= from.and.pos <= to) then
      if (from == 1) then
         tmp => current
         current => current%next
         
      else
      tmp => current%next
      current%next =>  current%next%next
      end if
      deallocate(tmp)
   end if  


   if (associated(current%next).and.pos<from) then
      Call Process(current%next, pos+1, from, to)
   else if (associated(current%next).and.pos<to) then
      Call Process(current, pos+1, from, to)
   end if
end subroutine Process

   

pure recursive subroutine InsertionSort(List,lastSorted)
    type(SourceLine), pointer :: List, tmp
    type(SourceLine), pointer, intent(inout) :: lastSorted
 
 
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
    type(SourceLine) , pointer :: current, itemToInsert, tmp
 
    if (Condition(itemToInsert,current)) then
 
    tmp => current
    current => itemToInsert
    current%next => tmp
 
 
    else
       call Paste(current%next,itemToInsert)
    end if
 
 end subroutine Paste




  pure logical function Condition( Current,tmp_stud)
 
 
 
         type(SourceLine), pointer, intent(in)  ::Current, tmp_stud
 
 
        Condition = .false.
        if ((len_trim(Current%String) <len_trim( tmp_stud%String))) then
              Condition = .true.

        else if ((len_trim(Current%String) ==len_trim(tmp_stud%String) .and. (Current%String < tmp_stud%String))) then
             Condition = .true.
        end if
     end function Condition












end module Source_process
