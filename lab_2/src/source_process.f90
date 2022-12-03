
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

   

















end module Source_process
