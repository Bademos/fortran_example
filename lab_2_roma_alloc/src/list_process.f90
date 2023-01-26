
module List_Process
   use Environment
!   use List_IO

   implicit none

contains


  
! pure recursive subroutine Process(current, pos, from, num)
!    type(symbol), pointer       :: current
!    integer,intent(in)              :: pos, from, num
!    integer                         :: to
!    type(symbol) ,pointer       :: tmp
!         
!    to = from + num 
!    
!    if(pos >= from.and.pos <= to) then
!       if (from == 1) then
!          tmp => current
!          current => current%next
! 
!       else
!       tmp => current%next
!       current%next =>  current%next%next
!       end if
!       deallocate(tmp)
!    end if
! 
! 
!    if (associated(current%next).and.pos<from) then
!       Call Process(current%next, pos+1, from, num)
!    else if (associated(current%next).and.pos<to) then
!       Call Process(current, pos+1, from, num)
!    end if
! end subroutine Process
!
!  recursive subroutine Conjuct(main_list, tail)
!      class(symbol) , pointer::main_list, tail,tmp
!         if (associated(main_list%next)) then
!            call Conjuct(main_list%next, tail)
!         else 
!            main_list%next => tail
!
!         end if
!  
!  end subroutine Conjuct
!
!recursive subroutine Insert(List,pos,pos_ins,sub_list)
!      class(symbol), pointer :: List, sub_list, tmp
!      integer, intent(in)    :: pos, pos_ins
!
!       if (associated(List%next).and.pos<pos_ins) then
!        Call Insert(List%next, pos+1, pos_ins, sub_list)
!     else if (pos==pos_ins) then
!         if (associated(List%next)) then
!             tmp => List%next
!             List%next => Null()
!             call conjuct(list,sub_list)
!             call conjuct(list, tmp)
!         else
!            call conjuct(list, sub_list)
!         end if
!
!     end if
!
!         
!
!
!
!end subroutine Insert
!    
!recursive   subroutine Exibition (Out,List, List_com)
!     class(symbol) , pointer    :: List,sym
!     class(command), pointer    :: List_com
!     integer                    :: start
!     integer, intent(in)        :: out
!     call output_value(Out, List)
!
!       if (Associated(List_com)) then
!           select type(List_com)
!             type is (input)
!                write(Out, '(/a, 1x,i2)') "I",List_com%start
!                call output_value(Out,List_com%symbolism)
!
!               write(Out, '(a)') " "
!               
!               call Insert(List,1,List_com%start,List_com%symbolism)
!             type is (delete)
!                write(Out, '(/a,1x,i2,1x,i2)') "D", List_com%start,List_com%last
!              
!                call Process(List,1,List_com%start,List_com%last) 
!           end select
!           call Exibition(out,List, List_com%next)
!        end if
!
!
!
!  end subroutine Exibition
!
!  subroutine Output_exibition(Output_File,List,List_com,Command_Name,Position)
!       character(*), intent(in) :: Output_File,Position, Command_Name
!       class(command),pointer   :: List_com
!       class(symbol), pointer   :: List
!       integer                  :: Out
!
!      open (file=Output_File, position=Position, newunit=Out)
!          write (out, '(/a)') Command_Name
!           call Exibition(Out, List, List_com)
!        close (Out)
!     end subroutine Output_exibition


end module List_process
