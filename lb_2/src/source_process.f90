
module Source_Process
   use Environment
   use Source_IO

   implicit none

contains
   
! pure recursive subroutine Delete(current, Str)
!       type(SourceLine), allocatable, intent(inout) :: current
!       character(:,CH_),allocatable, intent(in)     :: Str
! 
!       type(SourceLine), allocatable :: temp
! 
!       if (Allocated(current)) then
!          if (current%String == Str) then
!          call move_alloc(current%next, temp)
!             call move_alloc(temp, current)
!             call Delete(current,Str)
!          else
!             call Delete(current%next, Str)
!          end if
!      end if
!    end subroutine Delete
!

!   pure recursive subroutine Conjuct(current_1,current_2)
!      type(SourceLine), allocatable, intent(inout) :: current_1, current_2
!      if(Allocated(current_2)) then
!         call Delete(current_1,current_2%String)
!         call Conjuct(current_1,current_2%Next)
!      end if
!
!   end subroutine Conjuct   
    
pure recursive subroutine Process(current, pos, from, to)
     type(SourceLine), allocatable, intent(inout)       :: current
     integer,intent(in)              :: pos, from, to
     type(SourceLine) ,allocatable       :: tmp
  
     if(pos >= from.and.pos <= to) then
        call move_alloc(current%next, tmp)
        call move_alloc(tmp, current)
     end if
  
  
     if (Allocated(current).and.pos<from) then
        Call Process(current%next, pos+1, from, to)
     else if (Allocated(current).and.pos<to) then
        Call Process(current, pos+1, from, to)
     end if
  end subroutine Process




end module Source_process
