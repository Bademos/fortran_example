
module Source_Process
   use Environment
   use Source_IO

   implicit none

contains
   
 pure recursive subroutine Delete(current, Str)
       type(SourceLine), allocatable, intent(inout) :: current
       character(:,CH_),allocatable, intent(in)     :: Str
 
       type(SourceLine), allocatable :: temp
 
       if (Allocated(current)) then
          if (current%String == Str) then
          call move_alloc(current%next, temp)
             call move_alloc(temp, current)
             call Delete(current,Str)
          else
             call Delete(current%next, Str)
          end if
      end if
    end subroutine Delete


pure recursive subroutine Conjuct(current_1,current_2)
   type(SourceLine), allocatable, intent(inout) :: current_1, current_2
   if(Allocated(current_2)) then
      call Delete(current_1,current_2%String)
      call Conjuct(current_1,current_2%Next)
   end if

end subroutine Conjuct   
    
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

 recursive subroutine InsertionSort(List,lastSorted)
    type(SourceLine), allocatable, intent(inout) :: List, lastSorted
    type(SourceLine), allocatable                :: tmp,temp
 
 
    if (Allocated(lastSorted%next)) then

 
       if (len_trim(lastSorted%next%string) >len_trim (lastSorted%string))then
       
         
          !tmp => lastSorted%next
     ! call  move_alloc(lastSorted%next,tmp)
    ! temp = lastSorted%next%next 
    ! tmp = lastSorted%next
       print *,  lastSorted%next%next%String
      ! lastSorted%next => lastSorted%next%next
      lastSorted%next = tmp%next    
      !call move_alloc(lastSorted%next%next, lastSorted%next)
          call Paste(List,tmp)
          call InsertionSort(List,lastSorted)
       else
          call InsertionSort(List,lastSorted%next)
       end if
    end if
   end subroutine InsertionSort
 
 
 
   recursive subroutine Paste(current, itemToInsert)
    type(SourceLine) , allocatable, intent(inout) :: current, itemToInsert
    type(SourceLine),  allocatable               ::  tmp
   
   print*, "hell"
    if (len_trim(itemToInsert%String) > len_trim(current%String)) then
 
    tmp = current
   ! call move_alloc(current, tmp)
   ! current => itemToInsert
   
    call move_alloc(itemToInsert, current)
   ! current%next => tmp

   ! call move_alloc(tmp, current%next)
   current%next = tmp
 
 
    else
       call Paste(current%next,itemToInsert)
    end if
 
 end subroutine Paste
 



end module Source_process
