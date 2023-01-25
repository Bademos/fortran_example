
module List_Process
   use Environment
  
   use List_IO

   implicit none
contains


  pure recursive function isContainChar(chr,list,isChar) result(Res)
         logical , intent(in) :: isChar
         logical              :: Res
         type(node),intent(in)  :: list
         character(kind=CH_), intent(in) :: chr
         Res = isChar      
         Res =  (chr == list%value)
   
         If (.not.Res.and.allocated(list%next)) then
            
            Res = isContainChar(chr, list%next, res)
         end if
   
   end function isContainChar

     
          recursive subroutine setMaking(List, Res)
            type(node), allocatable :: List, Res,tmp
            
             if(.not.allocated(res)) then
               ! Allocate(res) 
               ! res => List
                call move_alloc(List,res)
                
               ! List =>  res%next
                call move_alloc(res%next, List)
                !res%next=>Null()
               ! deallocate(res%next)

             end if
               

             if (Allocated(List%next)) then
               if(.not.isContainChar(List%value,res,.false.).and.&
                .not.(List%value==' ').and..not.(List%value==',').and.&
                .not.(List%value=='.'))then
                !tmp => list
              ! allocate(tmp)
                call move_alloc(list,tmp)

                !list => tmp%next
                call move_alloc(tmp%next, list)

                !tmp%next => null()
                ! deallocate(tmp)
                call  Conjuct(res,tmp)
                call setMaking(List,Res)
               else
                call setMaking(List%next, Res)

               end if
            end if
          
          end subroutine setMaking



 
    pure recursive subroutine Delete(current, value)
       type(node), allocatable, intent(inout) :: current
       character(CH_), intent(in)     :: value
 
       type(node), allocatable :: temp
 
       if (Allocated(current)) then
          if (current%value == value) then
             ! current = current%next
          ! call move_alloc(current%next, current)
          call move_alloc(current%next, temp)
             call move_alloc(temp, current)
          else
             call Delete(current%next, value)
          end if
      end if
    end subroutine Delete


   recursive subroutine alterSetDivide(set1,set2)
      type(node), allocatable :: set1,set2, tmp

      if(isContainChar(set2%value,set1,.false.)) then
         call Delete(set2, set2%value)
         if(Allocated(set2%next)) then
            call alterSetDivide(set1,set2)
         end if
       else
         if(Allocated(set2%next)) then
          call alterSetDivide(set1,set2%next)
         end if
      end if

   end subroutine alterSetDivide



 recursive subroutine Conjuct(main_list, tail)
    type(node), allocatable::main_list, tail

      if(Allocated(main_list)) then

        if (allocated(main_list%next)) then
           call Conjuct(main_list%next, tail)
        else 
          ! main_list%next = tail
          call move_alloc(tail, main_list%next)

        end if
      else
         main_list = tail
         !call move_alloc(tail,main_list)
        end if
 
 end subroutine Conjuct

end module List_process
