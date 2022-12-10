
module Group_Process
   use Environment
   use Group_IO

   implicit none
   
contains


  pure subroutine Sort_ins(Group)
 
         type(student), intent(inout)  :: Group(:)
         integer :: i,j
 
         type(student)  :: tmp_stud
         
         do  i = 2, size(Group)
            tmp_stud = Group(i)
             j = i
           do while (j>1.and.Condition(Group,tmp_stud,j))
                Group(j) = Group(j-1)
                j = j-1
 
           end do
           Group(j) = tmp_stud
 
         end do
        end subroutine Sort_ins
 
 pure logical function Condition( Group, tmp_stud, j)
        
        type(student), intent(in)  :: Group(:), tmp_stud
        integer , intent (in)      :: j

       Condition = .false.
       if ((Group(j-1)%Num < tmp_stud%Num)) then
            Condition = .true.
       end if
    end function Condition

end module group_process
