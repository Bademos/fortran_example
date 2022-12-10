
module Group_Process
   use Environment
   use Group_IO

   implicit none
   
contains
 pure recursive subroutine Sort_rec(Group,i,N)
 
        type(student), intent(inout)  :: Group(:)
        integer :: i,j,N
        intent(in) :: i,N
 
        type(student)  :: tmp_stud
 
           tmp_stud = Group(i)
            j = i
          do while (j>1.and.Condition(Group,tmp_stud,j))
               Group(j) = Group(j-1)
               j = j-1
 
          end do
          Group(j) = tmp_stud
          if (i+1<N) then
            call Sort_rec(Group,i+1,N)
          end if

 
       end subroutine Sort_rec
 
 pure logical function Condition( Group, tmp_stud, j)
        
        type(student), intent(in)  :: Group(:), tmp_stud
        integer , intent (in)      :: j

       Condition = .false.
       if ((Group(j-1)%Num < tmp_stud%Num)) then
            Condition = .true.
       end if
    end function Condition

end module group_process
