! Copyright 2015 Fyodorov S. A.

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
pure recursive subroutine Sort_rec(Group,i,N)
 
        type(student), intent(inout)  :: Group(:)
        integer :: i,j,N
        intent(in) :: i,N
 
        type(student)  :: tmp_stud
 
           tmp_stud = Group(i) !i == j+1
           !if(Associated(Group%next))
            j = i-1
          do while (j>=1.and.Condition(Group,tmp_stud,j))
              ! Group(j+1) = Group(j)
              ! j = j-1
              call change(Group,j)
 
          end do
          !Group(j+1) = tmp_stud
          call rechange(Group, tmp_stud, j)
          if (i+1<N) then
            call Sort_rec(Group,i+1,N)
          end if

 
       end subroutine Sort_rec


  pure   subroutine   change(Group,j)
     type(student) ,intent(inout)     :: Group(:)
      integer, intent(inout)             :: j

      Group(j+1) = Group(j)
      j = j -1 
  end subroutine change

   pure subroutine rechange(Group, tmp_stud,j)

     type(student) ,intent(inout)     :: Group(:), tmp_stud

      integer, intent(inout)             :: j


          Group(j+1) = tmp_stud
      
  end subroutine rechange
 
 pure logical function Condition( Group, tmp_stud, j)
        
        type(student), intent(in)  :: Group(:), tmp_stud
        integer , intent (in)      :: j

       Condition = .false.
       if ((Group(j)%Surname > tmp_stud%Surname)) then
             Condition = .true.
       else if ((Group(j)%Surname == tmp_stud%Surname) .and. (Group(j)%Initials > tmp_stud%Initials)) then
            Condition = .true.
       end if
    end function Condition

end module group_process
