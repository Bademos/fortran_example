
module Group_Process
   use Environment
   use Group_IO

   implicit none
   
contains

    pure subroutine Sort_Ch(Group)
         type(student), intent(inout)  :: Group(:)
 
         integer        :: i, j,tempnum
         type(student)  :: tmp_stud

         do i = 1,Size(Group)-1
            tempnum = i
            tmp_stud = Group(i)
            
            do j=i+1, Size(Group)
               if(Swap_Ch(Group,tmp_stud,j)) then
                    tempnum = j
                    tmp_stud = Group(j)
                end if
 
 
 
            end do
            Group(tempnum) = Group(i)
            Group(i) = tmp_stud
 

         


     end do


    end subroutine Sort_Ch



    pure logical function Swap_ch(Group,tmp_std, j)
         type(student), intent(in)  :: Group(:), tmp_std
         integer, intent(in)        :: j
  
         Swap_ch = .false.
            if (tmp_std%Surname > Group(j)%Surname) then
               Swap_ch = .true.
            else if (tmp_std%Surname==Group(j)%Surname .and. tmp_std%Num>Group(j)%Num) then
               Swap_ch = .true.
            end if
      end function Swap_ch



 

end module group_process
