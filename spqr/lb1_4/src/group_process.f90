
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
           ! Surnames(:,tempnum) = Surnames(:,i)
           ! Nums(:,tempnum) = Nums(:,i)
            Group(tempnum) = Group(i)
            Group(i) = tmp_stud
 
           !Surnames(:,i) = tmpSurnames
           ! Nums(:,i) = tmpNums

         


     end do 


    end subroutine Sort_Ch


    pure recursive subroutine Sort_Ch_rec(Group,i,N)
           type(student), intent(inout)  :: Group(:)
           integer, intent(in)   :: i,N
           integer        ::  j,tempnum
           type(student)  :: tmp_stud
  
           !do i = 1,Size(Group)-1
              tempnum = i
              tmp_stud = Group(i)
  
              do j=i+1, N
                 if(Swap_Ch(Group,tmp_stud,j)) then
                      tempnum = j
                      tmp_stud = Group(j)
                  end if
  
  
  
              end do
              Group(tempnum) = Group(i)
              Group(i) = tmp_stud
  
  
               if (i < N-1)&
                  call Sort_ch_rec(Group,i+1,N)
                  
  
  
      ! end do
  
  
      end subroutine Sort_Ch_rec
                               



    pure subroutine Sort_class_list(Group)
       type(student), intent(inout)  :: Group(:)
 
       integer        :: i, j
       type(student)  :: tmp_stud
 
       do i = Size(Group), 2, -1
          do j = 1, i-1
             if (Swap(Group, j)) then
                tmp_stud = Group(j+1)
                Group(j+1) = Group(j)
                Group(j) = tmp_stud
             end if
          end do
       end do
    end subroutine Sort_class_list


     pure recursive subroutine Sort_dr_rec(Group, N)
        type(student), intent(inout)  :: Group(:)
        integer, intent(in)           :: N
  
        ! Работаем только с первыми N элементами: помещаем в ИХ конец менее успешного.
        call Drop_down(Group, 1, N-1)
  
        ! Если необходимо, делаем то же с последними N-1 элементами.
        if (N >= 3) &
           call Sort_dr_rec(Group, N-1)
     end subroutine Sort_dr_rec
  
     ! Помещаем c j-ой на N-ую позицию менее успешного, поочерёдно сравнивая.
     pure recursive subroutine Drop_down(Group, j, N)
        type(student), intent(inout)  :: Group(:)
        integer, intent(in)           :: j, N
  
        type(student)  :: tmp_stud
  
        ! Если требуется, то меняем местами текущего студента со следующим.
        if (Swap(Group, j)) then
           tmp_stud = Group(j+1)
           Group(j+1) = Group(j)
           Group(j) = tmp_stud
        end if
        if (j < N) &
           call Drop_down(Group, j+1, N)
     end subroutine Drop_down
     

 
    pure logical function Swap(Group, j)
       type(student), intent(in)  :: Group(:)
       integer, intent(in)        :: j
 
       Swap = .false.
          if (Group(j)%Surname > Group(j+1)%Surname) then
             Swap = .true.
          else if (Group(j)%Surname==Group(j+1)%Surname .and. Group(j)%Num>Group(j+1)%Num) then
             Swap = .true.
          end if
    end function Swap


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
       if ((Group(j-1)%Surname > tmp_stud%Surname)) then
             Condition = .true.
       else if ((Group(j-1)%Surname == tmp_stud%Surname) .and. (Group(j-1)%Num > tmp_stud%Num)) then
            Condition = .true.
       end if
    end function Condition

end module group_process
