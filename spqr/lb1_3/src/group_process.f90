
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






    pure subroutine Sort_class_list(Group)
       type(student), intent(inout)  :: Group(:)
 
       integer        :: i, j
       type(student)  :: tmp_stud
 
       ! Сортировка списка класса по среднему баллу методом пузырька.
       do i = Size(Group), 2, -1
          ! Просматриваем список с начала, ставя в конец менее успешного.
          do j = 1, i-1
             ! Проверка на то, стоит ли менять учащихся местами.
             if (Swap(Group, j)) then
                ! Перестановка местами двух эелементов списка, начиная с текущего.
                tmp_stud = Group(j+1)
                Group(j+1) = Group(j)
                Group(j) = tmp_stud
             ! Group(j:j+1) = Group(j+1:j:-1)
             end if
          end do
       end do
    end subroutine Sort_class_list
 
    ! Проверка того, стоит ли менять местами текущего учащегося со следующим.
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
