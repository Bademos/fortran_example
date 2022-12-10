! Copyright 2019 Fyodorov S. A.

module List_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use List_IO

   implicit none

contains

   pure recursive subroutine Delete_in_tree(current, value)
      type(node_tree), pointer  :: current
      integer, intent(in)  :: value

      type(node_tree), pointer  :: left, right
     
      if (Associated(current)) then
        if (current%value == value) then
           left  => current%left
           right => current%right
           deallocate (current)
           current => right
           if (Associated(left)) &
              call Put_to_left(current, left)
        else if (current%value > value) then
           call Delete_in_tree(current%left, value)
        else if (current%value < value) then
           call Delete_in_tree(current%right, value)
        end if
      end if
   end subroutine Delete_in_tree

   pure recursive subroutine Put_to_left(current, left)
      type(node_tree), pointer  :: current
      type(node_tree), pointer  :: left

      if (.not. Associated(current)) then
         current => left
      else
         call Put_to_left(current%left, left)
      end if
   end subroutine Put_to_left
end module List_process
