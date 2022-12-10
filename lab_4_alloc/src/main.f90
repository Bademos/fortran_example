! Copyright 2019 Fyodorov S. A.

program reference_lab_list
   use Environment
   use List_Process
   use List_IO

   implicit none
   character(:), allocatable :: input_file, output_file
   integer                   :: num_bracket = 0,pos = 1
   logical                   :: is_closed = .true.
   character(:), allocatable :: res

   class(node), pointer   :: List => Null()

   input_file  = "../data/list.txt"
   
   input_file  = "../data/test1.txt"

   input_file  = "../data/test2.txt"
   input_file  = "../data/test1.txt"
   input_file  = "../data/test1.txt"
   input_file  = "../data/test1.txt"
   input_file  = "../data/test1.txt"
   input_file  = "../data/test1.txt"
   input_file  = "../data/test1.txt"
   input_file  = "../data/test1.txt"
   input_file  = "../data/test1.txt"
   input_file  = "../data/test2.txt"


   output_file = "output.txt"
   
   List => Read_list(input_file)
   
   if (Allocated(List)) then
      !res =  Checking(List,is_closed,pos,num_bracket,"norm")
      call Output_list(output_file, List, "Исходный список:", "rewind",res)

      !call Put(List, 8)
      !call Output_list(output_file, List, "Список после вставки числа:", "append")

      !call Get(List, value)
      !call Output_list(output_file, List, "Список после забирания первого числа:", "append")
      !
      !call Delete(List, 7)
      !call Output_list(output_file, List, "Список после забирания первого числа:", "append")
      !
      !call Get(List, value)
      !call Output_list(output_file, List, "Список после забирания первого числа:", "append")
      !
      !call Output_list2(output_file, List2, "Двунаправленный список:", "append")
      !
      !call Output_ordered_list(output_file, List3, "Неотсортированный список:", "append")
      !call Output_sorted_list(output_file, Sorted_list, "Отсортированный список:", "append")
      !
      !call Output_tree(output_file, tree, "Простой обход дерева:", "append")
      !call Delete_in_tree(tree, 3)
      !call Output_tree(output_file, tree, "Дерево после удалени корня:", "append")
   end if
   
end program reference_lab_list
