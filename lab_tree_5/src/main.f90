! Copyright 2019 Fyodorov S. A.

program reference_lab_list
   use Environment
   use List_Process
   use List_IO

   implicit none
   character(:), allocatable :: input_file, output_file

   integer              :: value = 0
   type(node_tree), pointer :: tree => Null()

   input_file  = "../data/list.txt"
   output_file = "output.txt"
   
   tree => Read_tree(input_file)
   
   
      
      call Output_tree(output_file, tree, "Простой обход дерева:", "append")
      call Delete_in_tree(tree, 3)
      call Output_tree(output_file, tree, "Дерево после удалени корня:", "append")
   
   
end program reference_lab_list
