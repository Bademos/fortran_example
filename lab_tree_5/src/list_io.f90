! Copyright 2015 Fyodorov S. A.

module List_IO
   use Environment

   implicit none


   type node_tree 
      integer              :: value   = 0
      type(node_tree), pointer  :: left   => Null()
      type(node_tree), pointer  :: right  => Null()
   end type node_tree
contains
   function Read_tree(Input_File) result(tree)
      type(node_tree), pointer        :: tree
      character(*), intent(in)   :: Input_File
      integer  In

      ! При чтении только английских букв и цифр лучше открывать как ASCII.
      !open (file=Input_File, encoding=E_, newunit=In)
      open (file=Input_File, newunit=In)
         tree => Null()
         call Read_node_tree(In, tree)
      close (In)
   end function Read_tree
   
   recursive subroutine Read_node_tree(In, tree)
      type(node_tree), pointer, intent(inout)  :: tree
      integer, intent(in)     :: In
      integer  :: IO, value = 0
     
      read (In, '(i2)', iostat=IO, advance='no') value
      call Handle_IO_status(IO, "reading value from file")
      if (IO == 0) then
         call Put_tree(value, tree)
         call Read_node_tree(In, tree)
      end if
   end subroutine Read_node_tree

   recursive subroutine Put_tree(value, current)
      type(node_tree), pointer, intent(inout)  :: current
      integer, intent(in)     :: value

      If (.not. Associated(current)) then
         allocate (current, source=node_tree(value))
         !allocate (current, source=node_tree(value, Null(), Null()))
      else if (value < current%value) then
         call Put_tree(value, current%left)
      else if (value > current%value) then
         call Put_tree(value, current%right)
      end if
   end subroutine Put_tree
   

   subroutine Output_tree(Output_File, tree, List_Name, Position)
      character(*), intent(in)   :: Output_File, Position, List_Name
      type(node_tree), pointer        :: tree
      integer  :: Out
      
      ! При чтении только английских букв и цифр лучше открывать как ASCII.
      !open (file=Output_File, encoding=E_, position=Position, newunit=Out)
      open (file=Output_File, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         if (Associated(tree)) &
            call Output_tree_node(Out, tree)
      close (Out)
   end subroutine Output_tree

   recursive subroutine Output_tree_node(Out, current)
      integer, intent(in)     :: Out
      type(node_tree),intent(in)   :: current
      
      integer  :: IO

      if (Associated(current%left)) &
         call Output_tree_node(Out, current%left)

      write (Out, '(i0, 1x)', advance='no', iostat=IO) current%value 
      call Handle_IO_status(IO, "writing tree")
   
      if (Associated(current%right)) &
         call Output_tree_node(Out, current%right)
   end subroutine Output_tree_node
   
end module List_IO 
