module List_IO
   use Environment

   implicit none

   ! Структура данных для узла списка.
   ! Инициализация обязательна!
  
     type node
        class(node), allocatable :: next
     end type node
  
     type, extends(node) :: variable
        character(kind=CH_)  :: char = ""
     end type variable
  
     type, extends(variable) :: cringe
     end type cringe
  
     type, extends(variable) :: operation
     end type operation
  
     type, extends(node) :: operand
        integer(I_) :: value = 0
     end type operand
  
     type, extends(node) :: left_bracket
        character(kind=CH_) :: bracket = CH__"("
     end type left_bracket
  
     type, extends(node) :: right_bracket
        character(kind=CH_)  :: bracket = CH__")"
     end type right_bracket

contains


   ! Чтение списка.



   function Read_list(Input_File) result(List)
      type(node), allocatable    :: List
      character(*), intent(in)   :: Input_File
      integer  In

      ! При чтении только английских букв и цифр лучше открывать как ASCII.
      !open (file=Input_File, encoding=E_, newunit=In)
      open (file=Input_File, newunit=In)
        call Read_value(In, List)
      close (In)
   end function Read_list

   ! Чтение следующего значения.
   recursive subroutine Read_value(In, Elem)
      type(node), allocatable :: Elem
      integer, intent(in)     :: In
      integer  IO
      character(kind=CH_)  :: char = ""
      integer              :: value = 0

      
      allocate (Elem)
      read (In, '(a)', iostat=IO, advance='no') char
      call Handle_IO_status(IO, "reading value from file")
      if (IO == 0) then
      select case (char)
          case (CH__'a':CH__'z', CH__'A':CH__'Z')
             allocate (Elem, source=variable(char=char))
          case (CH__'+', CH__'-', CH__'*', CH__'/')
             allocate (Elem, source=operation(char=char))
          case (CH__'0':CH__'9')
             read (char, *) value
             allocate (Elem, source=operand(value=value))
          case (CH__'(')
             allocate (Elem, source=left_bracket())
          case (CH__')')
             allocate (Elem, source=right_bracket())
          case default
             allocate (Elem, source=cringe(char=char))
           end select


          call Read_value(In, Elem%next)
      else
         deallocate (Elem)
      end if
   end subroutine Read_value

   ! Вывод списка.
   subroutine Output_list(Output_File, List, List_Name, Position)
      character(*), intent(in)   :: Output_File, Position, List_Name
      type(node), allocatable    :: List
      integer  :: Out
      
      ! При чтении только английских букв и цифр лучше открывать как ASCII.
      !open (file=Output_File, encoding=E_, position=Position, newunit=Out)
      open (file=Output_File, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         call Output_value(Out, List)
      close (Out)
   end subroutine Output_list

   recursive subroutine Output_value(Out, Elem)
      integer, intent(in)     :: Out
      type(node), allocatable :: Elem
      
      integer  :: IO

      if (allocated(Elem)) then 
         select type (Elem)
             type is (variable)
                write (Out, '(a1)', advance='no', iostat=IO) Elem%char ! TODO g0
             type is (operation)
                write (Out, '(a1)', advance='no', iostat=IO) Elem%char
             type is (cringe)
                write (Out, '(a1)', advance='no', iostat=IO) Elem%char
             type is (left_bracket)
                write (Out, '(a1)', advance='no', iostat=IO) Elem%bracket
             type is (operand)
                write (Out, '(i1)', advance='no', iostat=IO) Elem%value
             type is (right_bracket)
                write (Out, '(a1)', advance='no', iostat=IO) Elem%bracket
   end select

         call Handle_IO_status(IO, "writing list")
         call Output_value(Out, Elem%next)
      end if
   end subroutine Output_value
   
end module List_IO 
