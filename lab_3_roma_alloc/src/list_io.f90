
module List_IO
   use Environment

   implicit none

     type node
        character(CH_)           :: value = ""
        type(node), allocatable :: next
     end type node
  
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
  
        allocate (Elem)
        read (In, '(a1)', iostat=IO, advance='no') Elem%value
        call Handle_IO_status(IO, "reading value from file")
        if (IO == 0) then
            call Read_value(In, Elem%next)
        else
           deallocate (Elem)
        end if
     end subroutine Read_value
  
     ! Вывод списка.
     subroutine Output_list(Output_File, List1, List2,  Set, Position)
        character(*), intent(in)   :: Output_File, Position
        type(node), allocatable    :: List1, List2, Set
        integer  :: Out
  
        ! При чтении только английских букв и цифр лучше открывать как ASCII.
        !open (file=Output_File, encoding=E_, position=Position, newunit=Out)
        open (file=Output_File, position=Position, newunit=Out)
           write (out, '(/a)') "First string:"
           call Output_value(Out, List1)

           write (out, '(/a)') "Second string:"
           
           call Output_value(Out,List2)

           
           write (out, '(/a)') "Result divided set:"
           call Output_value(Out,Set)
            
        close (Out)
     end subroutine Output_list
  
     recursive subroutine Output_value(Out, Elem)
        integer, intent(in)     :: Out
        type(node), allocatable :: Elem
  
        integer  :: IO
  
        if (allocated(Elem)) then
           write (Out, '(a1)', advance='no', iostat=IO) Elem%value
           call Handle_IO_status(IO, "writing list")
           call Output_value(Out, Elem%next)
        end if
     end subroutine Output_value



end module List_IO


