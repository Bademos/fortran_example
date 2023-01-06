
module List_IO
   use Environment

   implicit none

   type symbol
      character(kind=CH_) :: char = ""
      class(symbol), pointer :: next => Null()
   end type symbol



contains
   function Read_list(Input_File) result(List)
      class(symbol), pointer        :: List
      character(*), intent(in)   :: Input_File
      integer  In

      open (file=Input_File, newunit=In)
        List => Read_value(In)
       
      close (In)
   end function Read_list


   recursive function Read_value(In) result(Elem)
      class(symbol), pointer  :: Elem
      integer, intent(in)     :: In
      integer  IO

      character(kind=CH_)  :: char = ""
      integer              :: value = 0
      read (In, '(a1)', iostat=IO, advance='no') char
      if (IO == 0) then
               allocate (Elem, source=symbol(char=char))
         Elem%next => Read_value(In)
      else
         nullify (Elem)
      end if
   end function Read_value


    subroutine Output_list(Output_File, List, List_Name, Position)
      character(*), intent(in)   :: Output_File, Position, List_Name
      class(symbol), pointer        :: List
      integer  :: Out
      
      open (file=Output_File, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         call Output_value(Out, List)
      close (Out)
   end subroutine Output_list

   recursive subroutine Output_value(Out, Elem)
      integer, intent(in)     :: Out
      class(symbol), pointer     :: Elem
      
      integer  :: IO

      if (Associated(Elem)) then 
               write (Out, '(a1)', advance='no', iostat=IO) Elem%char 
         call Handle_IO_status(IO, "writing list")
         call Output_value(Out, Elem%next)
      end if
   end subroutine Output_value





end module List_IO


