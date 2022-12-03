
module Source_IO
   use Environment

   implicit none
   
   type SourceLine
      character(:, CH_),allocatable   :: String
      type(SourceLine), allocatable    :: Next 
   end type SourceLine

contains
   function Read_Source_Code(InputFile) result (Code)
      type(SourceLine), allocatable  :: Code
      character(*), intent(in)   :: InputFile
      integer  :: In
      
      open (file=InputFile, encoding=E_, newunit=In)
         call Read_Source_Line(in,Code)
      close (In)
   end function Read_Source_Code

   recursive subroutine Read_Source_Line(in,Code) 
      type(SourceLine), allocatable  :: Code
      integer, intent(in)        :: In
      

      integer, parameter      :: max_len = 1024
      character(max_len, CH_) :: String
      integer                 :: IO

      allocate(Code)
      read (In, "(a)", iostat=IO) String
      call Handle_IO_Status(IO, "reading line from source code")
      if (IO == 0) then
         Code%String =Trim(String)
         call Read_Source_Line(In,Code%Next)
      else
         deallocate (Code)
      end if
   end subroutine Read_Source_Line
 
   ! Вывод исходного кода.
   subroutine Output_Source_Code(OutputFile, Code,Position)
      character(*), intent(in)      :: OutputFile, Position 
      type(SourceLine), allocatable  :: Code 
      integer  :: Out
      
      open (file=OutputFile, encoding=E_,position = Position, newunit=Out)
         call Output_Source_Line(Out, Code)
      close (Out)
   end subroutine Output_Source_Code

   ! Вывод строки исходного кода.
   recursive subroutine Output_Source_Line(Out, Code)
      integer, intent(in)           :: Out
      type(SourceLine), allocatable  :: Code
      integer  :: IO

      if (Allocated(Code)) then

         write (Out, "(a)", iostat=IO) Code%String

         call Handle_IO_Status(IO, "writing line to file")
         call Output_Source_Line(Out, Code%next)
      end if   
   end subroutine Output_Source_Line
end module Source_IO 
