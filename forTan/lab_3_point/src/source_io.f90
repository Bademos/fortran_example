
module Source_IO
   use Environment

   implicit none
   
   type SourceLine
      character(:, CH_), allocatable   :: String 
      character(:, CH_), allocatable   :: date
      type(SourceLine), pointer        :: Next  => Null()
   end type SourceLine

contains
   function Read_Source_Code(InputFile) result (Code)
      type(SourceLine), pointer  :: Code
      character(*), intent(in)   :: InputFile
      integer  :: In
      
      open (file=InputFile, encoding=E_, newunit=In)
         Code => Read_Source_Line(in)
      close (In)
   end function Read_Source_Code

   ! Чтение строки исходного кода.
   recursive function Read_Source_Line(in) result(Code)
      type(SourceLine), pointer  :: Code
      integer, intent(in)        :: In
      integer, parameter      :: max_len = 14, max_leN_date =4
      character(max_len, CH_) :: string

      character(max_len_date, CH_) :: date
      integer                 :: IO

   

      read (In, '(2(a,1x))', iostat=IO) string, date
      call Handle_IO_Status(IO, "reading line from source code")
      if (IO == 0) then
         allocate (Code)
         Code%String = string
         Code%date = date
         Code%Next => Read_Source_Line(In)
      else
         Code => Null()
      end if
   end function Read_Source_Line
 
   subroutine Output_Source_Code(OutputFile, Code)
      character(*), intent(in)      :: OutputFile 
      type(SourceLine), intent(in)  :: Code 
      integer  :: Out
      
      open (file=OutputFile, encoding=E_, newunit=Out)
         call Output_Source_Line(Out, Code)
      close (Out)
   end subroutine Output_Source_Code

   recursive subroutine Output_Source_Line(Out, Code)
      integer, intent(in)           :: Out
      type(SourceLine), intent(in)  :: Code
      integer  :: IO

      write (Out, '(2(a,1x))', iostat=IO) Code%String, Code%date
      call Handle_IO_Status(IO, "writing line to file")
      if (Associated(Code%next)) &
         call Output_Source_Line(Out, Code%next)
   end subroutine Output_Source_Line
end module Source_IO 
