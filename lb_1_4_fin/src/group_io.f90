! Copyright 2015 Fyodorov S. A.

module Group_IO
   use Environment

   implicit none
   integer, parameter :: STUD_AMOUNT   = 9500
   integer, parameter :: SURNAME_LEN   = 15
   integer, parameter :: INITIALS_LEN  = 5
   integer, parameter :: MARKS_AMOUNT  = 5

   ! Структура данных для хранения данных о студенте.
   type student
      character(SURNAME_LEN, kind=CH_)    :: Surname              = ""
      character(INITIALS_LEN, kind=CH_)   :: Initials             = ""
   end type student
   
contains
   subroutine Create_data_file(Input_File, Data_File)
      character(*), intent(in)   :: Input_File, data_file
      
      type(student)              :: stud
      integer                    :: In, Out, IO, i, recl
      character(:), allocatable  :: format
      
      open (file=Input_File, encoding=E_, newunit=In)
      recl = (SURNAME_LEN + INITIALS_LEN)*CH_
      open (file=Data_File, form='unformatted', newunit=Out, access='direct', recl=recl)
         format = '(2(a, 1x),)'
         do i = 1, STUD_AMOUNT
            read (In, format, iostat=IO) stud
            call Handle_IO_status(IO, "reading formatted class list, line " // i)
            
            write (Out, iostat=IO, rec=i) stud
            call Handle_IO_status(IO, "creating unformatted file with class list, record " // i)
         end do
      close (In)
      close (Out)
   end subroutine Create_data_file

   function Read_class_list(Data_File) result(Group)
      type(student)                 Group(STUD_AMOUNT)
      character(*), intent(in)   :: Data_File

      integer In, IO, recl
      
      recl = ((SURNAME_LEN + INITIALS_LEN)*CH_) * STUD_AMOUNT
      open (file=Data_File, form='unformatted', newunit=In, access='direct', recl=recl)
         read (In, iostat=IO, rec=1) Group
         call Handle_IO_status(IO, "reading unformatted class list")
      close (In)
   end function Read_class_list
 
   subroutine Output_class_list(Output_File, Group, List_name, Position)
      character(*), intent(in)   :: Output_File, Position, List_name
      type(student), intent(in)  :: Group(:)
      integer                    :: Out, IO
      character(:), allocatable  :: format
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_name
         format = '(2(a, 1x))'
         write (Out, format, iostat=IO) Group
         call Handle_IO_status(IO, "writing " // List_name)
      close (Out)
   end subroutine Output_class_list
end module Group_IO 
