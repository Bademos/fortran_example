
module Group_IO
   use Environment

   implicit none
   integer, parameter :: STUD_AMOUNT   = 5
   integer, parameter :: SURNAME_LEN   = 15
   integer, parameter :: INITIALS_LEN  = 5
   integer, parameter :: MARKS_AMOUNT  = 5
   !integer            :: N = 0

   type student
      character(SURNAME_LEN, kind=CH_)    :: Surname              = ""
      character(INITIALS_LEN, kind=CH_)   :: Initials             = ""
      type(student), pointer              :: next                 => NULL()
      
   end type student
 
 

contains



   function Read_class_list(Input_File) result(Class_List)
      type(student), pointer     :: Class_List
      character(*), intent(in)   :: Input_File
      integer  In
 
      open (file=Input_File, encoding=E_, newunit=In)
         Class_List => Read_student(In)
      close (In)
   end function Read_class_list




 recursive function Read_student(In) result(Stud)
     type(student), pointer  :: Stud,tmp
     integer, intent(in)     :: In
     integer  IO
     character(:), allocatable  :: format

     allocate (Stud)
     allocate(tmp)
     format = '(2(a, 1x))'
     read (In, format, iostat=IO) stud%Surname, stud%Initials
     call Handle_IO_status(IO, "reading line from file")
     
     if (IO == 0) then
        
        Stud%next => Read_student(In)
        !tmp => Stud%next
        !tmp => Stud%next
       ! tmp%prev => Stud
     else
        deallocate (Stud)
        nullify (Stud) 
     end if
  end function Read_student

  
 !  subroutine two_ord(Group_List)
 !     type(student),    intent(inout):: Group_List

 !     type(student), pointer                  ::   tmp,gp
 !     integer              ::     N
 !     
 !     N=5
 !     gp => Group_List

 !     do while(N>0) 
 !        tmp => gp
 !        gp => gp%next
 !        gp%prev =>tmp
 !        N = N-1
 !        print*, gp%Initials
 !     end do
 !     print *, N
 !  end subroutine two_ord
 !     




  subroutine Output_class_list(Output_File, Class_List, List_Name, Position)
      character(*), intent(in)   :: Output_File, Position, List_Name
      type(student), intent(in)  :: Class_List
      integer  :: Out
 
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         call Output_student(Out, Class_List)
      close (Out)
   end subroutine Output_class_list
 
   recursive subroutine Output_student(Out, Stud)
      integer, intent(in)        :: Out
      type(student), intent(in)  :: Stud
 
      integer  :: IO
      character(:), allocatable  :: format
 
      format = '(2(a, 1x))'
      write (Out, format, iostat=IO) Stud%Surname, Stud%Initials
      call Handle_IO_status(IO, "writing student")
      if (Associated(Stud%next)) &
         call Output_student(Out, Stud%next)
   end subroutine Output_student






end module Group_IO 
