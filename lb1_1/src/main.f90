! Copyright 2015 Fyodorov S. A

program reference_lab_1_1
   use Environment

   implicit none
   integer, parameter               :: STUD_AMOUNT = 4, SURNAME_LEN = 15, INITIALS_LEN = 5
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_) !CH__"\u1052" 

   character(:), allocatable  :: input_file, output_file, format

   character(SURNAME_LEN, kind=CH_)                :: tmpSurname = "", Surnames(STUD_AMOUNT) = ""
   
   character(INITIALS_LEN, kind=CH_)               :: tmpInitials = "", Initials(STUD_AMOUNT) = ""

   integer :: In, Out, IO, i, j
   integer, parameter                              :: INDEXES(*) = [(i, i = 1, STUD_AMOUNT)]
   logical :: Swap

   
   input_file = "../data/input.txt"
   output_file = "output.txt"

   open (file=input_file, encoding=E_, newunit=In)

      format = '(2(a, 1x))'

      read (In, format, iostat=IO) (Surnames(i), Initials(i), i = 1, STUD_AMOUNT)

   close (In)

!   Out = OUTPUT_UNIT
!   open (Out, encoding=E_)
!   select case(io)
!      case(0)
!      case(IOSTAT_END)
!         write (Out, '(a)') "End of file has been reached while reading class list."
!      case(1:)
!         write (Out, '(a)') "Error while reading class list: ", io
!      case default
!         write (Out, '(a)') "Undetermined error has been reached while reading class list: ", io
!   end select
!
call Check_Error_rd(IO)

   open (file=output_file, encoding=E_, newunit=Out)
      write (out, '(a)') "Исходный список:"
      write (Out, format, iostat=IO) (Surnames(i), Initials(i), i = 1, STUD_AMOUNT)
   close (Out)
 
  ! Out = OUTPUT_UNIT
  ! open (Out, encoding=E_)
  ! select case(io)
  !    case(0)
  !    case(IOSTAT_END)
  !       write (Out, '(a)') "End of file has been reached while writing class list."
  !    case(1:)
  !       write (Out, '(a)') "Error while writing class list: ", io
  !    case default
  !       write (Out, '(a)') "Undetermined error has been reached while writing class list: ", io
  ! end select

  call Check_Error_wr(IO)


 ! do i = STUD_AMOUNT, 2, -1
 !     do j = 1, i-1
 !        Swap = .false.
 !       ! Проверка на то, стоит ли менять учащихся местами.
 !           if (Surnames(j) > Surnames(j+1)) then
 !              Swap = .true.
 !           else if (All([(Surnames(j)==Surnames(j+1))]) .and. Initials(j) > Initials(j+1)) then
 !              Swap = .true.
 !           end if

 !        if (Swap) then
 !           tmpSurname      = Surnames(j+1)
 !           Surnames(j+1)   = Surnames(j)
 !           Surnames(j)     = tmpSurname

 !           tmpInitials     = Initials(j+1)
 !           Initials(j+1)   = Initials(j)
 !           Initials(j)     = tmpInitials

 !        end if
 !     end do
 !  end do
 
 call Sort(Surnames,Initials)

  open (file=output_file, encoding=E_, position='append', newunit=Out)
     write (out, '(/a)') "Ordered list:"
     write (Out, format, iostat=IO) &
        (Surnames(i), Initials(i), i = 1, STUD_AMOUNT)
  close (Out)
  
 ! Out = OUTPUT_UNIT
 ! open (Out, encoding=E_)
 ! select case(io)
 !    case(0)
 !    case(IOSTAT_END)
 !       write (Out, '(a)') "End of file has been reached while writing sorted boys list."
 !    case(1:)
 !       write (Out, '(a)') "Error while writing sorted boys list: ", io
 !    case default
 !       write (Out, '(a)') "Undetermined error has been reached while writing sorted boys list: ", io
 ! end select

  call Check_Error_wr(IO)


contains
    subroutine Sort(Surnames,Initials)
      
      integer, parameter               :: STUD_AMOUNT = 4, SURNAME_LEN = 15, INITIALS_LEN = 5
       

      character(SURNAME_LEN,  kind=CH_), intent(inout) :: Surnames(:)
     
      character(INITIALS_LEN, kind=CH_), intent(inout) :: Initials(:)
      integer :: i,j
      character(SURNAME_LEN,  kind=CH_ ) :: tmpInitials
      character(INITIALS_LEN, kind=Ch_)  :: tmpSurnames
      logical :: Swap
      do i = STUD_AMOUNT, 2, -1
        do j = 1, i-1
           Swap = .false.
              if (Surnames(j) > Surnames(j+1)) then
                 Swap = .true.
              else if (ALL([(Surnames(j)==Surnames(j+1))]) .and. Initials(j) > Initials(j+1))     then
                 Swap = .true.
              end if
 
           if (Swap) then
              tmpSurname      = Surnames(j+1)
              Surnames(j+1)   = Surnames(j)
              Surnames(j)     = tmpSurname
 
              tmpInitials     = Initials(j+1)
              Initials(j+1)   = Initials(j)
              Initials(j)     = tmpInitials
 
           end if
        end do
     end do


   end subroutine Sort

   subroutine Check_Error_wr(IO)
      integer , intent(in) :: IO
      integer              :: OUT = OUTPUT_UNIT
       open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing  list."
      case(1:)
         write (Out, '(a)') "Error while writing sorted  list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing list: ", io
   end select
   end subroutine Check_Error_wr

   subroutine Check_Error_rd(IO)
      integer , intent(in) :: IO
      integer              :: OUT = OUTPUT_UNIT
       open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while reading list."
      case(1:)
         write (Out, '(a)') "Error while read list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while reading list: ", io
   end select
   end subroutine Check_Error_rd



end program reference_lab_1_1
