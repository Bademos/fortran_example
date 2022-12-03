
program lab_1_1
   use Environment

   implicit none
   integer, parameter               :: STUD_AMOUNT = 6, SURNAME_LEN = 15, INITIALS_LEN = 5
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

   
call Check_Error_rd(IO,isRead=.true.)

   open (file=output_file, encoding=E_, newunit=Out)
      write (out, '(a)') "Исходный список:"
      write (Out, format, iostat=IO) (Surnames(i), Initials(i), i = 1, STUD_AMOUNT)
   close (Out)
 

  call Check_Error_rd(IO,isRead=.false.)


 
 call Sort_ins(Surnames,Initials)

  open (file=output_file, encoding=E_, position='append', newunit=Out)
     write (out, '(/a)') "Ordered list:"
     write (Out, format, iostat=IO) &
        (Surnames(i), Initials(i), i = 1, STUD_AMOUNT)
  close (Out)
  

  call Check_Error_rd(IO,isRead=.false.)


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
              else if (ALL([((Surnames(j)==Surnames(j+1)) .and. (Initials(j) > Initials(j+1)))]))     then
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

   subroutine Sort_ins(Surnames,Initials)
 
       integer, parameter               :: STUD_AMOUNT = 6, SURNAME_LEN = 15, INITIALS_LEN = 5
 
 
       character(SURNAME_LEN,  kind=CH_), intent(inout) :: Surnames(:)
 
       character(INITIALS_LEN, kind=CH_), intent(inout) :: Initials(:)
       integer :: i,j
       character(INITIALS_LEN,  kind=CH_ ) :: tmpInitials
       character(SURNAME_LEN, kind=Ch_)  :: tmpSurnames
       logical :: Swap
       character(SURNAME_LEN+INITIALS_LEN, kind=CH_),allocatable  :: tmp(:)
       character(SURNAME_LEN+INITIALS_LEN, kind=CH_) :: tmpUN
       
       do i = 2, STUD_AMOUNT
          tmpSurnames = Surnames(i)
          tmpInitials = Initials(i)
           j = i
           
           do while (j>1.and.((Surnames(j-1)>tmpSurnames ).or.(Surnames(j-1)==tmpSurnames).and.(Initials(j-1)>tmpInitials)))
              
              Surnames(j) = Surnames(j-1)
              Initials(j) = Initials(j-1)
               j = j-1
                
         end do
         Surnames(j) = tmpSurnames
         Initials(j) = tmpInitials

      end do
 
 
   end subroutine Sort_ins

   subroutine Check_Error_rd(IO,isRead)
      integer , intent(in) :: IO
      logical , intent(in) :: isRead
      integer              :: OUT = OUTPUT_UNIT
      character(:), allocatable :: rw
       open (Out, encoding=E_)
  
       if (isRead) then
          rw = "perceiv"
       else
          rw = "writ"
       end if


    select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while ",rw,"ing list."
      case(1:)
         write (Out, '(a)') "Error while, ",rw,"e list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while ",rw,"ing list: ", io
   end select
   end subroutine Check_Error_rd



end program lab_1_1
