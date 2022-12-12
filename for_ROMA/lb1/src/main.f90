
program lab_1_1
   use Environment

   implicit none
   integer, parameter               ::STUD_AMOUNT=3, SURNAME_LEN = 14, NUM_LEN = 10

   character(:), allocatable  :: input_file, output_file, format

   character(SURNAME_LEN, kind=CH_)                :: tmpSurname = "", Surnames(STUD_AMOUNT) = ""
   
   character(NUM_LEN, kind=CH_)               :: tmpNum = "", Nums(STUD_AMOUNT) = ""

   integer :: In, Out, IO, i, j
   integer, parameter                              :: INDEXES(*) = [(i, i = 1, STUD_AMOUNT)]
   logical :: Swap

   
   input_file = "../data/list.exe"
   output_file = "output.txt"

   open (file=input_file, encoding=E_, newunit=In)

      format = '(2(a, 1x))'

      read (In, format, iostat=IO) (Surnames(i), Nums(i), i = 1, STUD_AMOUNT)

   close (In)

   
call Check_Error_rd(IO,isRead=.true.)

   open (file=output_file, encoding=E_, newunit=Out)
      write (out, '(a)') "Исходный список:"
      write (Out, format, iostat=IO) (Surnames(i), Nums(i), i = 1, STUD_AMOUNT)
   close (Out)
 

  call Check_Error_rd(IO,isRead=.false.)


 
 call Sort_ins(Surnames,Nums)

  open (file=output_file, encoding=E_, position='append', newunit=Out)
     write (out, '(/a)') "Ordered list:"
     write (Out, format, iostat=IO) &
        (Surnames(i), Nums(i), i = 1, STUD_AMOUNT)
  close (Out)
  

  call Check_Error_rd(IO,isRead=.false.)


contains

   subroutine Sort_ins(Surnames,Nums)
 
       integer, parameter               :: STUD_AMOUNT = 3, SURNAME_LEN = 14, NUMS_LEN = 10
 
 
       character(SURNAME_LEN,  kind=CH_), intent(inout) :: Surnames(:)
 
       character(NumS_LEN, kind=CH_), intent(inout) :: Nums(:)
       integer :: i,j
       character(NUMS_LEN,  kind=CH_ ) :: tmpNums
       character(SURNAME_LEN, kind=Ch_)  :: tmpSurnames
       logical :: Swap
       character(SURNAME_LEN+NUMS_LEN, kind=CH_),allocatable  :: tmp(:)
       character(SURNAME_LEN+NUMS_LEN, kind=CH_) :: tmpUN
       
       do i = 2, STUD_AMOUNT
          tmpSurnames = Surnames(i)
          tmpNums = Nums(i)
           j = i
           
           do while (j>1.and.(Nums(j-1)<tmpNums))
              
              Surnames(j) = Surnames(j-1)
              Nums(j) = Nums(j-1)
               j = j-1
                
         end do
         Surnames(j) = tmpSurnames
         Nums(j) = tmpNums

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
