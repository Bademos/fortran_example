program exercise_7_3
   use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0,N=25, M = 50
   real(R_), allocatable   :: A(:),B(:),C(:)

   open (file=input_file, newunit=In)
      !read (In, *) M
      allocate (A(N))
      read (In, *) A
      allocate (B(M))
      read (In, *) B 
   close (In)

  ! allocate(C(N+M))

   C = [A,B]
   

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "("//N//"f6.1)") A
   close (Out)
 
   open (file=output_file, encoding=E_, newunit=Out)
   
      write (Out, "("//M//"f6.1)") B
   close (Out)
   !call SortNegativesImp(A, Neg, Negatives)
   call Sort(C)
  ! call Invert(C)
   print *, size(c) 
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, "(/"//N+M//"f6.1)") C
   close (Out)

contains

   pure subroutine Sort(C)
      real(R_), intent(inout) :: C(:)

      real(R_) :: tmp
      integer  :: i, MinInd

      do i = 1, Size(C)-1
         MinInd = MinLoc(C(i:), 1) + i-1
         if (i /= MinInd) then
            tmp               = C(i)
            C(i)      = C(MinInd)
            C(MinInd) = tmp
         end if
      end do
   
   end subroutine Sort

   pure subroutine Invert(C)
      
      real(R_), intent(inout) :: C(:)
      real(R_) :: tmp
      integer  :: i, anti

      do i = 0, Size(C)/2+1

         anti = ubound(C,1)-i
         if ((i+1) /= anti) then
            tmp = C(i)
            C(i) = C(anti)
            C(anti) = tmp
         end if
      end do


   end subroutine Invert

end program exercise_7_3
