program exercise_7_30
   use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i = 0
   real(R_), allocatable   :: A(:, :)

   open (file=input_file, newunit=In)
      read (In, *) N, M
      allocate (A(M, N))
      read (In, *) (A(:, i), i = 1, N)
   close (In)
  
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//M//'f6.2)') (A(:, i), i = 1, N)
   close (Out)
   
   
   do concurrent( i = 1:N)
      call Sort(A(:,i))
   end do

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '(/'//N//'('//M//'f6.2/))') (A(1:M, i), i = 1, N)
   close (Out)

contains






 pure subroutine SortMatrix(N,M,A)
      integer, intent(in)      :: N,M
      real(R_), intent(inout)  :: A(:,:)

      integer::i,j
     
    
     do concurrent ( i = 1:N)
     call sort(A(:,i))
     end do



 end subroutine SortMatrix



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

end program exercise_7_30
