program exercise_7_42
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0,M = 0, i = 0, j = 0
   real(R_), allocatable   :: A(:, :), SumCol(:), SumRow(:)

   open (file=input_file, newunit=In)
      read (In, *) N
      read (In, *) M
      allocate (A(N, M))
      read (In, *) (A(j, :), j = 1, N)
   close (In)
   
   SumCol = Sum(A,dim=1)

   SumRow = Sum(A, dim=2)
   


   open (file=output_file, encoding=E_, newunit=Out)
     write (Out, '('//M//'f6.2)') SumCol
     write (Out, '('//N//'f6.2)') SumRow
   close (Out)
  
   

end program exercise_7_42
