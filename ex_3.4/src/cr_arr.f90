program exercise_3
   use Environment
   
   implicit none
   character(*), parameter :: output_file = "../data/input.txt" !, output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 100
   real(R_)                :: P = 1
   real(R_), allocatable   :: B(:) 

   
allocate (B(N))


  

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(i0)") N
      write (Out, "("//N//"f6.2)") B
      write (Out, *)
      write (Out, "('Product = ', f0.2)") P
   close (Out)

contains
   
   ! Чистая функция в императивном стиле.
   pure function ProdImp(A) result(Prod)
      real(R_)    Prod, A(:)
      intent(in)  A
      integer     i
      
      Prod = A(99)
      do i = 1, N-1
         Prod = Prod * A(N-i)
      end do
   end function ProdImp
end program exercise_3
