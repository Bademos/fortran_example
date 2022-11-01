program exercise_8_23
   use Environment
   use Integral_IO
   use Integral_calculate 

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   real(R_)                :: p1 = 0, p2 = 0, delta_p = 0, q1 = 0, q2 = 0, delta_q = 0;

   real(R_), allocatable   :: I(:), P(:), Q(:), X(:), Integ(:,:)
   integer                 :: M = 0, L = 0

   call ReadP(input_file, p1, p2, delta_p, q1, q2, delta_q)
   
   call OutputP(output_file, p1, p2, delta_p, q1, q2, delta_q)

   M = Int((p2 - p1) / delta_p + .5_R_) + 1
   
   L = Int((q2 - q1) / delta_q + .5_R_) + 1
   allocate(P(M),Q(L), X(N),I(M*L),Integ(M,L))

   !call Integral_Imp(p1, delta_p, P,Q, X, I)
   call Integral(p1, delta_p,q1, delta_q, P,Q, X, I, Integ)
   print *, Integ 
   call OutputIntegral(output_file, P,Q, I)
end program exercise_8_23
