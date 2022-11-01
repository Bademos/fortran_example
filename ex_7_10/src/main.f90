program exercise_7_10
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, i = 0
   real(R_), allocatable   :: Z(:,:), Sums(:), P(:), Q(:),K(:),PQ(:)
   real(R_)                :: s = 0
   logical, allocatable    :: Res(:)

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (Z(N, N),P(N),Q(N),Sums(N),K(N),Res(N))
      read (In, *) (Z(i, :), i = 1, N)
   close (In)

   call  PQmyst(Z,Sums,P,Q,N,K,Res)
   
   !print *, Sum([(Aps(i,i),i=1,N)]
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//N//'f6.2)') (P(i), i = 1, N)
      write (Out, '('//N//'f6.2)') (Q(i), i = 1, N)
      write (Out,* ) (Res(i), i =1 , N) 


  ! open (file=output_file, encoding=E_, newunit=Out, position='append')
  !    write (Out, '(a, T5, "= ", f9.6)') "Sum", s
  ! close (Out)

contains



   pure subroutine PQmyst(Z,Sums,P,Q,N,K,Res)
      real(R_)  Z(:,:),Sums(:),P(:),Q(:),K(:),dk
      intent(in) N
      intent (inout) P,Q,Sums,K,Z, Res
      integer i,j,N
      logical Res(:)
      
      Z = abs(Z)
      !Aps = abs(Z)

      Sums = [(Z(i,i),i = 1,N)]
      P = [(Sum(Z(i,:)),i=1,N)] - Sums
     
      Q = [(Sum(Z(:,j)),j=1,N)] - Sums
      
      dk = 1/N
      K = [(dk*i,i=1,N)]
      !K = [0,1]
     
         Res = Sums > (P**K(1)*Q**(1-K(1)))
      






   end subroutine PQmyst

end program exercise_7_10
