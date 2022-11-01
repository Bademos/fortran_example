program exercise_34
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt"! , output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 100, i,res !FIN(0:100), res
   real(R_)                :: P = 1, G(0:100), X
   integer, allocatable   :: B(:) 

  open (file=input_file, newunit=In)
     read (In, *) N
     allocate (B(N))

     read (In, *) B
  close (In)
  ! print*,'Hello'
  ! CALL RANDOM_NUMBER(X)
  ! CALL RANDOM_NUMBER(G)
  ! do i=0,99
  !    FIN(i) = G(i) *100
  ! end do
   !print*,'res = ', FIN
   !P = ProdImp(B)
   ! Чистая функция в регулярном стиле.
  !P = Product(B)
  res = SumOdd(N,B)
!  print*,N
!  IF(MOD(N,2)==0) then
!     N = N-1
!  
!  END IF
!   print*, N
!  res= B(N)
!  do i = 1, N-1
!     IF (MOD((N-i),2) /= 0) then
!        print*, res,N-i
!        res = res +  B(N-i)
!     END IF
!  end do
!
  print*, "res= ", res
   
   open (file="output.txt", encoding=E_, newunit=Out)
     ! write (Out, "(i0)") N
      !write (Out, "("//N//"f6.2)") FIN
      do i = 1,100
         write (Out,"(i0)") B(i)
      end do
     ! write (Out, *)
      write (Out, *)'Sum_odd = ', res
   close (Out)

contains
   
  
   pure function SumOdd(M,A) result(Prod)
         !real(R_)    Prod
       intent(in) M, A
       integer     i,M,A(M),Prod,N
       
      IF(MOD(M,2)/=0) then
         N = M-1
      ELSE
         N=M
      END IF
      
      Prod = A(N)
      do i = 1, N-1
         IF (MOD((N-i),2) == 0) then
            Prod = Prod + A(N-i) 
         END IF   
      end do
      !Prod = A
   end function SumOdd
end program exercise_34
