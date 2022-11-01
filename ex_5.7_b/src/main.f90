program exercise_5
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0
   integer                 :: S = 0,Norm = 0
   integer, allocatable    :: Z(:),Res(:),Temp(:)
   logical, allocatable    :: Pos(:)

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (Z(N))
      read (In, *) Z
   close (In)

   !call PositiveImp(Z, S, M)

   ! Размещение данных в НАЧАЛЕ работы программы,
   ! а не внутри подпрограммы при КАЖДОМ её вызове.
   allocate(Pos(N),Res(N))
   !call Positive(Z, Pos, Res, M)
   call Positive(Z,Norm, Pos, Res, M)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(i0)") N
      write (Out, "("//N//"(i0, 1x))") Z
      write (Out, '(/2(a, T12, "= ", i0/))') 'Pos. items', M, "Sum", S
   close (Out)

contains
   ! Чистая подпрограмма в императивном стиле.
   pure subroutine PositiveImp(Z, S, M)
      integer     Z(:), S, M
      intent(in)  Z
      intent(out) S, M
      integer     i

      S = 0
      M = 0
      do i = 1, N
         If (Z(i) > 0) then
            S = S + Z(i)
            M = M + 1
         end if
      end do
   end subroutine PositiveImp

   ! Чистая подпрограмма в императивном стиле.
   pure subroutine Positive(Z,Norm, Pos, Res, M)
      integer     Z(:), Res(:), M, Norm
      logical     Pos(:)
      intent(in)  Z, Norm
      intent(out) Pos, Res(:), M

      Pos = Z > Norm
      Res = Pack(Z,Pos)
      M = Count(Pos)
      

   end subroutine Positive
end program exercise_5
