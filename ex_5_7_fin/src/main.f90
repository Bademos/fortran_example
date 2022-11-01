program exercise_5
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, Norm = 0
   integer                 :: S = 0
   integer, allocatable    :: Z(:), Res(:), Temp(:)
   logical, allocatable    :: Pos(:)

   open (file=input_file, newunit=In)
      read (In, *) N
      read (In, *) Norm
      allocate (Z(N),Res(N))
      read (In, *) Z
   close (In)

   !call Normal(Z, M, Res, Norm)
   
   !allocate(Pos(N))
   !call NormReg(Z, Pos, M,Res,Norm)
   
   temp = pack(Z,Z > Norm)
   N = size(temp)
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(i0)") N
      write (Out, "("//N//"(i0, 1x))") temp
   close (Out)

contains
   !pure subroutine Normal(Z, M,Res,Norm)
   !   integer     Z(:), M, Res(:), Norm
   !   intent(in)  Z,Norm
   !   intent(out)  M, Res
   !   integer     i

   !   M = 0
   !   do i = 1, N
   !      If (Z(i) > Norm) then
   !         
   !         M = M + 1
   !         Res(M) = Z(i)
   !      end if
   !   end do
   !end subroutine Normal

   ! Чистая подпрограмма в императивном стиле.
   pure subroutine NormReg(Z, Pos, M, Res, Norm)
      integer     Z(:), M, Res(:), Norm
      logical     Pos(:)
      intent(in)  Z,Norm
      intent(out) Pos, M, Res

      Pos = Z > Norm
      M = Count(Pos)
      Res = pack(Z,Pos)
   end subroutine NormReg
end program exercise_5
