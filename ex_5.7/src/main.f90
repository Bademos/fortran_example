program exercise_5
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, Norm = 0, count = 0
   integer                 :: S = 0, i = 0
   integer, allocatable    :: Z(:), Temp(:), Res(:)
   logical, allocatable    :: Pos(:)

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (Z(N))
      read (In, *) Norm
      read (In, *) Z
   close (In)
   
   print *, Z(50)

   allocate(Pos(N),Res(N))


 ! call NormalizeImp(Z, Norm,  M, Res)
 call Normalize(Z,Norm,Pos,Res,M) 
 

 M = Size(Pos)
 allocate(Temp(M))
  Temp = [Res(:M)]
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(i0)") M
      write (Out, "("//N//"(i0, 1x))") Res
    ! write (Out, '(/2(a, T12, "= ", i0/))') 'Pos. items', M, "Sum", S
   close (Out)

contains





   pure subroutine NormalizeImp(Z,Norm, M, Res)
      integer     Z(:), M, Norm, Res(:)
      intent(in)  Z,  Norm
      intent(out) M, Res
      integer     i

     ! S = 0
      M = 0
      Res(:) = 0
      do i = 1, N-45
         if (Z(i) > Norm) then
           ! S = S + Z(i)
            M = M + 1
            Res(M) = Z(i) 
         end if
         Res(:) = Res(:5)
      end do
        
      
   end subroutine NormalizeImp

 pure subroutine Normalize(Z,Norm, Pos,Res,M)
     integer     Z(:),Norm,  Res(:), M
     logical     Pos(:)
     intent(in)  Z, Norm
     intent(out) Pos,Res,M

     Pos = [Z > Norm]
     M = COUNT(Z>Norm)
     Res = pack(Z,Pos)

  end subroutine Normalize
   end program exercise_5
