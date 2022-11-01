program exercise_7_19
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i = 0, cnt = 0
   real(R_), allocatable   :: C(:, :)
   integer, allocatable    :: Indexes(:, :), Res(:,:)
   logical, allocatable    :: Msk(:)

   open (file=input_file, newunit=In)
      read (In, *) N, M
      allocate (C(N, M))
      read (In, *) (C(i, :), i = 1, N)
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//M//'f6.2)') (C(i, :), i = 1, N)
   close (Out)

   allocate (Indexes(N*M, 2))
 
   allocate (Msk(N*M), source=.false.)

   call Fin(C,Msk,Indexes, cnt, Res)




   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '(a, i2)') "Count of negative elements:",cnt 
      write (Out, '(2i3)') (Res(i, :), i = 1, UBound(Res, 1))
   close (Out)

contains
      pure subroutine Fin(C,Msk,Indexes,cnt,Res)
         real(R_), intent(in)       :: C(:,:)
         logical, intent(out)       :: Msk(:)
         integer, intent(out)      :: cnt, Indexes(:,:)
         integer                    :: i,j
         
         integer, allocatable, intent(out) :: Res(:, :)
         Msk =[ C < 0 ]
         cnt = Count(Msk)
         
      Indexes(:, 1) = [((i, i = 1, N), j = 1, M)]

      Indexes(:, 2) = [((j, i = 1, N), j = 1, M)]

      allocate(Res(cnt, 2))

      Res(:, 1) = Pack(Indexes(:, 1), Msk)

      Res(:, 2) = Pack(Indexes(:, 2), Msk)
      end subroutine Fin



end program exercise_7_19
