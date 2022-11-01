module Integral_IO
   use Environment

   implicit none
contains
   ! Чтение параметра p.
   subroutine ReadP(input_file, p1, p2, delta_p,q1,q2, delta_q)
      character(*), intent(in) :: input_file
      real(R_), intent(out)    :: p1, p2, delta_p,q1,q2,delta_q

      integer :: In = 0
   
      open (file=input_file, newunit=In)
         read (In, *) p1, p2, delta_p, q1, q2, delta_q
      close (In)
   end subroutine ReadP
  
   ! Вывод параметра p.
   subroutine OutputP(output_file, p1, p2, delta_p, q1,q2,delta_q)
      character(*), intent(in) :: output_file
      real(R_), intent(in)     :: p1, p2, delta_p,q1,q2, delta_q

      integer :: Out = 0
   
      open (file=output_file, encoding=E_, newunit=Out)
         write (Out, '(6(a, T9, "= ", f0.4/))') "p1", p1, "p2", p2, "delta_p", delta_p,"q1",q1,"q2",q2,"delta_q",delta_q
      close (Out)
   end subroutine OutputP
   
   ! Вывод значений интеграла для разных p.
   subroutine OutputIntegral(output_file, P,Q, I)
      character(*), intent(in) :: output_file
      real(R_), intent(in) :: I(:), P(:), Q(:)

      integer :: Out = 0, j = 0,k =0
   
      open (file=output_file, encoding=E_, newunit=Out, position='append')

        write (Out, '("   p", T8, "|", T13, "q", T17, "|", T22, "I")')
        write (Out, '(f7.2, T8, "| ", f7.2, T17, "| ", f0.4)') ((P(k), Q(j), I(j+(k-1)*size(Q)), j = 1, Size(Q)), k = 1, size(P))



      close (Out)
   end subroutine OutputIntegral
end module Integral_IO
