module Integral_calculate 
   use Environment

   implicit none
   integer, parameter      :: N = 100
   real(R_), parameter     :: a = 0, b = 1, h = (b - a) / N

contains
   pure subroutine Integral(p1, delta_p,q1, delta_q, P,Q, X, I,Integ)
      real(R_), intent(in)    :: p1, delta_p, q1, delta_q
      real(R_), intent(out)   :: I(:), P(:), X(:),Q(:), Integ(:,:)
      integer                 :: j,k
      real(R_),allocatable    :: Mask(:)
      real(R_),allocatable    :: Xin(:)
      

     
      P = [(p1 + delta_p*j,  j = 0, Size(P))] 

      Q = [(q1 + delta_q*j,  j = 0, Size(Q))] 

      X =  [(a + j*h,         j = 1, Size(X)-1)]  

  !   I = [((Sum(F(P(j),Q(k), X))-(F(P(j),Q(k),[X(Size(X))])+F(P(j),Q(k),[X(1)]) )/2,     j = 1, Size(P)), k = 1,Size(Q))]
   !   I = h * I

     ! Xin = X(2:Size(x)-1)


     I = [((Sum(Fun(P(j),Q(k), X))+0.5*(Fun(P(j),Q(k),b)+Fun(P(j),Q(k),a)),     j = 1, Size(P)), k = 1,Size(Q))]

     I = I * h
    ! Integ = [((Sum(Fun(P(j),Q(k),X)),j=1,Size(P)),k = 1, Size(Q))]

      
      
   end subroutine Integral

   pure function F(p,q, X)
      real(R_)    p, X(:),q
      real(R_)    F(UBound(X, 1))
      intent(in)  p, X ,q
      
      F = sin(p*X)**2 / SqRt(x**X + q)
   end function F

  elemental real(R_)  function  Fun(p, q, x)
       real(R_)  :: p, q, x
      intent(in) p, q, x
  
      Fun =(sin(p*x))**2 / SqRt(x**2 + q)


   end function Fun




     pure function Foo(p,q, X)
        real(R_)    p, X(:),q
        real(R_)    Foo(UBound(X, 1))
        intent(in)  p, x, q
 
        Foo = sin(p*X)**2 / SqRt(X**2 + q)
      end function Foo

end  module Integral_calculate 
