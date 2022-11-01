program exercise_6
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0
   real(R_)                :: x = 0, a = 0,relerr,ln_x
   logical                 :: isRange

   open (file=input_file, newunit=In)
      read (In, *) x
      read (In, *) a

      read (In, *) relerr
   close (In)
   
  ln_x = LnR(x,a,relerr)

  !ln_x = LnImp(x,a,relerr)
   

   call  isRangeSr(x,a,isRange)
   
   if (isRange) then
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(5(a, T16, "= ", e13.6/))') 'x', x,"a",a, "Ln(x)", ln_x, "Fortran Log(x)", log(x+a), "Error", (ln_x-log(x+a))
   close (Out)
   else
      print *, "Input Error"
   end if

contains

   pure subroutine isRangeSr(x,a,isRange)
      real(R_) x,a
      logical isRange
      intent(in) x,a
      intent (out) isRange
      isRange =.false.
      if ((2*x+a)**2 > a**2) then
         isRange = .true.
        


      end if 
   end subroutine isRangeSr

   real(R_) pure function LnImp(x,a,relerr) result(LnX)
      real(R_), intent(in) :: x,a,relerr
      
      real(R_)     q, OldLnX
      
      integer     n

      
      n    = 1
     
      LnX  = log(x)

      do
         q       = 2*a**n/(n*(2*x+a)**n)
         n       = n + 2
         OldLnX  = Lnx
         LnX     = LnX + q
         if (q/Lnx < relerr) exit
      end do
   end function LnImp

   real(R_) pure function LnR(x,a,relerr) result(LnX)
      real(R_), intent(in) :: x,a,relerr
      
      real(R_) R(4), x_s, ln_add
      integer  Ns(4)

      x_s = a/(2*x+a)
      
      Ns = [3,5,7,9]

      R = x_s**Ns/Ns ! ВЕКТОРИЗАЦИЯ

      ln_add = x_s + Sum(R)
      
      do while (R(4)/ln_add > relerr )
         
         Ns = Ns + 8          
         R = x_s**Ns/Ns
         ln_add = ln_add + Sum(R)
         
      end do
      LnX = log(x) + 2*ln_add

   end function LnR
end program exercise_6
