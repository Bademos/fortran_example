program exercise_4
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, Nx = 0, Ny = 0,  i = 0, j = 0
   real(R_)                :: x1 = 0, x2 = 0, y1 = 0, y2 = 0, hx = 0, hy = 0
   real(R_), allocatable   :: X(:),Y(:),F(:,:), Cos_X(:), Arcsin_X(:) ,Func(:)
   
   open (file=input_file, newunit=In)
      read (In, *) x1, x2, hx
      read (In, *) y1, y2, hy
   close (In)
  ! print*, y1,y2,hy

  open (file=output_file, encoding=E_, newunit=Out)
     write (Out, '(3(a, T4, "= ", f0.4/))') "x1", x1, "x2", x2, "hx", hx
     write (Out, '(3(a, T4, "= ", f0.4/))') "y1", y1, "y2", y2, "hy", hy
  close (Out)
  
  Nx = Int((x2-x1) / hx + .5_R_)+ 1
  Ny = Int((y2 - y1) / hy + .5_R_) + 1
  print*,  Nx, Ny  
      allocate (X(Nx),Y(Ny), F(Nx,Ny), Cos_x(Nx),Func(Nx*Ny))
! print*, Size(X), Size(F)    
     call TabFImp(x1,y1, hx, hy, X,Y,F,Func)
      !call TabF(x1, h, X, F)
      
  open (file=output_file, encoding=E_, newunit=Out)
        write (Out, '("   x", T8, "|", T13, "y", T17, "|", T22, "f")')
        write (Out, '(f0.4, T8, "| ", f0.4, T17, "| ", f0.4)') ((X(i), Y(j), F(i,j), j = 1, Ny), i = 1, Nx)
    close (Out)



     
     
   !        open (file=output_file, encoding=E_, newunit=Out, position='append')
   !           write (Out, '("  X   |  Y  |  f")')
   !           do i =1, Nx
   !              do j = 1, Ny
   !                 write (Out,'(3(f0.4,"| "),T4)') X(i),Y(j),F(i,j)  
   !           end do
   !        end do
   !           close (Out)
   !       
contains
   pure subroutine TabFImp(x1,y1, hx,hy, X,Y , F)
      real(R_)    x1, hx, y1, hy,  X(:),Y(:) , F(:,:)
      intent(in)  x1, hx, y1, hy
      intent(out) X,Y ,F
      integer     Nx,Ny,i,j
      
      Nx = Size(X)
      X(1) = x2 ! we have a negative dx
      Ny = Size(Y)
      Y(1) = y1

      do i = 1, Nx-1
        ! F(i,j) = 1 + 1 / Sin(X(i))
         do j = 1, Ny-1
           F(i,j) = (1.35*Y(j)+ ASIN(X(i)) )/(0.5*Y(j) + Cos(X(i)))
            Y(j+1) = Y(j) + hy
         end do
         F(i,Ny) = (1.35*Y(Ny)+ASIN(X(i)))/(0.5*Y(Ny) + Cos(X(i)))  
         X(i+1) = X(i) - hx !because 
         
      end do
      
     F(Nx,Ny) =( 1.35*Y(Ny)+ASIN(X(i)))/(0.5*Y(Ny) + Cos(X(Nx)))
           do j = 1, Ny-1
             F(Nx,j) = (1.35*Y(j)+ ASIN(X(Nx)) )/(0.5*Y(j) + Cos(X(Nx)))
              
           end do

   end subroutine TabFImp


   pure subroutine TabF(x2,y1, hx,hy,  X,Y, F,Func)
        real(R_)    x2,y1, hx, hy ,X(:), Y(:), F(:,:),Func(:)
        intent(in)  x2, hx,y1,hy 
        intent(out) X,  Y,  F, Func
        integer     i
  
        X = [(x2 - hx*(i-1), i = 1, Size(X)-1)]
        Y = [(y1 + hy*(j-1), j = 1, Size(Y)-1)]
       
        ! Функция закодирована по-другому, чтобы дважды не вычислять Sin(x).
       
      !  do i = 1, size(Y)-1
      !      F(:,i) = (1.35*Y(i) + ASIN(X) )/(0.5*Y(i) + Cos(X))
      !  end do 
      
      !F = [((1.35*Y(j) + ASIN(X(i))/(0.5*Y(j) + Cos(X(i))),i=1,size(X)-1,j =1,size(Y)-1)]


         !  F ((1.35*Y(j) + ASIN(X(i)) )/(0.5*Y(j) + Cos(X(i)) ),i = 1,Size(X)-1), (1.35*Y(j) + ASIN(X(i)) )/(0.5*Y(j) + Cos(X(i))),j = 1, Size(Y)-1]
                 

         Func = [((j,j=1,4),i=1,2)]
         F = reshape(Func,(/size(X),size(Y)/))

  end subroutine TabF



   
   
end program exercise_4
