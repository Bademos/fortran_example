include 'environment.f90'
program ex_1_7b
 use Environment

 implicit none
 
 integer(R_) a,b
 integer                  :: In=0, Out=0
 character(*) , parameter :: input_file = "../data/input.txt", output_file="./output.txt"
! character(:) , allocatable :: fmt


!write(*,*) "Pleese, input values a and b!"
 !read(*,*) a,b
 
 open(file=input_file, newunit=In)
   read(In,*) a,b
 close(In)

 open(file=output_file , encoding=E_, newunit=Out)
  
   write (Out,*) "But now a =",a,"and b =",b 
 close(Out)

 write(*,*) "a =" , a,"b =",b
 a = a+b
 b = a-b
 a = a -b
 write(*,*) "But now:"
 write(*,*) "a =",a, "b =",b

end program ex_1_7b
