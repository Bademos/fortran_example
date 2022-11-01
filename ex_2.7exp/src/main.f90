include 'environment.f90'
program ex_2_7
 use Environment

 implicit none
 
 integer(R_) x,y,z
 integer                  :: In=0, Out=0, i=0
 character(*) , parameter :: input_file = "../data/input.txt", output_file="./output.txt"
 character(:) , allocatable :: fmt


 
 open(file=input_file, newunit=In)
   read(In,*) x,y
 close(In)

 if (x < y) then
    z=(abs(x) + abs(y))/2
 else
    z = 1 + (2 * abs(x))
 end if



 open(file=output_file , encoding=E_, newunit=Out)
  
   write (Out,*) "Result:",z
 close(Out)

 write(*,*) "x =" , x,"y =",y

 write(*,*) "RESULT:"
 write(*,*) z

end program ex_2_7
