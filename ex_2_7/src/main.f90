program ex_2_7
        implicit none
        integer(4) x,y,z
        write(*,*) "Pleese, input values x and y!"
        read(*,*) x,y
        if (x < y) then
                z = (abs(x) +abs(y))/2
        else 
                z = 1 + (2 * abs(x)) 
        end if 
        write(*,*) "Result:"
        write(*,*) "z =",z

end program ex_2_7
