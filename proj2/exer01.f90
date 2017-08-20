program raiz
	implicit none
	real*8 x,res,oldRes,top,bottom
	real*8 test
	integer*4 i
	read(*,*) x
	top=1
	bottom=1
	res=1
	i=0
	test=10.0**(-5)
	do while((top/bottom)*x**(i+1)>test)
		if(i>0) then
			top=top*(2*i-1)
		end if
		bottom=bottom*2*(i+1)
		res=res+((-1)**i)*(top/bottom)*x**(i+1)
		i=i+1
	end do
	top=top*(2*i-1)
	bottom=bottom*2*(i+1)
	res=res+((-1)**i)*(top/bottom)*x**(i+1)
	write(*,*),"a raiz quadrada eh",res
end program raiz
