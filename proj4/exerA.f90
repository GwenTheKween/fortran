program bike
	implicit none
	real(kind=8) :: v, m, t, dt, p, tmax

	call set_vars(m, t, p)
	open(1,file='4')

	read(*,*) v
	read(*,*) dt
	read(*,*) tmax

	write(1,*) t,v
	
	do while(t<tmax)
		v=v+p*dt/(m*v)
		t=t+dt
		write(1,*) t,v
	end do
end program

subroutine set_vars(m, t, p)
	implicit none
	real(kind=8) :: t,m,p
	m=70
	p=400
	t=0
end subroutine
