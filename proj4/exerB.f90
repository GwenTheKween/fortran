program bike
	implicit none
	real(kind=8) :: v,t,dt,m,p,rho,a,tmax,v_old,tt,x
	real(kind=8) :: prec

	open(1,file='b3')

	m=70._8
	p=400._8
	rho=0.6_8
	tt=0
	v_old=0
	prec=1e-5
	x=0


	!segunda parte:
	v=0.1
	tmax=1800
	dt=0.1
	a=2

!	read(*,*)v
!	read(*,*)dt
!	read(*,*)tmax
!	read(*,*)a

	do while(t<tmax)
		v_old=v
		v=v+P*dt/(m*v) - rho*a*v*v*dt/m
		t=t+dt
		x=x+v*dt
		if(tt==0 .and. abs(v-v_old)<prec) then
			tt=t
		end if
		write(1,*)t,v
	end do

	write(*,*) 'os ciclistas faz essas coisas para que a area que sofre resistencia do ar seja minimizada'
	write(*,*)'a velocidade final eh:', v
	write(*,*)'que eh alcancada no instante',tt
	write(*,*)'o espaco percorrido foi:',x
	write(*,*)'e a velocidade media:',x/t
end program
