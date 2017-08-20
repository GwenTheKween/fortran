program pendulo
	implicit none
	real(kind=8) :: m,l,t_max,t,dt,g
	real(kind=8) :: theta,omega
	real(kind=8) :: ep,ec

	!arquivo de saida
	open(1,file='exerA2_out.dat')
	open(2,file='e2')

	!valores iniciais
	read(*,*) m
	read(*,*) l
	read(*,*) dt
	read(*,*) theta
	read(*,*) t_max
	omega=0
	t=0
	g=9.8
	!conversao do theta_0 para radianos
	!pi=4*atan(1.)
	theta=theta*4*atan(1.)/180

	!escreve os valores iniciais no arquivo de saida
	write(1,*)t,theta
	write(2,*)t,m*g*l*(1-cos(theta))

	!calculos
	do while(t<t_max)
		omega=omega-g*theta*dt/l
		theta=theta+omega*dt
		t=t+dt

		!energias
		ep=m*g*l*(1-cos(theta))
		ec=m*(omega*l)*(omega*l)/2

		write(1,*)t,theta
		write(2,*)t,ec+ep
	end do
end program pendulo
