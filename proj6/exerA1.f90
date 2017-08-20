program pendulo
	implicit none
	real(kind=8) :: m,l,t_max,t,dt,g
	real(kind=8) :: theta,omega,theta_a
	real(kind=8) :: ep,ec

	!arquivo de saida
	open(1,file='exerA1_out.dat')
	open(2,file='e1')

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
		!arruma o valor do theta antigo
		theta_a=theta
		theta=theta+omega*dt
		omega=omega-g*theta_a*dt/l
		t=t+dt

		!energias
		ep=m*g*l*(1-cos(theta))
		ec=m*(omega*l)*(omega*l)/2

		write(1,*)t,theta
		write(2,*)t,ep+ec
	end do
end program pendulo
