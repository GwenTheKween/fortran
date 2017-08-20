program magnus
	implicit none
	real(kind=8) :: x,y,z,vx,vy,vz,v
	real(kind=8) :: theta,phi,beta,gama,omega,g,dt
	open(1,file='chute_out.dat')
	g=9.8
	v=100/3.6
	dt=0.01
	!pi=4*atan(1.)
	omega=39*2*4*atan(1.)
	read(*,*) beta
	read(*,*) theta
	read(*,*) phi
	x=0
	y=0
	z=0
	vx=v*sin(theta)*cos(phi)
	vy=v*sin(theta)*sin(phi)
	vz=v*cos(theta)
	write(1,*) 0,0
	do while(x<40 )
		!parametro do arraste
		gama=0.003+0.0058/(1+exp((v-35)/5))
		!atualizacao da posicao
		x=x+vx*dt
		y=y+vy*dt
		z=z+vz*dt
		!atualizacao da velocidade
		vx=vx-(gama*v*vx+beta*omega*vy)*dt
		vy=vy-(gama*v*vy-beta*omega*vx)*dt
		vz=vz-(g+gama*v*vz)*dt
		v=sqrt(vx*vx+vy*vy+vz*vz)
		!para o grafico
		write(1,*)x,y
		if(z<0) then
			exit
		end if
	end do
	if(x<40) then
		write(*,*) "a bola bate no chao antes, entao nao"
	else if(y<4 .OR. y>10) then
		write(*,*) "a bola vai muito para o lado, entao nao"
	else if(z>2.5) then
		write(*,*) "a bola passa muito alto, entao nao"
	else
		write(*,*) "o chute foi perfeito, entao sim"
	end if
end program
