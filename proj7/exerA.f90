program grav
        implicit none
        real(kind=8) :: x(2),y(2),vx,vy,pi,r,t,dt,const
        read(*,*) r
        read(*,*) vy
        read(*,*) dt
        pi=4*atan(1._8)
        x(1)=r
        y(1)=0    
        vx=0
        t=0
        const=-4*pi*pi
        x(2)=x(1)+vx*dt
        y(2)=y(1)+vy*dt
        open(1,file='trajA1_out.dat')
        write(1,*)0,x(2),y(2)
        do while(t>-1)
                !como nao eh necessario, diretamente, usar a velocidade eu usarei como valor temporario 
                vx=x(2)
                vy=y(2)
                r=sqrt(x(2)**2+y(2)**2)
                y(2)=2*y(2)-y(1)+(const*y(2)/(r**3))*dt**2
                x(2)=2*x(2)-x(1)+(const*x(2)/(r**3))*dt**2
                x(1)=vx
                y(1)=vy
                t=t+dt
                write(1,*)t,x(2),y(2)
		if(y(1)<0 .and. y(2)>=0) then
			exit
		end if
        end do
	write(*,*) 'o dt deve ser escolhido como, no maximo, 0.1% do tempo de orbita do planeta'
	write(*,*) t
end program
