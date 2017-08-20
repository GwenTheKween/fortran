program raizes
	IMPLICIT NONE
	COMPLEX*16 z
	INTEGER*4 n,i
	REAL*8 a,rho,re,im,arg,pi
	read(*,*) n
	read(*,*) z
	read(*,*) a
	pi=4*atan(1.)
	z=z**3+a**2
	rho=abs(z)
	arg=ATAN2(imagpart(z),realpart(z))
	rho=rho**(1./real(n))
	do i=0,n-1
		write(*,*)cmplx(rho*cos((arg+2*i*pi)/n),rho*sin((arg+2*i*pi)/n))
	end do	
end program
