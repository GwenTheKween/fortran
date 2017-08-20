program integrate
	implicit none
	real(kind=8) :: a,prec,val,f
	real(kind=8),dimension(5) :: ans
	integer(kind=4),dimension(11) :: h
	integer(kind=4) :: i
	character :: tab
	prec=1e-8
	val=-4._8/5.*DEXP(1._8/4)*(2*cos(1._8/2)-sin(1._8/2))+8._8/5
	tab=char(9)
	open(1,file='tabB_in.dat')
	open(2,file='tabB_out.dat')

	read(1,*), a
	read(1,*),h
	write(2,*)'N',tab,tab,tab,tab,'h',tab,tab,tab,tab,'trapezio',tab,tab,tab,'simpson',tab,tab,tab,'bode'

	do i=1,11
		a=0
		ans(1)=h(i)
		ans(2)=1._8/h(i)
		ans(3)=0
		ans(4)=0
		ans(5)=0
		do while(a<1)
			ans(3)=ans(3)+(f(a)+2*f(a+ans(2))+f(a+2*ans(2)))*ans(2)/2
			ans(4)=ans(4)+(f(a)+4*f(a+ans(2))+f(a+2*ans(2)))*ans(2)/3
			a=a+2*ans(2)
			if(a>10) then
				stop
			end if
		end do
		a=0
		do while(a<1)
			ans(5)=ans(5)+(7*f(a)+32*f(a+ans(2))+12*f(a+2*ans(2))+32*f(a+3*ans(2))+7*f(a+4*ans(2)))*2*ans(2)/45
			a=a+4*ans(2)
		end do
		ans(3)=abs(val-ans(3))
		ans(4)=abs(val-ans(4))
		ans(5)=abs(val-ans(5))
		write(2,*) ans
	end do
end program integrate

real(kind=8) function f(x)
	real(Kind=8) :: x
	f=exp(x/4.)*sin(x/2)
end function f
