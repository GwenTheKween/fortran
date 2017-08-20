program dif
	implicit none
	real(KIND=8), allocatable, dimension(:) :: h
	real(kind=8),dimension(6) :: ans
	real(kind=8) :: func,f0,deriv,const,df,best,sml,sum_best
	integer(kind=8) :: n,i
	open(1,file="tabA_in.dat")
	open(2,file='tabA_out.dat')
	read(1,*) n
	allocate(h(n))
	read(1,*) h
	const=1.0_8/3.0_8
	f0=func(const)
	do i=1,n
		df=14.1364074392774265751743
		ans(1)=abs(df-(func(const+h(i))-func(const-h(i)))/(2*h(i)))
		sml=ans(1)
		ans(2)=abs((func(const+h(i))-f0)/h(i)-df)
		if(sml>ans(2)) then
			sml=ans(2)
		end if
		ans(3)=abs((f0-func(const-h(i)))/h(i)-df)
		if(sml>ans(3)) then
			sml=ans(3)
		end if
		ans(4)=abs((-func(const+2*h(i))+8*func(const+h(i))-8*func(const-h(i))+func(const-2*h(i)))/(12*h(i))-df)
		if(sml>ans(4)) then
			sml=ans(4)
		end if
		df=52.270667309471349452276
		ans(5)=abs((-func(const+2*h(i))+16*func(const+h(i))-30*f0+16*func(const-h(i))-func(const-2*h(i)))/(12*h(i)**2)-df)
		df=185.43374779742089931103
		ans(6)=abs((func(const+2*h(i))-2*func(const+h(i))+2*func(const-h(i))-func(const-2*h(i)))/(2*h(i)**3)-df)
		if(i==1) then
			sum_best=sml+ans(5)+ans(6)
			best=h(i)
		else if(sml+ans(5)+ans(6)<sum_best) then
			best=h(i)
			sum_best=sml+ans(5)+ans(6)
		end if
		write(2,*), ans
	end do
		write(*,*), "a melhor escolha para h eh",best, &
				&"pois, usando os metodos mais precisos, esse foi o h que deu os menores valores de desvio"
end program

real(kind=8) function func(x)
	implicit none
	real(kind=8) :: x
	func = exp(4*x)*cos(x/2)**2
end function
