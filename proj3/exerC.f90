program root
	implicit none
	integer(kind=4) :: i,n
	real(kind=8) :: x, f, df,h, temp
	real(kind=8),dimension(4,3) :: ans
	open(1,file='tabC_out.dat')
	do i=1,3
		ans(i,1)=-0.42
		ans(i,2)=0.3
		ans(i,3)=0.6
	end do
	ans(4,1)=-0.55
	ans(4,2)=0.4
	ans(4,3)=0.7
	h=0.1
	read(*,*) n
	do i=1,n
		!primeiro metodo: busca direta
		!escolhendo um salto (h) e um chute inicial (ans(1))
		!e depois iteramos, sempre dividindo h pela metade e indo
		!pelo lado que muda o sinal ou mais se aproxima do 0

		!esse metodo nao funciona se o valor escolhido estiver
		!num ponto em que tanto mudando para cima quanto para baixo
		!o sinal muda

		!se os sinais mudarem ao aumentar o valor de X o valor
	       	!da resposta sera incrementado por h	
		if((f(ans(1,1)) > 0 .and. f(ans(1,1)+h)<0) .or. (f(ans(1,1))<0 .and. f(ans(1,1)+h)>0)) then
			ans(1,1)=ans(1,1)+h
		
		!se os sinais mudar ao diminuir o valor de X o valor
		!da resposta sera decrementado por h
		else if((f(ans(1,1)) > 0 .and. f(ans(1,1)-h)<0) .or.(f(ans(1,1)) < 0 .and. f(ans(1,1)-h)>0)) then
				ans(1,1)=ans(1,1)-h

		!caso contrario o valor da resposta sera incrementado
		!decrementado na direcao que produzir o menor valor, em modulo
		else if(abs(f(ans(1,1)-h)) < abs(f(ans(1,1)+h))) then
				ans(1,1)=ans(1,1)-h
		else if(abs(f(ans(1,1)-h)) > abs(f(ans(1,1)+h))) then
				ans(1,1)=ans(1,1)+h
		end if
		
		
		if((f(ans(1,2)) > 0 .and. f(ans(1,2)+h)<0) .or. (f(ans(1,2))<0 .and. f(ans(1,2)+h)>0)) then
			ans(1,2)=ans(1,2)+h
		
		!se os sinais mudar ao diminuir o valor de X o valor
		!da resposta sera decrementado por h
		else if((f(ans(1,2)) > 0 .and. f(ans(1,2)-h)<0) .or.(f(ans(1,2)) < 0 .and. f(ans(1,2)-h)>0)) then
				ans(1,2)=ans(1,2)-h

		!caso contrario o valor da resposta sera incrementado
		!decrementado na direcao que produzir o menor valor, em modulo
		else if(abs(f(ans(1,2)-h)) < abs(f(ans(1,2)+h))) then
				ans(1,2)=ans(1,2)-h
		else if(abs(f(ans(1,2)-h)) > abs(f(ans(1,2)+h))) then
				ans(1,2)=ans(1,2)+h
		end if
		
		
		if((f(ans(1,3)) > 0 .and. f(ans(1,3)+h)<0) .or. (f(ans(1,3))<0 .and. f(ans(1,3)+h)>0)) then
			ans(1,3)=ans(1,3)+h
		
		!se os sinais mudar ao diminuir o valor de X o valor
		!da resposta sera decrementado por h
		else if((f(ans(1,3)) > 0 .and. f(ans(1,3)-h)<0) .or.(f(ans(1,3)) < 0 .and. f(ans(1,3)-h)>0)) then
				ans(1,3)=ans(1,3)-h

		!caso contrario o valor da resposta sera incrementado
		!decrementado na direcao que produzir o menor valor, em modulo
		else if(abs(f(ans(1,3)-h)) < abs(f(ans(1,3)+h))) then
				ans(1,3)=ans(1,3)-h
		else if(abs(f(ans(1,3)-h)) > abs(f(ans(1,3)+h))) then
				ans(1,3)=ans(1,3)+h
		end if
		h=h/2

		!segundo metodo: Newton-Raphson
		!dado o chute inicial calcula-se o tamanho do passo
		!que levara o resultado a resposta mais proxima

		if(df(ans(2,1)) /= 0) then
			ans(2,1)=ans(2,1)-f(ans(2,1))/df(ans(2,1))
		end if
		if(df(ans(2,2))/=0) then 
			ans(2,2)=ans(2,2)-f(ans(2,2))/df(ans(2,2))
		end if
		if(df(ans(2,3))/=0) then
			ans(2,3)=ans(2,3)-f(ans(2,3))/df(ans(2,3))
		end if

		!terceiro metodo: metodo da secante
		!como o metodo de Newton-Raphson usa-se a propria
		!funcao para determinar o tamanho do passo

		if(f(ans(4,1))-f(ans(3,1)) /=0) then
			temp=ans(4,1)
			ans(4,1)=ans(4,1)-f(ans(4,1))*(ans(4,1) - ans(3,1))/(f(ans(4,1))-f(ans(3,1)))
			ans(3,1)=temp
		end if
		if(f(ans(4,2))-f(ans(3,2))/=0) then
			temp=ans(4,2)
			ans(4,2)=ans(4,2)-f(ans(4,2))*(ans(4,2) - ans(3,2))/(f(ans(4,2))-f(ans(3,2)))
			ans(3,2)=temp
		end if
		if(f(ans(4,3))-f(ans(3,3))/=0) then
			temp=ans(4,3)
			ans(4,3)=ans(4,3)-f(ans(4,3))*(ans(4,3) - ans(3,3))/(f(ans(4,3))-f(ans(3,3)))
			ans(3,3)=temp
		end if
		write(1,*) i,ans(1,1),ans(1,2),ans(1,3),ans(2,1),ans(2,2),ans(2,3),ans(4,1),ans(4,2),ans(4,3)
	end do
end program

real(kind=8) function f(x)
	implicit none
	real(kind=8) :: x
	f=18*x**3 - 9*x**2 - 5*x + 2
end function

real(kind=8) function df(x)
	implicit none
	real(kind=8):: x, f,h=0.0005
	df=(-f(x+2*h)+8*f(x+h)-8*f(x-h)+f(x+2*h))/(12*h)
end function
