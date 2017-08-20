module vars
	!vetores para os resultados, igualdade e permutacoes
	integer*4,allocatable,dimension(:) :: x,y,p
	!matriz dos coeficientes originais e alterados para calculo
	integer*4,allocatable,dimension(:,:) :: coef,A
	integer*4 :: n,det,val
end module

program eq
	use vars
	implicit none
	integer*4 :: i,j
	!abre os arquivos de entrada e saida
	open(1,file='equacao_in.dat')
	open(2,file='equacao_out.dat')

	!le o tamanho da matriz
	read(*,*) n

	!aloca todos os vetores e matrizes necessarios para o programa
	allocate(coef(n,n))
	allocate(a(n,n))
	allocate(x(n))
	allocate(y(n))
	allocate(p(n+1))
	!le os valores necessarios
	read(1,*) coef
	read(1,*) y
	coef=transpose(coef)
	a=coef
	!aqui comecam os calculos
	call determ()
	if(det == 0) then
		write(*,*) 'sistema sem solucao/indeterminado'
		stop
	end if
	do i=1,n
		do j=1,n
			a(j,i)=y(j)
		end do
		call deter()
		x(i)=val/det
		a=coef
	end do
	write(*,*) x
end program

subroutine determ()
	use vars
	implicit none
	integer*4 :: i
	det=0
	do i=1,n
		p(i)=i
	end do
	call perm(1,1)
end subroutine

subroutine deter()
	use vars
	implicit none
	integer*4 :: i
	val=0
	do i=1,n
		p(i)=i
	end do
	call perm(1,2)
end subroutine

recursive subroutine perm(i,v)
	use vars
	implicit none
	integer*4 :: i,j,v,temp,line
	if(i==n) then
		p(n+1)=0
		do j=1,n
			if(p(j)/=j) then
				p(n+1)=p(n+1)+1
			end if
		end do
		if(mod(p(n+1),2)==0 .and. p(n+1)/=0) then
			p(n+1)=-1
		else
			p(n+1)=1
		end if
		if(v==1) then
			det=det+line()
		else
			val=val+line()
		end if
	end if
	do j=i,n
		temp=p(i)
		p(i)=p(j)
		p(j)=temp
		call perm(i+1,v)
		temp=p(i)
		p(i)=p(j)
		p(j)=temp
	end do
end subroutine

integer*4 function line()
	use vars
	implicit none
	integer*4 i
	line=p(n+1)
	do i=1,n
		line=line*a(i,p(i))
	end do
end function
