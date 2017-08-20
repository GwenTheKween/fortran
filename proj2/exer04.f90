module vars
	integer*4 :: n,det,sig
	integer*4,allocatable,dimension (:) ::P
	integer*4,allocatable,dimension (:,:) :: M
end module

program determinante
	use vars
	implicit none
	integer*4 :: i
	read(*,*) n
	allocate(M(n,n))
	open(1,file='matriz_in.dat')
	read(1,*) M
	allocate(P(n+1))
	det=0
	do i=1,n
		p(i)=i
	end do
	call perm(1)
	write(*,*) det
end program

recursive subroutine perm(i)
	use vars
	implicit none
	integer*4 i,j,temp,line
	if(i==n) then
		sig=0
		do j=1,n
			if(p(j)/=j) then
				sig=sig+1
			end if
		end do
		if(mod(sig,2)==0 .and. sig/=0) then
			p(n+1)=-1
		else
			p(n+1)=1
		end if
		det=det+line()
	end if
	do j=i,n
		temp=p(i)
		p(i)=p(j)
		p(j)=temp
		call perm(i+1)
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
		line=line*m(i,p(i))
	end do
end function
