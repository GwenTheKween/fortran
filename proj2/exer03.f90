module vars
	integer*4 :: n
	integer*4, allocatable, dimension (:) :: p
end module

program permutation
	use vars
	implicit none
	integer*4 :: i
	read(*,*) n
	allocate(P(n+1))
	do i=1,n
		p(i)=i
	end do
	open(1,file='perm_out.dat')
	p(n+1)=0
	call perm(1)
end program

recursive subroutine perm(i)
	use vars
	implicit none
	integer*4 ::i,j,temp
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
		write(1,*) p
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
