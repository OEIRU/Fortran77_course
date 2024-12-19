	program generator
	open(1,FILE='data.txt')
	read(1,*)k,l,m
*Проверка матрицы на корректность
	if(l.le.1.or.m.le.l.or.k.le.l)then
		print*,'Parameters of the matrix is given not correct!'
		stop
	endif
	print*,'Generation...'
*Генерация данных	
	open(1,FILE='matrix.bin',ACCESS='direct',RECL=12)
	write(1,rec=1)k,l,m
	open(1,FILE='matrix.bin',ACCESS='direct',RECL=4)
	do i=1,k
		write(1,rec=3+i)1.
	end do
	do i=1,2*(k-1)
		write(1,rec=3+k+i)10.
	end do
	do i=1,2*(k-l)
		write(1,rec=3+3*k-2+i)100.
	end do
	do i=1,2*(k-m)
		write(1,rec=3+5*k-2*l+i)1000.
	end do
	open(1,FILE='vector.bin',ACCESS='direct',RECL=4)
	write(1,rec=1)k
	do i=1,k
		write(1,rec=1+i)1.
	end do
	end
