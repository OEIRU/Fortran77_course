	program  converter
	open(1,FILE='matrix.txt')
	read(1,*)k,l,m
*Проверка матрицы на корректность
	if(l.le.1.or.m.le.l.or.k.le.l)then
		print*,'Parameters of the matrix is given not correct!'
		stop
	endif
      print*,'Convertation...'
*Перевод в двоичный вид	
	open(2,FILE='matrix.bin',ACCESS='direct',RECL=12)
	write(2,rec=1)k,l,m
	open(2,FILE='matrix.bin',ACCESS='direct',RECL=4)
	do i=1,7*k-2*l-2*m-2
		read(1,*)x
		write(2,rec=3+i)x
	end do
	open(1,FILE='vector.txt')
	read(1,*)k
	open(2,FILE='vector.bin',ACCESS='direct',RECL=4)
	write(2,rec=1)k
	do i=1,k
		read(1,*)x
		write(2,rec=1+i)x
	end do
	end