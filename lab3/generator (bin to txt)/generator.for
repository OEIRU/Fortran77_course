	program generator
		integer :: k, l, m, i, io

		! Чтение параметров из data.txt
		open(10,FILE='data.txt',STATUS='old',iostat=io)
		if (io /= 0) then
			print *, 'Error opening data.txt'
			stop
		endif
		read(10,*,iostat=io) k, l, m
		if (io /= 0) then
			print *, 'Error reading parameters from data.txt'
			stop
		endif
		close(10)

		! Проверка параметров
		if (l <= 1 .or. m <= l .or. k <= l) then
			print *, 'Parameters of the matrix are not correct!'
			stop
		endif
		print *, 'Generation...'

		! Запись в matrix.bin
		open(11,FILE='matrix.bin',ACCESS='direct',RECL=4)
		write(11,rec=1) k
		write(11,rec=2) l
		write(11,rec=3) m

		do i=1,k
			write(11,rec=3+i) 1.0
		end do
		do i=1,2*(k-1)
			write(11,rec=3+k+i) 10.0
		end do
		do i=1,2*(k-l)
			write(11,rec=3+3*k-2+i) 100.0
		end do
		do i=1,2*(k-m)
			write(11,rec=3+5*k-2*l+i) 1000.0
		end do
		close(11)

		! Запись в vector.bin
		open(12,FILE='vector.bin',ACCESS='direct',RECL=4)
		write(12,rec=1) k
		do i=1,k
			write(12,rec=1+i) 1.0
		end do
		close(12)

		print *, 'Generation complete!'
	end program generator
