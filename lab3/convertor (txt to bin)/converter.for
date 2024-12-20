	program converter
		integer :: k, l, m, x, i, io

		! Открываем файл matrix.txt
		open(10,FILE='matrix.txt',STATUS='old',iostat=io)
		if (io /= 0) then
			print *, 'Error opening matrix.txt'
			stop
		endif
		read(10,*,iostat=io) k, l, m
		if (io /= 0) then
			print *, 'Error reading matrix dimensions'
			stop
		endif

		! Проверка матрицы на корректность
		if (l <= 1 .or. m <= l .or. k <= l) then
			print *, 'Parameters of the matrix are not correct!'
			stop
		endif
		print *, 'Conversion...'

		! Перевод матрицы в двоичный вид
		open(20,FILE='matrix.bin',ACCESS='direct',RECL=4,iostat=io)
		if (io /= 0) then
			print *, 'Error opening matrix.bin'
			stop
		endif
		write(20,rec=1) k
		write(20,rec=2) l
		write(20,rec=3) m

		i = 0
		do
			read(10,*,iostat=io) x
			if (io /= 0) exit
			i = i + 1
			write(20,rec=3+i) x
		end do

		close(10)
		close(20)

		! Чтение и запись вектора
		open(11,FILE='vector.txt',STATUS='old',iostat=io)
		if (io /= 0) then
			print *, 'Error opening vector.txt'
			stop
		endif
		read(11,*,iostat=io) k
		if (io /= 0) then
			print *, 'Error reading vector size'
			stop
		endif

		open(21,FILE='vector.bin',ACCESS='direct',RECL=4,iostat=io)
		if (io /= 0) then
			print *, 'Error opening vector.bin'
			stop
		endif
		write(21,rec=1) k

		i = 0
		do
			read(11,*,iostat=io) x
			if (io /= 0) exit
			i = i + 1
			write(21,rec=1+i) x
		end do

		close(11)
		close(21)

		print *, 'Conversion complete!'
	end program converter
