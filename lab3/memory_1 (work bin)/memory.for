program memory
	implicit none
	integer, parameter :: n = 1000000
	real :: A(n)
	integer :: i, i1, i2
	common /razm/ i, i1, i2

	! Ввод данных
	call vvod(A(1), n)

	! Умножение матрицы на вектор
	call multiplying(A(1), A(1+i), A(2*i), A(3*i-1), A(4*i-i1-1), &
		A(5*i-2*i1-1), A(6*i-2*i1-i2-1), A(7*i-2*i1-2*i2-1), &
		A(8*i-2*i1-2*i2-1), i, i1, i2)

	! Вывод результата
	call output(A(8*i-2*i1-2*i2-1), i)

	stop
end program memory

! Ввод данных
subroutine vvod(A, n)
	implicit none
	real, dimension(n) :: A
	integer :: n  ! Объявляем n как целое число
	integer :: i, i1, i2, m, io
	common /razm/ i, i1, i2

	! Открытие файла матрицы
	open(10, FILE='matrix.bin', ACCESS='direct', RECL=4, STATUS='old', IOSTAT=io)
	if (io /= 0) then
		print *, 'Error opening matrix.bin'
		stop
	endif

	! Чтение размерности матрицы и диагоналей
	read(10, rec=1, IOSTAT=io) i, i1, i2
	if (io /= 0) then
		print *, 'Error reading matrix dimensions'
		stop
	endif

	! Проверка корректности матрицы
	if (i1 <= 1 .or. i2 <= i1 .or. i <= i1) then
		print *, 'Matrix dimensions are incorrect: i =', i, ', i1 =', i1, ', i2 =', i2
		stop
	endif

	! Открытие файла вектора
	open(11, FILE='vector.bin', ACCESS='direct', RECL=4, STATUS='old', IOSTAT=io)
	if (io /= 0) then
		print *, 'Error opening vector.bin'
		stop
	endif

	! Чтение размерности вектора
	read(11, rec=1, IOSTAT=io) m
	if (io /= 0) then
		print *, 'Error reading vector dimensions'
		stop
	endif

	! Проверка вектора
	if (i /= m) then
		print *, 'Vector size is incorrect: i =', i, ', m =', m
		stop
	endif

	! Проверка нехватки памяти
	if (9 * i - 2 * i1 - 2 * i2 - 2 > n) then
		print *, 'Shortage of memory'
		stop
	endif

	! Чтение данных
	call readdata(A(1), A(1+i), A(2*i), A(3*i-1), A(4*i-i1-1), &
		A(5*i-2*i1-1), A(6*i-2*i1-i2-1), A(7*i-2*i1-2*i2-1), i, i1, i2)

	close(10)
	close(11)
	return
end subroutine vvod

! Чтение данных
subroutine readdata(A, B1, B2, C1, C2, D1, D2, E, k, l, m)
	implicit none
	real, dimension(k) :: A, E
	real, dimension(k-1) :: B1, B2
	real, dimension(k-l) :: C1, C2
	real, dimension(k-m) :: D1, D2
	integer :: k, l, m  ! Объявляем k, l, m как целые числа
	integer :: i, io

	! Открытие файла матрицы
	open(10, FILE='matrix.bin', ACCESS='direct', RECL=4, STATUS='old', IOSTAT=io)
	if (io /= 0) then
		print *, 'Error opening matrix.bin for data reading'
		stop
	endif

	! Чтение матрицы
	do i = 1, k
		read(10, rec=3+i, IOSTAT=io) A(i)
		if (io /= 0) then
			print *, 'Error reading matrix data at record', 3+i
			stop
		endif
	end do

	! Чтение диагоналей
	do i = 1, k-1
		read(10, rec=3+k+i, IOSTAT=io) B1(i)
		if (io /= 0) then
			print *, 'Error reading diagonal B1 at record', 3+k+i
			stop
		endif
	end do
	do i = 1, k-1
		read(10, rec=3+2*k-1+i, IOSTAT=io) B2(i)
		if (io /= 0) then
			print *, 'Error reading diagonal B2 at record', 3+2*k-1+i
			stop
		endif
	end do
	do i = 1, k-l
		read(10, rec=3+3*k-l-1+i, IOSTAT=io) C1(i)
		if (io /= 0) then
			print *, 'Error reading diagonal C1 at record', 3+3*k-l-1+i
			stop
		endif
	end do
	do i = 1, k-m
		read(10, rec=3+4*k-2*l-m-1+i, IOSTAT=io) D1(i)
		if (io /= 0) then
			print *, 'Error reading diagonal D1 at record', 3+4*k-2*l-m-1+i
			stop
		endif
	end do

	! Чтение вектора
	do i = 1, k
		read(11, rec=1+i, IOSTAT=io) E(i)
		if (io /= 0) then
			print *, 'Error reading vector data at record', 1+i
			stop
		endif
	end do

	close(10)
	return
end subroutine readdata

! Умножение матрицы на вектор
subroutine multiplying(A, B1, B2, C1, C2, D1, D2, E, F, k, l, m)
	implicit none
	real, dimension(k) :: A, E, F
	real, dimension(k-1) :: B1, B2
	real, dimension(k-l) :: C1, C2
	real, dimension(k-m) :: D1, D2
	integer :: k, l, m  
	integer :: i  

	! Умножение
	F(1) = (A(1) + B1(1)) * E(1)
	F(k) = (B2(k-1) + A(k)) * E(k)

	do i = 2, k-1
		F(i) = (B2(i-1) + A(i) + B1(i)) * E(i)
	end do
	do i = 1, k-m
		F(i) = F(i) + (C1(i) + D1(i)) * E(i)
	end do

	return
end subroutine multiplying

! Вывод результата
subroutine output(A, k)
	implicit none
	real, dimension(k) :: A
	integer :: k  ! Объявляем k как целое число
	integer :: i, io

	open(12, file='result.bin', ACCESS='direct', RECL=4, STATUS='unknown', IOSTAT=io)
	if (io /= 0) then
		print *, 'Error opening result.bin'
		stop
	endif

	do i = 1, k
		write(12, rec=i, IOSTAT=io) A(i)
		if (io /= 0) then
			print *, 'Error writing result at record', i
			stop
		endif
	end do

	close(12)
	return
end subroutine output
