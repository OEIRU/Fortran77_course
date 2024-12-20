program memory
    implicit none
    integer, parameter :: n = 1000000
    real :: A(n), F(n)
    integer :: i, i1, i2
    common /razm/ i, i1, i2

    ! Ввод данных
    call vvod(A, n)

    ! Умножение матрицы на вектор
    call multiplying(A, F, i, i1, i2)

    ! Вывод результата
    call output(F, i)

    stop
end program memory

subroutine vvod(A, n)
    implicit none
    real, dimension(n) :: A
    integer :: n, m, io
    integer :: i, i1, i2
    common /razm/ i, i1, i2

    ! Открытие файла матрицы
    open(10, FILE='matrix.txt', STATUS='old', IOSTAT=io)
    if (io /= 0) then
        print *, 'Error: Cannot open matrix.txt'
        stop
    endif

    ! Чтение размерности матрицы и диагоналей
    read(10, *, IOSTAT=io) i, i1, i2
    if (io /= 0) then
        print *, 'Error: Failed to read matrix dimensions'
        stop
    endif

    ! Проверка корректности матрицы
    if (i1 <= 1 .or. i2 <= i1 .or. i <= i1) then
        print *, 'Error: Matrix dimensions are invalid: i =', i, ', i1 =', i1, ', i2 =', i2
        stop
    endif
    close(10)

    ! Открытие файла вектора
    open(11, FILE='vector.txt', STATUS='old', IOSTAT=io)
    if (io /= 0) then
        print *, 'Error: Cannot open vector.txt'
        stop
    endif

    ! Чтение размерности вектора
    read(11, *, IOSTAT=io) m
    if (io /= 0) then
        print *, 'Error: Failed to read vector size'
        stop
    endif

    ! Проверка вектора
    if (i /= m) then
        print *, 'Error: Vector size does not match matrix: i =', i, ', m =', m
        stop
    endif
    close(11)

    ! Проверка нехватки памяти
    if (9 * i - 2 * i1 - 2 * i2 - 2 > n) then
        print *, 'Error: Shortage of memory'
        stop
    endif

    ! Чтение данных
    call readdata(A, i, i1, i2)
end subroutine vvod

subroutine readdata(A, i, i1, i2)
    implicit none
    real, dimension(:) :: A
    integer :: i, i1, i2, io
    integer :: k

    ! Открытие файла матрицы для чтения данных
    open(10, FILE='matrix.txt', STATUS='old', IOSTAT=io)
    if (io /= 0) then
        print *, 'Error: Cannot open matrix.txt for data reading'
        stop
    endif

    ! Чтение матрицы
    read(10, *, IOSTAT=io) (A(k), k=1, i)
    if (io /= 0) then
        print *, 'Error: Failed to read matrix data for A'
        stop
    endif

    close(10)

    ! Чтение вектора
    open(11, FILE='vector.txt', STATUS='old', IOSTAT=io)
    if (io /= 0) then
        print *, 'Error: Cannot open vector.txt for data reading'
        stop
    endif

    read(11, *, IOSTAT=io) (A(k), k=1, i)
    if (io /= 0) then
        print *, 'Error: Failed to read vector data'
        stop
    endif

    close(11)
end subroutine readdata

subroutine multiplying(A, F, i, i1, i2)
    implicit none
    real, dimension(:) :: A, F
    integer :: i, i1, i2, k

    ! Умножение
    F(1) = (A(1) + A(1)) * A(1)
    F(i) = (A(i) + A(i)) * A(i)

    do k = 2, i-1
        F(k) = (A(k-1) + A(k) + A(k+1)) * A(k)
    end do

    do k = 1, i-i2
        F(k) = F(k) + (A(k) + A(k+1)) * A(k)
    end do
end subroutine multiplying

subroutine output(F, i)
    implicit none
    real, dimension(:) :: F
    integer :: i, k, io

    open(12, FILE='result.txt', STATUS='unknown', IOSTAT=io)
    if (io /= 0) then
        print *, 'Error: Cannot open result.txt for writing'
        stop
    endif

    write(12, *, IOSTAT=io) (F(k), k=1, i)
    if (io /= 0) then
        print *, 'Error: Failed to write result to result.txt'
        stop
    endif

    close(12)
end subroutine output
