module memory_module
    implicit none
    integer, parameter :: n = 1000000
    integer :: i, i1, i2
    real :: A(n), F(n)
    common /razm/ i, i1, i2

contains

! Чтение данных из файлов
subroutine vvod(A, n)
    implicit none
    real, dimension(:), intent(out) :: A
    integer, intent(in) :: n
    integer :: m, io
    integer :: k
    character(len=256) :: line

    ! Открытие файла матрицы
    open(10, FILE='matrix.txt', STATUS='old', IOSTAT=io)
    if (io /= 0) then
        print *, 'Error: Cannot open matrix.txt'
        stop
    endif

    ! Чтение размеров матрицы
    read(10, '(A)', IOSTAT=io) line
    read(line, *) i, i1, i2
    if (io /= 0) then
        print *, 'Error: Failed to read matrix dimensions'
        stop
    endif

    ! Проверка корректности размеров
    if (i1 <= 1 .or. i2 <= i1 .or. i <= i2) then
        print *, 'Error: Invalid matrix dimensions: i =', i, ', i1 =', i1, ', i2 =', i2
        stop
    endif

    if (5 * i - 2 * i1 - 2 * i2 - 2 > n) then
        print *, 'Error: Insufficient memory'
        stop
    endif

    ! Чтение главной диагонали
    read(10, '(A)', IOSTAT=io) line
    read(line, *) (A(k), k = 1, i)
    if (io /= 0) then
        print *, 'Error: Failed to read main diagonal'
        stop
    endif

    ! Чтение первой верхней диагонали
    read(10, '(A)', IOSTAT=io) line
    read(line, *) (A(i + k), k = 1, i - 1)
    if (io /= 0) then
        print *, 'Error: Failed to read first upper diagonal'
        stop
    endif

    ! Чтение второй верхней диагонали
    read(10, '(A)', IOSTAT=io) line
    read(line, *) (A(2 * i - 1 + k), k = 1, i - i2)
    if (io /= 0) then
        print *, 'Error: Failed to read second upper diagonal'
        stop
    endif

    ! Чтение первой нижней диагонали
    read(10, '(A)', IOSTAT=io) line
    read(line, *) (A(2 * i - 1 + (i - i2) + k), k = 1, i - 1)
    if (io /= 0) then
        print *, 'Error: Failed to read first lower diagonal'
        stop
    endif

    ! Чтение второй нижней диагонали
    read(10, '(A)', IOSTAT=io) line
    read(line, *) (A(3 * i - 2 + (i - i2) + k), k = 1, i - i1)
    if (io /= 0) then
        print *, 'Error: Failed to read second lower diagonal'
        stop
    endif

    close(10)

    ! Открытие файла вектора
    open(11, FILE='vector.txt', STATUS='old', IOSTAT=io)
    if (io /= 0) then
        print *, 'Error: Cannot open vector.txt'
        stop
    endif

    read(11, *, IOSTAT=io) m
    if (io /= 0) then
        print *, 'Error: Failed to read vector size'
        stop
    endif

    if (i /= m) then
        print *, 'Error: Vector size does not match matrix: i =', i, ', m =', m
        stop
    endif

    ! Чтение вектора
    read(11, *, IOSTAT=io) (F(k), k = 1, i)
    if (io /= 0) then
        print *, 'Error: Failed to read vector elements'
        stop
    endif

    close(11)
end subroutine vvod

! Умножение матрицы на вектор
subroutine multiplying_classical(A, F, i, i1, i2)
    implicit none
    real, dimension(:), intent(in) :: A
    real, dimension(:), intent(inout) :: F
    integer, intent(in) :: i, i1, i2
    integer :: k

    integer :: main_diag_start, upper1_diag_start, upper2_diag_start
    integer :: lower1_diag_start, lower2_diag_start
    real :: temp(i)

    ! Индексы для диагоналей
    main_diag_start = 1
    upper1_diag_start = main_diag_start + i
    upper2_diag_start = upper1_diag_start + (i - 1)
    lower1_diag_start = upper2_diag_start + (i - i2)
    lower2_diag_start = lower1_diag_start + (i - 1)

    ! Инициализация временного вектора
    temp = 0.0

    print *, "Debug: Starting matrix-vector multiplication"

    ! Главная диагональ
    do k = 1, i
        temp(k) = temp(k) + A(main_diag_start + k - 1) * F(k)
        print *, "Main diag: temp(", k, ") += A(", main_diag_start + k - 1, ") * F(", k, ") = ", A(main_diag_start + k - 1), "*", F(k)
    end do

    ! Первая верхняя диагональ
    do k = 1, i - 1
        temp(k) = temp(k) + A(upper1_diag_start + k - 1) * F(k + 1)
        print *, "Upper1 diag: temp(", k, ") += A(", upper1_diag_start + k - 1, ") * F(", k + 1, ") = ", A(upper1_diag_start + k - 1), "*", F(k + 1)
    end do

    ! Вторая верхняя диагональ
    do k = 1, i - i2
        temp(k) = temp(k) + A(upper2_diag_start + k - 1) * F(k + i2)
        print *, "Upper2 diag: temp(", k, ") += A(", upper2_diag_start + k - 1, ") * F(", k + i2, ") = ", A(upper2_diag_start + k - 1), "*", F(k + i2)
    end do

    ! Первая нижняя диагональ
    do k = 2, i
        temp(k) = temp(k) + A(lower1_diag_start + k - 2) * F(k - 1)
        print *, "Lower1 diag: temp(", k, ") += A(", lower1_diag_start + k - 2, ") * F(", k - 1, ") = ", A(lower1_diag_start + k - 2), "*", F(k - 1)
    end do

    ! Вторая нижняя диагональ
    do k = i1 + 1, i
        temp(k) = temp(k) + A(lower2_diag_start + k - i1 - 1) * F(k - i1)
        print *, "Lower2 diag: temp(", k, ") += A(", lower2_diag_start + k - i1 - 1, ") * F(", k - i1, ") = ", A(lower2_diag_start + k - i1 - 1), "*", F(k - i1)
    end do

    print *, "Debug: Finished matrix-vector multiplication"

    ! Копирование временного вектора в F
    F = temp
end subroutine multiplying_classical


! Запись результата в файл
subroutine output(F, i)
    implicit none
    real, dimension(:), intent(in) :: F
    integer, intent(in) :: i
    integer :: io

    open(12, FILE='result.txt', STATUS='unknown', IOSTAT=io)
    if (io /= 0) then
        print *, 'Error: Cannot open result.txt for writing'
        stop
    endif

    write(12, *) F(1:i)

    close(12)
end subroutine output

end module memory_module

! Главная программа
program memory
    use memory_module
    implicit none

    call vvod(A, n)
    call multiplying_classical(A, F, i, i1, i2)
    call output(F, i)

end program memory
