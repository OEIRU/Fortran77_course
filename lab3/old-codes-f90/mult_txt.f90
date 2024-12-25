program matrix_multiply
    implicit none

    integer, parameter :: max_size = 1000000
    integer :: n, i1, i2
    real :: A(5, max_size)  ! Хранение диагоналей в двумерном массиве
    real :: F(max_size), result(max_size)

    ! Выполнение программы
    call read_matrix('matrix.txt', n, i1, i2, A)
    call read_vector('vector.txt', n, F)
    call multiply_matrix_vector(n, i1, i2, A, F, result)
    call write_vector('result_1.txt', n, F)

    print *, 'Matrix-vector multiplication completed successfully.'

contains

    ! Чтение матрицы из текстового файла
    subroutine read_matrix(filename, n, i1, i2, A)
        character(len=*), intent(in) :: filename
        integer, intent(out) :: n, i1, i2
        real, intent(out) :: A(5, max_size)
        integer :: io, i

        open(10, file=filename, status='old', iostat=io)
        if (io /= 0) then
            print *, 'Error: Cannot open file ', trim(filename)
            stop
        end if

        ! Чтение размеров матрицы
        read(10, *, iostat=io) n, i1, i2
        if (io /= 0) then
            print *, 'Error: Failed to read dimensions from file'
            stop
        end if

        ! Чтение всех диагоналей
        read(10, *, iostat=io) (A(1, i), i=1, n)    ! Главная диагональ
        read(10, *, iostat=io) (A(2, i), i=1, n-1)  ! Первая верхняя диагональ
        read(10, *, iostat=io) (A(3, i), i=1, n-i2) ! Вторая верхняя диагональ
        read(10, *, iostat=io) (A(4, i), i=1, n-1)  ! Первая нижняя диагональ
        read(10, *, iostat=io) (A(5, i), i=1, n-i1) ! Вторая нижняя диагональ

        if (io /= 0) then
            print *, 'Error: Failed to read matrix data from file'
            stop
        end if

        close(10)
    end subroutine read_matrix

    ! Чтение вектора из текстового файла
    subroutine read_vector(filename, n, F)
        character(len=*), intent(in) :: filename
        integer, intent(inout) :: n
        real, intent(out) :: F(max_size)
        integer :: io, i

        open(11, file=filename, status='old', iostat=io)
        if (io /= 0) then
            print *, 'Error: Cannot open file ', trim(filename)
            stop
        end if

        ! Чтение размера вектора
        read(11, *, iostat=io) n
        if (io /= 0) then
            print *, 'Error: Failed to read vector size from file'
            stop
        end if

        ! Чтение элементов вектора
        read(11, *, iostat=io) (F(i), i=1, n)
        if (io /= 0) then
            print *, 'Error: Failed to read vector data from file'
            stop
        end if

        close(11)
    end subroutine read_vector

    ! Умножение матрицы на вектор
subroutine multiply_matrix_vector(n, i1, i2, A, F, result)
    integer, intent(in) :: n, i1, i2
    real, intent(in) :: A(5, max_size)
    real, intent(inout) :: F(max_size)  ! INOUT, так как F изменяется
    real, intent(out) :: result(max_size)
    integer :: i

    do i = 1, n
        result(i) = A(1, i) * F(i)

        if (i > 1) result(i) = result(i) + A(4, i-1) * F(i-1)  ! Первая нижняя диагональ
        if (i > 2) result(i) = result(i) + A(5, i-2) * F(i-2)  ! Вторая нижняя диагональ

        if (i < n) result(i) = result(i) + A(2, i) * F(i+1)    ! Первая верхняя диагональ
        if (i <= n-i2) result(i) = result(i) + A(3, i) * F(i+i2)  ! Вторая верхняя диагональ
    end do

    F(1:n) = result(1:n)  ! F обновляется
end subroutine multiply_matrix_vector


    ! Запись вектора в текстовый файл
    subroutine write_vector(filename, n, F)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: n
        real, intent(in) :: F(max_size)
        integer :: io

        open(12, file=filename, status='replace', iostat=io)
        if (io /= 0) then
            print *, 'Error: Cannot write to file ', trim(filename)
            stop
        end if

        ! Запись элементов вектора
        write(12, '(F8.3)') F(1:n)
        close(12)

        print *, 'Result written to file: ', trim(filename)
    end subroutine write_vector

end program matrix_multiply
