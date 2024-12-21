program matrix_vector_multiply_bin
    implicit none

    ! Константы и переменные
    integer, parameter :: max_size = 1000000
    integer :: n, i1, i2
    real :: A(5, max_size)  ! Матрица, хранящаяся в виде 5 диагоналей
    real :: F(max_size), result(max_size)

    ! Объявление подпрограмм
    call read_matrix_binary('matrix.bin')   ! Чтение матрицы
    call read_vector_binary('vector.bin')   ! Чтение вектора
    call multiply_matrix_vector()           ! Умножение
    call write_vector_binary('result.bin')  ! Запись результата в бинарный файл
    call write_vector_text('result.txt')    ! Запись результата в текстовый файл

    print *, 'Multiplication completed successfully.'

contains

    ! Чтение бинарного файла матрицы
    subroutine read_matrix_binary(filename)
    character(len=*), intent(in) :: filename
    integer :: io, i

    ! Открываем бинарный файл
    open(10, file=filename, status='old', access='stream', form='unformatted', iostat=io)
    if (io /= 0) then
        print *, 'Error: Cannot open file ', filename
        stop
    end if

    ! Читаем размеры
    read(10, iostat=io) n, i1, i2
    if (io /= 0) then
        print *, 'Error: Failed to read dimensions from binary file'
        stop
    end if

    ! Читаем диагонали матрицы по отдельности
    read(10, iostat=io) (A(1, i), i=1, n)        ! Главная диагональ
    if (io /= 0) then
        print *, 'Error: Failed to read main diagonal'
        stop
    end if

    read(10, iostat=io) (A(2, i), i=1, n-1)      ! Первая верхняя диагональ
    if (io /= 0) then
        print *, 'Error: Failed to read first upper diagonal'
        stop
    end if

    read(10, iostat=io) (A(3, i), i=1, n-i2)     ! Вторая верхняя диагональ
    if (io /= 0) then
        print *, 'Error: Failed to read second upper diagonal'
        stop
    end if

    read(10, iostat=io) (A(4, i), i=1, n-1)      ! Первая нижняя диагональ
    if (io /= 0) then
        print *, 'Error: Failed to read first lower diagonal'
        stop
    end if

    read(10, iostat=io) (A(5, i), i=1, n-i1)     ! Вторая нижняя диагональ
    if (io /= 0) then
        print *, 'Error: Failed to read second lower diagonal'
        stop
    end if

    close(10)
end subroutine read_matrix_binary


    ! Чтение бинарного файла вектора
   subroutine read_vector_binary(filename)
    character(len=*), intent(in) :: filename
    integer :: io, i

    ! Открываем бинарный файл
    open(11, file=filename, status='old', access='stream', form='unformatted', iostat=io)
    if (io /= 0) then
        print *, 'Error: Cannot open file ', filename
        stop
    end if

    ! Читаем размер вектора
    read(11, iostat=io) n
    if (io /= 0) then
        print *, 'Error: Failed to read vector size from binary file'
        stop
    end if

    ! Читаем данные вектора
    read(11, iostat=io) (F(i), i=1, n)
    if (io /= 0) then
        print *, 'Error: Failed to read vector data from binary file'
        stop
    end if

    close(11)

end subroutine read_vector_binary

    ! Умножение матрицы на вектор
    subroutine multiply_matrix_vector()
        integer :: i

        do i = 1, n
            result(i) = A(1, i) * F(i)

            if (i > 1) result(i) = result(i) + A(4, i-1) * F(i-1)  ! Первая нижняя диагональ
            if (i > 2) result(i) = result(i) + A(5, i-2) * F(i-2)  ! Вторая нижняя диагональ

            if (i < n) result(i) = result(i) + A(2, i) * F(i+1)    ! Первая верхняя диагональ
            if (i <= n-i2) result(i) = result(i) + A(3, i) * F(i+i2)  ! Вторая верхняя диагональ

        end do

        F(1:n) = result(1:n)
    end subroutine multiply_matrix_vector

    ! Запись результата в бинарный файл
    subroutine write_vector_binary(filename)
        character(len=*), intent(in) :: filename
        integer :: io

        open(12, file=filename, status='replace', access='stream', form='unformatted', iostat=io)
        if (io /= 0) then
            print *, 'Error: Cannot write to file ', filename
            stop
        end if

        write(12) n
        write(12) F(1:n)
        close(12)

        print *, "Result vector written to binary file."
    end subroutine write_vector_binary

    ! Запись результата в текстовый файл
    subroutine write_vector_text(filename)
        character(len=*), intent(in) :: filename
        integer :: io, i

        open(13, file=filename, status='replace', iostat=io)
        if (io /= 0) then
            print *, 'Error: Cannot write to file ', filename
            stop
        end if

        write(13, *) "Result vector (size = ", n, "):"
        do i = 1, n
            write(13, *) F(i)
        end do
        close(13)

        print *, "Result vector written to text file."
    end subroutine write_vector_text

end program matrix_vector_multiply_bin
