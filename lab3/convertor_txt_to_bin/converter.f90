program convert_to_binary
    implicit none
    integer :: n, i1, i2, i, io
    real, allocatable :: A(:,:), F(:)

    ! Чтение текстового файла matrix.txt
    open(10, file='matrix.txt', status='old', iostat=io)
    if (io /= 0) then
        print *, 'Error: Cannot open file matrix.txt'
        stop
    end if

    ! Чтение размеров матрицы
    read(10, *, iostat=io) n, i1, i2
    if (io /= 0) then
        print *, 'Error: Failed to read dimensions from matrix.txt'
        stop
    end if

    allocate(A(5, n))
    A = 0.0

    ! Чтение диагоналей матрицы
    read(10, *, iostat=io) (A(1, i), i=1, n)    ! Главная диагональ
    read(10, *, iostat=io) (A(2, i), i=1, n-1)  ! Первая верхняя диагональ
    read(10, *, iostat=io) (A(3, i), i=1, n-i2) ! Вторая верхняя диагональ
    read(10, *, iostat=io) (A(4, i), i=1, n-1)  ! Первая нижняя диагональ
    read(10, *, iostat=io) (A(5, i), i=1, n-i1) ! Вторая нижняя диагональ
    close(10)

    ! Отладочный вывод для проверки заполненности массива A
    print *, "Matrix A to be written to binary file:"
    do i = 1, 5
        if (i == 1) then
            print *, A(i, 1:n)
        else if (i == 2 .or. i == 4) then
            print *, A(i, 1:n-1)
        else if (i == 3) then
            print *, A(i, 1:n-i2)
        else if (i == 5) then
            print *, A(i, 1:n-i1)
        end if
    end do

    ! Запись матрицы в бинарный файл
    open(20, file='matrix.bin', status='replace', access='stream', form='unformatted', iostat=io)
    if (io /= 0) then
        print *, 'Error: Cannot open file matrix.bin for writing'
        stop
    end if

    write(20) n, i1, i2       ! Записываем размеры
    write(20) A               ! Записываем данные матрицы
    close(20)

    print *, "Matrix successfully written to matrix.bin"
end program convert_to_binary
