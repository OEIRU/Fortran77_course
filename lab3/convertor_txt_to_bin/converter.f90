program convert_to_binary
    implicit none
    integer :: n, i1, i2, i, io
    real, allocatable :: main_diag(:), upper_diag_1(:), upper_diag_2(:)
    real, allocatable :: lower_diag_1(:), lower_diag_2(:)

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

    ! Выделение памяти для диагоналей
    allocate(main_diag(n))             ! Главная диагональ
    allocate(upper_diag_1(n-1))       ! Первая верхняя диагональ
    allocate(upper_diag_2(n-i2))      ! Вторая верхняя диагональ
    allocate(lower_diag_1(n-1))       ! Первая нижняя диагональ
    allocate(lower_diag_2(n-i1))      ! Вторая нижняя диагональ

    ! Чтение диагоналей матрицы
    read(10, *, iostat=io) (main_diag(i), i=1, n)
    read(10, *, iostat=io) (upper_diag_1(i), i=1, n-1)
    read(10, *, iostat=io) (upper_diag_2(i), i=1, n-i2)
    read(10, *, iostat=io) (lower_diag_1(i), i=1, n-1)
    read(10, *, iostat=io) (lower_diag_2(i), i=1, n-i1)
    close(10)

    ! Отладочный вывод для проверки заполненности диагоналей
    print *, "Main diagonal:", main_diag
    print *, "First upper diagonal:", upper_diag_1
    print *, "Second upper diagonal:", upper_diag_2
    print *, "First lower diagonal:", lower_diag_1
    print *, "Second lower diagonal:", lower_diag_2

    ! Запись данных в бинарный файл matrix.bin
    open(20, file='matrix.bin', status='replace', access='stream', form='unformatted', iostat=io)
    if (io /= 0) then
        print *, 'Error: Cannot open file matrix.bin for writing'
        stop
    end if

    write(20) n, i1, i2                 ! Записываем размеры
    write(20) main_diag                 ! Записываем главную диагональ
    write(20) upper_diag_1              ! Записываем первую верхнюю диагональ
    write(20) upper_diag_2              ! Записываем вторую верхнюю диагональ
    write(20) lower_diag_1              ! Записываем первую нижнюю диагональ
    write(20) lower_diag_2              ! Записываем вторую нижнюю диагональ
    close(20)

    print *, "Matrix successfully written to matrix.bin"
end program convert_to_binary
